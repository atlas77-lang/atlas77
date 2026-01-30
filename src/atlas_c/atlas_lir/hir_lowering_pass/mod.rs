use std::collections::HashMap;

use miette::NamedSource;

use crate::atlas_c::{
    atlas_hir::{
        HirModule,
        arena::HirArena,
        expr::{HirBinaryOperator, HirExpr, HirUnaryOp},
        item::{HirFunction, HirStruct, HirStructConstructor, HirStructMethod, HirUnion},
        monomorphization_pass::MonomorphizationPass,
        signature::{ConstantValue, HirStructMethodModifier},
        stmt::HirStatement,
        ty::{HirGenericTy, HirReferenceKind, HirTy},
    },
    atlas_lir::{
        error::{
            CurrentFunctionDoesntExistError, LirLoweringError, LirResult, NoReturnInFunctionError,
            UnknownTypeError, UnsupportedHirExprError,
        },
        program::{
            LirBlock, LirExternFunction, LirFunction, LirInstr, LirOperand, LirProgram, LirStruct,
            LirTerminator, LirTy, LirUnion,
        },
    },
    utils::{self, Span},
};

/// Hir to Lir lowering pass
///
/// This pass converts the Hir (after ownership analysis) into a simple SSA-like
/// Lir form suitable for optimization and final code generation.
pub struct HirLoweringPass<'hir> {
    hir_module: &'hir HirModule<'hir>,
    /// The function currently being lowered
    current_function: Option<LirFunction>,
    /// Counter for generating unique temp variable IDs
    temp_counter: u32,
    /// Counter for generating unique block labels
    block_counter: u32,
    /// Maps parameter names to their argument index
    param_map: HashMap<&'hir str, u8>,
    /// Maps local variable names to their temp ID
    local_map: HashMap<&'hir str, u32>,
    hir_arena: &'hir HirArena<'hir>,
}

impl<'hir> HirLoweringPass<'hir> {
    pub fn new(hir_module: &'hir HirModule<'hir>, hir_arena: &'hir HirArena<'hir>) -> Self {
        Self {
            hir_module,
            current_function: None,
            temp_counter: 0,
            block_counter: 0,
            param_map: HashMap::new(),
            local_map: HashMap::new(),
            hir_arena,
        }
    }

    /// Lower the entire Hir module to Lir
    pub fn lower(&mut self) -> LirResult<LirProgram> {
        let mut functions = Vec::new();

        for func in self.hir_module.body.functions.values() {
            if func.signature.is_external {
                continue; // Skip extern functions
            }
            let lir_func = self.lower_function(func)?;
            functions.push(lir_func);
        }

        let mut extern_functions = Vec::new();
        for (name, sig) in &self.hir_module.signature.functions {
            if sig.is_external {
                let lir_extern_func = LirExternFunction {
                    name: name.to_string(),
                    args: sig
                        .params
                        .iter()
                        .map(|p| self.hir_ty_to_lir_ty(p.ty, p.span))
                        .collect(),
                    return_type: {
                        let lir_ty = self.hir_ty_to_lir_ty(&sig.return_ty, sig.span);
                        if lir_ty == LirTy::Unit {
                            None
                        } else {
                            Some(lir_ty)
                        }
                    },
                };
                extern_functions.push(lir_extern_func);
            }
        }

        let mut structs = Vec::new();
        for body in self.hir_module.body.structs.values() {
            structs.push(self.lower_struct(body, &mut functions)?);
        }
        let mut unions = Vec::new();
        for body in self.hir_module.body.unions.values() {
            unions.push(self.lower_union(body)?);
        }

        Ok(LirProgram {
            functions,
            extern_functions,
            structs,
            unions,
        })
    }

    /// Generate a new unique temp variable
    fn new_temp(&mut self) -> LirOperand {
        let id = self.temp_counter;
        self.temp_counter += 1;
        LirOperand::Temp(id)
    }

    /// Generate a new unique block label
    fn new_block_label(&mut self, prefix: &str) -> String {
        let id = self.block_counter;
        self.block_counter += 1;
        format!("{}_{}", prefix, id)
    }

    /// Creates a new block and returns its label
    fn create_block(&mut self, label: String) -> LirResult<String> {
        if let Some(func) = &mut self.current_function {
            func.blocks.push(LirBlock {
                label: label.clone(),
                instructions: Vec::new(),
                terminator: LirTerminator::None,
            });
            Ok(label)
        } else {
            Err(Box::new(LirLoweringError::CurrentFunctionDoesntExist(
                CurrentFunctionDoesntExistError,
            )))
        }
    }

    /// Push an instruction to the current (last) block
    fn emit(&mut self, instr: LirInstr) -> LirResult<()> {
        if let Some(func) = &mut self.current_function
            && let Some(block) = func.blocks.last_mut()
        {
            block.instructions.push(instr);
            return Ok(());
        }
        Err(Box::new(LirLoweringError::CurrentFunctionDoesntExist(
            CurrentFunctionDoesntExistError,
        )))
    }

    fn already_has_terminator(&mut self) -> LirResult<bool> {
        if let Some(func) = &mut self.current_function
            && let Some(block) = func.blocks.last_mut()
        {
            return Ok(!matches!(block.terminator, LirTerminator::None));
        }

        Err(Box::new(LirLoweringError::CurrentFunctionDoesntExist(
            CurrentFunctionDoesntExistError,
        )))
    }

    fn emit_terminator(&mut self, terminator: LirTerminator) -> LirResult<()> {
        if let Some(func) = &mut self.current_function {
            if let Some(block) = func.blocks.last_mut() {
                block.terminator = terminator;
                Ok(())
            } else {
                Err(Box::new(LirLoweringError::CurrentFunctionDoesntExist(
                    CurrentFunctionDoesntExistError,
                )))
            }
        } else {
            Err(Box::new(LirLoweringError::CurrentFunctionDoesntExist(
                CurrentFunctionDoesntExistError,
            )))
        }
    }

    fn lower_union(&mut self, union_body: &'hir HirUnion<'hir>) -> LirResult<LirUnion> {
        let lir_union = LirUnion {
            name: union_body.name.to_string(),
            variants: union_body
                .signature
                .variants
                .iter()
                .map(|(variant_name, variant_sig)| {
                    (
                        variant_name.to_string(),
                        self.hir_ty_to_lir_ty(variant_sig.ty, variant_sig.span),
                    )
                })
                .collect(),
        };

        Ok(lir_union)
    }

    fn lower_struct(
        &mut self,
        struct_body: &'hir HirStruct<'hir>,
        functions: &mut Vec<LirFunction>,
    ) -> LirResult<LirStruct> {
        let lir_struct = LirStruct {
            name: struct_body.name.to_string(),
            fields: struct_body
                .signature
                .fields
                .iter()
                .map(|(field_name, field_sig)| {
                    (
                        field_name.to_string(),
                        self.hir_ty_to_lir_ty(field_sig.ty, field_sig.span),
                    )
                })
                .collect(),
        };

        for method in struct_body.methods.iter() {
            let lir_method = self.lower_method(struct_body.name, method)?;
            functions.push(lir_method);
        }

        functions.push(self.lower_constructor(
            struct_body.name,
            &struct_body.constructor,
            "new",
        )?);

        Ok(lir_struct)
    }

    /// Lower a struct constructor
    /// kind can be "new"/"copy"/"default"/"move"
    /// TODO: Add an enum for kind instead of using &str
    fn lower_constructor(
        &mut self,
        struct_name: &str,
        ctor: &'hir HirStructConstructor<'hir>,
        kind: &str,
    ) -> LirResult<LirFunction> {
        // Reset state for new function
        self.temp_counter = 0;
        self.block_counter = 0;
        self.param_map.clear();
        self.local_map.clear();

        self.param_map.insert("this", 0);
        let mut args = Vec::new();
        args.push(LirTy::Ptr {
            is_const: false,
            inner: Box::new(LirTy::StructType(struct_name.to_string())),
        });
        // Build parameter map
        for (idx, param) in ctor.params.iter().enumerate() {
            self.param_map.insert(param.name, (idx + 1) as u8);
            args.push(self.hir_ty_to_lir_ty(param.ty, param.span));
        }

        // Initialize current function with entry block
        self.current_function = Some(LirFunction {
            name: format!("{}_{}", struct_name, kind),
            args,
            // a constructor does NOT have a return type. It constructs the object in-place
            return_type: Some(LirTy::Unit),
            blocks: vec![LirBlock {
                label: "entry".to_string(),
                instructions: Vec::new(),
                terminator: LirTerminator::None,
            }],
        });

        // Lower the function body
        for stmt in &ctor.body.statements {
            self.lower_stmt(stmt)?;
        }

        // Take the completed function and clean up dead blocks
        let mut result = self.current_function.take().unwrap();
        result.remove_dead_blocks();
        Ok(result)
    }

    fn lower_method(
        &mut self,
        struct_name: &str,
        method: &'hir HirStructMethod<'hir>,
    ) -> LirResult<LirFunction> {
        // Reset state for new function
        self.temp_counter = 0;
        self.block_counter = 0;
        self.param_map.clear();
        self.local_map.clear();

        let mut args = Vec::new();
        if matches!(
            method.signature.modifier,
            |HirStructMethodModifier::Mutable| HirStructMethodModifier::Consuming
        ) {
            // The first parameter is always "this"
            self.param_map.insert("this", 0);
            args.push(LirTy::Ptr {
                is_const: false,
                inner: Box::new(LirTy::StructType(struct_name.to_string())),
            });
        } else {
            self.param_map.insert("this", 0);
            args.push(LirTy::Ptr {
                is_const: true,
                inner: Box::new(LirTy::StructType(struct_name.to_string())),
            });
        }
        // Build parameter map
        for param in method.signature.params.iter() {
            let idx = self.param_map.len();
            self.param_map.insert(param.name, idx as u8);
            args.push(self.hir_ty_to_lir_ty(param.ty, param.span));
        }

        // Initialize current function with entry block
        self.current_function = Some(LirFunction {
            name: format!("{}_{}", struct_name, method.name),
            args,
            return_type: {
                let lir_ty =
                    self.hir_ty_to_lir_ty(&method.signature.return_ty, method.signature.span);
                if lir_ty == LirTy::Unit {
                    None
                } else {
                    Some(lir_ty)
                }
            },
            blocks: vec![LirBlock {
                label: "entry".to_string(),
                instructions: Vec::new(),
                terminator: LirTerminator::None,
            }],
        });

        // Lower the function body
        for stmt in &method.body.statements {
            self.lower_stmt(stmt)?;
        }

        // Take the completed function and clean up dead blocks
        let mut result = self.current_function.take().unwrap();
        result.remove_dead_blocks();
        // TODO: This is very goofy and should never be done, I'll rework it later to properly work
        self.current_function = Some(result);
        if let Some(b) = self.current_function.as_ref().unwrap().blocks.last() {
            if method.signature.return_ty.is_unit() {
                if matches!(b.terminator, LirTerminator::None) {
                    // For methods returning unit, ensure there's a return at the end

                    self.emit_terminator(LirTerminator::Return { value: None })?;
                }
            } else if !matches!(
                b.terminator,
                LirTerminator::Return { value: Some(_) } | LirTerminator::Halt
            ) {
                // It should return something, but doesn't
                // TODO: Add a ! type so if the last statement is a call to a function returning !, we don't error
                // TODO: Add CFG analysis to check all paths because right now only the else branch has to return,
                //  the if branch can just fallthrough
                return Err(Box::new(LirLoweringError::NoReturnInFunction(
                    NoReturnInFunctionError {
                        name: method.name.to_string(),
                    },
                )));
            }
        }
        let result = self.current_function.take().unwrap();
        Ok(result)
    }

    /// Lower a single function
    fn lower_function(&mut self, func: &'hir HirFunction<'hir>) -> LirResult<LirFunction> {
        // Reset state for new function
        self.temp_counter = 0;
        self.block_counter = 0;
        self.param_map.clear();
        self.local_map.clear();

        // Build parameter map
        for (idx, param) in func.signature.params.iter().enumerate() {
            self.param_map.insert(param.name, idx as u8);
        }

        // Initialize current function with entry block
        self.current_function = Some(LirFunction {
            name: func.name.to_string(),
            args: func
                .signature
                .params
                .iter()
                .map(|p| self.hir_ty_to_lir_ty(p.ty, p.span))
                .collect(),
            return_type: {
                let lir_ty = self.hir_ty_to_lir_ty(&func.signature.return_ty, func.signature.span);
                if lir_ty == LirTy::Unit {
                    None
                } else {
                    Some(lir_ty)
                }
            },
            blocks: vec![LirBlock {
                label: "entry".to_string(),
                instructions: Vec::new(),
                terminator: LirTerminator::None,
            }],
        });

        // Lower the function body
        for stmt in &func.body.statements {
            self.lower_stmt(stmt)?;
        }

        // Take the completed function and clean up dead blocks
        let mut result = self.current_function.take().unwrap();
        result.remove_dead_blocks();
        // TODO: This is very goofy and should never be done, I'll rework it later to properly work
        self.current_function = Some(result);
        if let Some(b) = self.current_function.as_ref().unwrap().blocks.last() {
            if func.signature.return_ty.is_unit() {
                if matches!(b.terminator, LirTerminator::None) {
                    // For functions returning unit, ensure there's a return at the end
                    if func.name == "main" {
                        self.emit_terminator(LirTerminator::Halt)?;
                    } else {
                        self.emit_terminator(LirTerminator::Return { value: None })?;
                    }
                }
            } else if !matches!(
                b.terminator,
                LirTerminator::Return { value: Some(_) } | LirTerminator::Halt
            ) {
                // It should return something, but doesn't
                // TODO: Add a ! type so if the last statement is a call to a function returning !, we don't error
                // TODO: Add CFG analysis to check all paths because right now only the else branch has to return,
                //  the if branch can just fallthrough
                return Err(Box::new(LirLoweringError::NoReturnInFunction(
                    NoReturnInFunctionError {
                        name: func.name.to_string(),
                    },
                )));
            }
        }
        let result = self.current_function.take().unwrap();
        Ok(result)
    }

    /// Lower a statement
    fn lower_stmt(&mut self, stmt: &'hir HirStatement<'hir>) -> LirResult<()> {
        match stmt {
            HirStatement::Return(ret) => {
                let value = self.lower_expr(&ret.value)?;
                self.emit_terminator(LirTerminator::Return { value: Some(value) })?;
            }
            HirStatement::IfElse(if_else) => {
                // Lower condition
                let cond = self.lower_expr(&if_else.condition)?;

                // Create block labels
                let then_label = self.new_block_label("then");
                let else_label = self.new_block_label("else");
                let merge_label = self.new_block_label("merge");

                // Emit branch
                self.emit_terminator(LirTerminator::BranchIf {
                    condition: cond,
                    then_label: then_label.clone(),
                    else_label: else_label.clone(),
                })?;

                // === Then block ===
                self.create_block(then_label)?;
                for stmt in &if_else.then_branch.statements {
                    self.lower_stmt(stmt)?;
                }
                // Jump to merge if the terminator is not already set
                if !self.already_has_terminator()? {
                    self.emit_terminator(LirTerminator::Branch {
                        target: merge_label.clone(),
                    })?;
                }

                // === Else block ===
                self.create_block(else_label)?;
                if let Some(else_branch) = &if_else.else_branch {
                    for stmt in &else_branch.statements {
                        self.lower_stmt(stmt)?;
                    }
                }

                // === Merge block (may be unused if both branches return) ===
                self.create_block(merge_label)?;
            }
            HirStatement::Expr(expr_stmt) => {
                // Lower expression for side effects, discard result
                self.lower_expr(&expr_stmt.expr)?;
            }
            HirStatement::Const(const_stmt) => {
                let value = self.lower_expr(&const_stmt.value)?;

                if let LirOperand::Temp(id) = value {
                    self.local_map.insert(const_stmt.name, id);
                } else {
                    // Immediate values don't generate temps, so load them into one
                    let temp = self.new_temp();
                    self.emit(LirInstr::LoadImm {
                        ty: self.hir_ty_to_lir_ty(const_stmt.ty, const_stmt.span),
                        dst: temp.clone(),
                        value,
                    })?;
                    if let LirOperand::Temp(id) = temp {
                        self.local_map.insert(const_stmt.name, id);
                    } else {
                        panic!("Expected a temp operand");
                    }
                }
            }
            HirStatement::Let(let_stmt) => {
                let value = self.lower_expr(&let_stmt.value)?;

                if let LirOperand::Temp(id) = value {
                    self.local_map.insert(let_stmt.name, id);
                } else {
                    // Immediate values don't generate temps, so load them into one
                    let temp = self.new_temp();
                    self.emit(LirInstr::LoadImm {
                        ty: self.hir_ty_to_lir_ty(let_stmt.ty, let_stmt.span),
                        dst: temp.clone(),
                        value,
                    })?;
                    if let LirOperand::Temp(id) = temp {
                        self.local_map.insert(let_stmt.name, id);
                    } else {
                        panic!("Expected a temp operand");
                    }
                }
            }
            HirStatement::Assign(assign) => {
                let value = self.lower_expr(&assign.val)?;
                let l_value = self.lower_assign_l_value(&assign.dst)?;
                self.emit(LirInstr::Assign {
                    ty: self.hir_ty_to_lir_ty(assign.ty, assign.span),
                    dst: l_value,
                    src: value,
                })?;
            }
            HirStatement::While(while_stmt) => {
                // Lower while loop
                let cond_label = self.new_block_label("while_cond");
                let body_label = self.new_block_label("while_body");
                let after_label = self.new_block_label("while_after");

                // Jump to condition check
                self.emit_terminator(LirTerminator::Branch {
                    target: cond_label.clone(),
                })?;

                // Condition block
                self.create_block(cond_label.clone())?;
                let cond = self.lower_expr(&while_stmt.condition)?;
                self.emit_terminator(LirTerminator::BranchIf {
                    condition: cond,
                    then_label: body_label.clone(),
                    else_label: after_label.clone(),
                })?;

                // Body block
                self.create_block(body_label.clone())?;
                for stmt in &while_stmt.body.statements {
                    self.lower_stmt(stmt)?;
                }
                // After body, jump back to condition
                self.emit_terminator(LirTerminator::Branch {
                    target: cond_label.clone(),
                })?;

                // After block
                self.create_block(after_label.clone())?;
            }
            _ => {
                // For now, skip unsupported statements
                // In a complete implementation, handle all variants
            }
        }
        Ok(())
    }

    // Helper function to take care of the unary unwrapping for l-values
    fn lower_assign_l_value(&mut self, l_value: &'hir HirExpr<'hir>) -> LirResult<LirOperand> {
        match l_value {
            HirExpr::Unary(unary) if unary.op.is_none() => self.lower_assign_l_value(&unary.expr),
            HirExpr::Unary(unary) if matches!(unary.op, Some(HirUnaryOp::Deref)) => {
                self.lower_expr(l_value)
            }
            HirExpr::Ident(_) | HirExpr::FieldAccess(_) | HirExpr::Indexing(_) => {
                self.lower_expr(l_value)
            }
            _ => Err(unsupported_expr(l_value.span())),
        }
    }

    /// Lower an expression, returning the operand holding the result
    fn lower_expr(&mut self, expr: &'hir HirExpr<'hir>) -> LirResult<LirOperand> {
        match expr {
            // === Literals ===
            HirExpr::IntegerLiteral(lit) => Ok(LirOperand::ImmInt(lit.value)),

            HirExpr::UnsignedIntegerLiteral(lit) => Ok(LirOperand::ImmUInt(lit.value)),

            HirExpr::BooleanLiteral(lit) => Ok(LirOperand::ImmBool(lit.value)),

            HirExpr::CharLiteral(lit) => Ok(LirOperand::ImmChar(lit.value)),

            HirExpr::StringLiteral(lit) => {
                let dest = self.new_temp();
                self.emit(LirInstr::LoadConst {
                    dst: dest.clone(),
                    value: LirOperand::Const(ConstantValue::String(String::from(lit.value))),
                })?;
                Ok(dest)
            }

            HirExpr::UnitLiteral(_) => Ok(LirOperand::ImmUnit),

            HirExpr::ThisLiteral(_) => {
                // "this" is always the first argument (arg 0)
                Ok(LirOperand::Arg(0))
            }

            // === Casting ===
            HirExpr::Casting(casting_expr) => {
                let expr_operand = self.lower_expr(&casting_expr.expr)?;
                let dest = self.new_temp();
                let target_ty = self.hir_ty_to_lir_ty(casting_expr.target_ty, casting_expr.span);
                self.emit(LirInstr::Cast {
                    ty: target_ty,
                    from: self.hir_ty_to_lir_ty(casting_expr.expr.ty(), casting_expr.span),
                    dst: dest.clone(),
                    src: expr_operand,
                })?;
                Ok(dest)
            }

            // === Identifiers (variables/parameters) ===
            HirExpr::Ident(ident) => {
                // Check if it's a parameter
                if let Some(&arg_idx) = self.param_map.get(ident.name) {
                    Ok(LirOperand::Arg(arg_idx))
                }
                // Check if it's a local variable
                else if let Some(&temp_id) = self.local_map.get(ident.name) {
                    Ok(LirOperand::Temp(temp_id))
                } else {
                    // Unknown identifier - shouldn't happen after type checking
                    panic!("Unknown identifier: {}", ident.name);
                }
            }

            HirExpr::Unary(unary) => match unary.op {
                Some(HirUnaryOp::Deref) => {
                    let expr_operand = self.lower_expr(&unary.expr)?;
                    Ok(LirOperand::Deref(Box::new(expr_operand)))
                }
                Some(HirUnaryOp::AsRef) => {
                    let expr_operand = self.lower_expr(&unary.expr)?;
                    Ok(LirOperand::AsRef(Box::new(expr_operand)))
                }
                Some(HirUnaryOp::Neg) => {
                    let expr_operand = self.lower_expr(&unary.expr)?;
                    let dest = self.new_temp();
                    let ty = self.hir_ty_to_lir_ty(unary.ty, unary.span);
                    self.emit(LirInstr::Negate {
                        ty,
                        dest: dest.clone(),
                        src: expr_operand,
                    })?;
                    Ok(dest)
                }
                Some(HirUnaryOp::Not) => {
                    let expr_operand = self.lower_expr(&unary.expr)?;
                    let dest = self.new_temp();
                    let ty = self.hir_ty_to_lir_ty(unary.ty, unary.span);
                    self.emit(LirInstr::Not {
                        ty,
                        dest: dest.clone(),
                        src: expr_operand,
                    })?;
                    Ok(dest)
                }
                _ => self.lower_expr(&unary.expr),
            },

            // === Binary operations ===
            HirExpr::HirBinaryOperation(binop) => {
                let lhs = self.lower_expr(&binop.lhs)?;
                let rhs = self.lower_expr(&binop.rhs)?;
                let dest = self.new_temp();

                let ty = self.hir_ty_to_lir_ty(binop.ty, binop.span);

                let instr = match binop.op {
                    HirBinaryOperator::Add => LirInstr::Add {
                        ty,
                        dest: dest.clone(),
                        a: lhs,
                        b: rhs,
                    },
                    HirBinaryOperator::Sub => LirInstr::Sub {
                        ty,
                        dest: dest.clone(),
                        a: lhs,
                        b: rhs,
                    },
                    HirBinaryOperator::Mul => LirInstr::Mul {
                        ty,
                        dest: dest.clone(),
                        a: lhs,
                        b: rhs,
                    },
                    HirBinaryOperator::Div => LirInstr::Div {
                        ty,
                        dest: dest.clone(),
                        a: lhs,
                        b: rhs,
                    },
                    HirBinaryOperator::Mod => LirInstr::Mod {
                        ty,
                        dest: dest.clone(),
                        a: lhs,
                        b: rhs,
                    },
                    HirBinaryOperator::Lt => LirInstr::LessThan {
                        ty,
                        dest: dest.clone(),
                        a: lhs,
                        b: rhs,
                    },
                    HirBinaryOperator::Lte => LirInstr::LessThanOrEqual {
                        ty,
                        dest: dest.clone(),
                        a: lhs,
                        b: rhs,
                    },
                    HirBinaryOperator::Gt => LirInstr::GreaterThan {
                        ty,
                        dest: dest.clone(),
                        a: lhs,
                        b: rhs,
                    },
                    HirBinaryOperator::Gte => LirInstr::GreaterThanOrEqual {
                        ty,
                        dest: dest.clone(),
                        a: lhs,
                        b: rhs,
                    },
                    HirBinaryOperator::Eq => LirInstr::Equal {
                        ty,
                        dest: dest.clone(),
                        a: lhs,
                        b: rhs,
                    },
                    HirBinaryOperator::Neq => LirInstr::NotEqual {
                        ty,
                        dest: dest.clone(),
                        a: lhs,
                        b: rhs,
                    },
                    _ => {
                        return Err(unsupported_expr(expr.span()));
                    }
                };

                self.emit(instr)?;
                Ok(dest)
            }

            // === Constructor ===
            HirExpr::NewObj(new_obj) => {
                let mut args = Vec::new();
                for arg in &new_obj.args {
                    args.push(self.lower_expr(arg)?);
                }

                let dest = self.new_temp();

                self.emit(LirInstr::Construct {
                    ty: self.hir_ty_to_lir_ty(new_obj.ty, new_obj.span),
                    dst: dest.clone(),
                    args,
                })?;
                Ok(dest)
            }
            // This just allocate on the stack an array with 0-initialized elements
            HirExpr::NewArray(new_arr) => {
                let dest = self.new_temp();
                let ty = self.hir_ty_to_lir_ty(new_arr.ty, new_arr.span);
                let size = match ty {
                    LirTy::ArrayTy { inner: _, size } => size,
                    _ => {
                        return Err(unknown_type_err(&ty.to_string(), new_arr.size.span()));
                    }
                };
                self.emit(LirInstr::ConstructArray {
                    dst: dest.clone(),
                    size,
                    ty,
                })?;
                Ok(dest)
            }

            // === ObjLiteral ===
            HirExpr::ObjLiteral(obj_lit) => {
                let mut args = Vec::new();
                for field_value in &obj_lit.fields {
                    let value_operand = self.lower_expr(&field_value.value)?;
                    args.push((field_value.name.to_string(), value_operand));
                }

                let dest = self.new_temp();

                self.emit(LirInstr::ConstructUnion {
                    ty: self.hir_ty_to_lir_ty(obj_lit.ty, obj_lit.span),
                    dst: dest.clone(),
                    field_values: args.into_iter().collect(),
                })?;
                Ok(dest)
            }

            // === Function calls ===
            HirExpr::Call(call) => {
                // Get function name from callee
                // "take_this" indicates if there is an implicit "this" argument
                let (func_name, take_this) = match call.callee.as_ref() {
                    HirExpr::Ident(ident) => {
                        if !call.generics.is_empty()
                            // If it's an external function, the name hasn't been mangled, so this returns false
                            // If it's an actual function in the module, the name is mangled in the signature, so this returns true
                            && !self.hir_module.signature.functions.contains_key(ident.name)
                        {
                            (
                                MonomorphizationPass::generate_mangled_name(
                                    self.hir_arena,
                                    &HirGenericTy {
                                        name: ident.name,
                                        inner: call
                                            .generics
                                            .iter()
                                            .map(|g| (*g).clone())
                                            .collect::<Vec<_>>(),
                                        span: ident.span,
                                    },
                                    "function",
                                )
                                .to_string(),
                                false,
                            )
                        } else {
                            (ident.name.to_string(), false)
                        }
                    }
                    HirExpr::StaticAccess(static_access) => {
                        let object_name = match static_access.target {
                            HirTy::Named(n) => n.name,
                            HirTy::Generic(g) => MonomorphizationPass::generate_mangled_name(
                                self.hir_arena,
                                g,
                                "struct",
                            ),
                            _ => {
                                return Err(unsupported_expr(expr.span()));
                            }
                        };
                        (
                            format!("{}_{}", object_name, static_access.field.name),
                            false,
                        )
                    }
                    HirExpr::FieldAccess(field_access) => {
                        let object_name = match field_access.target.ty() {
                            HirTy::Named(n) => n.name,
                            HirTy::Generic(g) => {
                                let mangled_name = MonomorphizationPass::generate_mangled_name(
                                    self.hir_arena,
                                    g,
                                    "struct",
                                );
                                if self.hir_module.signature.structs.contains_key(mangled_name) {
                                    mangled_name
                                } else {
                                    // Might be an union
                                    let mangled_name = MonomorphizationPass::generate_mangled_name(
                                        self.hir_arena,
                                        g,
                                        "union",
                                    );
                                    if self.hir_module.signature.unions.contains_key(mangled_name) {
                                        mangled_name
                                    } else {
                                        return Err(unsupported_expr(expr.span()));
                                    }
                                }
                            }
                            _ => {
                                return Err(unsupported_expr(expr.span()));
                            }
                        };
                        (format!("{}_{}", object_name, field_access.field.name), true)
                    }
                    _ => {
                        return Err(unsupported_expr(expr.span()));
                    }
                };
                // Lower arguments
                let mut args = Vec::new();
                if take_this {
                    if let HirExpr::FieldAccess(field_access) = call.callee.as_ref() {
                        let target_operand = self.lower_expr(&field_access.target)?;
                        args.push(target_operand);
                    } else {
                        return Err(unsupported_expr(expr.span()));
                    }
                }
                for arg in &call.args {
                    args.push(self.lower_expr(arg)?);
                }

                // Check if it's an external function
                let is_extern = self
                    .hir_module
                    .signature
                    .functions
                    .get(func_name.as_str())
                    .is_some_and(|f| f.is_external);

                let dest = if matches!(call.ty, HirTy::Unit(_)) {
                    None
                } else {
                    Some(self.new_temp())
                };

                let instr = if is_extern {
                    LirInstr::ExternCall {
                        ty: self.hir_ty_to_lir_ty(call.ty, call.span),
                        dst: dest.clone(),
                        func_name,
                        args,
                    }
                } else {
                    LirInstr::Call {
                        ty: self.hir_ty_to_lir_ty(call.ty, call.span),
                        dst: dest.clone(),
                        func_name,
                        args,
                    }
                };

                self.emit(instr)?;
                Ok(dest.unwrap_or(LirOperand::ImmInt(0))) // unit value
            }

            HirExpr::StaticAccess(_) => {
                let dst = self.new_temp();
                self.emit(LirInstr::LoadConst {
                    dst: dst.clone(),
                    value: LirOperand::Const(ConstantValue::String(
                        "There should be a static access here".to_string(),
                    )),
                })?;
                Ok(dst)
            }

            HirExpr::FieldAccess(field_access) => {
                let target_operand = self.lower_expr(&field_access.target)?;

                Ok(LirOperand::FieldAccess {
                    ty: self.hir_ty_to_lir_ty(field_access.target.ty(), field_access.target.span()),
                    src: Box::new(target_operand),
                    field_name: field_access.field.name.to_string(),
                })
            }

            HirExpr::Indexing(indexing_expr) => {
                let collection_operand = self.lower_expr(&indexing_expr.target)?;
                let index_operand = self.lower_expr(&indexing_expr.index)?;

                Ok(LirOperand::Index {
                    src: Box::new(collection_operand),
                    index: Box::new(index_operand),
                })
            }

            HirExpr::Copy(copy_expr) => {
                // For primitives, copy is just a value use
                // For objects, this would call the copy constructor
                self.lower_expr(&copy_expr.expr)
            }

            HirExpr::Delete(delete_expr) => {
                let dst = self.new_temp(); // Placeholder
                let src = self.lower_expr(&delete_expr.expr)?;
                let ty = self.hir_ty_to_lir_ty(delete_expr.expr.ty(), delete_expr.span);
                self.emit(LirInstr::Delete { ty, src })?;
                Ok(dst)
            }
            _ => Err(unsupported_expr(expr.span())),
        }
    }

    /// Convert HIR type to Lir type
    fn hir_ty_to_lir_ty(&self, ty: &HirTy, span: Span) -> LirTy {
        match ty {
            HirTy::Integer(i) => match i.size_in_bits {
                8 => LirTy::Int8,
                16 => LirTy::Int16,
                32 => LirTy::Int32,
                64 => LirTy::Int64,
                _ => {
                    let report: miette::Report =
                        (*unknown_type_err(&format!("{}", ty), span)).into();
                    eprintln!("{:?}", report);
                    std::process::exit(1);
                }
            },
            HirTy::UnsignedInteger(ui) => match ui.size_in_bits {
                8 => LirTy::UInt8,
                16 => LirTy::UInt16,
                32 => LirTy::UInt32,
                64 => LirTy::UInt64,
                _ => {
                    let report: miette::Report =
                        (*unknown_type_err(&format!("{}", ty), span)).into();
                    eprintln!("{:?}", report);
                    std::process::exit(1);
                }
            },
            HirTy::Float(flt) => match flt.size_in_bits {
                32 => LirTy::Float32,
                64 => LirTy::Float64,
                _ => {
                    let report: miette::Report =
                        (*unknown_type_err(&format!("{}", ty), span)).into();
                    eprintln!("{:?}", report);
                    std::process::exit(1);
                }
            },
            HirTy::Boolean(_) => LirTy::Boolean,
            HirTy::Char(_) => LirTy::Char,
            HirTy::String(_) => LirTy::Str,
            HirTy::Unit(_) => LirTy::Unit,
            HirTy::ReadOnlyReference(r) => {
                let inner = self.hir_ty_to_lir_ty(r.inner, span);
                LirTy::Ptr {
                    is_const: true,
                    inner: Box::new(inner),
                }
            }
            HirTy::MutableReference(r) => {
                let inner = self.hir_ty_to_lir_ty(r.inner, span);
                LirTy::Ptr {
                    is_const: false,
                    inner: Box::new(inner),
                }
            }
            HirTy::Uninitialized(_) => {
                let report: miette::Report = (*unknown_type_err(&format!("{}", ty), span)).into();
                eprintln!("{:?}", report);
                std::process::exit(1);
            }
            HirTy::Slice(l) => LirTy::Ptr {
                is_const: false,
                inner: Box::new(self.hir_ty_to_lir_ty(l.inner, span)),
            },
            HirTy::InlineArray(arr) => LirTy::ArrayTy {
                inner: Box::new(self.hir_ty_to_lir_ty(arr.inner, span)),
                size: arr.size,
            },
            HirTy::Named(n) => LirTy::StructType(n.name.to_string()),
            HirTy::Generic(g) => {
                let mangled_name =
                    MonomorphizationPass::generate_mangled_name(self.hir_arena, g, "struct");
                if self.hir_module.signature.structs.contains_key(mangled_name) {
                    LirTy::StructType(mangled_name.to_string())
                } else {
                    // Might be an union
                    let mangled_name =
                        MonomorphizationPass::generate_mangled_name(self.hir_arena, g, "union");
                    if self.hir_module.signature.unions.contains_key(mangled_name) {
                        LirTy::UnionType(mangled_name.to_string())
                    } else {
                        let report: miette::Report =
                            (*unknown_type_err(&format!("{}", ty), span)).into();
                        eprintln!("{:?}", report);
                        std::process::exit(1);
                    }
                }
            }
            HirTy::PtrTy(ptr_ty) => {
                let inner = self.hir_ty_to_lir_ty(ptr_ty.inner, span);
                LirTy::Ptr {
                    is_const: false,
                    inner: Box::new(inner),
                }
            }
            HirTy::Reference(r) => {
                let inner = self.hir_ty_to_lir_ty(r.inner, span);
                LirTy::Ptr {
                    is_const: r.kind == HirReferenceKind::ReadOnly,
                    inner: Box::new(inner),
                }
            }
            _ => {
                let report: miette::Report = (*unknown_type_err(&format!("{}", ty), span)).into();
                eprintln!("{:?}", report);
                std::process::exit(1);
            } // Default fallback
        }
    }
}

fn unknown_type_err(ty_name: &str, span: Span) -> Box<LirLoweringError> {
    Box::new(LirLoweringError::UnknownType(UnknownTypeError {
        ty_name: ty_name.to_string(),
        span,
        src: NamedSource::new(span.path, utils::get_file_content(span.path).unwrap()),
    }))
}

fn unsupported_expr(span: Span) -> Box<LirLoweringError> {
    Box::new(LirLoweringError::UnsupportedHirExpr(
        UnsupportedHirExprError {
            span,
            src: NamedSource::new(span.path, utils::get_file_content(span.path).unwrap()),
        },
    ))
}

// ============================================================================
// Pretty printing for debugging
// ============================================================================

impl std::fmt::Display for LirProgram {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for strukt in &self.structs {
            writeln!(f, "{}", strukt)?;
        }
        for extern_func in &self.extern_functions {
            writeln!(f, "{}", extern_func)?;
        }
        for func in &self.functions {
            writeln!(f, "{}", func)?;
        }
        Ok(())
    }
}

impl std::fmt::Display for LirExternFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "extern fun {}({}): {}",
            self.name,
            self.args
                .iter()
                .map(|arg| format!("{}", arg))
                .collect::<Vec<_>>()
                .join(", "),
            match &self.return_type {
                Some(ty) => format!("{}", ty),
                None => "".to_string(),
            }
        )
    }
}

impl std::fmt::Display for LirStruct {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "struct {} {{", self.name)?;
        for (field_name, field_type) in &self.fields {
            writeln!(f, "\t{}: {},", field_name, field_type)?;
        }
        writeln!(f, "}}")
    }
}

impl std::fmt::Display for LirFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "fun {}({}): {}",
            self.name,
            self.args
                .iter()
                .map(|arg| format!("{}", arg))
                .collect::<Vec<_>>()
                .join(", "),
            match &self.return_type {
                Some(ty) => format!("{}", ty),
                None => "".to_string(),
            }
        )?;
        for block in &self.blocks {
            writeln!(f, "{}", block)?;
        }
        Ok(())
    }
}

impl std::fmt::Display for LirBlock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "\t{}:", self.label)?;
        for instr in &self.instructions {
            writeln!(f, "\t\t{}", instr)?;
        }
        // Print the terminator (unless it's None)
        if !matches!(self.terminator, LirTerminator::None) {
            writeln!(f, "\t\t{}", self.terminator)?;
        }
        Ok(())
    }
}

impl std::fmt::Display for LirInstr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LirInstr::Add { dest, a, b, ty } => {
                write!(f, "{} = add.{} {}, {}", dest, ty, a, b)
            }
            LirInstr::Sub { dest, a, b, ty } => {
                write!(f, "{} = sub.{} {}, {}", dest, ty, a, b)
            }
            LirInstr::Mul { dest, a, b, ty } => {
                write!(f, "{} = mul.{} {}, {}", dest, ty, a, b)
            }
            LirInstr::Div { dest, a, b, ty } => {
                write!(f, "{} = div.{} {}, {}", dest, ty, a, b)
            }
            LirInstr::Mod { dest, a, b, ty } => {
                write!(f, "{} = mod.{} {}, {}", dest, ty, a, b)
            }
            LirInstr::LessThan { dest, a, b, ty } => {
                write!(f, "{} = lt.{} {}, {}", dest, ty, a, b)
            }
            LirInstr::LessThanOrEqual { dest, a, b, ty } => {
                write!(f, "{} = le.{} {}, {}", dest, ty, a, b)
            }
            LirInstr::GreaterThan { dest, a, b, ty } => {
                write!(f, "{} = gt.{} {}, {}", dest, ty, a, b)
            }
            LirInstr::GreaterThanOrEqual { dest, a, b, ty } => {
                write!(f, "{} = ge.{} {}, {}", dest, ty, a, b)
            }
            LirInstr::Equal { dest, a, b, ty } => {
                write!(f, "{} = eq.{} {}, {}", dest, ty, a, b)
            }
            LirInstr::NotEqual { dest, a, b, ty } => {
                write!(f, "{} = ne.{} {}, {}", dest, ty, a, b)
            }
            LirInstr::Negate { ty: _, dest, src } => {
                write!(f, "{} = neg {}", dest, src)
            }
            LirInstr::Not { ty: _, dest, src } => {
                write!(f, "{} = not {}", dest, src)
            }
            LirInstr::Index {
                ty: _,
                dst,
                src,
                index,
            } => {
                write!(f, "{} = index {}[{}]", dst, src, index)
            }
            LirInstr::LoadConst { dst, value } => {
                write!(f, "{} = ld_const {}", dst, value)
            }
            LirInstr::LoadImm { ty: _, dst, value } => {
                write!(f, "{} = ld_imm {}", dst, value)
            }
            LirInstr::Call {
                ty: _,
                dst,
                func_name,
                args,
            } => {
                let args_str = args
                    .iter()
                    .map(|a| format!("{}", a))
                    .collect::<Vec<_>>()
                    .join(", ");
                if let Some(d) = dst {
                    write!(f, "{} = call @{}({})", d, func_name, args_str)
                } else {
                    write!(f, "call @{}({})", func_name, args_str)
                }
            }
            LirInstr::ExternCall {
                ty: _,
                dst,
                func_name,
                args,
            } => {
                let args_str = args
                    .iter()
                    .map(|a| format!("{}", a))
                    .collect::<Vec<_>>()
                    .join(", ");
                if let Some(d) = dst {
                    write!(f, "{} = call_extern @{}({})", d, func_name, args_str)
                } else {
                    write!(f, "call_extern @{}({})", func_name, args_str)
                }
            }
            LirInstr::Construct { ty, dst, args } => {
                let args_str = args
                    .iter()
                    .map(|a| format!("{}", a))
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "{} = new {}({})", dst, ty, args_str)
            }
            LirInstr::ConstructArray { ty, dst, size } => {
                write!(f, "{} = new_array {}[{}]", dst, ty, size)
            }
            LirInstr::ConstructUnion {
                ty,
                dst,
                field_values,
            } => {
                let fields_str = field_values
                    .iter()
                    .map(|(name, value)| format!("{}: {}", name, value))
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "{} = raw_obj {} {{ {} }}", dst, ty, fields_str)
            }
            LirInstr::Delete { ty, src } => {
                write!(f, "delete {} {}", ty, src)
            }
            LirInstr::FieldAccess {
                ty: _,
                dst,
                src,
                field_name,
            } => {
                write!(f, "{} = {}.{}", dst, src, field_name)
            }
            LirInstr::Assign { ty: _, dst, src } => {
                write!(f, "{} = assign {}", dst, src)
            }
            LirInstr::Cast { ty, from, dst, src } => {
                write!(f, "{} = cast {}->{} {}", dst, from, ty, src)
            }
        }
    }
}

impl std::fmt::Display for LirOperand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LirOperand::Temp(id) => write!(f, "%t{}", id),
            LirOperand::Arg(idx) => write!(f, "%arg{}", idx),
            LirOperand::Const(val) => write!(f, "#{}", val),
            LirOperand::ImmInt(i) => write!(f, "%imm{}", i),
            LirOperand::ImmUInt(u) => write!(f, "%imm{}", u),
            LirOperand::ImmFloat(fl) => write!(f, "%imm{}", fl),
            LirOperand::ImmBool(b) => write!(f, "%imm{}", b),
            LirOperand::ImmChar(c) => write!(f, "%imm{}", c),
            LirOperand::ImmUnit => write!(f, "%imm()"),
            LirOperand::Deref(d) => write!(f, "*{}", d),
            LirOperand::AsRef(a) => write!(f, "&{}", a),
            LirOperand::FieldAccess {
                src, field_name, ..
            } => {
                write!(f, "{}.{}", src, field_name)
            }
            LirOperand::Index { src, index } => write!(f, "{}[{}]", src, index),
        }
    }
}

impl std::fmt::Display for LirTy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LirTy::Int8 => write!(f, "int8"),
            LirTy::UInt8 => write!(f, "uint8"),
            LirTy::Int16 => write!(f, "int16"),
            LirTy::UInt16 => write!(f, "uint16"),
            LirTy::Int32 => write!(f, "int32"),
            LirTy::UInt32 => write!(f, "uint32"),
            LirTy::Int64 => write!(f, "int64"),
            LirTy::UInt64 => write!(f, "uint64"),
            LirTy::Float32 => write!(f, "float32"),
            LirTy::Float64 => write!(f, "float64"),
            LirTy::Boolean => write!(f, "bool"),
            LirTy::Char => write!(f, "char"),
            LirTy::Str => write!(f, "str"),
            LirTy::Unit => write!(f, "unit"),
            LirTy::Ptr { is_const, inner } => write!(f, "ptr<{}>", inner),
            LirTy::StructType(name) => write!(f, "struct {}", name),
            LirTy::UnionType(name) => write!(f, "union {}", name),
            LirTy::ArrayTy { inner, size } => write!(f, "[{}; {}]", inner, size),
        }
    }
}

impl std::fmt::Display for LirTerminator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LirTerminator::BranchIf {
                condition,
                then_label,
                else_label,
            } => {
                write!(f, "br_if {}, [{}, {}]", condition, then_label, else_label)
            }
            LirTerminator::Return { value } => {
                if let Some(v) = value {
                    write!(f, "ret {}", v)
                } else {
                    write!(f, "ret")
                }
            }
            LirTerminator::Branch { target } => {
                write!(f, "br {}", target)
            }
            LirTerminator::Halt => {
                write!(f, "hlt")
            }
            LirTerminator::None => write!(f, "<no terminator>"),
        }
    }
}
