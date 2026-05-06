use std::collections::{BTreeMap, HashMap, HashSet};

use miette::NamedSource;

use crate::atlas_c::{
    atlas_hir::{
        HirModule,
        arena::HirArena,
        expr::{HirBinaryOperator, HirExpr, HirUnaryOp},
        item::{HirFunction, HirStruct, HirStructDestructor, HirStructMethod, HirUnion},
        monomorphization_pass::MonomorphizationPass,
        signature::{ConstantValue, HirOverloadableOperatorKind, HirStructMethodModifier},
        special_methods::{
            INTRINSIC_PRIMITIVE_COPY, INTRINSIC_PRIMITIVE_DEFAULT, INTRINSIC_PRIMITIVE_HASH,
        },
        stmt::HirStatement,
        ty::{HirGenericTy, HirTy, HirTyId},
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

    fn overloaded_operator_owner_name(&self, ty: &'hir HirTy<'hir>) -> String {
        match ty {
            HirTy::Named(n) => n.name.to_string(),
            HirTy::Generic(g) => {
                MonomorphizationPass::generate_mangled_name(self.hir_arena, g, "struct").to_string()
            }
            _ => ty.get_valid_c_string(),
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
                    c_name: sig.c_name.map(|s| s.to_string()),
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
        let mut variants = BTreeMap::new();
        for variant in union_body.variants.iter() {
            variants.insert(
                variant.name.to_string(),
                self.hir_ty_to_lir_ty(variant.ty, variant.span),
            );
        }

        let lir_union = LirUnion {
            name: union_body.name.to_string(),
            c_name: union_body.signature.c_name.map(|s| s.to_string()),
            variants,
        };

        Ok(lir_union)
    }

    fn lower_struct(
        &mut self,
        struct_body: &'hir HirStruct<'hir>,
        functions: &mut Vec<LirFunction>,
    ) -> LirResult<LirStruct> {
        let mut fields = BTreeMap::new();
        for field in struct_body.fields.iter() {
            fields.insert(
                field.name.to_string(),
                self.hir_ty_to_lir_ty(field.ty, field.span),
            );
        }

        let lir_struct = LirStruct {
            name: struct_body.name.to_string(),
            fields,
            is_extern: struct_body.signature.is_extern,
            c_name: struct_body.signature.c_name.map(|s| s.to_string()),
        };

        for method in struct_body.methods.iter() {
            let lir_method = self.lower_method(struct_body.name, method)?;
            functions.push(lir_method);
        }

        for operator in struct_body.operators.iter() {
            let lir_operator = self.lower_method(struct_body.name, operator)?;
            functions.push(lir_operator);
        }

        if let Some(destructor) = &struct_body.destructor {
            functions.push(self.lower_destructor(struct_body.name, destructor, "__dtor")?);
        }

        Ok(lir_struct)
    }

    fn lower_destructor(
        &mut self,
        struct_name: &str,
        ctor: &'hir HirStructDestructor<'hir>,
        kind: &str,
    ) -> LirResult<LirFunction> {
        // Reset state for new function
        self.temp_counter = 0;
        self.block_counter = 0;
        self.param_map.clear();
        self.local_map.clear();

        self.param_map.insert("this", 0);
        let args = vec![LirTy::Ptr {
            is_const: false,
            inner: Box::new(LirTy::StructType(struct_name.to_string())),
        }];

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
            HirStructMethodModifier::Mutable | HirStructMethodModifier::Const
        ) {
            // The first parameter is always "this"
            self.param_map.insert("this", 0);
            args.push(LirTy::Ptr {
                is_const: method.signature.modifier == HirStructMethodModifier::Const,
                inner: Box::new(LirTy::StructType(struct_name.to_string())),
            });
        } else if matches!(
            method.signature.modifier,
            HirStructMethodModifier::Consuming
        ) {
            // Consuming methods take ownership of `this` by value.
            self.param_map.insert("this", 0);
            args.push(LirTy::StructType(struct_name.to_string()));
        } else {
            // Static method, no "this" parameter
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

        // Find the last non-empty block (has instructions or a terminator)
        if let Some(idx) = (0..result.blocks.len()).rev().find(|i| {
            let b = &result.blocks[*i];
            !b.instructions.is_empty() || !matches!(b.terminator, LirTerminator::None)
        }) {
            if method.signature.return_ty.is_unit() {
                if matches!(result.blocks[idx].terminator, LirTerminator::None) {
                    // For methods returning unit, ensure there's a return at the end
                    result.blocks[idx].terminator = LirTerminator::Return { value: None };
                }
            } else if !matches!(
                result.blocks[idx].terminator,
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

        // Find the last non-empty block (has instructions or a terminator)
        if let Some(idx) = (0..result.blocks.len()).rev().find(|i| {
            let b = &result.blocks[*i];
            !b.instructions.is_empty() || !matches!(b.terminator, LirTerminator::None)
        }) {
            if func.signature.return_ty.is_unit() {
                if matches!(result.blocks[idx].terminator, LirTerminator::None) {
                    // For functions returning unit, ensure there's a return at the end
                    if func.name == "main" {
                        result.blocks[idx].terminator = LirTerminator::Halt;
                    } else {
                        result.blocks[idx].terminator = LirTerminator::Return { value: None };
                    }
                }
            } else if !matches!(
                result.blocks[idx].terminator,
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

        Ok(result)
    }

    /// Lower a statement
    fn lower_stmt(&mut self, stmt: &'hir HirStatement<'hir>) -> LirResult<()> {
        match stmt {
            HirStatement::Return(ret) => {
                if let Some(value) = &ret.value {
                    let value = self.lower_expr(value)?;
                    self.emit_terminator(LirTerminator::Return { value: Some(value) })?;
                } else {
                    self.emit_terminator(LirTerminator::Return { value: None })?;
                }
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
            _ => Err(unsupported_expr(l_value.span(), format!("{:?}", l_value))),
        }
    }

    fn class_name_from_receiver_ty(&self, ty: &'hir HirTy<'hir>) -> Option<&'hir str> {
        match ty {
            HirTy::Named(n) => Some(n.name),
            HirTy::Generic(g) => {
                let mangled_struct =
                    MonomorphizationPass::generate_mangled_name(self.hir_arena, g, "struct");
                if self
                    .hir_module
                    .signature
                    .structs
                    .contains_key(mangled_struct)
                {
                    Some(mangled_struct)
                } else {
                    let mangled_union =
                        MonomorphizationPass::generate_mangled_name(self.hir_arena, g, "union");
                    if self.hir_module.signature.unions.contains_key(mangled_union) {
                        Some(mangled_union)
                    } else {
                        None
                    }
                }
            }
            HirTy::PtrTy(ptr) => self.class_name_from_receiver_ty(ptr.inner),
            _ => None,
        }
    }

    /// Lower an expression, returning the operand holding the result
    fn lower_expr(&mut self, expr: &'hir HirExpr<'hir>) -> LirResult<LirOperand> {
        match expr {
            // === Literals ===
            HirExpr::IntegerLiteral(lit) => {
                let size = match lit.ty {
                    HirTy::Integer(i) => i.size_in_bits,
                    HirTy::LiteralInteger(i) => i.get_minimal_int_ty().size_in_bits,
                    _ => {
                        return Err(unsupported_expr(lit.span, format!("{:?}", expr)));
                    }
                };
                Ok(LirOperand::ImmInt {
                    val: lit.value,
                    size,
                })
            }

            HirExpr::UnsignedIntegerLiteral(lit) => {
                let size = match lit.ty {
                    HirTy::UnsignedInteger(u) => u.size_in_bits,
                    HirTy::LiteralUnsignedInteger(u) => u.get_minimal_uint_ty().size_in_bits,
                    _ => {
                        return Err(unsupported_expr(lit.span, format!("{:?}", expr)));
                    }
                };
                Ok(LirOperand::ImmUInt {
                    val: lit.value,
                    size,
                })
            }

            HirExpr::BooleanLiteral(lit) => Ok(LirOperand::ImmBool(lit.value)),

            HirExpr::FloatLiteral(lit) => {
                let size = match lit.ty {
                    HirTy::Float(f) => f.size_in_bits,
                    HirTy::LiteralFloat(f) => f.get_float_ty().size_in_bits,
                    _ => {
                        return Err(unsupported_expr(lit.span, format!("{:?}", expr)));
                    }
                };
                Ok(LirOperand::ImmFloat {
                    val: lit.value,
                    size,
                })
            }

            HirExpr::CharLiteral(lit) => Ok(LirOperand::ImmChar(lit.value)),

            HirExpr::StringLiteral(lit) => {
                let dest = self.new_temp();
                self.emit(LirInstr::LoadConst {
                    dst: dest.clone(),
                    value: LirOperand::Const(ConstantValue::String(String::from(lit.value))),
                })?;
                Ok(dest)
            }
            HirExpr::NullLiteral(_) | HirExpr::UnitLiteral(_) => Ok(LirOperand::ImmUnit),

            HirExpr::ThisLiteral(_) => {
                // "this" is always the first argument (arg 0)
                Ok(LirOperand::Arg(0))
            }

            HirExpr::ListLiteral(list) => {
                let dst = self.new_temp();
                let lir_arr_ty = self.hir_ty_to_lir_ty(list.ty, list.span);
                self.emit(LirInstr::ConstructArray {
                    ty: lir_arr_ty,
                    dst: dst.clone(),
                    size: list.items.len(),
                })?;

                let elem_hir_ty = match list.ty {
                    HirTy::InlineArray(arr) => arr.inner,
                    _ => {
                        if let Some(first) = list.items.first() {
                            first.ty()
                        } else {
                            return Ok(dst);
                        }
                    }
                };
                let elem_lir_ty = self.hir_ty_to_lir_ty(elem_hir_ty, list.span);

                for (idx, item) in list.items.iter().enumerate() {
                    let src = self.lower_expr(item)?;
                    let index_operand = LirOperand::Index {
                        src: Box::new(dst.clone()),
                        index: Box::new(LirOperand::ImmUInt {
                            val: idx as u64,
                            size: 64,
                        }),
                    };
                    self.emit(LirInstr::Assign {
                        ty: elem_lir_ty.clone(),
                        dst: index_operand,
                        src,
                    })?;
                }

                Ok(dst)
            }
            HirExpr::ListLiteralWithSize(list) => {
                fn const_list_size(expr: &HirExpr<'_>) -> Option<usize> {
                    match expr {
                        HirExpr::IntegerLiteral(i) => Some(i.value as usize),
                        HirExpr::UnsignedIntegerLiteral(u) => Some(u.value as usize),
                        HirExpr::Unary(unary) if unary.op.is_none() => const_list_size(&unary.expr),
                        _ => None,
                    }
                }

                let size = const_list_size(&list.size).ok_or_else(|| {
                    unsupported_expr(
                        list.span,
                        "non-constant list-with-size length after type-check".to_string(),
                    )
                })?;

                let dst = self.new_temp();
                let lir_arr_ty = self.hir_ty_to_lir_ty(list.ty, list.span);
                self.emit(LirInstr::ConstructArray {
                    ty: lir_arr_ty,
                    dst: dst.clone(),
                    size,
                })?;

                if size == 0 {
                    return Ok(dst);
                }

                let elem_hir_ty = match list.ty {
                    HirTy::InlineArray(arr) => arr.inner,
                    _ => list.item.ty(),
                };
                let elem_lir_ty = self.hir_ty_to_lir_ty(elem_hir_ty, list.span);

                // Evaluate the repeated item once and reuse it for each slot.
                let repeated_item = self.lower_expr(&list.item)?;
                for idx in 0..size {
                    let index_operand = LirOperand::Index {
                        src: Box::new(dst.clone()),
                        index: Box::new(LirOperand::ImmUInt {
                            val: idx as u64,
                            size: 64,
                        }),
                    };
                    self.emit(LirInstr::Assign {
                        ty: elem_lir_ty.clone(),
                        dst: index_operand,
                        src: repeated_item.clone(),
                    })?;
                }

                Ok(dst)
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
                }
                // Check if it's a global function symbol (used as a function pointer value)
                else if self.hir_module.signature.functions.contains_key(ident.name) {
                    let function_name = self
                        .hir_module
                        .signature
                        .functions
                        .get(ident.name)
                        .and_then(|sig| if sig.is_external { sig.c_name } else { None })
                        .unwrap_or(ident.name)
                        .to_string();
                    Ok(LirOperand::GlobalFn(function_name))
                } else {
                    // Unknown identifier - shouldn't happen after type checking
                    panic!("Unknown identifier: {}", ident.name);
                }
            }

            HirExpr::Unary(unary) => {
                if unary.is_overloaded
                    && let Some(op) = unary.op
                {
                    let expr_operand = self.lower_expr(&unary.expr)?;
                    let owner_name = self.overloaded_operator_owner_name(unary.expr.ty());

                    let dest = self.new_temp();
                    self.emit(LirInstr::Call {
                        ty: self.hir_ty_to_lir_ty(unary.ty, unary.span),
                        dst: Some(dest.clone()),
                        func_name: format!(
                            "{}_{}",
                            owner_name,
                            Into::<String>::into(Into::<HirOverloadableOperatorKind>::into(op))
                        ),
                        args: vec![LirOperand::AsRef(Box::new(expr_operand))],
                    })?;
                    return Ok(dest);
                }
                match unary.op {
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
                        if ty == LirTy::Boolean {
                            self.emit(LirInstr::Not {
                                ty,
                                dest: dest.clone(),
                                src: expr_operand,
                            })?;
                        } else {
                            self.emit(LirInstr::BinaryNot {
                                ty,
                                dest: dest.clone(),
                                src: expr_operand,
                            })?;
                        }
                        Ok(dest)
                    }
                    _ => self.lower_expr(&unary.expr),
                }
            }

            // === Binary operations ===
            HirExpr::HirBinaryOperation(binop) => {
                let lhs = self.lower_expr(&binop.lhs)?;
                let rhs = self.lower_expr(&binop.rhs)?;
                let dest = self.new_temp();

                if binop.is_overloaded {
                    let owner_name = self.overloaded_operator_owner_name(binop.lhs.ty());
                    self.emit(LirInstr::Call {
                        ty: self.hir_ty_to_lir_ty(binop.ty, binop.span),
                        dst: Some(dest.clone()),
                        func_name: format!(
                            "{}_{}",
                            owner_name,
                            Into::<String>::into(Into::<HirOverloadableOperatorKind>::into(
                                binop.op
                            ))
                        ),
                        args: vec![
                            LirOperand::AsRef(Box::new(lhs)),
                            LirOperand::AsRef(Box::new(rhs)),
                        ],
                    })?;
                    return Ok(dest);
                }

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
                        ty: LirTy::Boolean,
                        dest: dest.clone(),
                        a: lhs,
                        b: rhs,
                    },
                    HirBinaryOperator::Lte => LirInstr::LessThanOrEqual {
                        ty: LirTy::Boolean,
                        dest: dest.clone(),
                        a: lhs,
                        b: rhs,
                    },
                    HirBinaryOperator::Gt => LirInstr::GreaterThan {
                        ty: LirTy::Boolean,
                        dest: dest.clone(),
                        a: lhs,
                        b: rhs,
                    },
                    HirBinaryOperator::Gte => LirInstr::GreaterThanOrEqual {
                        ty: LirTy::Boolean,
                        dest: dest.clone(),
                        a: lhs,
                        b: rhs,
                    },
                    HirBinaryOperator::Eq => LirInstr::Equal {
                        ty: LirTy::Boolean,
                        dest: dest.clone(),
                        a: lhs,
                        b: rhs,
                    },
                    HirBinaryOperator::Neq => LirInstr::NotEqual {
                        ty: LirTy::Boolean,
                        dest: dest.clone(),
                        a: lhs,
                        b: rhs,
                    },
                    HirBinaryOperator::And => LirInstr::LogicalAnd {
                        ty: LirTy::Boolean,
                        dest: dest.clone(),
                        a: lhs,
                        b: rhs,
                    },
                    HirBinaryOperator::Or => LirInstr::LogicalOr {
                        ty: LirTy::Boolean,
                        dest: dest.clone(),
                        a: lhs,
                        b: rhs,
                    },
                    HirBinaryOperator::ShL => LirInstr::ShiftLeft {
                        ty,
                        dest: dest.clone(),
                        a: lhs,
                        b: rhs,
                    },
                    HirBinaryOperator::ShR => LirInstr::ShiftRight {
                        ty,
                        dest: dest.clone(),
                        a: lhs,
                        b: rhs,
                    },
                    HirBinaryOperator::BinAnd => LirInstr::BinaryAnd {
                        ty,
                        dest: dest.clone(),
                        a: lhs,
                        b: rhs,
                    },
                    HirBinaryOperator::BinOr => LirInstr::BinaryOr {
                        ty,
                        dest: dest.clone(),
                        a: lhs,
                        b: rhs,
                    },
                    HirBinaryOperator::BinXor => LirInstr::BinaryXor {
                        ty,
                        dest: dest.clone(),
                        a: lhs,
                        b: rhs,
                    },
                };

                self.emit(instr)?;
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

                self.emit(LirInstr::ConstructObject {
                    ty: self.hir_ty_to_lir_ty(obj_lit.ty, obj_lit.span),
                    dst: dest.clone(),
                    field_values: args.into_iter().collect(),
                })?;
                Ok(dest)
            }

            // === Function calls ===
            HirExpr::Call(call) => {
                if call.is_reference {
                    if let HirExpr::Ident(ident) = call.callee.as_ref() {
                        let func_name = if !call.generics.is_empty()
                            && !self.hir_module.signature.functions.contains_key(ident.name)
                        {
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
                            .to_string()
                        } else {
                            ident.name.to_string()
                        };
                        return Ok(LirOperand::GlobalFn(func_name));
                    }
                    return Err(unsupported_expr(expr.span(), format!("{:?}", expr)));
                }

                // Indirect call through a function value (identifier variable, field, static field, etc.).
                if let HirTy::Function(fn_ty) = call.callee.ty() {
                    let callee = self.lower_expr(&call.callee)?;
                    let mut args = Vec::new();
                    for (idx, arg) in call.args.iter().enumerate() {
                        let lowered = self.lower_expr(arg)?;
                        let adjusted = if let Some(expected_ty) = call.args_ty.get(idx) {
                            if matches!(expected_ty, HirTy::PtrTy(_))
                                && !matches!(
                                    arg.ty(),
                                    HirTy::PtrTy(_) | HirTy::Slice(_) | HirTy::String(_)
                                )
                            {
                                LirOperand::AsRef(Box::new(lowered))
                            } else {
                                lowered
                            }
                        } else {
                            lowered
                        };
                        args.push(adjusted);
                    }

                    let dest = if matches!(call.ty, HirTy::Unit(_)) {
                        None
                    } else {
                        Some(self.new_temp())
                    };

                    self.emit(LirInstr::CallPtr {
                        ty: self.hir_ty_to_lir_ty(call.ty, call.span),
                        dst: dest.clone(),
                        callee,
                        args,
                        param_tys: fn_ty
                            .params
                            .iter()
                            .map(|p| self.hir_ty_to_lir_ty(p, call.span))
                            .collect(),
                    })?;

                    return Ok(dest.unwrap_or(LirOperand::ImmInt { val: 0, size: 64 }));
                }

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
                                return Err(unsupported_expr(
                                    expr.span(),
                                    format!("{:?}", static_access),
                                ));
                            }
                        };
                        (
                            format!("{}_{}", object_name, static_access.field.name),
                            false,
                        )
                    }
                    HirExpr::FieldAccess(field_access) => {
                        let object_name = match self
                            .class_name_from_receiver_ty(field_access.target.ty())
                        {
                            Some(name) => name,
                            None => {
                                return Err(unsupported_expr(expr.span(), format!("{:?}", expr)));
                            }
                        };
                        (format!("{}_{}", object_name, field_access.field.name), true)
                    }
                    _ => {
                        return Err(unsupported_expr(expr.span(), format!("{:?}", expr)));
                    }
                };
                // Lower arguments
                let mut args = Vec::new();
                if take_this {
                    if let HirExpr::FieldAccess(field_access) = call.callee.as_ref() {
                        let target_operand = self.lower_expr(&field_access.target)?;
                        let is_consuming_method = self
                            .class_name_from_receiver_ty(field_access.target.ty())
                            .and_then(|name| self.hir_module.signature.structs.get(name).copied())
                            .and_then(|class| class.methods.get(field_access.field.name))
                            .is_some_and(|method| {
                                method.modifier == HirStructMethodModifier::Consuming
                            });

                        // Unify receiver lowering:
                        // - `obj.method(...)` lowers by value only for consuming methods
                        // - otherwise it lowers to `&obj` (implicit reference)
                        // - `ptr->method(...)` lowers to `ptr` (already a pointer)
                        let receiver = if field_access.is_arrow
                            || matches!(field_access.target.ty(), HirTy::PtrTy(_))
                            || is_consuming_method
                        {
                            target_operand
                        } else {
                            LirOperand::AsRef(Box::new(target_operand))
                        };
                        args.push(receiver);
                    } else if let HirExpr::StaticAccess(_) = call.callee.as_ref() {
                        for (idx, arg) in call.args.iter().enumerate() {
                            let lowered = self.lower_expr(arg)?;
                            let adjusted = if let Some(expected_ty) = call.args_ty.get(idx) {
                                if matches!(expected_ty, HirTy::PtrTy(_))
                                    && !matches!(
                                        arg.ty(),
                                        HirTy::PtrTy(_) | HirTy::Slice(_) | HirTy::String(_)
                                    )
                                {
                                    LirOperand::AsRef(Box::new(lowered))
                                } else {
                                    lowered
                                }
                            } else {
                                lowered
                            };
                            args.push(adjusted);
                        }
                        return Err(unsupported_expr(
                            expr.span(),
                            String::from(
                                "There is no special static method taking an implicit \"this\" in the language yet",
                            ),
                        ));
                    } else {
                        return Err(unsupported_expr(
                            expr.span(),
                            String::from(
                                "There is no special static method taking an implicit \"this\" in the language yet",
                            ),
                        ));
                    }
                }
                for (idx, arg) in call.args.iter().enumerate() {
                    let lowered = self.lower_expr(arg)?;
                    let adjusted = if let Some(expected_ty) = call.args_ty.get(idx) {
                        if matches!(expected_ty, HirTy::PtrTy(_))
                            && !matches!(
                                arg.ty(),
                                HirTy::PtrTy(_) | HirTy::Slice(_) | HirTy::String(_)
                            )
                        {
                            LirOperand::AsRef(Box::new(lowered))
                        } else {
                            lowered
                        }
                    } else {
                        lowered
                    };
                    args.push(adjusted);
                }

                // Check if it's an external function
                let extern_sig = self
                    .hir_module
                    .signature
                    .functions
                    .get(func_name.as_str())
                    .filter(|f| f.is_external);
                let is_extern = extern_sig.is_some();
                let extern_callee_name = extern_sig
                    .and_then(|f| f.c_name)
                    .map(str::to_string)
                    .unwrap_or_else(|| func_name.clone());

                let dest = if matches!(call.ty, HirTy::Unit(_)) {
                    None
                } else {
                    Some(self.new_temp())
                };

                let instr = if is_extern {
                    LirInstr::ExternCall {
                        ty: self.hir_ty_to_lir_ty(call.ty, call.span),
                        dst: dest.clone(),
                        func_name: extern_callee_name,
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
                Ok(dest.unwrap_or(LirOperand::ImmInt { val: 0, size: 64 })) // unit value
            }

            HirExpr::StaticAccess(_) => {
                if let HirExpr::StaticAccess(static_access) = expr {
                    let object_name = match static_access.target {
                        HirTy::Named(n) => n.name,
                        HirTy::Generic(g) => {
                            MonomorphizationPass::generate_mangled_name(self.hir_arena, g, "struct")
                        }
                        _ => {
                            return Err(unsupported_expr(
                                static_access.span,
                                format!("{:?}", static_access),
                            ));
                        }
                    };

                    if self
                        .hir_module
                        .signature
                        .structs
                        .get(object_name)
                        .and_then(|class| class.methods.get(static_access.field.name))
                        .is_some()
                    {
                        return Ok(LirOperand::GlobalFn(format!(
                            "{}_{}",
                            object_name, static_access.field.name
                        )));
                    }
                }

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
                    ty: self.hir_ty_to_lir_ty(field_access.ty, field_access.span),
                    src: Box::new(target_operand),
                    field_name: field_access.field.name.to_string(),
                    is_arrow: field_access.is_arrow,
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

            HirExpr::Delete(delete_expr) => {
                // If the value type is copyable / trivially copyable, it has no destructor
                // and we can emit an empty/unit value so later passes can optimize it away.
                if delete_expr
                    .expr
                    .ty()
                    .is_trivially_copyable(&self.hir_module.signature)
                {
                    let dst = self.new_temp();
                    self.emit(LirInstr::LoadConst {
                        dst: dst.clone(),
                        value: LirOperand::ImmUnit,
                    })?;
                    return Ok(dst);
                }

                let dst = self.new_temp(); // Placeholder
                let src = self.lower_expr(&delete_expr.expr)?;
                let ty = self.hir_ty_to_lir_ty(delete_expr.expr.ty(), delete_expr.span);
                let should_free = matches!(delete_expr.expr.ty(), HirTy::PtrTy(_));
                self.emit(LirInstr::Delete {
                    ty,
                    src,
                    should_free,
                })?;
                Ok(dst)
            }

            HirExpr::IntrinsicCall(intrinsic) => match intrinsic.name {
                "type_of" => {
                    let target_ty = intrinsic.args_ty.first().copied().unwrap_or(intrinsic.ty);
                    self.construct_type_info_object(target_ty, intrinsic.span)
                }
                "type_id" => {
                    let target_ty = intrinsic.args_ty.first().copied().unwrap_or(intrinsic.ty);
                    self.construct_type_id(target_ty)
                }
                "size_of" => {
                    let target_ty = intrinsic.args_ty.first().copied().unwrap_or(intrinsic.ty);
                    let lir_target_ty = self.hir_ty_to_lir_ty(target_ty, intrinsic.span);
                    let size = self.lir_type_size_and_align(&lir_target_ty).0;
                    let dest = self.new_temp();
                    self.emit(LirInstr::LoadImm {
                        ty: LirTy::UInt64,
                        dst: dest.clone(),
                        value: LirOperand::ImmUInt {
                            val: size as u64,
                            size: 64,
                        },
                    })?;
                    Ok(dest)
                }
                "align_of" => {
                    let target_ty = intrinsic.args_ty.first().copied().unwrap_or(intrinsic.ty);
                    let lir_target_ty = self.hir_ty_to_lir_ty(target_ty, intrinsic.span);
                    let align = self.lir_type_size_and_align(&lir_target_ty).1;
                    let dest = self.new_temp();
                    self.emit(LirInstr::LoadImm {
                        ty: LirTy::UInt64,
                        dst: dest.clone(),
                        value: LirOperand::ImmUInt {
                            val: align as u64,
                            size: 64,
                        },
                    })?;
                    Ok(dest)
                }
                INTRINSIC_PRIMITIVE_DEFAULT => {
                    let target_ty = intrinsic.args_ty.first().copied().unwrap_or(intrinsic.ty);
                    let lir_target_ty = self.hir_ty_to_lir_ty(target_ty, intrinsic.span);
                    let dest = self.new_temp();
                    match &lir_target_ty {
                        LirTy::Int8 | LirTy::Int16 | LirTy::Int32 | LirTy::Int64 => {
                            self.emit(LirInstr::LoadImm {
                                ty: lir_target_ty,
                                dst: dest.clone(),
                                value: LirOperand::ImmInt { val: 0, size: 64 },
                            })?;
                        }
                        LirTy::UInt8 | LirTy::UInt16 | LirTy::UInt32 | LirTy::UInt64 => {
                            self.emit(LirInstr::LoadImm {
                                ty: lir_target_ty,
                                dst: dest.clone(),
                                value: LirOperand::ImmUInt { val: 0, size: 64 },
                            })?;
                        }
                        LirTy::Float32 | LirTy::Float64 => {
                            self.emit(LirInstr::LoadImm {
                                ty: lir_target_ty,
                                dst: dest.clone(),
                                value: LirOperand::ImmFloat { val: 0.0, size: 64 },
                            })?;
                        }
                        LirTy::Boolean => {
                            self.emit(LirInstr::LoadImm {
                                ty: lir_target_ty,
                                dst: dest.clone(),
                                value: LirOperand::ImmBool(false),
                            })?;
                        }
                        LirTy::Char => {
                            self.emit(LirInstr::LoadImm {
                                ty: lir_target_ty,
                                dst: dest.clone(),
                                value: LirOperand::ImmChar('\0'),
                            })?;
                        }
                        LirTy::Unit => {
                            self.emit(LirInstr::LoadConst {
                                dst: dest.clone(),
                                value: LirOperand::ImmUnit,
                            })?;
                        }
                        LirTy::Str => {
                            self.emit(LirInstr::LoadConst {
                                dst: dest.clone(),
                                value: LirOperand::Const(ConstantValue::String(String::new())),
                            })?;
                        }
                        // Pointer-like defaults are null.
                        LirTy::Ptr { .. } | LirTy::FnPtr { .. } => {
                            let zero = self.new_temp();
                            self.emit(LirInstr::LoadImm {
                                ty: LirTy::UInt64,
                                dst: zero.clone(),
                                value: LirOperand::ImmUInt { val: 0, size: 64 },
                            })?;
                            self.emit(LirInstr::Cast {
                                ty: lir_target_ty,
                                from: LirTy::UInt64,
                                dst: dest.clone(),
                                src: zero,
                            })?;
                        }
                        // This intrinsic is intended for primitive-like targets; keep a safe fallback.
                        _ => {
                            let zero = self.new_temp();
                            self.emit(LirInstr::LoadImm {
                                ty: LirTy::UInt64,
                                dst: zero.clone(),
                                value: LirOperand::ImmUInt { val: 0, size: 64 },
                            })?;
                            self.emit(LirInstr::Cast {
                                ty: lir_target_ty,
                                from: LirTy::UInt64,
                                dst: dest.clone(),
                                src: zero,
                            })?;
                        }
                    }
                    Ok(dest)
                }
                // This purely exists to allow for constraints and use of `primitive.copy()`
                INTRINSIC_PRIMITIVE_COPY => {
                    let arg = self.lower_expr(&intrinsic.args[0])?;
                    Ok(arg)
                }
                INTRINSIC_PRIMITIVE_HASH => {
                    let arg = self.lower_expr(&intrinsic.args[0])?;
                    let arg_ty = self.hir_ty_to_lir_ty(intrinsic.args[0].ty(), intrinsic.span);

                    let dest = self.new_temp();
                    match arg_ty {
                        LirTy::UInt64 => Ok(arg),
                        LirTy::Unit => {
                            self.emit(LirInstr::LoadImm {
                                ty: LirTy::UInt64,
                                dst: dest.clone(),
                                value: LirOperand::ImmUInt { val: 0, size: 64 },
                            })?;
                            Ok(dest)
                        }
                        LirTy::Str => {
                            self.emit(LirInstr::Call {
                                ty: LirTy::UInt64,
                                dst: Some(dest.clone()),
                                func_name: "atlas77_string_hash".into(),
                                args: vec![arg],
                            })?;
                            Ok(dest)
                        }
                        // Default numeric/address-like hashing: cast to uint64.
                        _ => {
                            self.emit(LirInstr::Cast {
                                ty: LirTy::UInt64,
                                from: arg_ty,
                                dst: dest.clone(),
                                src: arg,
                            })?;
                            Ok(dest)
                        }
                    }
                }
                "std::move" => self.lower_expr(&intrinsic.args[0]),
                "std::ptr::read" => {
                    let ptr = self.lower_expr(&intrinsic.args[0])?;
                    Ok(LirOperand::Deref(Box::new(ptr)))
                }
                "std::ptr::write" => {
                    let ptr = self.lower_expr(&intrinsic.args[0])?;
                    let val = self.lower_expr(&intrinsic.args[1])?;
                    self.emit(LirInstr::Assign {
                        ty: self.hir_ty_to_lir_ty(intrinsic.args[1].ty(), intrinsic.span),
                        dst: LirOperand::Deref(Box::new(ptr)),
                        src: val,
                    })?;
                    Ok(LirOperand::ImmUnit)
                }
                _ => {
                    let mut args = Vec::new();
                    for arg in &intrinsic.args {
                        args.push(self.lower_expr(arg)?);
                    }
                    let dest = if matches!(intrinsic.ty, HirTy::Unit(_)) {
                        None
                    } else {
                        Some(self.new_temp())
                    };
                    self.emit(LirInstr::Call {
                        ty: self.hir_ty_to_lir_ty(intrinsic.ty, intrinsic.span),
                        dst: dest.clone(),
                        func_name: intrinsic.name.to_string(),
                        args,
                    })?;
                    Ok(dest.unwrap_or(LirOperand::ImmInt { val: 0, size: 64 }))
                }
            },
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
            HirTy::LiteralInteger(li) => match li.get_minimal_int_ty().size_in_bits {
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
            HirTy::LiteralUnsignedInteger(lu) => match lu.get_minimal_uint_ty().size_in_bits {
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
            HirTy::LiteralFloat(lf) => match lf.get_float_ty().size_in_bits {
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
            HirTy::Uninitialized(_) => {
                let report: miette::Report = (*unknown_type_err(&format!("{}", ty), span)).into();
                eprintln!("{:?}", report);
                std::process::exit(1);
            }
            HirTy::Error(_) => {
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
            HirTy::Named(n) => {
                if let Some(sig) = self.hir_module.signature.unions.get(n.name) {
                    if sig.is_extern {
                        let name = sig.c_name.unwrap_or(n.name);
                        LirTy::UnionType(name.to_string())
                    } else {
                        LirTy::UnionType(n.name.to_string())
                    }
                } else {
                    let name = self
                        .hir_module
                        .signature
                        .structs
                        .get(n.name)
                        .filter(|sig| sig.is_extern)
                        .and_then(|sig| sig.c_name)
                        .unwrap_or(n.name);
                    LirTy::StructType(name.to_string())
                }
            }
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
            HirTy::Function(func_ty) => LirTy::FnPtr {
                ret: Box::new(self.hir_ty_to_lir_ty(func_ty.ret_ty, span)),
                args: func_ty
                    .params
                    .iter()
                    .map(|param_ty| self.hir_ty_to_lir_ty(param_ty, span))
                    .collect(),
            },
            HirTy::PtrTy(ptr_ty) => {
                let inner = self.hir_ty_to_lir_ty(ptr_ty.inner, span);
                LirTy::Ptr {
                    is_const: ptr_ty.is_const,
                    inner: Box::new(inner),
                }
            }
        }
    }

    fn lir_type_size_and_align(&self, ty: &LirTy) -> (usize, usize) {
        self.lir_type_size_and_align_impl(ty, &mut HashSet::new())
    }

    fn lir_type_size_and_align_impl(
        &self,
        ty: &LirTy,
        visiting: &mut HashSet<String>,
    ) -> (usize, usize) {
        match ty {
            LirTy::Int8 | LirTy::UInt8 | LirTy::Boolean => (1, 1),
            LirTy::Int16 | LirTy::UInt16 => (2, 2),
            LirTy::Int32 | LirTy::UInt32 | LirTy::Float32 => (4, 4),
            LirTy::Int64 | LirTy::UInt64 | LirTy::Float64 => (8, 8),
            LirTy::Char => (4, 4),
            LirTy::Str | LirTy::Ptr { .. } | LirTy::FnPtr { .. } | LirTy::Unit => (8, 8),
            LirTy::ArrayTy { inner, size } => {
                let (inner_size, inner_align) = self.lir_type_size_and_align_impl(inner, visiting);
                (inner_size.saturating_mul(*size), inner_align)
            }
            LirTy::StructType(name) => {
                let visit_key = format!("S:{}", name);
                if !visiting.insert(visit_key.clone()) {
                    return (8, 8);
                }

                let mut offset = 0usize;
                let mut max_align = 1usize;

                if let Some(strukt) = self.hir_module.signature.structs.get(name.as_str()) {
                    for field in strukt.fields.values() {
                        let field_lir = self.hir_ty_to_lir_ty(field.ty, field.span);
                        let (field_size, field_align) =
                            self.lir_type_size_and_align_impl(&field_lir, visiting);
                        let field_align = field_align.max(1);
                        offset = Self::align_to(offset, field_align);
                        offset = offset.saturating_add(field_size);
                        max_align = max_align.max(field_align);
                    }
                } else {
                    visiting.remove(&visit_key);
                    return (8, 8);
                }

                visiting.remove(&visit_key);
                (Self::align_to(offset, max_align), max_align)
            }
            LirTy::UnionType(name) => {
                let visit_key = format!("U:{}", name);
                if !visiting.insert(visit_key.clone()) {
                    return (8, 8);
                }

                let mut max_size = 0usize;
                let mut max_align = 1usize;

                if let Some(union) = self.hir_module.signature.unions.get(name.as_str()) {
                    for variant in union.variants.values() {
                        let variant_lir = self.hir_ty_to_lir_ty(variant.ty, variant.span);
                        let (variant_size, variant_align) =
                            self.lir_type_size_and_align_impl(&variant_lir, visiting);
                        max_size = max_size.max(variant_size);
                        max_align = max_align.max(variant_align.max(1));
                    }
                } else {
                    visiting.remove(&visit_key);
                    return (8, 8);
                }

                visiting.remove(&visit_key);
                (Self::align_to(max_size, max_align), max_align)
            }
        }
    }

    fn align_to(value: usize, align: usize) -> usize {
        if align <= 1 {
            value
        } else {
            value.div_ceil(align) * align
        }
    }

    fn construct_type_id(&mut self, ty: &HirTy) -> LirResult<LirOperand> {
        let type_id: HirTyId = ty.into();
        let dest = self.new_temp();
        self.emit(LirInstr::LoadImm {
            ty: LirTy::UInt64,
            dst: dest.clone(),
            value: LirOperand::ImmUInt {
                val: type_id.0,
                size: 64,
            },
        })?;
        Ok(dest)
    }

    /// The goal of this function is to construct the core::type_info object for a given type.
    /// The core::type_info object declaration is contained into "core/reflection.atlas", and is of this form:
    /// ```atlas
    /// namespace core {
    ///     #[std::trivially_copyable]
    ///     public struct type_info {
    ///       public:
    ///         id: uint64;
    ///         name: *const uint8;
    ///         mangled_name: *const uint8;
    ///         size: uint64;
    ///         align: uint64;
    ///         method_names: [*const uint8];
    ///         method_count: uint64;
    ///         field_names: [*const uint8];
    ///         field_count: uint64;
    ///     }
    /// }
    ///
    /// ```
    fn construct_type_info_object(&mut self, ty: &HirTy, span: Span) -> LirResult<LirOperand> {
        let mut method_count = 0u64;
        let mut method_names = Vec::new();
        let mut field_count = 0u64;
        let mut field_names = Vec::new();
        let mut is_default = false;
        let type_name;
        let mangled_name;

        match ty {
            HirTy::Named(named) => {
                if let Some(sig) = self.hir_module.signature.structs.get(named.name) {
                    method_count = sig.methods.len() as u64;
                    method_names = sig.methods.keys().copied().collect();
                    field_count = sig.fields.len() as u64;
                    field_names = sig.fields.keys().copied().collect();
                    is_default = sig.is_std_default;
                    if let Some(c_name) = &sig.c_name {
                        type_name = c_name.to_string();
                        mangled_name = c_name.to_string();
                    } else if let Some(pre_mangled_ty) = sig.pre_mangled_ty {
                        type_name = pre_mangled_ty.name.to_string();
                        mangled_name = MonomorphizationPass::generate_mangled_name(
                            self.hir_arena,
                            pre_mangled_ty,
                            "struct",
                        )
                        .to_string()
                    } else {
                        type_name = named.name.to_string();
                        mangled_name = named.name.to_string();
                    }
                } else if let Some(sig) = self.hir_module.signature.unions.get(named.name) {
                    field_count = sig.variants.len() as u64;
                    field_names = sig.variants.keys().copied().collect();
                    if let Some(c_name) = &sig.c_name {
                        type_name = c_name.to_string();
                        mangled_name = c_name.to_string();
                    } else if let Some(pre_mangled_ty) = sig.pre_mangled_ty {
                        type_name = pre_mangled_ty.name.to_string();
                        mangled_name = MonomorphizationPass::generate_mangled_name(
                            self.hir_arena,
                            pre_mangled_ty,
                            "union",
                        )
                        .to_string();
                    } else {
                        type_name = named.name.to_string();
                        mangled_name = named.name.to_string();
                    }
                } else {
                    type_name = named.name.to_string();
                    mangled_name = named.name.to_string();
                }
            }
            HirTy::Generic(generic) => {
                let struct_name =
                    MonomorphizationPass::generate_mangled_name(self.hir_arena, generic, "struct");
                if let Some(sig) = self.hir_module.signature.structs.get(struct_name) {
                    method_count = sig.methods.len() as u64;
                    method_names = sig.methods.keys().copied().collect();
                    field_count = sig.fields.len() as u64;
                    field_names = sig.fields.keys().copied().collect();
                    is_default = sig.is_std_default;
                    if let Some(c_name) = &sig.c_name {
                        type_name = c_name.to_string();
                        mangled_name = c_name.to_string();
                    } else if let Some(pre_mangled_ty) = sig.pre_mangled_ty {
                        type_name = pre_mangled_ty.name.to_string();
                        mangled_name = MonomorphizationPass::generate_mangled_name(
                            self.hir_arena,
                            pre_mangled_ty,
                            "struct",
                        )
                        .to_string();
                    } else {
                        type_name = struct_name.to_string();
                        mangled_name = struct_name.to_string();
                    }
                } else {
                    let union_name = MonomorphizationPass::generate_mangled_name(
                        self.hir_arena,
                        generic,
                        "union",
                    );
                    if let Some(sig) = self.hir_module.signature.unions.get(union_name) {
                        field_count = sig.variants.len() as u64;
                        field_names = sig.variants.keys().copied().collect();
                        if let Some(c_name) = &sig.c_name {
                            type_name = c_name.to_string();
                            mangled_name = c_name.to_string();
                        } else if let Some(pre_mangled_ty) = sig.pre_mangled_ty {
                            type_name = pre_mangled_ty.name.to_string();
                            mangled_name = MonomorphizationPass::generate_mangled_name(
                                self.hir_arena,
                                pre_mangled_ty,
                                "union",
                            )
                            .to_string();
                        } else {
                            type_name = union_name.to_string();
                            mangled_name = union_name.to_string();
                        }
                    } else {
                        type_name = MonomorphizationPass::generate_mangled_name(
                            self.hir_arena,
                            generic,
                            "struct",
                        )
                        .to_string();
                        mangled_name = type_name.to_string();
                    }
                }
            }
            _ => {
                type_name = format!("{}", ty);
                mangled_name = type_name.to_string();
            }
        }

        let is_trivially_copyable = ty.is_trivially_copyable(&self.hir_module.signature);
        let is_copyable = ty.is_copyable(&self.hir_module.signature);

        let lir_target_ty = self.hir_ty_to_lir_ty(ty, span);
        let (size, align) = self.lir_type_size_and_align(&lir_target_ty);

        let mut field_values = BTreeMap::new();
        field_values.insert("id".to_string(), self.construct_type_id(ty)?);
        field_values.insert(
            "name".to_string(),
            LirOperand::Const(ConstantValue::String(type_name)),
        );
        field_values.insert(
            "mangled_name".to_string(),
            LirOperand::Const(ConstantValue::String(mangled_name)),
        );
        field_values.insert(
            "size".to_string(),
            LirOperand::ImmUInt {
                val: size as u64,
                size: 64,
            },
        );
        field_values.insert(
            "align".to_string(),
            LirOperand::ImmUInt {
                val: align as u64,
                size: 64,
            },
        );
        let method_names_array = if method_names.is_empty() {
            LirOperand::ImmUnit
        } else {
            let array_dst = self.new_temp();
            self.emit(LirInstr::LoadConst {
                dst: array_dst.clone(),
                value: LirOperand::ImmUnit, // Placeholder for the actual array data
            })?;
            let dst = self.new_temp();
            self.emit(LirInstr::ConstructArray {
                ty: LirTy::ArrayTy {
                    inner: Box::new(LirTy::Ptr {
                        is_const: true,
                        inner: Box::new(LirTy::UInt8),
                    }),
                    size: method_names.len(),
                },
                dst: dst.clone(),
                size,
            })?;
            for (idx, item) in method_names.iter().enumerate() {
                let src = LirOperand::Const(ConstantValue::String(item.to_string()));
                let index_operand = LirOperand::Index {
                    src: Box::new(dst.clone()),
                    index: Box::new(LirOperand::ImmUInt {
                        val: idx as u64,
                        size: 64,
                    }),
                };
                self.emit(LirInstr::Assign {
                    ty: LirTy::Ptr {
                        is_const: true,
                        inner: Box::new(LirTy::UInt8),
                    },
                    dst: index_operand,
                    src,
                })?;
            }
            dst
        };
        field_values.insert("method_names".to_string(), method_names_array);
        field_values.insert(
            "method_count".to_string(),
            LirOperand::ImmUInt {
                val: method_count,
                size: 64,
            },
        );
        let field_names_array = if field_names.is_empty() {
            LirOperand::ImmUnit
        } else {
            let array_dst = self.new_temp();
            self.emit(LirInstr::LoadConst {
                dst: array_dst.clone(),
                value: LirOperand::ImmUnit, // Placeholder for the actual array data
            })?;
            let dst = self.new_temp();
            self.emit(LirInstr::ConstructArray {
                ty: LirTy::ArrayTy {
                    inner: Box::new(LirTy::Ptr {
                        is_const: true,
                        inner: Box::new(LirTy::UInt8),
                    }),
                    size: field_names.len(),
                },
                dst: dst.clone(),
                size,
            })?;
            for (idx, item) in field_names.iter().enumerate() {
                let src = LirOperand::Const(ConstantValue::String(item.to_string()));
                let index_operand = LirOperand::Index {
                    src: Box::new(dst.clone()),
                    index: Box::new(LirOperand::ImmUInt {
                        val: idx as u64,
                        size: 64,
                    }),
                };
                self.emit(LirInstr::Assign {
                    ty: LirTy::Ptr {
                        is_const: true,
                        inner: Box::new(LirTy::UInt8),
                    },
                    dst: index_operand,
                    src,
                })?;
            }
            dst
        };
        field_values.insert("field_names".to_string(), field_names_array);
        field_values.insert(
            "field_count".to_string(),
            LirOperand::ImmUInt {
                val: field_count,
                size: 64,
            },
        );
        field_values.insert(
            "is_trivially_copyable".to_string(),
            LirOperand::ImmBool(is_trivially_copyable),
        );
        field_values.insert("is_copyable".to_string(), LirOperand::ImmBool(is_copyable));
        field_values.insert("is_default".to_string(), LirOperand::ImmBool(is_default));

        let dst = self.new_temp();
        self.emit(LirInstr::ConstructObject {
            ty: LirTy::StructType("core::type_info".to_string()),
            dst: dst.clone(),
            field_values,
        })?;

        Ok(dst)
    }
}

fn unknown_type_err(ty_name: &str, span: Span) -> Box<LirLoweringError> {
    Box::new(LirLoweringError::UnknownType(UnknownTypeError {
        ty_name: ty_name.to_string(),
        span,
        src: NamedSource::new(span.path, utils::get_file_content(span.path).unwrap()),
    }))
}

fn unsupported_expr(span: Span, expr: String) -> Box<LirLoweringError> {
    Box::new(LirLoweringError::UnsupportedHirExpr(
        UnsupportedHirExprError {
            span,
            src: NamedSource::new(span.path, utils::get_file_content(span.path).unwrap()),
            expr,
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
            LirInstr::LogicalAnd { ty, dest, a, b } => {
                write!(f, "{} = logical_and.{} {}, {}", dest, ty, a, b)
            }
            LirInstr::LogicalOr { ty, dest, a, b } => {
                write!(f, "{} = logical_or.{} {}, {}", dest, ty, a, b)
            }
            LirInstr::Negate { ty: _, dest, src } => {
                write!(f, "{} = neg {}", dest, src)
            }
            LirInstr::Not { ty: _, dest, src } => {
                write!(f, "{} = not {}", dest, src)
            }
            LirInstr::BinaryNot { ty: _, dest, src } => {
                write!(f, "{} = bin_not {}", dest, src)
            }
            LirInstr::ShiftLeft { dest, a, b, ty } => {
                write!(f, "{} = shl.{} {}, {}", dest, ty, a, b)
            }
            LirInstr::ShiftRight { dest, a, b, ty } => {
                write!(f, "{} = shr.{} {}, {}", dest, ty, a, b)
            }
            LirInstr::BinaryAnd { dest, a, b, ty } => {
                write!(f, "{} = bin_and.{} {}, {}", dest, ty, a, b)
            }
            LirInstr::BinaryOr { dest, a, b, ty } => {
                write!(f, "{} = bin_or.{} {}, {}", dest, ty, a, b)
            }
            LirInstr::BinaryXor { dest, a, b, ty } => {
                write!(f, "{} = bin_xor.{} {}, {}", dest, ty, a, b)
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
            LirInstr::CallPtr {
                ty: _,
                dst,
                callee,
                args,
                param_tys: _,
            } => {
                let args_str = args
                    .iter()
                    .map(|a| format!("{}", a))
                    .collect::<Vec<_>>()
                    .join(", ");
                if let Some(d) = dst {
                    write!(f, "{} = call_ptr {}({})", d, callee, args_str)
                } else {
                    write!(f, "call_ptr {}({})", callee, args_str)
                }
            }
            LirInstr::ConstructArray { ty, dst, size } => {
                write!(f, "{} = new_array {}[{}]", dst, ty, size)
            }
            LirInstr::ConstructObject {
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
            LirInstr::Delete {
                ty,
                src,
                should_free,
            } => {
                write!(f, "delete(free={}) {} {}", should_free, ty, src)
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
            LirInstr::HeapAllocCopy { ty, dst, src } => {
                write!(f, "heap_alloc_copy.{} {}, {}", ty, dst, src)
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
            LirOperand::GlobalFn(name) => write!(f, "@{}", name),
            LirOperand::Const(val) => write!(f, "#{}", val),
            LirOperand::ImmInt { val: i, size: _ } => write!(f, "%imm{}", i),
            LirOperand::ImmUInt { val: u, size: _ } => write!(f, "%imm{}", u),
            LirOperand::ImmFloat { val: fl, size: _ } => write!(f, "%imm{}", fl),
            LirOperand::ImmBool(b) => write!(f, "%imm{}", b),
            LirOperand::ImmChar(c) => write!(f, "%imm{}", c),
            LirOperand::ImmUnit => write!(f, "%imm()"),
            LirOperand::Deref(d) => write!(f, "*{}", d),
            LirOperand::AsRef(a) => write!(f, "&{}", a),
            LirOperand::FieldAccess {
                src,
                field_name,
                is_arrow,
                ..
            } => {
                if *is_arrow {
                    write!(f, "{}->{}", src, field_name)
                } else {
                    write!(f, "{}.{}", src, field_name)
                }
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
            LirTy::Ptr { is_const: _, inner } => write!(f, "ptr<{}>", inner),
            LirTy::FnPtr { ret, args } => {
                let args = args
                    .iter()
                    .map(|arg| format!("{}", arg))
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "fnptr<({}) -> {}>", args, ret)
            }
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
