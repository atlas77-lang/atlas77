use std::collections::HashMap;

use miette::NamedSource;

use crate::atlas_c::{
    atlas_hir::{
        HirModule,
        expr::{HirBinaryOperator, HirExpr},
        item::HirFunction,
        signature::ConstantValue,
        stmt::HirStatement,
        ty::HirTy,
    },
    atlas_lir::{
        error::{
            CurrentFunctionDoesntExistError, LirLoweringError, LirResult, NoReturnInFunctionError,
            UnsupportedHirExprError,
        },
        program::{
            LirBlock, LirFunction, LirInstr, LirOperand, LirPrimitiveType, LirProgram,
            LirTerminator,
        },
    },
    utils,
};

/// Hir to Lir lowering pass
///
/// This pass converts the Hir (after ownership analysis) into a simple SSA-like
/// Lir form suitable for optimization and final code generation.
///
/// Currently supports: fib example (arithmetic, comparisons, if-else, calls, return)
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
}

impl<'hir> HirLoweringPass<'hir> {
    pub fn new(hir_module: &'hir HirModule<'hir>) -> Self {
        Self {
            hir_module,
            current_function: None,
            temp_counter: 0,
            block_counter: 0,
            param_map: HashMap::new(),
            local_map: HashMap::new(),
        }
    }

    /// Lower the entire Hir module to Lir
    pub fn lower(&mut self) -> LirResult<LirProgram> {
        let mut functions = Vec::new();

        for func in self.hir_module.body.functions.values() {
            let lir_func = self.lower_function(func)?;
            functions.push(lir_func);
        }

        Ok(LirProgram { functions })
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
        if let Some(func) = &mut self.current_function {
            if let Some(block) = func.blocks.last_mut() {
                block.instructions.push(instr);
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
                .map(|p| self.hir_ty_to_lir_primitive(p.ty))
                .collect(),
            return_type: {
                let lir_ty = self.hir_ty_to_lir_primitive(&func.signature.return_ty);
                if lir_ty == LirPrimitiveType::Unit {
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
                // If the then branch doesn't end with a return, jump to merge
                // (For fib, both branches return, so this won't execute)

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

            HirStatement::Let(let_stmt) => {
                let value = self.lower_expr(&let_stmt.value)?;
                // Allocate a temp for this local variable
                //let local_temp = self.new_temp();
                if let LirOperand::Temp(id) = value {
                    self.local_map.insert(let_stmt.name, id);
                }
            }

            _ => {
                // For now, skip unsupported statements
                // In a complete implementation, handle all variants
            }
        }
        Ok(())
    }

    /// Lower an expression, returning the operand holding the result
    fn lower_expr(&mut self, expr: &'hir HirExpr<'hir>) -> LirResult<LirOperand> {
        match expr {
            // === Literals ===
            HirExpr::IntegerLiteral(lit) => {
                let dest = self.new_temp();
                // For simplicity, we store the constant value directly
                // A real impl would use a constant pool
                self.emit(LirInstr::LoadImm {
                    dst: dest.clone(),
                    value: LirOperand::ImmInt(lit.value),
                })?;
                Ok(dest)
            }

            HirExpr::BooleanLiteral(lit) => {
                let dest = self.new_temp();
                self.emit(LirInstr::LoadImm {
                    dst: dest.clone(),
                    value: LirOperand::ImmBool(lit.value),
                })?;
                Ok(dest)
            }

            HirExpr::StringLiteral(lit) => {
                let dest = self.new_temp();
                self.emit(LirInstr::LoadConst {
                    dst: dest.clone(),
                    value: LirOperand::Const(ConstantValue::String(String::from(lit.value))),
                })?;
                Ok(dest)
            }

            HirExpr::UnitLiteral(_) => {
                let dest = self.new_temp();
                self.emit(LirInstr::LoadImm {
                    dst: dest.clone(),
                    value: LirOperand::ImmUnit,
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

            HirExpr::Unary(unary) => {
                // Because this implementation should only handle fib, we skip unary ops
                self.lower_expr(&unary.expr)
            }

            // === Binary operations ===
            HirExpr::HirBinaryOperation(binop) => {
                let lhs = self.lower_expr(&binop.lhs)?;
                let rhs = self.lower_expr(&binop.rhs)?;
                let dest = self.new_temp();

                let ty = self.hir_ty_to_lir_primitive(binop.ty);

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
                        let path = expr.span().path;
                        let src = utils::get_file_content(path).unwrap();
                        return Err(Box::new(LirLoweringError::UnsupportedHirExpr(
                            UnsupportedHirExprError {
                                span: expr.span(),
                                src: NamedSource::new(path, src),
                            },
                        )));
                    }
                };

                self.emit(instr)?;
                Ok(dest)
            }

            // === Function calls ===
            HirExpr::Call(call) => {
                // Lower arguments
                let mut args = Vec::new();
                for arg in &call.args {
                    args.push(self.lower_expr(arg)?);
                }

                // Get function name from callee
                let func_name = match call.callee.as_ref() {
                    HirExpr::Ident(ident) => ident.name.to_string(),
                    _ => {
                        let path = expr.span().path;
                        let src = utils::get_file_content(path).unwrap();
                        return Err(Box::new(LirLoweringError::UnsupportedHirExpr(
                            UnsupportedHirExprError {
                                span: expr.span(),
                                src: NamedSource::new(path, src),
                            },
                        )));
                    }
                };

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
                        dst: dest.clone(),
                        func_name,
                        args,
                    }
                } else {
                    LirInstr::Call {
                        dst: dest.clone(),
                        func_name,
                        args,
                    }
                };

                self.emit(instr)?;
                Ok(dest.unwrap_or(LirOperand::ImmInt(0))) // unit value
            }

            // === Move/Copy (ownership pass artifacts) ===
            HirExpr::Move(move_expr) => {
                // Move is just a value use in Lir (no runtime work)
                self.lower_expr(&move_expr.expr)
            }

            HirExpr::Copy(copy_expr) => {
                // For primitives, copy is just a value use
                // For objects, this would call _copy method
                // For fib (all primitives), just lower the inner expression
                self.lower_expr(&copy_expr.expr)
            }

            _ => {
                let path = expr.span().path;
                let src = utils::get_file_content(path).unwrap();
                Err(Box::new(LirLoweringError::UnsupportedHirExpr(
                    UnsupportedHirExprError {
                        span: expr.span(),
                        src: NamedSource::new(path, src),
                    },
                )))
            }
        }
    }

    /// Convert HIR type to Lir primitive type
    fn hir_ty_to_lir_primitive(&self, ty: &HirTy) -> LirPrimitiveType {
        match ty {
            HirTy::Int64(_) => LirPrimitiveType::Int64,
            HirTy::UInt64(_) => LirPrimitiveType::UInt64,
            HirTy::Float64(_) => LirPrimitiveType::Float64,
            HirTy::Boolean(_) => LirPrimitiveType::Boolean,
            HirTy::Char(_) => LirPrimitiveType::Char,
            HirTy::String(_) => LirPrimitiveType::Str,
            HirTy::Unit(_) => LirPrimitiveType::Unit,
            _ => LirPrimitiveType::Int64, // Default fallback
        }
    }
}

// ============================================================================
// Pretty printing for debugging
// ============================================================================

impl std::fmt::Display for LirProgram {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for func in &self.functions {
            writeln!(f, "{}", func)?;
        }
        Ok(())
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
            LirInstr::LoadConst { dst, value } => {
                write!(f, "{} = ld_const {}", dst, value)
            }
            LirInstr::LoadImm { dst, value } => {
                write!(f, "{} = ld_imm {}", dst, value)
            }
            LirInstr::Call {
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
        }
    }
}

impl std::fmt::Display for LirPrimitiveType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LirPrimitiveType::Int8 => write!(f, "int8"),
            LirPrimitiveType::UInt8 => write!(f, "uint8"),
            LirPrimitiveType::Int16 => write!(f, "int16"),
            LirPrimitiveType::UInt16 => write!(f, "uint16"),
            LirPrimitiveType::Int32 => write!(f, "int32"),
            LirPrimitiveType::UInt32 => write!(f, "uint32"),
            LirPrimitiveType::Int64 => write!(f, "int64"),
            LirPrimitiveType::UInt64 => write!(f, "uint64"),
            LirPrimitiveType::Float32 => write!(f, "float32"),
            LirPrimitiveType::Float64 => write!(f, "float64"),
            LirPrimitiveType::Boolean => write!(f, "bool"),
            LirPrimitiveType::Char => write!(f, "char"),
            LirPrimitiveType::Str => write!(f, "str"),
            LirPrimitiveType::Unit => write!(f, "unit"),
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
