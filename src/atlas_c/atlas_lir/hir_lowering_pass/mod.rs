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
            CurrentFunctionDoesntExistError, LIRLoweringError, LIRResult, NoReturnInFunctionError,
            UnsupportedHirExprError,
        },
        program::{
            LIRBlock, LIRFunction, LIRInstr, LIROperand, LIRPrimitiveType, LIRProgram,
            LIRTerminator,
        },
    },
    utils,
};

/// HIR to LIR lowering pass
///
/// This pass converts the HIR (after ownership analysis) into a simple SSA-like
/// LIR form suitable for optimization and final code generation.
///
/// Currently supports: fib example (arithmetic, comparisons, if-else, calls, return)
pub struct HirLoweringPass<'hir> {
    hir_module: &'hir HirModule<'hir>,
    /// The function currently being lowered
    current_function: Option<LIRFunction>,
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

    /// Lower the entire HIR module to LIR
    pub fn lower(&mut self) -> LIRResult<LIRProgram> {
        let mut functions = Vec::new();

        for func in self.hir_module.body.functions.values() {
            let lir_func = self.lower_function(func)?;
            functions.push(lir_func);
        }

        Ok(LIRProgram { functions })
    }

    /// Generate a new unique temp variable
    fn new_temp(&mut self) -> LIROperand {
        let id = self.temp_counter;
        self.temp_counter += 1;
        LIROperand::Temp(id)
    }

    /// Generate a new unique block label
    fn new_block_label(&mut self, prefix: &str) -> String {
        let id = self.block_counter;
        self.block_counter += 1;
        format!("{}_{}", prefix, id)
    }

    /// Creates a new block and returns its label
    fn create_block(&mut self, label: String) -> LIRResult<String> {
        if let Some(func) = &mut self.current_function {
            func.blocks.push(LIRBlock {
                label: label.clone(),
                instructions: Vec::new(),
                terminator: LIRTerminator::None,
            });
            Ok(label)
        } else {
            Err(Box::new(LIRLoweringError::CurrentFunctionDoesntExist(
                CurrentFunctionDoesntExistError,
            )))
        }
    }

    /// Push an instruction to the current (last) block
    fn emit(&mut self, instr: LIRInstr) -> LIRResult<()> {
        if let Some(func) = &mut self.current_function {
            if let Some(block) = func.blocks.last_mut() {
                block.instructions.push(instr);
                Ok(())
            } else {
                Err(Box::new(LIRLoweringError::CurrentFunctionDoesntExist(
                    CurrentFunctionDoesntExistError,
                )))
            }
        } else {
            Err(Box::new(LIRLoweringError::CurrentFunctionDoesntExist(
                CurrentFunctionDoesntExistError,
            )))
        }
    }

    fn emit_terminator(&mut self, terminator: LIRTerminator) -> LIRResult<()> {
        if let Some(func) = &mut self.current_function {
            if let Some(block) = func.blocks.last_mut() {
                block.terminator = terminator;
                Ok(())
            } else {
                Err(Box::new(LIRLoweringError::CurrentFunctionDoesntExist(
                    CurrentFunctionDoesntExistError,
                )))
            }
        } else {
            Err(Box::new(LIRLoweringError::CurrentFunctionDoesntExist(
                CurrentFunctionDoesntExistError,
            )))
        }
    }

    /// Lower a single function
    fn lower_function(&mut self, func: &'hir HirFunction<'hir>) -> LIRResult<LIRFunction> {
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
        self.current_function = Some(LIRFunction {
            name: func.name.to_string(),
            args: func
                .signature
                .params
                .iter()
                .map(|p| self.hir_ty_to_lir_primitive(p.ty))
                .collect(),
            return_type: {
                let lir_ty = self.hir_ty_to_lir_primitive(&func.signature.return_ty);
                if lir_ty == LIRPrimitiveType::Unit {
                    None
                } else {
                    Some(lir_ty)
                }
            },
            blocks: vec![LIRBlock {
                label: "entry".to_string(),
                instructions: Vec::new(),
                terminator: LIRTerminator::None,
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
                if matches!(b.terminator, LIRTerminator::None) {
                    // For functions returning unit, ensure there's a return at the end
                    if func.name == "main" {
                        self.emit_terminator(LIRTerminator::Halt)?;
                    } else {
                        self.emit_terminator(LIRTerminator::Return { value: None })?;
                    }
                }
            } else if !matches!(
                b.terminator,
                LIRTerminator::Return { value: Some(_) } | LIRTerminator::Halt
            ) {
                // It should return something, but doesn't
                // TODO: Add a ! type so if the last statement is a call to a function returning !, we don't error
                // TODO: Add CFG analysis to check all paths because right now only the else branch has to return,
                //  the if branch can just fallthrough
                return Err(Box::new(LIRLoweringError::NoReturnInFunction(
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
    fn lower_stmt(&mut self, stmt: &'hir HirStatement<'hir>) -> LIRResult<()> {
        match stmt {
            HirStatement::Return(ret) => {
                let value = self.lower_expr(&ret.value)?;
                self.emit_terminator(LIRTerminator::Return { value: Some(value) })?;
            }

            HirStatement::IfElse(if_else) => {
                // Lower condition
                let cond = self.lower_expr(&if_else.condition)?;

                // Create block labels
                let then_label = self.new_block_label("then");
                let else_label = self.new_block_label("else");
                let merge_label = self.new_block_label("merge");

                // Emit branch
                self.emit_terminator(LIRTerminator::BranchIf {
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
                let local_temp = self.new_temp();
                if let LIROperand::Temp(id) = local_temp {
                    self.local_map.insert(let_stmt.name, id);
                }
                self.emit(LIRInstr::Copy {
                    dst: local_temp,
                    src: value,
                })?;
            }

            _ => {
                // For now, skip unsupported statements
                // In a complete implementation, handle all variants
            }
        }
        Ok(())
    }

    /// Lower an expression, returning the operand holding the result
    fn lower_expr(&mut self, expr: &'hir HirExpr<'hir>) -> LIRResult<LIROperand> {
        match expr {
            // === Literals ===
            HirExpr::IntegerLiteral(lit) => {
                let dest = self.new_temp();
                // For simplicity, we store the constant value directly
                // A real impl would use a constant pool
                self.emit(LIRInstr::LoadImm {
                    dst: dest.clone(),
                    value: LIROperand::ImmInt(lit.value),
                })?;
                Ok(dest)
            }

            HirExpr::BooleanLiteral(lit) => {
                let dest = self.new_temp();
                self.emit(LIRInstr::LoadImm {
                    dst: dest.clone(),
                    value: LIROperand::ImmBool(lit.value),
                })?;
                Ok(dest)
            }

            HirExpr::StringLiteral(lit) => {
                let dest = self.new_temp();
                self.emit(LIRInstr::LoadConst {
                    dst: dest.clone(),
                    value: LIROperand::Const(ConstantValue::String(String::from(lit.value))),
                })?;
                Ok(dest)
            }

            // === Identifiers (variables/parameters) ===
            HirExpr::Ident(ident) => {
                // Check if it's a parameter
                if let Some(&arg_idx) = self.param_map.get(ident.name) {
                    Ok(LIROperand::Arg(arg_idx))
                }
                // Check if it's a local variable
                else if let Some(&temp_id) = self.local_map.get(ident.name) {
                    Ok(LIROperand::Temp(temp_id))
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
                    HirBinaryOperator::Add => LIRInstr::Add {
                        ty,
                        dest: dest.clone(),
                        a: lhs,
                        b: rhs,
                    },
                    HirBinaryOperator::Sub => LIRInstr::Sub {
                        ty,
                        dest: dest.clone(),
                        a: lhs,
                        b: rhs,
                    },
                    HirBinaryOperator::Mul => LIRInstr::Mul {
                        ty,
                        dest: dest.clone(),
                        a: lhs,
                        b: rhs,
                    },
                    HirBinaryOperator::Div => LIRInstr::Div {
                        ty,
                        dest: dest.clone(),
                        a: lhs,
                        b: rhs,
                    },
                    HirBinaryOperator::Mod => LIRInstr::Mod {
                        ty,
                        dest: dest.clone(),
                        a: lhs,
                        b: rhs,
                    },
                    HirBinaryOperator::Lt => LIRInstr::LessThan {
                        ty,
                        dest: dest.clone(),
                        a: lhs,
                        b: rhs,
                    },
                    HirBinaryOperator::Lte => LIRInstr::LessThanOrEqual {
                        ty,
                        dest: dest.clone(),
                        a: lhs,
                        b: rhs,
                    },
                    HirBinaryOperator::Gt => LIRInstr::GreaterThan {
                        ty,
                        dest: dest.clone(),
                        a: lhs,
                        b: rhs,
                    },
                    HirBinaryOperator::Gte => LIRInstr::GreaterThanOrEqual {
                        ty,
                        dest: dest.clone(),
                        a: lhs,
                        b: rhs,
                    },
                    HirBinaryOperator::Eq => LIRInstr::Equal {
                        ty,
                        dest: dest.clone(),
                        a: lhs,
                        b: rhs,
                    },
                    HirBinaryOperator::Neq => LIRInstr::NotEqual {
                        ty,
                        dest: dest.clone(),
                        a: lhs,
                        b: rhs,
                    },
                    _ => {
                        let path = expr.span().path;
                        let src = utils::get_file_content(path).unwrap();
                        return Err(Box::new(LIRLoweringError::UnsupportedHirExpr(
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
                        return Err(Box::new(LIRLoweringError::UnsupportedHirExpr(
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
                    LIRInstr::ExternCall {
                        dst: dest.clone(),
                        func_name,
                        args,
                    }
                } else {
                    LIRInstr::Call {
                        dst: dest.clone(),
                        func_name,
                        args,
                    }
                };

                self.emit(instr)?;
                Ok(dest.unwrap_or(LIROperand::ImmInt(0))) // unit value
            }

            // === Move/Copy (ownership pass artifacts) ===
            HirExpr::Move(move_expr) => {
                // Move is just a value use in LIR (no runtime work)
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
                Err(Box::new(LIRLoweringError::UnsupportedHirExpr(
                    UnsupportedHirExprError {
                        span: expr.span(),
                        src: NamedSource::new(path, src),
                    },
                )))
            }
        }
    }

    /// Convert HIR type to LIR primitive type
    fn hir_ty_to_lir_primitive(&self, ty: &HirTy) -> LIRPrimitiveType {
        match ty {
            HirTy::Int64(_) => LIRPrimitiveType::Int64,
            HirTy::UInt64(_) => LIRPrimitiveType::UInt64,
            HirTy::Float64(_) => LIRPrimitiveType::Float64,
            HirTy::Boolean(_) => LIRPrimitiveType::Boolean,
            HirTy::Char(_) => LIRPrimitiveType::Char,
            HirTy::String(_) => LIRPrimitiveType::Str,
            HirTy::Unit(_) => LIRPrimitiveType::Unit,
            _ => LIRPrimitiveType::Int64, // Default fallback
        }
    }
}

// ============================================================================
// Pretty printing for debugging
// ============================================================================

impl std::fmt::Display for LIRProgram {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for func in &self.functions {
            writeln!(f, "{}", func)?;
        }
        Ok(())
    }
}

impl std::fmt::Display for LIRFunction {
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

impl std::fmt::Display for LIRBlock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "\t{}:", self.label)?;
        for instr in &self.instructions {
            writeln!(f, "\t\t{}", instr)?;
        }
        // Print the terminator (unless it's None)
        if !matches!(self.terminator, LIRTerminator::None) {
            writeln!(f, "\t\t{}", self.terminator)?;
        }
        Ok(())
    }
}

impl std::fmt::Display for LIRInstr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LIRInstr::Add { dest, a, b, ty } => {
                write!(f, "{} = add.{} {}, {}", dest, ty, a, b)
            }
            LIRInstr::Sub { dest, a, b, ty } => {
                write!(f, "{} = sub.{} {}, {}", dest, ty, a, b)
            }
            LIRInstr::Mul { dest, a, b, ty } => {
                write!(f, "{} = mul.{} {}, {}", dest, ty, a, b)
            }
            LIRInstr::Div { dest, a, b, ty } => {
                write!(f, "{} = div.{} {}, {}", dest, ty, a, b)
            }
            LIRInstr::Mod { dest, a, b, ty } => {
                write!(f, "{} = mod.{} {}, {}", dest, ty, a, b)
            }
            LIRInstr::LessThan { dest, a, b, ty } => {
                write!(f, "{} = lt.{} {}, {}", dest, ty, a, b)
            }
            LIRInstr::LessThanOrEqual { dest, a, b, ty } => {
                write!(f, "{} = le.{} {}, {}", dest, ty, a, b)
            }
            LIRInstr::GreaterThan { dest, a, b, ty } => {
                write!(f, "{} = gt.{} {}, {}", dest, ty, a, b)
            }
            LIRInstr::GreaterThanOrEqual { dest, a, b, ty } => {
                write!(f, "{} = ge.{} {}, {}", dest, ty, a, b)
            }
            LIRInstr::Equal { dest, a, b, ty } => {
                write!(f, "{} = eq.{} {}, {}", dest, ty, a, b)
            }
            LIRInstr::NotEqual { dest, a, b, ty } => {
                write!(f, "{} = ne.{} {}, {}", dest, ty, a, b)
            }
            LIRInstr::Copy { dst, src } => {
                write!(f, "{} = copy {}", dst, src)
            }
            LIRInstr::LoadConst { dst, value } => {
                write!(f, "{} = ld_const {}", dst, value)
            }
            LIRInstr::LoadImm { dst, value } => {
                write!(f, "{} = ld_imm {}", dst, value)
            }
            LIRInstr::Call {
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
            LIRInstr::ExternCall {
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

impl std::fmt::Display for LIROperand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LIROperand::Temp(id) => write!(f, "%t{}", id),
            LIROperand::Arg(idx) => write!(f, "%arg{}", idx),
            LIROperand::Const(val) => write!(f, "#{}", val),
            LIROperand::ImmInt(i) => write!(f, "%imm{}", i),
            LIROperand::ImmUInt(u) => write!(f, "%imm{}", u),
            LIROperand::ImmFloat(fl) => write!(f, "%imm{}", fl),
            LIROperand::ImmBool(b) => write!(f, "%imm{}", b),
            LIROperand::ImmChar(c) => write!(f, "%imm{}", c),
        }
    }
}

impl std::fmt::Display for LIRPrimitiveType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LIRPrimitiveType::Int8 => write!(f, "int8"),
            LIRPrimitiveType::UInt8 => write!(f, "uint8"),
            LIRPrimitiveType::Int16 => write!(f, "int16"),
            LIRPrimitiveType::UInt16 => write!(f, "uint16"),
            LIRPrimitiveType::Int32 => write!(f, "int32"),
            LIRPrimitiveType::UInt32 => write!(f, "uint32"),
            LIRPrimitiveType::Int64 => write!(f, "int64"),
            LIRPrimitiveType::UInt64 => write!(f, "uint64"),
            LIRPrimitiveType::Float32 => write!(f, "float32"),
            LIRPrimitiveType::Float64 => write!(f, "float64"),
            LIRPrimitiveType::Boolean => write!(f, "bool"),
            LIRPrimitiveType::Char => write!(f, "char"),
            LIRPrimitiveType::Str => write!(f, "str"),
            LIRPrimitiveType::Unit => write!(f, "unit"),
        }
    }
}

impl std::fmt::Display for LIRTerminator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LIRTerminator::BranchIf {
                condition,
                then_label,
                else_label,
            } => {
                write!(f, "br_if {}, [{}, {}]", condition, then_label, else_label)
            }
            LIRTerminator::Return { value } => {
                if let Some(v) = value {
                    write!(f, "ret {}", v)
                } else {
                    write!(f, "ret")
                }
            }
            LIRTerminator::Branch { target } => {
                write!(f, "br {}", target)
            }
            LIRTerminator::Halt => {
                write!(f, "hlt")
            }
            LIRTerminator::None => write!(f, "<no terminator>"),
        }
    }
}
