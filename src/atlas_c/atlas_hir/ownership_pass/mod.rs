//! Ownership Analysis Pass
//!
//! This pass implements MOVE/COPY semantics for Atlas77 following these principles:
//!
//! 1. **Ownership Model**: Every variable owns its value. Ownership can be:
//!    - **Moved**: single owner transfers (source becomes invalid)
//!    - **Copied**: new owner created via copy constructor (source remains valid)
//!
//! 2. **Copy Eligibility**: A type is copyable IFF:
//!    - It's a primitive type (int, float, bool, char, uint)
//!    - It's a reference type (&T, &const T) - references are just pointers
//!    - It's a struct that defines a `_copy` method
//!    - It's a string (built-in copyable)
//!
//! 3. **COPY-Biased Lowering**: Initially assume everything is a COPY if the type allows it.
//!    This guarantees correctness because copy constructors preserve source validity.
//!
//! 4. **Move Resolution (Last-Use Analysis)**: After initial lowering, identify the last
//!    ownership-consuming use of each variable and rewrite COPY → MOVE for that use.
//!
//! 5. **Destructor Insertion**: At the end of each scope, emit `delete` for every variable
//!    that still owns a value (status == Owned).
//!
//! ## Phases
//!
//! The pass operates in multiple phases:
//!
//! 1. **Phase 1: Use Collection** - Walk the AST and record all uses of each variable,
//!    classifying them as Read or OwnershipConsuming.
//!
//! 2. **Phase 2: COPY-Biased Lowering** - For each ownership-consuming use, insert a COPY
//!    expression if the type is copyable, otherwise mark as needing MOVE.
//!
//! 3. **Phase 3: Last-Use Optimization** - For each variable, find the last ownership-consuming
//!    use and convert COPY → MOVE if it was a COPY.
//!
//! 4. **Phase 4: Destructor Insertion** - At scope ends, insert DELETE for owned variables.
//!
//! 5. **Phase 5: Validation** - Check for illegal programs (use after move, copying non-copyable, etc.)

mod context;
pub use context::{ScopeMap, UseKind, VarData, VarKind, VarMap, VarStatus, VarUse};

use crate::atlas_c::{
    atlas_hir::{
        HirModule,
        arena::HirArena,
        error::{
            HirError, HirResult, TryingToAccessAMovedValueError,
        },
        expr::{HirCopyExpr, HirDeleteExpr, HirExpr, HirIdentExpr, HirMoveExpr},
        item::HirFunction,
        signature::HirModuleSignature,
        stmt::{HirBlock, HirExprStmt, HirStatement, HirVariableStmt},
        ty::HirTy,
    },
    utils::{self, Span},
};
use miette::NamedSource;

/// The Ownership Analysis Pass
///
/// Implements move/copy semantics and destructor insertion.
pub struct OwnershipPass<'hir> {
    /// Tracks variable ownership state across scopes
    pub scope_map: ScopeMap<'hir>,
    /// Collected errors during the pass
    pub errors: Vec<HirError>,
    /// Module signature for checking copy constructors
    pub hir_signature: HirModuleSignature<'hir>,
    /// Arena for allocating new HIR nodes
    hir_arena: &'hir HirArena<'hir>,
    /// Current statement index (for use tracking)
    current_stmt_index: usize,
}

impl<'hir> OwnershipPass<'hir> {
    pub fn new(
        hir_signature: HirModuleSignature<'hir>,
        hir_arena: &'hir HirArena<'hir>,
    ) -> Self {
        Self {
            scope_map: ScopeMap::new(),
            errors: Vec::new(),
            hir_signature,
            hir_arena,
            current_stmt_index: 0,
        }
    }

    /// Run the ownership pass on the entire module
    pub fn run(
        &mut self,
        hir: &'hir mut HirModule<'hir>,
    ) -> HirResult<&'hir mut HirModule<'hir>> {
        // Process top-level functions
        for func in hir.body.functions.values_mut() {
            // Reset state for each function
            self.scope_map = ScopeMap::new();
            self.current_stmt_index = 0;

            if let Err(e) = self.analyze_function(func) {
                self.errors.push(e);
            }
        }
        
        // Process struct methods and constructors
        for struct_def in hir.body.structs.values_mut() {
            // Process methods
            for method in struct_def.methods.iter_mut() {
                self.scope_map = ScopeMap::new();
                self.current_stmt_index = 0;
                
                // Register method parameters including 'this'
                for param in method.signature.params.iter() {
                    let kind = self.classify_type_kind(param.ty);
                    let is_copyable = self.is_type_copyable(param.ty);
                    self.scope_map.insert(
                        param.name,
                        VarData::new(param.name, param.span, kind, param.ty, is_copyable, 0),
                    );
                }
                
                // First pass: collect uses
                if let Err(e) = self.collect_uses_in_block(&method.body) {
                    self.errors.push(e);
                    continue;
                }
                
                // Second pass: transform
                self.current_stmt_index = 0;
                if let Err(e) = self.transform_block(&mut method.body) {
                    self.errors.push(e);
                }
            }
            
            // Process constructor
            {
                self.scope_map = ScopeMap::new();
                self.current_stmt_index = 0;
                
                for param in struct_def.constructor.params.iter() {
                    let kind = self.classify_type_kind(param.ty);
                    let is_copyable = self.is_type_copyable(param.ty);
                    self.scope_map.insert(
                        param.name,
                        VarData::new(param.name, param.span, kind, param.ty, is_copyable, 0),
                    );
                }
                
                if let Err(e) = self.collect_uses_in_block(&struct_def.constructor.body) {
                    self.errors.push(e);
                } else {
                    self.current_stmt_index = 0;
                    if let Err(e) = self.transform_block(&mut struct_def.constructor.body) {
                        self.errors.push(e);
                    }
                }
            }
            
            // Process destructor
            {
                self.scope_map = ScopeMap::new();
                self.current_stmt_index = 0;
                
                for param in struct_def.destructor.params.iter() {
                    let kind = self.classify_type_kind(param.ty);
                    let is_copyable = self.is_type_copyable(param.ty);
                    self.scope_map.insert(
                        param.name,
                        VarData::new(param.name, param.span, kind, param.ty, is_copyable, 0),
                    );
                }
                
                if let Err(e) = self.collect_uses_in_block(&struct_def.destructor.body) {
                    self.errors.push(e);
                } else {
                    self.current_stmt_index = 0;
                    if let Err(e) = self.transform_block(&mut struct_def.destructor.body) {
                        self.errors.push(e);
                    }
                }
            }
        }
        
        // Return the first error if any were collected
        if let Some(err) = self.errors.pop() {
            return Err(err);
        }
        
        Ok(hir)
    }

    /// Analyze a single function for ownership semantics
    fn analyze_function(&mut self, func: &mut HirFunction<'hir>) -> HirResult<()> {
        // Phase 1: Register function parameters as owned variables
        for param in func.signature.params.iter() {
            let kind = self.classify_type_kind(param.ty);
            let is_copyable = self.is_type_copyable(param.ty);

            self.scope_map.insert(
                param.name,
                VarData::new(param.name, param.span, kind, param.ty, is_copyable, 0),
            );
        }

        // Phase 2: Analyze the function body
        // First pass: collect all uses
        self.collect_uses_in_block(&func.body)?;

        // Second pass: apply COPY-biased lowering and then optimize to MOVE
        self.current_stmt_index = 0;
        self.transform_block(&mut func.body)?;

        Ok(())
    }

    // =========================================================================
    // Phase 1: Use Collection
    // =========================================================================

    /// Collect all variable uses in a block (first pass)
    fn collect_uses_in_block(&mut self, block: &HirBlock<'hir>) -> HirResult<()> {
        for stmt in block.statements.iter() {
            self.collect_uses_in_stmt(stmt)?;
            self.current_stmt_index += 1;
        }
        Ok(())
    }

    /// Collect uses from a statement
    fn collect_uses_in_stmt(&mut self, stmt: &HirStatement<'hir>) -> HirResult<()> {
        match stmt {
            HirStatement::Let(var_stmt) => {
                // The RHS expression may consume ownership
                self.collect_uses_in_expr(&var_stmt.value, true)?;

                // Register the new variable
                let ty = var_stmt.ty.unwrap_or(var_stmt.value.ty());
                let kind = self.classify_type_kind(ty);
                let is_copyable = self.is_type_copyable(ty);

                self.scope_map.insert(
                    var_stmt.name,
                    VarData::new(var_stmt.name, var_stmt.span, kind, ty, is_copyable, self.current_stmt_index),
                );
            }
            HirStatement::Const(var_stmt) => {
                self.collect_uses_in_expr(&var_stmt.value, true)?;

                let ty = var_stmt.ty.unwrap_or(var_stmt.value.ty());
                let kind = self.classify_type_kind(ty);
                let is_copyable = self.is_type_copyable(ty);

                self.scope_map.insert(
                    var_stmt.name,
                    VarData::new(var_stmt.name, var_stmt.span, kind, ty, is_copyable, self.current_stmt_index),
                );
            }
            HirStatement::Expr(expr_stmt) => {
                self.collect_uses_in_expr(&expr_stmt.expr, false)?;
            }
            HirStatement::Return(ret) => {
                // Return always moves
                self.collect_uses_in_expr(&ret.value, true)?;
            }
            HirStatement::IfElse(if_else) => {
                // Condition is read-only
                self.collect_uses_in_expr(&if_else.condition, false)?;

                // Analyze branches with scope snapshot/restore
                let snapshot = self.scope_map.clone();
                self.collect_uses_in_block(&if_else.then_branch)?;

                self.scope_map = snapshot;
                if let Some(else_branch) = &if_else.else_branch {
                    self.collect_uses_in_block(else_branch)?;
                }
            }
            HirStatement::While(while_stmt) => {
                self.collect_uses_in_expr(&while_stmt.condition, false)?;
                let snapshot = self.scope_map.clone();
                self.collect_uses_in_block(&while_stmt.body)?;
                self.scope_map = snapshot;
            }
            HirStatement::Break(_) | HirStatement::Continue(_) => {}
            HirStatement::_Block(block) => {
                self.scope_map.new_scope();
                self.collect_uses_in_block(block)?;
                self.scope_map.end_scope();
            }
        }
        Ok(())
    }

    /// Collect uses from an expression
    ///
    /// `is_ownership_consuming` indicates whether this expression's result will consume ownership
    fn collect_uses_in_expr(
        &mut self,
        expr: &HirExpr<'hir>,
        is_ownership_consuming: bool,
    ) -> HirResult<()> {
        match expr {
            HirExpr::Ident(ident) => {
                let use_kind = if is_ownership_consuming {
                    UseKind::OwnershipConsuming
                } else {
                    UseKind::Read
                };

                self.scope_map.record_use(
                    ident.name,
                    VarUse {
                        span: ident.span,
                        kind: use_kind,
                        stmt_index: self.current_stmt_index,
                    },
                );
            }
            HirExpr::Assign(assign) => {
                // LHS: if assigning to an existing variable, we need to track the old value
                self.collect_uses_in_expr(&assign.lhs, false)?;
                // RHS: ownership consuming
                self.collect_uses_in_expr(&assign.rhs, true)?;
            }
            HirExpr::Call(call) => {
                self.collect_uses_in_expr(&call.callee, false)?;
                for arg in call.args.iter() {
                    // Arguments by value consume ownership
                    self.collect_uses_in_expr(arg, true)?;
                }
            }
            HirExpr::HirBinaryOperation(binop) => {
                // Binary operations don't consume ownership (they read values)
                self.collect_uses_in_expr(&binop.lhs, false)?;
                self.collect_uses_in_expr(&binop.rhs, false)?;
            }
            HirExpr::Unary(unary) => {
                let inner_consumes = match &unary.op {
                    Some(crate::atlas_c::atlas_hir::expr::HirUnaryOp::AsRef) => {
                        // Taking a reference borrows, doesn't consume
                        if let HirExpr::Ident(ident) = unary.expr.as_ref() {
                            self.scope_map.mark_as_borrowed(ident.name);
                        }
                        false
                    }
                    Some(crate::atlas_c::atlas_hir::expr::HirUnaryOp::Deref) => false,
                    Some(crate::atlas_c::atlas_hir::expr::HirUnaryOp::Neg) => false,
                    Some(crate::atlas_c::atlas_hir::expr::HirUnaryOp::Not) => false,
                    // No operator - propagate the consuming flag
                    None => is_ownership_consuming,
                };
                self.collect_uses_in_expr(&unary.expr, inner_consumes)?;
            }
            HirExpr::FieldAccess(field) => {
                self.collect_uses_in_expr(&field.target, false)?;
            }
            HirExpr::Indexing(indexing) => {
                self.collect_uses_in_expr(&indexing.target, false)?;
                self.collect_uses_in_expr(&indexing.index, false)?;
            }
            HirExpr::Casting(cast) => {
                self.collect_uses_in_expr(&cast.expr, is_ownership_consuming)?;
            }
            HirExpr::NewObj(new_obj) => {
                for arg in new_obj.args.iter() {
                    self.collect_uses_in_expr(arg, true)?;
                }
            }
            HirExpr::ObjLiteral(obj_lit) => {
                for field in obj_lit.fields.iter() {
                    self.collect_uses_in_expr(&field.value, true)?;
                }
            }
            HirExpr::ListLiteral(list) => {
                for item in list.items.iter() {
                    self.collect_uses_in_expr(item, true)?;
                }
            }
            HirExpr::NewArray(arr) => {
                self.collect_uses_in_expr(&arr.size, false)?;
            }
            HirExpr::Delete(del) => {
                self.collect_uses_in_expr(&del.expr, true)?;
            }
            HirExpr::StaticAccess(_) => {}
            // Literals don't use variables
            HirExpr::IntegerLiteral(_)
            | HirExpr::FloatLiteral(_)
            | HirExpr::BooleanLiteral(_)
            | HirExpr::CharLiteral(_)
            | HirExpr::StringLiteral(_)
            | HirExpr::UnitLiteral(_)
            | HirExpr::UnsignedIntegerLiteral(_)
            | HirExpr::ThisLiteral(_) => {}
            // Move/Copy expressions - analyze inner expression
            HirExpr::Move(mv) => {
                self.collect_uses_in_expr(&mv.expr, true)?;
            }
            HirExpr::Copy(cp) => {
                self.collect_uses_in_expr(&cp.expr, true)?;
            }
        }
        Ok(())
    }

    // =========================================================================
    // Phase 2 & 3: COPY-Biased Lowering + Last-Use Optimization
    // =========================================================================

    /// Transform a block by inserting COPY/MOVE expressions and DELETE statements
    fn transform_block(&mut self, block: &mut HirBlock<'hir>) -> HirResult<()> {
        let mut new_statements = Vec::new();

        for (idx, stmt) in block.statements.iter_mut().enumerate() {
            self.current_stmt_index = idx;

            match self.transform_stmt(stmt) {
                Ok(transformed_stmts) => {
                    new_statements.extend(transformed_stmts);
                }
                Err(e) => {
                    self.errors.push(e);
                    new_statements.push(stmt.clone());
                }
            }
        }

        // Phase 4: Insert destructors at end of scope
        // BUT: Don't add destructors if the block ends with a return (they're handled there)
        // Also check if the block ends with an if-else where both branches return
        let ends_with_return = new_statements.last().is_some_and(|s| Self::statement_always_returns(s));
        if !ends_with_return {
            let delete_stmts = self.generate_scope_destructors(block.span);
            new_statements.extend(delete_stmts);
        }

        block.statements = new_statements;
        Ok(())
    }

    /// Check if a statement always returns (either is a return, or is an if-else where both branches return)
    fn statement_always_returns(stmt: &HirStatement) -> bool {
        match stmt {
            HirStatement::Return(_) => true,
            HirStatement::IfElse(if_else) => {
                // Check if both branches end with return
                let then_returns = if_else.then_branch.statements.last()
                    .is_some_and(|s| Self::statement_always_returns(s));
                let else_returns = if_else.else_branch.as_ref()
                    .is_some_and(|b| b.statements.last().is_some_and(|s| Self::statement_always_returns(s)));
                then_returns && else_returns
            }
            _ => false,
        }
    }

    /// Transform a statement, possibly generating multiple statements
    fn transform_stmt(
        &mut self,
        stmt: &mut HirStatement<'hir>,
    ) -> HirResult<Vec<HirStatement<'hir>>> {
        match stmt {
            HirStatement::Let(var_stmt) => {
                let transformed_value = self.transform_expr_ownership(&var_stmt.value, true)?;

                // Note: Variable is already registered during Phase 1 (use collection).
                // Do NOT re-insert here as that would erase the recorded uses.
                // The variable already exists in scope_map with its uses tracked.

                let new_stmt = HirStatement::Let(HirVariableStmt {
                    span: var_stmt.span,
                    name: var_stmt.name,
                    name_span: var_stmt.name_span,
                    ty: var_stmt.ty,
                    ty_span: var_stmt.ty_span,
                    value: transformed_value,
                });

                Ok(vec![new_stmt])
            }
            HirStatement::Const(var_stmt) => {
                let transformed_value = self.transform_expr_ownership(&var_stmt.value, true)?;

                // Note: Variable is already registered during Phase 1 (use collection).
                // Do NOT re-insert here as that would erase the recorded uses.

                let new_stmt = HirStatement::Const(HirVariableStmt {
                    span: var_stmt.span,
                    name: var_stmt.name,
                    name_span: var_stmt.name_span,
                    ty: var_stmt.ty,
                    ty_span: var_stmt.ty_span,
                    value: transformed_value,
                });

                Ok(vec![new_stmt])
            }
            HirStatement::Expr(expr_stmt) => {
                let transformed = self.transform_expr_ownership(&expr_stmt.expr, false)?;
                Ok(vec![HirStatement::Expr(HirExprStmt {
                    span: expr_stmt.span,
                    expr: transformed,
                })])
            }
            HirStatement::Return(ret) => {
                // Return always moves the value
                let transformed = self.transform_expr_for_return(&ret.value)?;

                // Collect ALL variables used in the return expression
                // These should NOT be deleted because the return expression needs them
                let mut vars_used_in_return = Vec::new();
                self.collect_vars_used_in_expr(&ret.value, &mut vars_used_in_return);

                let mut result = Vec::new();

                // Generate delete statements for owned vars declared before this statement
                // EXCEPT for any variable used in the return expression
                let owned_vars = self.scope_map.get_all_owned_vars();
                for var in owned_vars {
                    // Skip variables declared after this return statement
                    if var.declaration_stmt_index > self.current_stmt_index {
                        continue;
                    }
                    // Skip variables used in the return expression
                    if vars_used_in_return.contains(&var.name) {
                        continue;
                    }
                    // Skip primitives (no destructor needed)
                    if var.kind == VarKind::Primitive {
                        continue;
                    }
                    // Skip references (they don't own anything)
                    if var.kind == VarKind::Reference {
                        continue;
                    }
                    result.push(Self::create_delete_stmt(var.name, var.ty, ret.span));
                }

                // Add the return statement
                result.push(HirStatement::Return(crate::atlas_c::atlas_hir::stmt::HirReturn {
                    span: ret.span,
                    value: transformed,
                    ty: ret.ty,
                }));

                Ok(result)
            }
            HirStatement::IfElse(if_else) => {
                let transformed_condition =
                    self.transform_expr_ownership(&if_else.condition, false)?;

                // Save state for else branch
                let snapshot = self.scope_map.clone();

                // Transform then branch
                self.scope_map.new_scope();
                self.transform_block(&mut if_else.then_branch)?;
                self.scope_map.end_scope();

                // Restore state for else branch
                self.scope_map = snapshot;

                if let Some(else_branch) = &mut if_else.else_branch {
                    self.scope_map.new_scope();
                    self.transform_block(else_branch)?;
                    self.scope_map.end_scope();
                }

                Ok(vec![HirStatement::IfElse(
                    crate::atlas_c::atlas_hir::stmt::HirIfElseStmt {
                        span: if_else.span,
                        condition: transformed_condition,
                        then_branch: if_else.then_branch.clone(),
                        else_branch: if_else.else_branch.clone(),
                    },
                )])
            }
            HirStatement::While(while_stmt) => {
                let transformed_condition =
                    self.transform_expr_ownership(&while_stmt.condition, false)?;

                let snapshot = self.scope_map.clone();
                self.scope_map.new_scope();
                self.transform_block(&mut while_stmt.body)?;
                self.scope_map.end_scope();
                self.scope_map = snapshot;

                Ok(vec![HirStatement::While(
                    crate::atlas_c::atlas_hir::stmt::HirWhileStmt {
                        span: while_stmt.span,
                        condition: transformed_condition,
                        body: while_stmt.body.clone(),
                    },
                )])
            }
            HirStatement::Break(span) => Ok(vec![HirStatement::Break(*span)]),
            HirStatement::Continue(span) => Ok(vec![HirStatement::Continue(*span)]),
            HirStatement::_Block(block) => {
                self.scope_map.new_scope();
                self.transform_block(block)?;
                self.scope_map.end_scope();
                Ok(vec![HirStatement::_Block(block.clone())])
            }
        }
    }

    /// Transform an expression, inserting COPY/MOVE as needed
    ///
    /// `is_ownership_consuming` indicates if this expression's result will consume ownership
    fn transform_expr_ownership(
        &mut self,
        expr: &HirExpr<'hir>,
        is_ownership_consuming: bool,
    ) -> HirResult<HirExpr<'hir>> {
        match expr {
            HirExpr::Ident(ident) => {
                if is_ownership_consuming {
                    self.transform_ownership_consuming_ident(ident)
                } else {
                    // Just a read - check if variable is still valid
                    self.check_variable_valid(ident)?;
                    Ok(expr.clone())
                }
            }
            HirExpr::Assign(assign) => {
                // For assignment to existing variable, we need to delete old value first
                // This is handled at a higher level - here we just transform the RHS
                let transformed_lhs = self.transform_expr_ownership(&assign.lhs, false)?;
                let transformed_rhs = self.transform_expr_ownership(&assign.rhs, true)?;

                Ok(HirExpr::Assign(crate::atlas_c::atlas_hir::expr::HirAssignExpr {
                    span: assign.span,
                    lhs: Box::new(transformed_lhs),
                    rhs: Box::new(transformed_rhs),
                    ty: assign.ty,
                }))
            }
            HirExpr::Call(call) => {
                let transformed_callee = self.transform_expr_ownership(&call.callee, false)?;
                let mut transformed_args = Vec::new();

                for (i, arg) in call.args.iter().enumerate() {
                    // Check if the parameter type is a reference - if so, don't consume ownership
                    let is_ref_param = call.args_ty.get(i).is_some_and(|ty| {
                        matches!(ty, HirTy::ReadOnlyReference(_) | HirTy::MutableReference(_))
                    });
                    // Arguments by value consume ownership, references don't
                    let consumes_ownership = !is_ref_param;
                    transformed_args.push(self.transform_expr_ownership(arg, consumes_ownership)?);
                }

                Ok(HirExpr::Call(crate::atlas_c::atlas_hir::expr::HirFunctionCallExpr {
                    span: call.span,
                    callee: Box::new(transformed_callee),
                    callee_span: call.callee_span,
                    args: transformed_args,
                    args_ty: call.args_ty.clone(),
                    generics: call.generics.clone(),
                    ty: call.ty,
                }))
            }
            HirExpr::HirBinaryOperation(binop) => {
                let transformed_lhs = self.transform_expr_ownership(&binop.lhs, false)?;
                let transformed_rhs = self.transform_expr_ownership(&binop.rhs, false)?;

                Ok(HirExpr::HirBinaryOperation(
                    crate::atlas_c::atlas_hir::expr::HirBinaryOpExpr {
                        span: binop.span,
                        op: binop.op.clone(),
                        op_span: binop.op_span,
                        lhs: Box::new(transformed_lhs),
                        rhs: Box::new(transformed_rhs),
                        ty: binop.ty,
                    },
                ))
            }
            HirExpr::Unary(unary) => {
                // For unary expressions, determine if inner should consume ownership
                let inner_consumes = match &unary.op {
                    // Reference operators don't consume ownership (borrow)
                    Some(crate::atlas_c::atlas_hir::expr::HirUnaryOp::AsRef) => false,
                    Some(crate::atlas_c::atlas_hir::expr::HirUnaryOp::Deref) => false,
                    // Neg and Not are typically for primitives - don't consume
                    Some(crate::atlas_c::atlas_hir::expr::HirUnaryOp::Neg) => false,
                    Some(crate::atlas_c::atlas_hir::expr::HirUnaryOp::Not) => false,
                    // No operator means this is just a wrapper - propagate the consuming flag
                    None => is_ownership_consuming,
                };
                
                let transformed_inner = self.transform_expr_ownership(&unary.expr, inner_consumes)?;

                Ok(HirExpr::Unary(crate::atlas_c::atlas_hir::expr::UnaryOpExpr {
                    span: unary.span,
                    op: unary.op.clone(),
                    expr: Box::new(transformed_inner),
                    ty: unary.ty,
                }))
            }
            HirExpr::NewObj(new_obj) => {
                let mut transformed_args = Vec::new();
                for arg in new_obj.args.iter() {
                    transformed_args.push(self.transform_expr_ownership(arg, true)?);
                }

                Ok(HirExpr::NewObj(crate::atlas_c::atlas_hir::expr::HirNewObjExpr {
                    span: new_obj.span,
                    ty: new_obj.ty,
                    args: transformed_args,
                    args_ty: new_obj.args_ty.clone(),
                }))
            }
            HirExpr::ObjLiteral(obj_lit) => {
                let mut transformed_fields = Vec::new();
                for field in obj_lit.fields.iter() {
                    let transformed_value = self.transform_expr_ownership(&field.value, true)?;
                    transformed_fields.push(crate::atlas_c::atlas_hir::expr::HirFieldInit {
                        span: field.span,
                        name: field.name,
                        name_span: field.name_span,
                        ty: field.ty,
                        value: Box::new(transformed_value),
                    });
                }

                Ok(HirExpr::ObjLiteral(
                    crate::atlas_c::atlas_hir::expr::HirObjLiteralExpr {
                        span: obj_lit.span,
                        ty: obj_lit.ty,
                        fields: transformed_fields,
                    },
                ))
            }
            HirExpr::ListLiteral(list) => {
                let mut transformed_items = Vec::new();
                for item in list.items.iter() {
                    transformed_items.push(self.transform_expr_ownership(item, true)?);
                }

                Ok(HirExpr::ListLiteral(
                    crate::atlas_c::atlas_hir::expr::HirListLiteralExpr {
                        span: list.span,
                        items: transformed_items,
                        ty: list.ty,
                    },
                ))
            }
            HirExpr::FieldAccess(field) => {
                let transformed_target = self.transform_expr_ownership(&field.target, false)?;
                let field_access_expr = HirExpr::FieldAccess(
                    crate::atlas_c::atlas_hir::expr::HirFieldAccessExpr {
                        span: field.span,
                        target: Box::new(transformed_target),
                        field: field.field.clone(),
                        ty: field.ty,
                    },
                );
                
                // If ownership is being consumed (e.g., returning a field, passing to a function),
                // we need to either COPY the field value (if copyable) or MOVE it (if not).
                // This prevents the aliasing issue where both the struct and the returned value
                // point to the same heap object.
                if is_ownership_consuming {
                    let is_copyable = self.is_type_copyable(field.ty);
                    if is_copyable {
                        // Generate a COPY expression for the field value
                        Ok(HirExpr::Copy(HirCopyExpr {
                            span: field.span,
                            source_name: field.field.name,
                            expr: Box::new(field_access_expr),
                            ty: field.ty,
                        }))
                    } else {
                        // Non-copyable field being consumed - generate a MOVE
                        // This is safe when the containing struct is also being consumed
                        // (like Rust's Option::unwrap)
                        Ok(HirExpr::Move(HirMoveExpr {
                            span: field.span,
                            source_name: field.field.name,
                            expr: Box::new(field_access_expr),
                            ty: field.ty,
                        }))
                    }
                } else {
                    Ok(field_access_expr)
                }
            }
            HirExpr::Indexing(indexing) => {
                let transformed_target = self.transform_expr_ownership(&indexing.target, false)?;
                let transformed_index = self.transform_expr_ownership(&indexing.index, false)?;
                let indexing_expr = HirExpr::Indexing(crate::atlas_c::atlas_hir::expr::HirIndexingExpr {
                    span: indexing.span,
                    target: Box::new(transformed_target),
                    index: Box::new(transformed_index),
                    ty: indexing.ty,
                });
                
                // Same as FieldAccess: if ownership is being consumed, we need to COPY or MOVE
                if is_ownership_consuming {
                    let is_copyable = self.is_type_copyable(indexing.ty);
                    if is_copyable {
                        Ok(HirExpr::Copy(HirCopyExpr {
                            span: indexing.span,
                            source_name: "<indexed>",
                            expr: Box::new(indexing_expr),
                            ty: indexing.ty,
                        }))
                    } else {
                        // Non-copyable element being consumed - generate a MOVE
                        Ok(HirExpr::Move(HirMoveExpr {
                            span: indexing.span,
                            source_name: "<indexed>",
                            expr: Box::new(indexing_expr),
                            ty: indexing.ty,
                        }))
                    }
                } else {
                    Ok(indexing_expr)
                }
            }
            HirExpr::Casting(cast) => {
                let transformed = self.transform_expr_ownership(&cast.expr, is_ownership_consuming)?;
                Ok(HirExpr::Casting(crate::atlas_c::atlas_hir::expr::HirCastExpr {
                    span: cast.span,
                    expr: Box::new(transformed),
                    ty: cast.ty,
                }))
            }
            // Delete expressions - mark the variable as deleted so it won't be auto-deleted later
            HirExpr::Delete(del) => {
                // If deleting an identifier, mark it as deleted
                // The inner expression might be wrapped in a Unary with op=None, so unwrap it
                let inner_expr = match del.expr.as_ref() {
                    HirExpr::Unary(u) if u.op.is_none() => u.expr.as_ref(),
                    other => other,
                };
                
                if let HirExpr::Ident(ident) = inner_expr {
                    if let Some(var_data) = self.scope_map.get_mut(ident.name) {
                        var_data.status = VarStatus::Deleted { delete_span: del.span };
                    }
                }
                // Return the delete expression as-is
                Ok(expr.clone())
            }
            // Literals are always owned by the expression creating them
            _ => Ok(expr.clone()),
        }
    }

    /// Transform an identifier in an ownership-consuming context
    ///
    /// Applies COPY-biased lowering + last-use optimization:
    /// - If copyable AND not last use: emit COPY
    /// - If copyable AND is last use: emit MOVE (optimization)
    /// - If not copyable: emit MOVE (or error if already moved)
    fn transform_ownership_consuming_ident(
        &mut self,
        ident: &HirIdentExpr<'hir>,
    ) -> HirResult<HirExpr<'hir>> {
        // Check if variable exists and is valid
        let var_data = match self.scope_map.get(ident.name) {
            Some(data) => data.clone(),
            None => {
                // Variable not found - might be a function name or external
                return Ok(HirExpr::Ident(ident.clone()));
            }
        };

        // Check for use-after-move
        if let VarStatus::Moved { move_span } = &var_data.status {
            let path = ident.span.path;
            let src = utils::get_file_content(path).unwrap_or_default();
            return Err(HirError::TryingToAccessAMovedValue(TryingToAccessAMovedValueError {
                move_span: *move_span,
                access_span: ident.span,
                src: NamedSource::new(path, src),
            }));
        }

        // Reference types don't transfer ownership
        if var_data.kind == VarKind::Reference {
            return Ok(HirExpr::Ident(ident.clone()));
        }

        // Primitives are always implicitly copied (bitwise copy)
        if var_data.kind == VarKind::Primitive {
            return Ok(HirExpr::Ident(ident.clone()));
        }

        // For copyable types: always use COPY to be safe
        // (The "move optimization" is disabled because loop analysis isn't reliable yet)
        if var_data.is_copyable {
            return Ok(HirExpr::Copy(HirCopyExpr {
                span: ident.span,
                source_name: ident.name,
                expr: Box::new(HirExpr::Ident(ident.clone())),
                ty: ident.ty,
            }));
        }

        // For non-copyable types: must MOVE
        // Check if we can safely move (no future uses of any kind)
        let can_move = var_data.can_move_at(self.current_stmt_index);

        if !can_move {
            // Non-copyable type used multiple times - this will fail on second use
            // We'll catch this when they try to use it again
        }

        // Mark as moved
        self.scope_map.mark_as_moved(ident.name, ident.span);

        Ok(HirExpr::Move(HirMoveExpr {
            span: ident.span,
            source_name: ident.name,
            expr: Box::new(HirExpr::Ident(ident.clone())),
            ty: ident.ty,
        }))
    }

    /// Transform an expression for return context (always moves)
    fn transform_expr_for_return(&mut self, expr: &HirExpr<'hir>) -> HirResult<HirExpr<'hir>> {
        match expr {
            HirExpr::Ident(ident) => {
                // Check if variable exists
                if let Some(var_data) = self.scope_map.get(ident.name) {
                    // Check for use-after-move
                    if let VarStatus::Moved { move_span } = &var_data.status {
                        let path = ident.span.path;
                        let src = utils::get_file_content(path).unwrap_or_default();
                        return Err(HirError::TryingToAccessAMovedValue(
                            TryingToAccessAMovedValueError {
                                move_span: *move_span,
                                access_span: ident.span,
                                src: NamedSource::new(path, src),
                            },
                        ));
                    }

                    // References can be returned as-is (they're just pointers)
                    if var_data.kind == VarKind::Reference {
                        return Ok(HirExpr::Ident(ident.clone()));
                    }

                    // Primitives are copied on return (trivial)
                    if var_data.kind == VarKind::Primitive {
                        return Ok(HirExpr::Ident(ident.clone()));
                    }

                    // Mark as moved (ownership transferred to caller)
                    self.scope_map.mark_as_moved(ident.name, ident.span);

                    // Return always moves
                    Ok(HirExpr::Move(HirMoveExpr {
                        span: ident.span,
                        source_name: ident.name,
                        expr: Box::new(HirExpr::Ident(ident.clone())),
                        ty: ident.ty,
                    }))
                } else {
                    Ok(expr.clone())
                }
            }
            // Unary expressions (e.g., negation, None-wrapping) - recurse into inner
            HirExpr::Unary(unary) => {
                let transformed_inner = self.transform_expr_for_return(&unary.expr)?;
                Ok(HirExpr::Unary(crate::atlas_c::atlas_hir::expr::UnaryOpExpr {
                    span: unary.span,
                    op: unary.op.clone(),
                    expr: Box::new(transformed_inner),
                    ty: unary.ty,
                }))
            }
            // For complex expressions, transform recursively
            _ => self.transform_expr_ownership(expr, true),
        }
    }

    // =========================================================================
    // Phase 4: Destructor Insertion
    // =========================================================================

    /// Generate delete statements for all owned variables in current scope
    fn generate_scope_destructors(&self, scope_end_span: Span) -> Vec<HirStatement<'hir>> {
        let mut deletes = Vec::new();

        for var in self.scope_map.get_owned_vars_in_current_scope() {
            // Skip references (they don't own anything)
            if var.kind == VarKind::Reference {
                continue;
            }
            // Skip primitives (no destructor needed)
            if var.kind == VarKind::Primitive {
                continue;
            }
            deletes.push(Self::create_delete_stmt(var.name, var.ty, scope_end_span));
        }

        deletes
    }

    /// Create a delete statement for a variable
    fn create_delete_stmt(var_name: &'hir str, var_ty: &'hir HirTy<'hir>, span: Span) -> HirStatement<'hir> {
        HirStatement::Expr(HirExprStmt {
            span,
            expr: HirExpr::Delete(HirDeleteExpr {
                span,
                expr: Box::new(HirExpr::Ident(HirIdentExpr {
                    name: var_name,
                    span,
                    ty: var_ty,
                })),
            }),
        })
    }

    // =========================================================================
    // Phase 5: Validation Helpers
    // =========================================================================

    /// Check that a variable is still valid (not moved or deleted)
    fn check_variable_valid(&self, ident: &HirIdentExpr<'hir>) -> HirResult<()> {
        if let Some(var_data) = self.scope_map.get(ident.name) {
            let path = ident.span.path;
            let src = utils::get_file_content(path).unwrap_or_default();
            match &var_data.status {
                VarStatus::Moved { move_span } => {
                    return Err(HirError::TryingToAccessAMovedValue(
                        TryingToAccessAMovedValueError {
                            move_span: *move_span,
                            access_span: ident.span,
                            src: NamedSource::new(path, src),
                        },
                    ));
                }
                VarStatus::Deleted { delete_span } => {
                    return Err(HirError::TryingToAccessADeletedValue(
                        crate::atlas_c::atlas_hir::error::TryingToAccessADeletedValueError {
                            delete_span: *delete_span,
                            access_span: ident.span,
                            src: NamedSource::new(path, src),
                        },
                    ));
                }
                VarStatus::Owned | VarStatus::Borrowed => {}
            }
        }
        Ok(())
    }

    /// Get the variable name being returned, if it's a simple identifier
    fn get_returned_var_name(&self, expr: &HirExpr<'hir>) -> Option<&'hir str> {
        match expr {
            HirExpr::Ident(ident) => Some(ident.name),
            HirExpr::Move(mv) => self.get_returned_var_name(&mv.expr),
            HirExpr::Copy(cp) => self.get_returned_var_name(&cp.expr),
            HirExpr::Unary(unary) => self.get_returned_var_name(&unary.expr),
            _ => None,
        }
    }

    /// Collect all variable names used in an expression
    /// This is used to avoid deleting variables that are used in return expressions
    fn collect_vars_used_in_expr(&self, expr: &HirExpr<'hir>, vars: &mut Vec<&'hir str>) {
        match expr {
            HirExpr::Ident(ident) => {
                vars.push(ident.name);
            }
            HirExpr::Move(mv) => self.collect_vars_used_in_expr(&mv.expr, vars),
            HirExpr::Copy(cp) => self.collect_vars_used_in_expr(&cp.expr, vars),
            HirExpr::Unary(unary) => self.collect_vars_used_in_expr(&unary.expr, vars),
            HirExpr::HirBinaryOperation(binary) => {
                self.collect_vars_used_in_expr(&binary.lhs, vars);
                self.collect_vars_used_in_expr(&binary.rhs, vars);
            }
            HirExpr::FieldAccess(field) => {
                self.collect_vars_used_in_expr(&field.target, vars);
            }
            HirExpr::Indexing(idx) => {
                self.collect_vars_used_in_expr(&idx.target, vars);
                self.collect_vars_used_in_expr(&idx.index, vars);
            }
            HirExpr::Call(call) => {
                for arg in call.args.iter() {
                    self.collect_vars_used_in_expr(arg, vars);
                }
            }
            HirExpr::NewObj(new_obj) => {
                for arg in new_obj.args.iter() {
                    self.collect_vars_used_in_expr(arg, vars);
                }
            }
            HirExpr::ListLiteral(arr) => {
                for elem in arr.items.iter() {
                    self.collect_vars_used_in_expr(elem, vars);
                }
            }
            HirExpr::ObjLiteral(obj) => {
                for field in obj.fields.iter() {
                    self.collect_vars_used_in_expr(&field.value, vars);
                }
            }
            HirExpr::Casting(cast) => {
                self.collect_vars_used_in_expr(&cast.expr, vars);
            }
            // Literals and other expressions don't use variables
            _ => {}
        }
    }

    // =========================================================================
    // Type Classification Helpers
    // =========================================================================

    /// Classify a type into VarKind
    fn classify_type_kind(&self, ty: &HirTy<'hir>) -> VarKind {
        match ty {
            HirTy::Boolean(_)
            | HirTy::Int64(_)
            | HirTy::Float64(_)
            | HirTy::Char(_)
            | HirTy::UInt64(_)
            | HirTy::Unit(_) => VarKind::Primitive,

            HirTy::ReadOnlyReference(_) | HirTy::MutableReference(_) => VarKind::Reference,

            HirTy::String(_) => VarKind::String,

            HirTy::List(_) => VarKind::List,

            HirTy::Named(_) | HirTy::Generic(_) | HirTy::Function(_) => VarKind::Object,

            _ => VarKind::Object,
        }
    }

    /// Check if a type is copyable
    ///
    /// A type is copyable if:
    /// - It's a primitive type (always implicitly copyable)
    /// - It's a reference type (just a pointer, trivially copyable)
    /// - It's a string (built-in copyable)
    /// - It's a struct with a `_copy` method
    fn is_type_copyable(&self, ty: &HirTy<'hir>) -> bool {
        match ty {
            // Primitives are always copyable (bitwise copy)
            HirTy::Boolean(_)
            | HirTy::Int64(_)
            | HirTy::Float64(_)
            | HirTy::Char(_)
            | HirTy::UInt64(_)
            | HirTy::Unit(_) => true,

            // References are copyable (they're just pointers)
            HirTy::ReadOnlyReference(_) | HirTy::MutableReference(_) => true,

            // Strings are copyable (built-in)
            HirTy::String(_) => true,

            // Function pointers are copyable
            HirTy::Function(_) => true,

            // Lists are NOT copyable - they must be moved to avoid double-free
            // When a list is assigned to a field or passed as argument, ownership transfers
            // This prevents the original variable from being deleted and freeing the shared data
            HirTy::List(_) => false,

            // Named types (structs) are copyable if they have a _copy method
            HirTy::Named(named) => {
                self.hir_signature
                    .structs
                    .get(named.name)
                    .is_some_and(|s| s.methods.contains_key("_copy"))
            }

            // Generic types - need to check the instantiated struct
            HirTy::Generic(g) => {
                // For now, assume generic types are not copyable unless proven otherwise
                // This is conservative - we could improve this with more sophisticated analysis
                self.hir_signature
                    .structs
                    .get(g.name)
                    .is_some_and(|s| s.methods.contains_key("_copy"))
            }

            // Other types are not copyable
            _ => false,
        }
    }
}

// Legacy alias for backwards compatibility
pub type LifeTimePass<'hir> = OwnershipPass<'hir>;
