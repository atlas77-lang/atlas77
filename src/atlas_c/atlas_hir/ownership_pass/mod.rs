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
pub use context::{
    ReferenceOrigin, ScopeMap, UseKind, VarData, VarKind, VarMap, VarStatus, VarUse,
};

use crate::atlas_c::{
    atlas_hir::{
        HirModule,
        arena::HirArena,
        error::{
            CannotMoveOutOfContainerError, CannotMoveOutOfLoopError,
            CannotTransferOwnershipInBorrowingMethodError, HirError, HirResult,
            RecursiveCopyConstructorError, TryingToAccessADeletedValueError,
            TryingToAccessAMovedValueError, TryingToAccessAPotentiallyMovedValueError,
        },
        expr::{HirCopyExpr, HirDeleteExpr, HirExpr, HirIdentExpr, HirMoveExpr},
        item::HirFunction,
        monomorphization_pass::MonomorphizationPass,
        pretty_print::HirPrettyPrinter,
        signature::{HirModuleSignature, HirStructMethodModifier, HirStructMethodSignature},
        stmt::{HirBlock, HirExprStmt, HirStatement, HirVariableStmt},
        ty::HirTy,
        warning::{
            ConsumingMethodMayLeakThisWarning, HirWarning, ReferenceEscapesToConstructorWarning,
            TemporaryValueCannotBeFreedWarning, UnnecessaryCopyDueToLaterBorrowsWarning,
        },
    },
    utils::{self, Span},
};
use miette::{ErrReport, NamedSource};

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
    /// Current method modifier (None if not in a method, or the modifier if in a method)
    /// Used to detect ownership transfer in borrowing methods
    current_method_context: Option<MethodContext<'hir>>,
}

/// Context for the current method being analyzed
#[derive(Clone)]
struct MethodContext<'hir> {
    /// The method modifier (&this, &const this, this, or Static)
    modifier: HirStructMethodModifier,
    /// Span of the method signature (for error reporting)
    method_span: Span,
    /// The name of the struct this method belongs to (for checking if struct is copyable)
    struct_name: &'hir str,
    /// The name of the method (for detecting _copy methods)
    method_name: &'hir str,
    /// The return type of the method (for detecting recursive copy)
    return_ty: Option<HirTy<'hir>>,
    /// Whether the struct has type parameters (is generic)
    /// For generic structs, we skip some ownership checks for field accesses
    /// because the concrete types are unknown at definition time
    is_generic_struct: bool,
}

impl<'hir> OwnershipPass<'hir> {
    pub fn new(hir_signature: HirModuleSignature<'hir>, hir_arena: &'hir HirArena<'hir>) -> Self {
        Self {
            scope_map: ScopeMap::new(),
            errors: Vec::new(),
            hir_signature,
            hir_arena,
            current_stmt_index: 0,
            current_method_context: None,
        }
    }

    /// Run the ownership pass on the entire module
    pub fn run(
        &mut self,
        hir: &'hir mut HirModule<'hir>,
    ) -> Result<&'hir mut HirModule<'hir>, (&'hir mut HirModule<'hir>, HirError)> {
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
            // Check if this is a generic struct (has type parameters).
            // For generic structs, skip field ownership checks because we can't reliably
            // determine if the concrete type parameter is copyable at definition time.
            // NB: there shouldn't be any generic structs remaining in the HIR at this point
            //  in the compilation pipeline, but we keep this check for safety.
            let is_generic_struct = !struct_def.signature.generics.is_empty();

            // Process methods
            for method in struct_def.methods.iter_mut() {
                self.scope_map = ScopeMap::new();
                self.current_stmt_index = 0;

                // Set method context for ownership checking
                self.current_method_context = Some(MethodContext {
                    modifier: method.signature.modifier.clone(),
                    method_span: method.signature.span,
                    struct_name: struct_def.signature.name,
                    method_name: method.name,
                    return_ty: Some(method.signature.return_ty.clone()),
                    is_generic_struct,
                });

                // Register method parameters
                for param in method.signature.params.iter() {
                    let kind = self.classify_type_kind(param.ty);
                    let is_copyable = self.is_type_copyable(param.ty);
                    self.scope_map.insert(
                        param.name,
                        VarData::new(param.name, param.span, kind, param.ty, is_copyable, 0),
                    );
                }

                // For consuming methods (modifier == None), we would ideally register 'this'
                // as an owned variable so it gets deleted. However, this is complex because:
                // 1. If the method returns a field of `this` (like optional.value()), we can't
                //    delete `this` before reading the field
                // 2. If the destructor deletes the returned field, we'd have a double-free
                //
                // For now, we skip automatic `this` deletion for consuming methods.
                // The caller is responsible for ensuring proper cleanup (or the method
                // should explicitly handle it like setting has_value=false before deleting).
                //
                // TODO: Implement proper consuming method semantics:
                // - Either the method should explicitly delete `this` after extracting values
                // - Or the runtime should support deallocating without destructor call

                // First pass: collect uses
                if let Err(e) = self.collect_uses_in_block(&method.body) {
                    self.errors.push(e);
                    self.current_method_context = None;
                    continue;
                }

                // Second pass: transform
                self.current_stmt_index = 0;
                if let Err(e) = self.transform_block(&mut method.body) {
                    self.errors.push(e);
                }

                // Warn if this is a consuming method without `delete this` or ownership transfer
                // TODO: Add a way to warn only once per method
                if method.signature.modifier == HirStructMethodModifier::None {
                    // Don't warn if:
                    // 1. The method contains `delete this`, OR
                    // 2. The method returns/passes `this` or `this.field` to another function
                    //    (ownership is being transferred, not leaked)
                    let has_delete_this = Self::block_contains_delete_this(&method.body);
                    let transfers_this_ownership =
                        Self::block_transfers_this_ownership(&method.body);

                    if !has_delete_this && !transfers_this_ownership {
                        Self::emit_consuming_method_warning(
                            method.name,
                            method.signature,
                            method.signature.span,
                        );
                    }
                }

                self.current_method_context = None;
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
        //Let's print all errors for better debugging except the last one that we returns
        if let Some(err) = self.errors.pop() {
            while let Some(err) = self.errors.pop() {
                eprintln!("{:?}", Into::<miette::Report>::into(err));
            }
            Err((hir, err))
        } else {
            Ok(hir)
        }
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
        for (idx, stmt) in block.statements.iter().enumerate() {
            self.current_stmt_index = idx;
            self.collect_uses_in_stmt(stmt)?;
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
                let ty = var_stmt.ty;
                let kind = self.classify_type_kind(ty);
                let is_copyable = self.is_type_copyable(ty);

                // For reference types, compute the origin so we can track use-after-free
                let origin = if kind == VarKind::Reference {
                    self.compute_reference_origin(&var_stmt.value)
                } else {
                    ReferenceOrigin::None
                };

                self.scope_map.insert(
                    var_stmt.name,
                    VarData::with_origin(
                        var_stmt.name,
                        var_stmt.span,
                        kind,
                        ty,
                        is_copyable,
                        self.current_stmt_index,
                        origin,
                    ),
                );
            }
            HirStatement::Const(var_stmt) => {
                self.collect_uses_in_expr(&var_stmt.value, true)?;

                let ty = var_stmt.ty;
                let kind = self.classify_type_kind(ty);
                let is_copyable = self.is_type_copyable(ty);

                // For reference types, compute the origin
                let origin = if kind == VarKind::Reference {
                    self.compute_reference_origin(&var_stmt.value)
                } else {
                    ReferenceOrigin::None
                };

                self.scope_map.insert(
                    var_stmt.name,
                    VarData::with_origin(
                        var_stmt.name,
                        var_stmt.span,
                        kind,
                        ty,
                        is_copyable,
                        self.current_stmt_index,
                        origin,
                    ),
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
                // Note: We create new scopes here to match transform_stmt behavior,
                // so variables declared inside branches are registered in the correct scope
                let snapshot = self.scope_map.clone();
                self.scope_map.new_scope();
                self.collect_uses_in_block(&if_else.then_branch)?;
                self.scope_map.end_scope();

                self.scope_map = snapshot;
                if let Some(else_branch) = &if_else.else_branch {
                    self.scope_map.new_scope();
                    self.collect_uses_in_block(else_branch)?;
                    self.scope_map.end_scope();
                }
            }
            HirStatement::While(while_stmt) => {
                self.collect_uses_in_expr(&while_stmt.condition, false)?;
                let snapshot = self.scope_map.clone();
                // Create a new scope for the while body to match transform_stmt behavior,
                // so variables declared inside the loop are registered in the correct scope
                // and will be properly deleted at the end of each iteration
                self.scope_map.new_scope();
                self.collect_uses_in_block(&while_stmt.body)?;
                self.scope_map.end_scope();
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
        // Check for ownership transfer in borrowing methods
        if is_ownership_consuming
            && let Some(err) = self.check_ownership_transfer_in_borrowing_method(expr, expr.span())
        {
            return Err(err);
        }

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
                // Check if this is a method call that consumes `this` (modifier == None)
                // Methods with `&this` or `&const this` don't consume ownership
                let method_consumes_this = self.method_consumes_this(&call.callee);

                if method_consumes_this {
                    // For method calls that consume `this`, mark the target as ownership-consuming
                    if let HirExpr::FieldAccess(field_access) = call.callee.as_ref() {
                        self.collect_uses_in_expr(&field_access.target, true)?;
                    } else {
                        self.collect_uses_in_expr(&call.callee, false)?;
                    }
                } else {
                    self.collect_uses_in_expr(&call.callee, false)?;
                }

                for (i, arg) in call.args.iter().enumerate() {
                    // Check if the parameter type is a reference - if so, don't consume ownership
                    let is_ref_param = call.args_ty.get(i).is_some_and(|ty| {
                        matches!(ty, HirTy::ReadOnlyReference(_) | HirTy::MutableReference(_))
                    });
                    // Arguments by value consume ownership, references don't
                    let consumes_ownership = !is_ref_param;
                    self.collect_uses_in_expr(arg, consumes_ownership)?;
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
        let ends_with_return = new_statements
            .last()
            .is_some_and(|s| Self::statement_always_returns(s));
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
                let then_returns = if_else
                    .then_branch
                    .statements
                    .last()
                    .is_some_and(|s| Self::statement_always_returns(s));
                let else_returns = if_else.else_branch.as_ref().is_some_and(|b| {
                    b.statements
                        .last()
                        .is_some_and(|s| Self::statement_always_returns(s))
                });
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

                // Register the variable in the current scope.
                // This is needed because for while/if-else, the scope from Phase 1 was discarded
                // due to snapshot/restore, so we need to re-register for destructor generation.
                // For function bodies, this will update the existing entry (which is fine).
                let ty = if var_stmt.ty == self.hir_arena.types().get_uninitialized_ty() {
                    transformed_value.ty()
                } else {
                    var_stmt.ty
                };
                let kind = self.classify_type_kind(ty);
                let is_copyable = self.is_type_copyable(ty);

                // Preserve the uses array and origin from the collection phase
                let (existing_uses, existing_origin) = self
                    .scope_map
                    .get(var_stmt.name)
                    .map(|v| (v.uses.clone(), v.origin.clone()))
                    .unwrap_or_default();

                let mut new_var_data = VarData::with_origin(
                    var_stmt.name,
                    var_stmt.span,
                    kind,
                    ty,
                    is_copyable,
                    self.current_stmt_index,
                    existing_origin,
                );
                new_var_data.uses = existing_uses;

                self.scope_map.insert(var_stmt.name, new_var_data);

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

                // Register the variable in the current scope (same reasoning as Let above).
                let ty = if var_stmt.ty == self.hir_arena.types().get_uninitialized_ty() {
                    var_stmt.value.ty()
                } else {
                    var_stmt.ty
                };
                let kind = self.classify_type_kind(ty);
                let is_copyable = self.is_type_copyable(ty);

                // Preserve the uses array and origin from the collection phase
                let (existing_uses, existing_origin) = self
                    .scope_map
                    .get(var_stmt.name)
                    .map(|v| (v.uses.clone(), v.origin.clone()))
                    .unwrap_or_default();

                let mut new_var_data = VarData::with_origin(
                    var_stmt.name,
                    var_stmt.span,
                    kind,
                    ty,
                    is_copyable,
                    self.current_stmt_index,
                    existing_origin,
                );
                new_var_data.uses = existing_uses;

                self.scope_map.insert(var_stmt.name, new_var_data);

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
                Self::collect_vars_used_in_expr(&ret.value, &mut vars_used_in_return);

                // Special case: In methods that consume `this` (modifier == None), we need to
                // delete `this` even if it's "used" in the return expression (e.g., returning a field).
                // The method is responsible for cleaning up `this` since it took ownership.
                // Note: This currently may cause issues with destructors that delete the returned field,
                // but that's a library design issue (optional.value() should set has_value=false first).
                let is_consuming_method = self
                    .current_method_context
                    .as_ref()
                    .is_some_and(|ctx| ctx.modifier == HirStructMethodModifier::None);

                // Check if we're returning `this` directly (not a field of this)
                let returning_this_directly =
                    matches!(&ret.value, HirExpr::Ident(ident) if ident.name == "this");

                let mut result = Vec::new();

                // Generate delete statements for owned vars declared before this statement

                // Generate delete statements for owned vars declared before this statement
                // EXCEPT for any variable used in the return expression
                let owned_vars = self.scope_map.get_all_owned_vars();
                for var in owned_vars {
                    // Skip variables declared after this return statement
                    if var.declaration_stmt_index > self.current_stmt_index {
                        continue;
                    }
                    // Skip variables used in the return expression
                    // BUT: In consuming methods, don't skip `this` unless we're returning it directly
                    if vars_used_in_return.contains(&var.name)
                        && !(is_consuming_method && var.name == "this" && !returning_this_directly)
                    {
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
                result.push(HirStatement::Return(
                    crate::atlas_c::atlas_hir::stmt::HirReturn {
                        span: ret.span,
                        value: transformed,
                        ty: ret.ty,
                    },
                ));

                Ok(result)
            }
            HirStatement::IfElse(if_else) => {
                let transformed_condition =
                    self.transform_expr_ownership(&if_else.condition, false)?;

                // Save state before if-else for comparison and else branch
                let pre_if_state = self.scope_map.clone();

                // Transform then branch
                self.scope_map.new_scope();
                self.transform_block(&mut if_else.then_branch)?;
                self.scope_map.end_scope();

                // Save state after then branch
                let post_then_state = self.scope_map.clone();

                // Restore state for else branch
                self.scope_map = pre_if_state.clone();

                // Transform else branch (if present)
                let mut transformed_else_branch = if_else.else_branch.clone();
                if let Some(else_branch) = &mut transformed_else_branch {
                    self.scope_map.new_scope();
                    self.transform_block(else_branch)?;
                    self.scope_map.end_scope();
                }

                // Save state after else branch (or pre_if_state if no else)
                let post_else_state = self.scope_map.clone();

                // Find variables that are conditionally moved (moved in one branch but not other)
                // and insert delete statements in the branch that didn't move them
                let conditionally_moved =
                    pre_if_state.get_conditionally_moved_vars(&post_then_state, &post_else_state);

                let mut final_then_branch = if_else.then_branch.clone();
                let mut final_else_branch = transformed_else_branch;

                for (var_name, moved_in_then, var_data) in conditionally_moved {
                    // Skip primitives and references
                    if var_data.kind == VarKind::Primitive || var_data.kind == VarKind::Reference {
                        continue;
                    }

                    let delete_stmt = Self::create_delete_stmt(var_name, var_data.ty, if_else.span);

                    if moved_in_then {
                        // Moved in then branch, so insert delete at end of else branch
                        if let Some(else_branch) = &mut final_else_branch {
                            // Insert before any return statement at the end
                            let insert_pos =
                                Self::find_delete_insert_position(&else_branch.statements);
                            else_branch.statements.insert(insert_pos, delete_stmt);
                        } else {
                            // No else branch exists - we need to create one with just the delete
                            final_else_branch = Some(HirBlock {
                                span: if_else.span,
                                statements: vec![delete_stmt],
                            });
                        }
                    } else {
                        // Moved in else branch, so insert delete at end of then branch
                        let insert_pos =
                            Self::find_delete_insert_position(&final_then_branch.statements);
                        final_then_branch.statements.insert(insert_pos, delete_stmt);
                    }
                }

                // Merge the branch states to update the current scope map
                self.scope_map = pre_if_state;
                self.scope_map
                    .merge_branch_states(&post_then_state, &post_else_state);

                Ok(vec![HirStatement::IfElse(
                    crate::atlas_c::atlas_hir::stmt::HirIfElseStmt {
                        span: if_else.span,
                        condition: transformed_condition,
                        then_branch: final_then_branch,
                        else_branch: final_else_branch,
                    },
                )])
            }
            HirStatement::While(while_stmt) => {
                let transformed_condition =
                    self.transform_expr_ownership(&while_stmt.condition, false)?;

                // Save state before loop
                let pre_loop_state = self.scope_map.clone();

                self.scope_map.new_scope();
                self.transform_block(&mut while_stmt.body)?;
                self.scope_map.end_scope();

                // Save state after loop body
                let post_loop_state = self.scope_map.clone();

                // Check for variables that were moved inside the loop
                // Mark them as ConditionallyMoved in the outer scope since:
                // 1. The loop might not execute at all
                // 2. The loop might execute once (move is valid)
                // 3. The loop might execute multiple times (would be use-after-move)
                let mut moved_in_loop = Vec::new();
                for scope in &pre_loop_state.scopes {
                    for (name, var_data) in &scope.var_status {
                        let post_var = post_loop_state.get(name);
                        if let Some(post_v) = post_var {
                            let was_owned = matches!(var_data.status, VarStatus::Owned);
                            let is_moved = matches!(post_v.status, VarStatus::Moved { .. });

                            if was_owned
                                && is_moved
                                && let VarStatus::Moved { move_span } = &post_v.status
                            {
                                moved_in_loop.push((*name, *move_span));
                            }
                        }
                    }
                }

                // Restore to pre-loop state
                self.scope_map = pre_loop_state;

                // Mark moved variables as ConditionallyMoved and emit error
                for (var_name, move_span) in &moved_in_loop {
                    // Mark as conditionally moved in the outer scope
                    if let Some(var) = self.scope_map.get_mut(var_name) {
                        var.status = VarStatus::ConditionallyMoved {
                            move_span: *move_span,
                        };
                    }
                }

                // If any variables were moved inside the loop, that's an error
                // because the loop could run multiple times, causing use-after-move
                if let Some((var_name, move_span)) = moved_in_loop.first() {
                    let path = move_span.path;
                    let src = utils::get_file_content(path).unwrap_or_default();
                    return Err(HirError::CannotMoveOutOfLoop(CannotMoveOutOfLoopError {
                        var_name: var_name.to_string(),
                        move_span: *move_span,
                        loop_span: Span {
                            start: while_stmt.span.start,
                            end: while_stmt.condition.span().end,
                            path: while_stmt.span.path,
                        },
                        src: NamedSource::new(path, src),
                    }));
                }

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

                // Special case: If LHS is an indexing expression (container slot), we allow
                // moving from another container slot without the strict check. This enables
                // reallocation patterns like: new_data[i] = this.data[i]
                let lhs_is_container_slot = Self::is_indexing_expr(&assign.lhs);
                let rhs_is_container_slot = Self::is_indexing_expr(&assign.rhs);

                // If we're moving from one container slot to another, don't apply strict
                // ownership checking on the RHS - this is a transfer, not an escape
                let rhs_consumes_ownership = if lhs_is_container_slot && rhs_is_container_slot {
                    false // Will just copy the reference, which is fine for reallocation
                } else {
                    true
                };

                let transformed_rhs =
                    self.transform_expr_ownership(&assign.rhs, rhs_consumes_ownership)?;

                Ok(HirExpr::Assign(
                    crate::atlas_c::atlas_hir::expr::HirAssignExpr {
                        span: assign.span,
                        lhs: Box::new(transformed_lhs),
                        rhs: Box::new(transformed_rhs),
                        ty: assign.ty,
                    },
                ))
            }
            HirExpr::Call(call) => {
                // Check if this is an explicit _copy() method call inside a _copy method
                // This would cause infinite recursion
                if let Some(method_ctx) = &self.current_method_context
                    && method_ctx.method_name == "_copy"
                    && let HirExpr::FieldAccess(field_access) = call.callee.as_ref()
                    && field_access.field.name == "_copy"
                    // Check if the return type of the call matches our return type
                    && let Some(return_ty) = &method_ctx.return_ty
                    && self.types_match(call.ty, return_ty)
                {
                    let path = call.span.path;
                    let src = utils::get_file_content(path).unwrap_or_default();
                    return Err(HirError::RecursiveCopyConstructor(
                        RecursiveCopyConstructorError {
                            copy_span: call.span,
                            method_span: method_ctx.method_span,
                            type_name: Self::get_type_name(call.ty),
                            src: NamedSource::new(path, src),
                        },
                    ));
                }

                // Check if this is a method call that consumes `this` (modifier == None)
                let method_consumes_this = self.method_consumes_this(&call.callee);

                let transformed_callee = if method_consumes_this {
                    // For method calls that consume `this`, transform the target as ownership-consuming
                    if let HirExpr::FieldAccess(field_access) = call.callee.as_ref() {
                        let transformed_target =
                            self.transform_expr_ownership(&field_access.target, true)?;

                        // Invalidate any references whose origin is the consumed target.
                        // This prevents use-after-free when a consuming method destroys the object
                        // that references point into.
                        // Example:
                        //   let ref = my_vec.get(0);
                        //   my_vec.consume();  // consuming method
                        //   println(*ref);     // ERROR: ref's origin (my_vec) was consumed
                        if let HirExpr::Ident(ident) = field_access.target.as_ref() {
                            self.scope_map
                                .invalidate_references_with_origin(ident.name, call.span);
                        }

                        HirExpr::FieldAccess(crate::atlas_c::atlas_hir::expr::HirFieldAccessExpr {
                            span: field_access.span,
                            target: Box::new(transformed_target),
                            field: field_access.field.clone(),
                            ty: field_access.ty,
                        })
                    } else {
                        self.transform_expr_ownership(&call.callee, false)?
                    }
                } else {
                    self.transform_expr_ownership(&call.callee, false)?
                };

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

                Ok(HirExpr::Call(
                    crate::atlas_c::atlas_hir::expr::HirFunctionCallExpr {
                        span: call.span,
                        callee: Box::new(transformed_callee),
                        callee_span: call.callee_span,
                        args: transformed_args,
                        args_ty: call.args_ty.clone(),
                        generics: call.generics.clone(),
                        ty: call.ty,
                    },
                ))
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

                let transformed_inner =
                    self.transform_expr_ownership(&unary.expr, inner_consumes)?;

                let unary_expr = HirExpr::Unary(crate::atlas_c::atlas_hir::expr::UnaryOpExpr {
                    span: unary.span,
                    op: unary.op.clone(),
                    expr: Box::new(transformed_inner),
                    ty: unary.ty,
                });

                // Special case: Deref in an ownership-consuming context with a copyable result type
                // needs to produce a copy of the object, not just read the pointer.
                // This handles cases like `*this.field` in _copy methods where field is an object.
                if is_ownership_consuming
                    && matches!(
                        unary.op,
                        Some(crate::atlas_c::atlas_hir::expr::HirUnaryOp::Deref)
                    )
                {
                    let is_copyable = self.is_type_copyable(unary.ty);
                    if is_copyable && !Self::is_primitive_type(unary.ty) {
                        // Wrap in Copy to ensure proper deep copy semantics
                        return Ok(HirExpr::Copy(HirCopyExpr {
                            span: unary.span,
                            source_name: "", // No specific source name for deref expression
                            expr: Box::new(unary_expr),
                            ty: unary.ty,
                        }));
                    }
                }

                Ok(unary_expr)
            }
            HirExpr::NewObj(new_obj) => {
                let mut transformed_args = Vec::new();
                for arg in new_obj.args.iter() {
                    // Check if this argument is a reference with an origin
                    // If so, emit a warning that it might escape into the constructor
                    self.check_reference_escapes_to_constructor(arg, new_obj.ty, new_obj.span);

                    transformed_args.push(self.transform_expr_ownership(arg, true)?);
                }

                Ok(HirExpr::NewObj(
                    crate::atlas_c::atlas_hir::expr::HirNewObjExpr {
                        span: new_obj.span,
                        ty: new_obj.ty,
                        args: transformed_args,
                        args_ty: new_obj.args_ty.clone(),
                    },
                ))
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
                // Check if the target expression creates a temporary value that needs to be freed
                // This catches method chaining like: foo.bar().baz() where bar() returns a temporary
                if Self::should_warn_about_temporary_in_method_chain(&field.target) {
                    let path = field.span.path;
                    let src = utils::get_file_content(path).unwrap_or_default();

                    let target_expr_str = Self::get_expr_description(&field.target);
                    let field_name = field.field.name;

                    let warning: ErrReport = HirWarning::TemporaryValueCannotBeFreed(
                        TemporaryValueCannotBeFreedWarning {
                            src: NamedSource::new(path, src),
                            span: field.target.span(),
                            expr_kind: target_expr_str,
                            var_name: "result".to_string(),
                            target_expr: format!(
                                ".{} //Rest of your method chain here",
                                field_name
                            ),
                        },
                    )
                    .into();
                    eprintln!("{:?}", warning);
                }

                let transformed_target = self.transform_expr_ownership(&field.target, false)?;
                let field_access_expr =
                    HirExpr::FieldAccess(crate::atlas_c::atlas_hir::expr::HirFieldAccessExpr {
                        span: field.span,
                        target: Box::new(transformed_target),
                        field: field.field.clone(),
                        ty: field.ty,
                    });

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
                let indexing_expr =
                    HirExpr::Indexing(crate::atlas_c::atlas_hir::expr::HirIndexingExpr {
                        span: indexing.span,
                        target: Box::new(transformed_target),
                        index: Box::new(transformed_index),
                        ty: indexing.ty,
                    });

                // If ownership is being consumed, we need to COPY or emit an error
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
                        // Cannot move non-copyable element out of a container.
                        // Use methods like try_take() that use memcpy to extract elements.
                        let path = indexing.span.path;
                        let src = utils::get_file_content(path).unwrap_or_default();
                        Err(HirError::CannotMoveOutOfContainer(
                            CannotMoveOutOfContainerError {
                                span: indexing.span,
                                ty_name: indexing.ty.to_string(),
                                src: NamedSource::new(path, src),
                            },
                        ))
                    }
                } else {
                    Ok(indexing_expr)
                }
            }
            HirExpr::Casting(cast) => {
                // Check if the inner expression creates a temporary value that needs to be freed
                // If so, emit a warning because the ownership pass can't properly free it
                if Self::should_warn_about_temporary_in_cast(&cast.expr) {
                    let path = cast.span.path;
                    let src = utils::get_file_content(path).unwrap_or_default();

                    let expr_kind = Self::get_expr_description(&cast.expr);
                    let target_type = Self::get_type_name(cast.ty);

                    let warning: ErrReport = HirWarning::TemporaryValueCannotBeFreed(
                        TemporaryValueCannotBeFreedWarning {
                            src: NamedSource::new(path, src),
                            span: cast.expr.span(),
                            expr_kind,
                            var_name: "result".to_string(), // Generic name for help message
                            target_expr: format!(" as {}", target_type),
                        },
                    )
                    .into();
                    eprintln!("{:?}", warning);
                }

                let transformed =
                    self.transform_expr_ownership(&cast.expr, is_ownership_consuming)?;
                Ok(HirExpr::Casting(
                    crate::atlas_c::atlas_hir::expr::HirCastExpr {
                        span: cast.span,
                        expr: Box::new(transformed),
                        ty: cast.ty,
                    },
                ))
            }
            // Delete expressions - mark the variable as deleted so it won't be auto-deleted later
            // Also invalidate any references that point into the deleted variable
            HirExpr::Delete(del) => {
                // If deleting an identifier, mark it as deleted
                // The inner expression might be wrapped in a Unary with op=None, so unwrap it
                let inner_expr = match del.expr.as_ref() {
                    HirExpr::Unary(u) if u.op.is_none() => u.expr.as_ref(),
                    other => other,
                };

                if let HirExpr::Ident(ident) = inner_expr {
                    // Mark the variable itself as deleted
                    if let Some(var_data) = self.scope_map.get_mut(ident.name) {
                        var_data.status = VarStatus::Deleted {
                            delete_span: del.span,
                        };
                    }

                    // Invalidate all references whose origin includes this variable
                    // This prevents use-after-free bugs like:
                    //   let ref = my_vec.get(0);
                    //   delete my_vec;
                    //   println(*ref);  // ERROR: ref's origin (my_vec) was deleted
                    self.scope_map
                        .invalidate_references_with_origin(ident.name, del.span);
                }
                // Return the delete expression as-is
                Ok(expr.clone())
            }
            // Explicit move expression from user code: move<>(x)
            // This forces a move regardless of later uses
            HirExpr::Move(move_expr) => {
                // Transform the inner expression as ownership-consuming (will mark as moved)
                let transformed_inner = self.transform_expr_ownership(&move_expr.expr, true)?;

                // If the inner expression is an identifier that we just transformed,
                // we need to ensure it's marked as moved
                if let HirExpr::Ident(ident) = move_expr.expr.as_ref()
                    && let Some(var_data) = self.scope_map.get(ident.name)
                    // Check if it wasn't already marked as moved by transform_expr_ownership
                    // (it might have been if it's copyable and we decided to copy)
                    // Force the move since user explicitly wrote move<>()
                    && matches!(var_data.status, VarStatus::Owned | VarStatus::Borrowed)
                {
                    self.scope_map.mark_as_moved(ident.name, ident.span);
                }

                // The transformed inner might already be a Move/Copy,
                // so we may end up with nested Move(Move(...)) - that's OK,
                // or we can unwrap it
                match transformed_inner {
                    // If transform already produced a Move, use it as-is
                    HirExpr::Move(inner_move) => Ok(HirExpr::Move(inner_move)),
                    // If transform produced a Copy (because it thought there were later uses),
                    // but user explicitly asked for move, convert to Move
                    HirExpr::Copy(copy_expr) => {
                        // Mark as moved since user explicitly requested move
                        if let HirExpr::Ident(ident) = copy_expr.expr.as_ref() {
                            self.scope_map.mark_as_moved(ident.name, ident.span);
                        }
                        Ok(HirExpr::Move(HirMoveExpr {
                            span: move_expr.span,
                            source_name: copy_expr.source_name,
                            expr: copy_expr.expr,
                            ty: move_expr.ty,
                        }))
                    }
                    // Otherwise, wrap in a Move
                    other => Ok(HirExpr::Move(HirMoveExpr {
                        span: move_expr.span,
                        source_name: move_expr.source_name,
                        expr: Box::new(other),
                        ty: move_expr.ty,
                    })),
                }
            }
            // Explicit copy expression from user code: copy<>(x)
            HirExpr::Copy(copy_expr) => {
                // Transform the inner expression (not ownership-consuming since we're copying)
                let transformed_inner = self.transform_expr_ownership(&copy_expr.expr, false)?;

                // Check that the value is still valid to copy from
                if let HirExpr::Ident(ident) = copy_expr.expr.as_ref() {
                    self.check_variable_valid(ident)?;
                }

                Ok(HirExpr::Copy(HirCopyExpr {
                    span: copy_expr.span,
                    source_name: copy_expr.source_name,
                    expr: Box::new(transformed_inner),
                    ty: copy_expr.ty,
                }))
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
            return Err(HirError::TryingToAccessAMovedValue(
                TryingToAccessAMovedValueError {
                    move_span: *move_span,
                    access_span: ident.span,
                    src: NamedSource::new(path, src),
                },
            ));
        }

        // Check for use-after-conditional-move (moved in one branch of an if/else)
        if let VarStatus::ConditionallyMoved { move_span } = &var_data.status {
            let path = ident.span.path;
            let src = utils::get_file_content(path).unwrap_or_default();
            return Err(HirError::TryingToAccessAPotentiallyMovedValue(
                TryingToAccessAPotentiallyMovedValueError {
                    move_span: *move_span,
                    access_span: ident.span,
                    src: NamedSource::new(path, src),
                },
            ));
        }

        // Check for use-after-delete (includes references whose origin was deleted)
        if let VarStatus::Deleted { delete_span } = &var_data.status {
            let path = ident.span.path;
            let src = utils::get_file_content(path).unwrap_or_default();
            return Err(HirError::TryingToAccessADeletedValue(
                TryingToAccessADeletedValueError {
                    delete_span: *delete_span,
                    access_span: ident.span,
                    src: NamedSource::new(path, src),
                },
            ));
        }

        // Reference types don't transfer ownership
        if var_data.kind == VarKind::Reference {
            return Ok(HirExpr::Ident(ident.clone()));
        }

        // Primitives are always implicitly copied (bitwise copy)
        if var_data.kind == VarKind::Primitive {
            return Ok(HirExpr::Ident(ident.clone()));
        }

        // Check if we can safely move (no future uses of any kind)
        let can_move = var_data.can_move_at(self.current_stmt_index);

        // For copyable types: prefer MOVE if this is the last use, otherwise COPY
        if var_data.is_copyable {
            if can_move {
                // This is the last use - MOVE to avoid unnecessary copy
                self.scope_map.mark_as_moved(ident.name, ident.span);
                return Ok(HirExpr::Move(HirMoveExpr {
                    span: ident.span,
                    source_name: ident.name,
                    expr: Box::new(HirExpr::Ident(ident.clone())),
                    ty: ident.ty,
                }));
            } else {
                // Still has future uses - must COPY
                // Check if all remaining uses are just reads (borrows) - if so, emit a warning
                let later_uses: Vec<&VarUse> = var_data
                    .uses
                    .iter()
                    .filter(|u| u.stmt_index > self.current_stmt_index)
                    .collect();

                let all_later_uses_are_reads = later_uses.iter().all(|u| u.kind == UseKind::Read);

                if all_later_uses_are_reads && !later_uses.is_empty() {
                    // Emit warning: copying here but only borrowing later
                    // Collect all the borrow uses as related diagnostics
                    let path = ident.span.path;
                    let src = utils::get_file_content(path).unwrap_or_default();

                    let borrow_uses: Vec<Span> =
                        later_uses.iter().map(|var_use| var_use.span).collect();

                    let warning: ErrReport = HirWarning::UnnecessaryCopyDueToLaterBorrows(
                        UnnecessaryCopyDueToLaterBorrowsWarning {
                            span: ident.span,
                            var_name: ident.name.to_string(),
                            src: NamedSource::new(path, src),
                            borrow_uses,
                        },
                    )
                    .into();
                    eprintln!("{:?}", warning);
                }

                // Check for recursive copy in _copy methods
                if let Some(method_ctx) = &self.current_method_context
                    && method_ctx.method_name == "_copy"
                    && let Some(return_ty) = &method_ctx.return_ty
                    // Check if we're trying to copy the same type we're returning
                    && self.types_match(ident.ty, return_ty)
                {
                    let path = ident.span.path;
                    let src = utils::get_file_content(path).unwrap_or_default();
                    return Err(HirError::RecursiveCopyConstructor(
                        RecursiveCopyConstructorError {
                            copy_span: ident.span,
                            method_span: method_ctx.method_span,
                            type_name: Self::get_type_name(ident.ty),
                            src: NamedSource::new(path, src),
                        },
                    ));
                }

                return Ok(HirExpr::Copy(HirCopyExpr {
                    span: ident.span,
                    source_name: ident.name,
                    expr: Box::new(HirExpr::Ident(ident.clone())),
                    ty: ident.ty,
                }));
            }
        }

        // For non-copyable types: must MOVE
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
                // Special case: AsRef (&) and Deref (*) don't consume ownership of the inner expression
                // We need to process the inner expression with is_ownership_consuming = false
                let inner_consumes = match unary.op {
                    Some(crate::atlas_c::atlas_hir::expr::HirUnaryOp::AsRef) => false,
                    Some(crate::atlas_c::atlas_hir::expr::HirUnaryOp::Deref) => false,
                    // For other unary ops in return context, propagate the ownership-consuming behavior
                    _ => true,
                };

                let transformed_inner = if inner_consumes {
                    self.transform_expr_for_return(&unary.expr)?
                } else {
                    self.transform_expr_ownership(&unary.expr, false)?
                };

                let unary_expr = HirExpr::Unary(crate::atlas_c::atlas_hir::expr::UnaryOpExpr {
                    span: unary.span,
                    op: unary.op.clone(),
                    expr: Box::new(transformed_inner),
                    ty: unary.ty,
                });

                // Special case: Deref in a return context with a copyable result type
                // needs to produce a copy of the dereferenced object.
                // This handles cases like `*this.field` in methods that return owned values.
                if matches!(
                    unary.op,
                    Some(crate::atlas_c::atlas_hir::expr::HirUnaryOp::Deref)
                ) {
                    let is_copyable = self.is_type_copyable(unary.ty);
                    if is_copyable && !Self::is_primitive_type(unary.ty) {
                        // Wrap in Copy to ensure proper deep copy semantics
                        return Ok(HirExpr::Copy(HirCopyExpr {
                            span: unary.span,
                            source_name: "", // No specific source name for deref expression
                            expr: Box::new(unary_expr),
                            ty: unary.ty,
                        }));
                    }
                }

                Ok(unary_expr)
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
    fn create_delete_stmt(
        var_name: &'hir str,
        var_ty: &'hir HirTy<'hir>,
        span: Span,
    ) -> HirStatement<'hir> {
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

    /// Find the position to insert delete statements in a block.
    /// This should be before any return statement at the end.
    fn find_delete_insert_position(statements: &[HirStatement]) -> usize {
        if let Some(last) = statements.last()
            && matches!(last, HirStatement::Return(_))
        {
            return statements.len().saturating_sub(1);
        }
        statements.len()
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
                VarStatus::ConditionallyMoved { move_span } => {
                    return Err(HirError::TryingToAccessAPotentiallyMovedValue(
                        TryingToAccessAPotentiallyMovedValueError {
                            move_span: *move_span,
                            access_span: ident.span,
                            src: NamedSource::new(path, src),
                        },
                    ));
                }
                VarStatus::Deleted { delete_span } => {
                    return Err(HirError::TryingToAccessADeletedValue(
                        TryingToAccessADeletedValueError {
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

    /// Collect all variable names used in an expression
    /// This is used to avoid deleting variables that are used in return expressions
    fn collect_vars_used_in_expr(expr: &HirExpr<'hir>, vars: &mut Vec<&'hir str>) {
        match expr {
            HirExpr::Ident(ident) => {
                vars.push(ident.name);
            }
            HirExpr::Move(mv) => Self::collect_vars_used_in_expr(&mv.expr, vars),
            HirExpr::Copy(cp) => Self::collect_vars_used_in_expr(&cp.expr, vars),
            HirExpr::Unary(unary) => Self::collect_vars_used_in_expr(&unary.expr, vars),
            HirExpr::HirBinaryOperation(binary) => {
                Self::collect_vars_used_in_expr(&binary.lhs, vars);
                Self::collect_vars_used_in_expr(&binary.rhs, vars);
            }
            HirExpr::FieldAccess(field) => {
                Self::collect_vars_used_in_expr(&field.target, vars);
            }
            HirExpr::Indexing(idx) => {
                Self::collect_vars_used_in_expr(&idx.target, vars);
                Self::collect_vars_used_in_expr(&idx.index, vars);
            }
            HirExpr::Call(call) => {
                for arg in call.args.iter() {
                    Self::collect_vars_used_in_expr(arg, vars);
                }
            }
            HirExpr::NewObj(new_obj) => {
                for arg in new_obj.args.iter() {
                    Self::collect_vars_used_in_expr(arg, vars);
                }
            }
            HirExpr::ListLiteral(arr) => {
                for elem in arr.items.iter() {
                    Self::collect_vars_used_in_expr(elem, vars);
                }
            }
            HirExpr::ObjLiteral(obj) => {
                for field in obj.fields.iter() {
                    Self::collect_vars_used_in_expr(&field.value, vars);
                }
            }
            HirExpr::Casting(cast) => {
                Self::collect_vars_used_in_expr(&cast.expr, vars);
            }
            // Literals and other expressions don't use variables
            _ => {}
        }
    }

    /// Check if an expression is (or contains) an indexing expression.
    /// Unwraps Unary wrappers since the parser sometimes wraps expressions.
    fn is_indexing_expr(expr: &HirExpr<'hir>) -> bool {
        match expr {
            HirExpr::Indexing(_) => true,
            // Unwrap unary wrappers (parser sometimes wraps expressions)
            HirExpr::Unary(unary) if unary.op.is_none() => Self::is_indexing_expr(&unary.expr),
            _ => false,
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

    /// Check if a method call consumes `this` (takes ownership, not a reference)
    ///
    /// Returns true if:
    /// - The callee is a FieldAccess (method call on object)
    /// - The method has modifier == None (meaning `fun foo(this)` not `fun foo(&this)` or `fun foo(&const this)`)
    fn method_consumes_this(&self, callee: &HirExpr<'hir>) -> bool {
        if let HirExpr::FieldAccess(field_access) = callee {
            // Get the type of the target to find the struct
            let target_ty = field_access.target.ty();

            // For generic types, we need to use the mangled name to look up in the signature
            let struct_name: Option<&str> = match target_ty {
                HirTy::Named(n) => Some(n.name),
                HirTy::Generic(g) => {
                    // Generic types are stored under their mangled name
                    Some(MonomorphizationPass::generate_mangled_name(
                        self.hir_arena,
                        g,
                        "struct",
                    ))
                }
                _ => None,
            };

            if let Some(name) = struct_name
                && let Some(struct_sig) = self.hir_signature.structs.get(name)
                && let Some(method_sig) = struct_sig.methods.get(field_access.field.name)
            {
                // Only `fun foo(this)` (modifier == None) consumes ownership
                // `fun foo(&this)` (Mutable) and `fun foo(&const this)` (Const) borrow
                return method_sig.modifier == HirStructMethodModifier::None;
            }
        }
        false
    }

    // =========================================================================
    // Reference Origin Tracking
    // =========================================================================

    /// Check if a reference argument escapes into a constructor and emit a warning.
    ///
    /// When a reference with a known origin is passed to a constructor, the constructed
    /// object might store it. If the origin is later deleted, using the stored reference
    /// would be a use-after-free bug. Since we can't know for sure if the constructor
    /// stores the reference, we emit a warning.
    fn check_reference_escapes_to_constructor(
        &self,
        arg: &HirExpr<'hir>,
        constructed_ty: &HirTy<'hir>,
        constructor_span: Span,
    ) {
        // Check if the argument is a reference type
        if !arg.ty().is_ref() {
            return;
        }

        // Unwrap Unary with op=None (which is just a wrapper)
        let inner = match arg {
            HirExpr::Unary(u) if u.op.is_none() => u.expr.as_ref(),
            other => other,
        };

        // Check if the argument is an identifier with a known origin
        if let HirExpr::Ident(ident) = inner {
            if let Some(var_data) = self.scope_map.get(ident.name) {
                // Only warn if the reference has a trackable origin
                if let ReferenceOrigin::Variable(origin_name) = &var_data.origin {
                    // Get the origin variable's span for the label
                    let origin_span = self
                        .scope_map
                        .get(origin_name)
                        .map(|v| v.span)
                        .unwrap_or(ident.span);

                    let path = constructor_span.path;
                    let src = utils::get_file_content(path).unwrap_or_default();

                    let warning: ErrReport = HirWarning::ReferenceEscapesToConstructor(
                        ReferenceEscapesToConstructorWarning {
                            src: NamedSource::new(path, src),
                            span: constructor_span,
                            origin_span,
                            ref_var: ident.name.to_string(),
                            origin_var: origin_name.to_string(),
                            constructed_type: constructed_ty.to_string(),
                        },
                    )
                    .into();

                    eprintln!("{:?}", warning);
                }
            }
        }
    }

    /// Compute the origin of a reference expression.
    ///
    /// This tracks where a reference "points into" so we can invalidate it
    /// when the origin is deleted or consumed.
    ///
    /// Examples:
    /// - `&x` → origin is `x`
    /// - `my_vec.get(0)` → origin is `my_vec` (method returning reference)
    /// - `foo.bar` where bar is a reference field → origin is `foo`
    fn compute_reference_origin(&self, expr: &HirExpr<'hir>) -> ReferenceOrigin<'hir> {
        match expr {
            // Direct borrow: &x → origin is x
            HirExpr::Unary(unary) => {
                match &unary.op {
                    Some(crate::atlas_c::atlas_hir::expr::HirUnaryOp::AsRef) => {
                        // The reference points into the inner expression
                        self.get_origin_from_lvalue(&unary.expr)
                    }
                    Some(crate::atlas_c::atlas_hir::expr::HirUnaryOp::Deref) => {
                        // Dereferencing a reference - inherit origin from the reference
                        self.compute_reference_origin(&unary.expr)
                    }
                    _ => {
                        // Other unary ops (Neg, Not, None) - propagate
                        self.compute_reference_origin(&unary.expr)
                    }
                }
            }

            // Method call: x.get(i) → if returns reference, origin includes x
            HirExpr::Call(call) => {
                // Check if the return type is a reference
                if !matches!(
                    call.ty,
                    HirTy::ReadOnlyReference(_) | HirTy::MutableReference(_)
                ) {
                    return ReferenceOrigin::None;
                }

                let mut origin = ReferenceOrigin::None;

                // If it's a method call, the receiver is a possible origin
                if let HirExpr::FieldAccess(field) = call.callee.as_ref() {
                    origin = origin.merge(self.get_origin_from_lvalue(&field.target));
                }

                // Any reference arguments are also possible origins
                for (i, arg) in call.args.iter().enumerate() {
                    let is_ref_param = call.args_ty.get(i).is_some_and(|ty| {
                        matches!(ty, HirTy::ReadOnlyReference(_) | HirTy::MutableReference(_))
                    });
                    if is_ref_param {
                        origin = origin.merge(self.get_origin_from_lvalue(arg));
                    }
                }

                origin
            }

            // Field access: x.field → origin is x (if field is reference type)
            HirExpr::FieldAccess(field) => self.get_origin_from_lvalue(&field.target),

            // Indexing: arr[i] → origin is arr
            HirExpr::Indexing(indexing) => self.get_origin_from_lvalue(&indexing.target),

            // Variable: y (where y is a reference) → inherit y's origin
            HirExpr::Ident(ident) => {
                if let Some(var) = self.scope_map.get(ident.name) {
                    if var.kind == VarKind::Reference {
                        // Inherit the origin from the existing reference variable
                        var.origin.clone()
                    } else {
                        // It's not a reference, so if we're creating a reference to it,
                        // this variable IS the origin
                        ReferenceOrigin::Variable(ident.name)
                    }
                } else {
                    ReferenceOrigin::None
                }
            }

            // Move/Copy wrappers - look inside
            HirExpr::Move(mv) => self.compute_reference_origin(&mv.expr),
            HirExpr::Copy(cp) => self.compute_reference_origin(&cp.expr),

            // Casting - propagate through
            HirExpr::Casting(cast) => self.compute_reference_origin(&cast.expr),

            // Literals and other expressions have no origin
            _ => ReferenceOrigin::None,
        }
    }

    /// Get the origin from an lvalue expression (something that can be referenced).
    /// This returns the variable name that "owns" the data being referenced.
    fn get_origin_from_lvalue(&self, expr: &HirExpr<'hir>) -> ReferenceOrigin<'hir> {
        match expr {
            // Direct variable reference: the variable is the origin
            HirExpr::Ident(ident) => ReferenceOrigin::Variable(ident.name),

            // Field access: origin is the root object
            HirExpr::FieldAccess(field) => self.get_origin_from_lvalue(&field.target),

            // Indexing: origin is the container
            HirExpr::Indexing(indexing) => self.get_origin_from_lvalue(&indexing.target),

            // Deref: origin is from the inner reference
            HirExpr::Unary(unary)
                if matches!(
                    unary.op,
                    Some(crate::atlas_c::atlas_hir::expr::HirUnaryOp::Deref)
                ) =>
            {
                // Dereferencing a reference - get the origin from the reference
                if let HirExpr::Ident(ident) = unary.expr.as_ref()
                    && let Some(var) = self.scope_map.get(ident.name)
                {
                    return var.origin.clone();
                }
                self.compute_reference_origin(&unary.expr)
            }

            // Other unary ops - look through
            HirExpr::Unary(unary) => self.get_origin_from_lvalue(&unary.expr),

            // ThisLiteral: use "this" as the origin name
            HirExpr::ThisLiteral(_) => ReferenceOrigin::Variable("this"),

            // Move/Copy wrappers
            HirExpr::Move(mv) => self.get_origin_from_lvalue(&mv.expr),
            HirExpr::Copy(cp) => self.get_origin_from_lvalue(&cp.expr),

            // Other expressions don't have a clear origin
            _ => ReferenceOrigin::None,
        }
    }

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
            // Extern types are assumed copyable (no ownership semantics)
            HirTy::ExternTy(_) => true,

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
            HirTy::Named(named) => self
                .hir_signature
                .structs
                .get(named.name)
                .is_some_and(|s| s.methods.contains_key("_copy")),

            // Generic types - need to check the monomorphized/instantiated struct
            HirTy::Generic(g) => {
                // Compute the mangled name that the monomorphized struct is stored under
                // Format: __atlas77__struct__<Name>__<Type1>_<Type2>_...
                let mangled_name =
                    MonomorphizationPass::generate_mangled_name(self.hir_arena, g, "struct");
                self.hir_signature
                    .structs
                    .get(mangled_name)
                    .is_some_and(|s| s.methods.contains_key("_copy"))
            }

            // Other types are not copyable
            _ => false,
        }
    }

    /// Check if a type is a primitive type (bitwise copyable, no heap allocation)
    fn is_primitive_type(ty: &HirTy<'hir>) -> bool {
        matches!(
            ty,
            HirTy::Boolean(_)
                | HirTy::Int64(_)
                | HirTy::Float64(_)
                | HirTy::Char(_)
                | HirTy::UInt64(_)
                | HirTy::Unit(_)
        )
    }

    /// Check if two types match (used for detecting recursive copy)
    fn types_match(&self, ty1: &HirTy<'hir>, ty2: &HirTy<'hir>) -> bool {
        match (ty1, ty2) {
            (HirTy::Named(n1), HirTy::Named(n2)) => n1.name == n2.name,
            (HirTy::Generic(g1), HirTy::Generic(g2)) => {
                // Compare mangled names for generic types
                let name1 =
                    MonomorphizationPass::generate_mangled_name(self.hir_arena, g1, "struct");
                let name2 =
                    MonomorphizationPass::generate_mangled_name(self.hir_arena, g2, "struct");
                name1 == name2
            }
            (HirTy::ReadOnlyReference(r1), HirTy::ReadOnlyReference(r2)) => {
                self.types_match(r1.inner, r2.inner)
            }
            (HirTy::MutableReference(r1), HirTy::MutableReference(r2)) => {
                self.types_match(r1.inner, r2.inner)
            }
            (HirTy::List(l1), HirTy::List(l2)) => self.types_match(l1.inner, l2.inner),
            (HirTy::Boolean(_), HirTy::Boolean(_))
            | (HirTy::Int64(_), HirTy::Int64(_))
            | (HirTy::Float64(_), HirTy::Float64(_))
            | (HirTy::Char(_), HirTy::Char(_))
            | (HirTy::UInt64(_), HirTy::UInt64(_))
            | (HirTy::Unit(_), HirTy::Unit(_))
            | (HirTy::String(_), HirTy::String(_)) => true,
            _ => false,
        }
    }

    /// Get a human-readable name for a type (used in error messages)
    fn get_type_name(ty: &HirTy<'hir>) -> String {
        match ty {
            HirTy::Named(n) => n.name.to_string(),
            HirTy::Generic(g) => {
                let type_args = g
                    .inner
                    .iter()
                    .map(Self::get_type_name)
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{}<{}>", g.name, type_args)
            }
            HirTy::ReadOnlyReference(r) => format!("&const {}", Self::get_type_name(r.inner)),
            HirTy::MutableReference(r) => format!("&{}", Self::get_type_name(r.inner)),
            HirTy::List(l) => format!("[{}]", Self::get_type_name(l.inner)),
            HirTy::Boolean(_) => "bool".to_string(),
            HirTy::Int64(_) => "int64".to_string(),
            HirTy::Float64(_) => "float64".to_string(),
            HirTy::Char(_) => "char".to_string(),
            HirTy::UInt64(_) => "uint64".to_string(),
            HirTy::Unit(_) => "()".to_string(),
            HirTy::String(_) => "string".to_string(),
            HirTy::Function(_) => "function".to_string(),
            _ => "unknown".to_string(),
        }
    }

    /// Check if a type is a type parameter (uninstantiated generic like T, K, V)
    ///
    /// Type parameters are represented as HirTy::Named where the name is not a known struct.
    /// We skip ownership checks for these because the actual type is unknown at generic
    /// definition time - the check will happen at instantiation when concrete types are used.
    fn is_type_parameter(&self, ty: &HirTy<'hir>) -> bool {
        match ty {
            HirTy::Named(named) => {
                // If the name is not a known struct, it's likely a type parameter
                !self.hir_signature.structs.contains_key(named.name)
            }
            _ => false,
        }
    }

    /// Check if we're in a borrowing method (`&this` or `&const this`) and trying to
    /// transfer ownership of `this` or a non-copyable field of `this`.
    ///
    /// This catches errors like:
    /// ```atlas
    /// fun into_iter(&this) -> Iter<T> {  // ERROR: &this means we don't own this
    ///     return new Iter<T>(this);       // but we're trying to give it away
    /// }
    /// ```
    fn check_ownership_transfer_in_borrowing_method(
        &self,
        expr: &HirExpr<'hir>,
        expr_span: Span,
    ) -> Option<HirError> {
        // Only check if we're in a borrowing method
        let ctx = self.current_method_context.as_ref()?;

        // Only &this (Mutable) and &const this (Const) are borrowing methods
        // `this` (None) owns and can transfer, Static has no `this`
        if !matches!(
            ctx.modifier,
            HirStructMethodModifier::Mutable | HirStructMethodModifier::Const
        ) {
            return None;
        }

        // Check if the expression is `this` or `this.field`
        let (value_name, is_this_related) = match expr {
            HirExpr::ThisLiteral(_) => {
                // Check if the struct type is copyable by looking it up in hir_signature
                if self
                    .hir_signature
                    .structs
                    .get(ctx.struct_name)
                    .is_some_and(|s| s.methods.contains_key("_copy"))
                {
                    return None;
                }
                ("this".to_string(), true)
            }
            HirExpr::FieldAccess(fa) => {
                // Check if target is `this`
                if matches!(fa.target.as_ref(), HirExpr::ThisLiteral(_)) {
                    // For generic structs, skip check for field accesses
                    // The concrete types are unknown at definition time, and the check
                    // will happen on the monomorphized version with concrete types
                    if ctx.is_generic_struct {
                        return None;
                    }

                    // Check if the field type is copyable or is a type parameter
                    let field_ty = fa.ty;

                    // Skip check for type parameters (uninstantiated generics like T, K, V)
                    // These are represented as HirTy::Named with a single-letter name typically
                    // The check will happen at instantiation time when the concrete type is known
                    if self.is_type_parameter(field_ty) {
                        return None;
                    }

                    if !self.is_type_copyable(field_ty) {
                        (format!("this.{}", fa.field.name), true)
                    } else {
                        // Field is copyable, no problem
                        return None;
                    }
                } else {
                    return None;
                }
            }
            HirExpr::Ident(ident) if ident.name == "this" => ("this".to_string(), true),
            _ => return None,
        };

        if !is_this_related {
            return None;
        }

        // We're in a borrowing method and trying to transfer ownership of this or this.field
        let path = ctx.method_span.path;
        let src = utils::get_file_content(path).unwrap_or_default();
        Some(HirError::CannotTransferOwnershipInBorrowingMethod(
            CannotTransferOwnershipInBorrowingMethodError {
                method_span: ctx.method_span,
                transfer_span: expr_span,
                value_name,
                src: NamedSource::new(path, src),
            },
        ))
    }

    /// Checks if a block (recursively) contains a `delete this` statement
    fn block_contains_delete_this(block: &HirBlock<'hir>) -> bool {
        for stmt in &block.statements {
            if Self::stmt_contains_delete_this(stmt) {
                return true;
            }
        }
        false
    }

    /// Checks if a statement (recursively) contains a `delete this` statement
    fn stmt_contains_delete_this(stmt: &HirStatement<'hir>) -> bool {
        match stmt {
            HirStatement::Expr(expr_stmt) => Self::expr_is_delete_this(&expr_stmt.expr),
            HirStatement::IfElse(if_else) => {
                Self::block_contains_delete_this(&if_else.then_branch)
                    || if_else
                        .else_branch
                        .as_ref()
                        .is_some_and(Self::block_contains_delete_this)
            }
            HirStatement::While(while_stmt) => Self::block_contains_delete_this(&while_stmt.body),
            HirStatement::_Block(block) => Self::block_contains_delete_this(block),
            _ => false,
        }
    }

    /// Checks if an expression is `delete this`
    fn expr_is_delete_this(expr: &HirExpr<'hir>) -> bool {
        match expr {
            HirExpr::Delete(delete_expr) => {
                // The inner expression might be wrapped in a Unary with op=None, so unwrap it
                let inner = match delete_expr.expr.as_ref() {
                    HirExpr::Unary(u) if u.op.is_none() => u.expr.as_ref(),
                    other => other,
                };
                matches!(inner, HirExpr::ThisLiteral(_))
            }
            // The entire delete expression might be wrapped in a Unary with op=None
            // (this happens because parse_unary always wraps in AstUnaryOpExpr)
            HirExpr::Unary(u) if u.op.is_none() => Self::expr_is_delete_this(&u.expr),
            // Also check Move/Copy wrappers in case transform wrapped the delete
            HirExpr::Move(move_expr) => Self::expr_is_delete_this(&move_expr.expr),
            HirExpr::Copy(copy_expr) => Self::expr_is_delete_this(&copy_expr.expr),
            _ => false,
        }
    }

    /// Checks if a block transfers ownership of `this` (e.g., by returning `this.field` or passing `this` to another function)
    fn block_transfers_this_ownership(block: &HirBlock<'hir>) -> bool {
        for stmt in &block.statements {
            if Self::stmt_transfers_this_ownership(stmt) {
                return true;
            }
        }
        false
    }

    /// Checks if a statement transfers ownership of `this`
    fn stmt_transfers_this_ownership(stmt: &HirStatement<'hir>) -> bool {
        match stmt {
            HirStatement::Return(ret) => Self::expr_uses_this(&ret.value),
            HirStatement::IfElse(if_else) => {
                Self::block_transfers_this_ownership(&if_else.then_branch)
                    || if_else
                        .else_branch
                        .as_ref()
                        .is_some_and(Self::block_transfers_this_ownership)
            }
            HirStatement::While(while_stmt) => {
                Self::block_transfers_this_ownership(&while_stmt.body)
            }
            HirStatement::_Block(block) => Self::block_transfers_this_ownership(block),
            _ => false,
        }
    }

    /// Checks if an expression uses `this` or a field of `this` (indicating ownership transfer)
    fn expr_uses_this(expr: &HirExpr<'hir>) -> bool {
        match expr {
            HirExpr::ThisLiteral(_) => true,
            HirExpr::FieldAccess(fa) => Self::expr_uses_this(&fa.target),
            HirExpr::Call(call) => {
                Self::expr_uses_this(&call.callee) || call.args.iter().any(Self::expr_uses_this)
            }
            HirExpr::Move(m) => Self::expr_uses_this(&m.expr),
            HirExpr::Copy(c) => Self::expr_uses_this(&c.expr),
            HirExpr::Unary(u) => Self::expr_uses_this(&u.expr),
            HirExpr::NewObj(obj) => obj.args.iter().any(Self::expr_uses_this),
            HirExpr::ObjLiteral(s) => s.fields.iter().any(|f| Self::expr_uses_this(&f.value)),
            _ => false,
        }
    }

    /// Emits a warning for consuming methods that don't delete `this`
    fn emit_consuming_method_warning(
        method_name: &str,
        method_sig: &HirStructMethodSignature,
        span: Span,
    ) {
        let mut pretty_printer = HirPrettyPrinter::new();
        pretty_printer.print_method_signature(method_name, method_sig);
        let path = span.path;
        let src = utils::get_file_content(path).unwrap_or_default();
        let warning: ErrReport =
            HirWarning::ConsumingMethodMayLeakThis(ConsumingMethodMayLeakThisWarning {
                src: NamedSource::new(path, src),
                span,
                method_signature: pretty_printer.get_output(),
            })
            .into();
        eprintln!("{:?}", warning);
    }

    /// Check if an expression creates a temporary value that needs to be freed
    /// but can't be freed when used inside a cast expression
    fn should_warn_about_temporary_in_cast(expr: &HirExpr<'hir>) -> bool {
        match expr {
            // Unary expressions: check recursively for operator-less (grouping),
            // or check if the result type needs management for operators like * (dereference)
            HirExpr::Unary(unary) => {
                if unary.op.is_none() {
                    // Just grouping/wrapping - check the inner expression
                    Self::should_warn_about_temporary_in_cast(&unary.expr)
                } else {
                    // Has an operator (like dereference *) - check if result needs management
                    // Example: *(&my_string) might create a copy that needs freeing
                    !unary.ty.is_ref() && Self::type_needs_memory_management(unary.ty)
                }
            }
            // Function calls that return owned values (not references) create temporaries
            HirExpr::Call(call) => {
                let return_type = call.ty;
                // If the return type is not a reference and needs memory management, warn
                // This includes strings (which are copyable but still need freeing)
                !return_type.is_ref() && Self::type_needs_memory_management(return_type)
            }
            // New object expressions create temporaries
            HirExpr::NewObj(new_obj) => {
                !new_obj.ty.is_ref() && Self::type_needs_memory_management(new_obj.ty)
            }
            // Binary operations that create new values (like string concatenation)
            HirExpr::HirBinaryOperation(binop) => {
                !binop.ty.is_ref() && Self::type_needs_memory_management(binop.ty)
            }
            // Any other expression that creates a value needing memory management
            _ => false,
        }
    }

    /// Check if a type needs memory management (i.e., it's not a primitive that can be trivially copied)
    /// This includes strings, structs, lists, etc. - even if they're copyable, they still need freeing
    fn type_needs_memory_management(ty: &HirTy<'hir>) -> bool {
        match ty {
            // Primitives don't need memory management
            HirTy::Int64(_)
            | HirTy::Float64(_)
            | HirTy::UInt64(_)
            | HirTy::Char(_)
            | HirTy::Boolean(_)
            | HirTy::Unit(_) => false,

            // References don't need to be freed (they're borrowed)
            HirTy::MutableReference(_) | HirTy::ReadOnlyReference(_) => false,

            // Strings need memory management even though they're copyable
            HirTy::String(_) => true,

            // Lists, structs, and other complex types need memory management
            HirTy::List(_) | HirTy::Named(_) | HirTy::Generic(_) => true,

            // Nullable types need management if their inner type does
            HirTy::Nullable(nullable) => Self::type_needs_memory_management(nullable.inner),

            // Other types
            HirTy::Uninitialized(_) | HirTy::ExternTy(_) | HirTy::Function(_) => false,
        }
    }

    /// Check if an expression creates a temporary value in a method chain
    /// Example: foo.bar().baz() - bar() creates a temporary that's used for .baz()
    fn should_warn_about_temporary_in_method_chain(expr: &HirExpr<'hir>) -> bool {
        match expr {
            // Unwrap unary expressions with no operator
            HirExpr::Unary(unary) if unary.op.is_none() => {
                Self::should_warn_about_temporary_in_method_chain(&unary.expr)
            }
            // Function/method calls that return owned values create temporaries
            HirExpr::Call(call) => {
                let return_type = call.ty;
                !return_type.is_ref() && Self::type_needs_memory_management(return_type)
            }
            // Field access on a temporary: check if the target is a temporary
            HirExpr::FieldAccess(field) => {
                Self::should_warn_about_temporary_in_method_chain(&field.target)
            }
            // Other expressions don't create problematic temporaries in method chains
            _ => false,
        }
    }

    /// Get a human-readable description of an expression for error messages
    fn get_expr_description(expr: &HirExpr) -> String {
        let mut pretty_printer = HirPrettyPrinter::new();
        pretty_printer.print_expr(expr);

        pretty_printer.get_output()
    }
}
