mod context;
pub use context::{
    ReferenceOrigin, ScopeMap, UseKind, VarData, VarKind, VarMap, VarStatus, VarUse,
};

use crate::atlas_c::{
    atlas_hir::{
        HirModule,
        arena::HirArena,
        error::{
            CannotImplicitlyCopyNonCopyableValueError, CannotMoveFromRvalueError,
            CannotMoveOutOfContainerError, CannotMoveOutOfReferenceError,
            CannotTransferOwnershipInBorrowingMethodError, HirError, HirResult,
            LifetimeDependencyViolationError, RecursiveCopyConstructorError,
            ReturningValueWithLocalLifetimeDependencyError, TryingToAccessADeletedValueError,
            TryingToAccessAMovedValueError, TryingToAccessAPotentiallyMovedValueError,
            TypeNotCopyableError,
        },
        expr::{
            HirBinaryOpExpr, HirCastExpr, HirCopyExpr, HirDeleteExpr, HirExpr, HirFieldAccessExpr,
            HirFieldInit, HirFunctionCallExpr, HirIdentExpr, HirIndexingExpr, HirListLiteralExpr,
            HirNewObjExpr, HirObjLiteralExpr, HirUnaryOp, UnaryOpExpr,
        },
        item::HirFunction,
        monomorphization_pass::MonomorphizationPass,
        pretty_print::HirPrettyPrinter,
        signature::{
            HirGenericConstraint, HirGenericConstraintKind, HirModuleSignature,
            HirStructMethodModifier, HirStructMethodSignature,
        },
        stmt::{HirAssignStmt, HirBlock, HirExprStmt, HirStatement, HirVariableStmt},
        ty::HirTy,
        warning::{
            ConsumingMethodMayLeakThisWarning, HirWarning, MoveInLoopWarning, UseAfterMoveWarning,
        },
    },
    utils::{self, Span},
};
use miette::{ErrReport, NamedSource};

const COPY_CONSTRUCTOR_MANGLED_NAME: &str = "atlas77__copy_ctor";

/// Information about ownership semantics of a function call
struct CallOwnershipInfo {
    moved_args: Vec<usize>,
    copied_args: Vec<usize>,
    taken_args: Vec<usize>,
}

/// The Ownership Analysis Pass
///
/// Implements move/copy semantics and destructor insertion.
pub struct OwnershipPass<'hir> {
    /// Tracks variable ownership state across scopes
    pub scope_map: ScopeMap<'hir>,
    /// Collected errors during the pass
    pub errors: Vec<HirError>,
    /// Collected warnings during the pass
    pub warnings: Vec<HirWarning>,
    /// Module signature for checking copy constructors
    pub hir_signature: HirModuleSignature<'hir>,
    /// Arena for allocating new HIR nodes
    hir_arena: &'hir HirArena<'hir>,
    /// Current statement index (for use tracking)
    current_stmt_index: usize,
    /// Current method modifier (None if not in a method, or the modifier if in a method)
    /// Used to detect ownership transfer in borrowing methods
    current_method_context: Option<MethodContext<'hir>>,
    /// Counter for generating unique temporary variable names
    temp_var_counter: usize,
    /// Statements to prepend before the current statement (for temporary extraction)
    pending_statements: Vec<HirStatement<'hir>>,
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
    /// The name of the method
    method_name: &'hir str,
    /// The return type of the method (for detecting recursive copy)
    return_ty: Option<HirTy<'hir>>,
    /// Whether the struct has type parameters (is generic)
    /// For generic structs, we skip some ownership checks for field accesses
    /// because the concrete types are unknown at definition time
    is_generic_struct: bool,
    /// Whether this context is a constructor
    is_constructor: bool,
}

impl<'hir> OwnershipPass<'hir> {
    pub fn new(hir_signature: HirModuleSignature<'hir>, hir_arena: &'hir HirArena<'hir>) -> Self {
        Self {
            scope_map: ScopeMap::new(),
            errors: Vec::new(),
            warnings: Vec::new(),
            hir_signature,
            hir_arena,
            current_stmt_index: 0,
            current_method_context: None,
            temp_var_counter: 0,
            pending_statements: Vec::new(),
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
            self.temp_var_counter = 0;

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
                // Skip methods with unsatisfied constraints (they won't be code generated)
                if !method.signature.is_constraint_satisfied {
                    continue;
                }

                self.scope_map = ScopeMap::new();
                self.current_stmt_index = 0;
                self.temp_var_counter = 0;

                // Set method context for ownership checking
                self.current_method_context = Some(MethodContext {
                    modifier: method.signature.modifier.clone(),
                    method_span: method.signature.span,
                    struct_name: struct_def.signature.name,
                    method_name: method.name,
                    return_ty: Some(method.signature.return_ty.clone()),
                    is_generic_struct,
                    is_constructor: false,
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
                if method.signature.modifier == HirStructMethodModifier::Consuming {
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

            // Process copy constructor (if present)
            if let Some(copy_ctor) = struct_def.move_constructor.as_mut() {
                // Skip copy constructor with unsatisfied constraints (it won't be code generated)
                if !copy_ctor.signature.is_constraint_satisfied {
                    continue;
                }

                self.scope_map = ScopeMap::new();
                self.current_stmt_index = 0;
                self.temp_var_counter = 0;

                // Set method context so checks like recursive-copy detection work
                self.current_method_context = Some(MethodContext {
                    modifier: HirStructMethodModifier::Consuming,
                    method_span: copy_ctor.signature.span,
                    struct_name: struct_def.signature.name,
                    // Use the special name "atlas77__copy_ctor" so existing checks that looked
                    // for method_name == "atlas77__copy_ctor" continue to function.
                    method_name: COPY_CONSTRUCTOR_MANGLED_NAME,
                    // The copy constructor constructs the struct type, so set return_ty
                    // to the struct's named type to allow types_match comparisons.
                    return_ty: Some(HirTy::Named(crate::atlas_c::atlas_hir::ty::HirNamedTy {
                        name: struct_def.signature.name,
                        span: struct_def.name_span,
                    })),
                    is_generic_struct,
                    is_constructor: true,
                });

                // Register constructor parameters
                for param in copy_ctor.params.iter() {
                    let kind = self.classify_type_kind(param.ty);
                    let is_copyable = self.is_type_copyable(param.ty);
                    self.scope_map.insert(
                        param.name,
                        VarData::new(param.name, param.span, kind, param.ty, is_copyable, 0),
                    );
                }

                // First pass: collect uses
                if let Err(e) = self.collect_uses_in_block(&copy_ctor.body) {
                    self.errors.push(e);
                    self.current_method_context = None;
                } else {
                    // Second pass: transform
                    self.current_stmt_index = 0;
                    if let Err(e) = self.transform_block(&mut copy_ctor.body) {
                        self.errors.push(e);
                    }
                    self.current_method_context = None;
                }
            }

            // Process constructor
            {
                self.scope_map = ScopeMap::new();
                self.current_stmt_index = 0;
                self.temp_var_counter = 0;

                // Set a constructor context so the ownership pass can special-case
                // first-time `this.field = ...` initializations.
                self.current_method_context = Some(MethodContext {
                    modifier: HirStructMethodModifier::Consuming,
                    method_span: struct_def.constructor.signature.span,
                    struct_name: struct_def.signature.name,
                    method_name: "__constructor",
                    return_ty: None,
                    is_generic_struct,
                    is_constructor: true,
                });

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

                self.current_method_context = None;
            }

            // Process destructor
            {
                self.scope_map = ScopeMap::new();
                self.current_stmt_index = 0;
                self.temp_var_counter = 0;
                if let Some(destructor) = struct_def.destructor.as_mut() {
                    for param in destructor.params.iter() {
                        let kind = self.classify_type_kind(param.ty);
                        let is_copyable = self.is_type_copyable(param.ty);
                        self.scope_map.insert(
                            param.name,
                            VarData::new(param.name, param.span, kind, param.ty, is_copyable, 0),
                        );
                    }

                    if let Err(e) = self.collect_uses_in_block(&destructor.body) {
                        self.errors.push(e);
                    } else {
                        self.current_stmt_index = 0;
                        if let Err(e) = self.transform_block(&mut destructor.body) {
                            self.errors.push(e);
                        }
                    }
                } else {
                    unreachable!(
                        "Struct destructor missing during ownership pass for struct {}",
                        struct_def.signature.name
                    );
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

    /// Analyze a function call to determine ownership semantics based on parameter constraints
    fn analyze_call_ownership_semantics(
        &self,
        call: &HirFunctionCallExpr<'hir>,
    ) -> CallOwnershipInfo {
        let mut moved_args = Vec::new();
        let mut copied_args = Vec::new();
        let mut taken_args = Vec::new();

        // Helper to process a list of parameters and associated generics
        let process_params = |params: &Vec<
            crate::atlas_c::atlas_hir::signature::HirFunctionParameterSignature<'hir>,
        >,
                              generics: Option<&Vec<&'hir HirGenericConstraint<'hir>>>,
                              moved_args: &mut Vec<usize>,
                              copied_args: &mut Vec<usize>,
                              taken_args: &mut Vec<usize>| {
            for (i, param) in params.iter().enumerate() {
                let constraints = self.get_type_parameter_constraints(param.ty, generics);
                if constraints.iter().any(|c| c == "std::moveable") {
                    moved_args.push(i);
                } else if constraints.iter().any(|c| c == "std::copyable") {
                    copied_args.push(i);
                } else if constraints.iter().any(|c| c == "std::default") {
                    taken_args.push(i);
                }
            }
        };

        // Resolve direct function calls
        if let HirExpr::Ident(ident) = call.callee.as_ref() {
            if let Some(&sig) = self.hir_signature.functions.get(ident.name) {
                process_params(
                    &sig.params,
                    Some(&sig.generics),
                    &mut moved_args,
                    &mut copied_args,
                    &mut taken_args,
                );
            }
        }
        // Static access or type method: Module::fn or Type::fn
        else if let HirExpr::StaticAccess(static_access) = call.callee.as_ref() {
            if let HirTy::Named(named) = static_access.target {
                if let Some(struct_sig) = self.hir_signature.structs.get(named.name) {
                    if let Some(method_sig) = struct_sig.methods.get(static_access.field.name) {
                        process_params(
                            &method_sig.params,
                            method_sig.generics.as_ref().map(|g| g),
                            &mut moved_args,
                            &mut copied_args,
                            &mut taken_args,
                        );
                    }
                }
            }
        }
        // Method call: obj.method
        else if let HirExpr::FieldAccess(field_access) = call.callee.as_ref() {
            let target_ty = field_access.target.ty();
            match target_ty {
                HirTy::Named(n) => {
                    if let Some(struct_sig) = self.hir_signature.structs.get(n.name) {
                        if let Some(method_sig) = struct_sig.methods.get(field_access.field.name) {
                            process_params(
                                &method_sig.params,
                                method_sig.generics.as_ref().map(|g| g),
                                &mut moved_args,
                                &mut copied_args,
                                &mut taken_args,
                            );
                        }
                    }
                }
                HirTy::Generic(g) => {
                    let mangled =
                        MonomorphizationPass::generate_mangled_name(self.hir_arena, g, "struct");
                    if let Some(struct_sig) = self.hir_signature.structs.get(mangled) {
                        if let Some(method_sig) = struct_sig.methods.get(field_access.field.name) {
                            process_params(
                                &method_sig.params,
                                method_sig.generics.as_ref().map(|g| g),
                                &mut moved_args,
                                &mut copied_args,
                                &mut taken_args,
                            );
                        }
                    }
                }
                _ => {}
            }
        }

        CallOwnershipInfo {
            moved_args,
            copied_args,
            taken_args,
        }
    }

    /// Extract constraint names from a type parameter
    fn get_type_parameter_constraints(
        &self,
        param_ty: &HirTy<'hir>,
        func_generics: Option<&Vec<&'hir HirGenericConstraint<'hir>>>,
    ) -> Vec<String> {
        let mut constraints = Vec::new();

        // If parameter is &T or &const T, get T
        let inner_ty: &HirTy = if let Some(inner) = param_ty.get_inner_ref_ty() {
            inner
        } else {
            param_ty
        };

        // If T is a named type that matches a generic parameter, collect its constraints
        if let HirTy::Named(named) = inner_ty {
            if let Some(gens) = func_generics {
                for generic in gens.iter() {
                    if generic.generic_name == named.name {
                        for kind in &generic.kind {
                            match kind {
                                HirGenericConstraintKind::Std { name, .. } => {
                                    constraints.push(format!("std.{}", name));
                                }
                                HirGenericConstraintKind::Concept { name, .. } => {
                                    constraints.push(name.to_string());
                                }
                                _ => {}
                            }
                        }
                        break;
                    }
                }
            }
        }

        constraints
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
            HirStatement::Assign(assign_stmt) => {
                // LHS: don't consume ownership when evaluating LHS (we're writing to it)
                self.collect_uses_in_expr(&assign_stmt.dst, false)?;
                // RHS: ownership-consuming
                self.collect_uses_in_expr(&assign_stmt.val, true)?;
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
            HirStatement::Block(block) => {
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
                    Some(HirUnaryOp::AsRef) => {
                        // Taking a reference borrows, doesn't consume
                        if let HirExpr::Ident(ident) = unary.expr.as_ref() {
                            self.scope_map.mark_as_borrowed(ident.name);
                        }
                        false
                    }
                    Some(HirUnaryOp::Deref) => false,
                    Some(HirUnaryOp::Neg) => false,
                    Some(HirUnaryOp::Not) => false,
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
            // Copy expressions - analyze inner expression
            HirExpr::Copy(cp) => {
                self.collect_uses_in_expr(&cp.expr, true)?;
            }
        }
        Ok(())
    }

    // =========================================================================
    // Phase 2 & 3: COPY-Biased Lowering
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
            HirStatement::Block(block) => {
                // A block always returns if its last statement always returns
                block
                    .statements
                    .last()
                    .is_some_and(|s| Self::statement_always_returns(s))
            }
            _ => false,
        }
    }

    /// Transform a statement, possibly generating multiple statements
    fn transform_stmt(
        &mut self,
        stmt: &mut HirStatement<'hir>,
    ) -> HirResult<Vec<HirStatement<'hir>>> {
        // Clear pending statements from previous transformation
        self.pending_statements.clear();

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

                // Preserve the uses array from the collection phase
                // For origin, compute it based on the value expression
                let existing_uses = self
                    .scope_map
                    .get(var_stmt.name)
                    .map(|v| v.uses.clone())
                    .unwrap_or_default();

                // Compute origin - this applies to both reference types AND objects that capture references
                let origin = if kind == VarKind::Reference {
                    // For reference types, track what they point to
                    self.compute_reference_origin(&var_stmt.value)
                } else if kind == VarKind::Object {
                    // For objects created via constructors, check if any constructor argument is a reference
                    // If so, the object's lifetime is tied to those references
                    self.compute_object_lifetime_dependencies(&var_stmt.value)
                } else {
                    ReferenceOrigin::None
                };

                let mut new_var_data = VarData::with_origin(
                    var_stmt.name,
                    var_stmt.span,
                    kind,
                    ty,
                    is_copyable,
                    self.current_stmt_index,
                    origin,
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

                // Prepend any pending statements (temporary extractions)
                let mut result = self.pending_statements.clone();
                result.push(new_stmt);
                Ok(result)
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

                // Prepend any pending statements (temporary extractions)
                let mut result = self.pending_statements.clone();
                result.push(new_stmt);
                Ok(result)
            }
            HirStatement::Assign(assign_stmt) => {
                // Let helper handle special assignment deletion/temporaries
                if let Some(stmts) = self.handle_assignment_with_deletes(assign_stmt)? {
                    return Ok(stmts);
                }

                // Default handling: transform RHS (consuming) and LHS (non-consuming)
                let transformed_val = self.transform_expr_ownership(&assign_stmt.val, true)?;
                let transformed_dst = self.transform_expr_ownership(&assign_stmt.dst, false)?;

                let new_assign = HirStatement::Assign(HirAssignStmt {
                    span: assign_stmt.span,
                    dst: transformed_dst,
                    val: transformed_val,
                    ty: assign_stmt.ty,
                });

                // Prepend pending temporaries
                let mut result = self.pending_statements.clone();
                result.push(new_assign);

                // If LHS is a simple ident, mark it Owned now
                if let HirExpr::Ident(ident) = &assign_stmt.dst
                    && let Some(var) = self.scope_map.get_mut(ident.name)
                {
                    var.status = VarStatus::Owned;
                }

                Ok(result)
            }
            HirStatement::Expr(expr_stmt) => {
                let transformed = self.transform_expr_ownership(&expr_stmt.expr, false)?;
                let new_stmt = HirStatement::Expr(HirExprStmt {
                    span: expr_stmt.span,
                    expr: transformed,
                });
                // Prepend any pending statements (temporary extractions)
                let mut result = self.pending_statements.clone();
                result.push(new_stmt);
                Ok(result)
            }
            HirStatement::Return(ret) => {
                // Transform the return value and mark returned variables
                let transformed = self.transform_expr_for_return(&ret.value, ret.span)?;

                // Collect variables used in return expression
                let mut vars_used_in_return = Vec::new();
                Self::collect_vars_used_in_expr(&ret.value, &mut vars_used_in_return);

                let mut result = Vec::new();

                // Generate delete statements for owned/moved-from vars
                // EXCEPT those used in return expression (they're being returned)
                // Collect variable names to avoid borrowing issues
                let owned_var_names: Vec<_> = self
                    .scope_map
                    .get_all_owned_vars()
                    .iter()
                    .filter(|var| {
                        // Skip variables declared after this return statement
                        if var.declaration_stmt_index > self.current_stmt_index {
                            return false;
                        }
                        // Skip variables used in return expression
                        if vars_used_in_return.contains(&var.name) {
                            return false;
                        }
                        // Skip primitives and references
                        if var.kind == VarKind::Primitive || var.kind == VarKind::Reference {
                            return false;
                        }
                        // Skip unions
                        if self.type_is_union(var.ty) {
                            return false;
                        }
                        // Only include if status says we should delete
                        var.status.should_delete_at_scope_end()
                    })
                    .map(|var| (var.name, var.ty))
                    .collect();

                for (var_name, var_ty) in owned_var_names {
                    result.push(Self::create_delete_stmt(var_name, var_ty, ret.span));
                    self.scope_map.mark_as_deleted(var_name, ret.span);
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
                // Extract temporaries from the condition first
                let condition_with_temps =
                    self.extract_temporaries_from_expr(if_else.condition.clone())?;
                let transformed_condition =
                    self.transform_expr_ownership(&condition_with_temps, false)?;

                // Save the pending statements (they'll be cleared when we transform the branches)
                let condition_pending_statements = self.pending_statements.clone();

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

                for (var_name, moved_in_then, var_data) in &conditionally_moved {
                    // Skip primitives, references, and unions
                    if var_data.kind == VarKind::Primitive || var_data.kind == VarKind::Reference {
                        continue;
                    }
                    if self.type_is_union(var_data.ty) {
                        continue;
                    }

                    let delete_stmt = Self::create_delete_stmt(var_name, var_data.ty, if_else.span);

                    if *moved_in_then {
                        // MovedFrom in then branch, so insert delete at end of else branch
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
                        // MovedFrom in else branch, so insert delete at end of then branch
                        let insert_pos =
                            Self::find_delete_insert_position(&final_then_branch.statements);
                        final_then_branch.statements.insert(insert_pos, delete_stmt);
                    }
                }

                // Merge the branch states to update the current scope map
                self.scope_map = pre_if_state;
                self.scope_map
                    .merge_branch_states(&post_then_state, &post_else_state);

                // Prepend any pending statements (temporary extractions from condition)
                let mut result = condition_pending_statements;
                result.push(HirStatement::IfElse(
                    crate::atlas_c::atlas_hir::stmt::HirIfElseStmt {
                        span: if_else.span,
                        condition: transformed_condition,
                        then_branch: final_then_branch,
                        else_branch: final_else_branch,
                    },
                ));
                Ok(result)
            }
            HirStatement::While(while_stmt) => {
                // Extract temporaries from the condition first
                let condition_with_temps =
                    self.extract_temporaries_from_expr(while_stmt.condition.clone())?;
                let transformed_condition =
                    self.transform_expr_ownership(&condition_with_temps, false)?;

                // Save the pending statements (they'll be cleared when we transform the loop body)
                let condition_pending_statements = self.pending_statements.clone();

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
                let mut deleted_in_loop = Vec::new();
                for scope in &pre_loop_state.scopes {
                    for (name, var_data) in &scope.var_status {
                        let post_var = post_loop_state.get(name);
                        if let Some(post_v) = post_var {
                            let was_owned = matches!(var_data.status, VarStatus::Owned);
                            let is_moved = matches!(post_v.status, VarStatus::MovedFrom { .. });
                            let is_deleted = matches!(post_v.status, VarStatus::Deleted { .. });

                            if was_owned
                                && is_moved
                                && let VarStatus::MovedFrom { move_span } = &post_v.status
                            {
                                moved_in_loop.push((*name, *move_span));
                            }
                            if was_owned
                                && is_deleted
                                && let VarStatus::Deleted { delete_span } = &post_v.status
                            {
                                deleted_in_loop.push((*name, *delete_span));
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
                // Mark deleted variables as ConditionallyDeleted
                for (var_name, delete_span) in &deleted_in_loop {
                    if let Some(var) = self.scope_map.get_mut(var_name) {
                        // TODO: Add a ConditionallyDeleted status
                        var.status = VarStatus::ConditionallyMoved {
                            move_span: *delete_span,
                        };
                    }
                }

                // If any variables were moved inside the loop, that's an error
                // because the loop could run multiple times, causing use-after-move
                if let Some((var_name, move_span)) = moved_in_loop.first() {
                    // Emit a warning instead of an error: moving a variable inside a loop
                    // could be UB if loop executes multiple times. Marked as ConditionallyMoved above.
                    let path = move_span.path;
                    let src = utils::get_file_content(path).unwrap_or_default();
                    let warn = HirWarning::MoveInLoop(MoveInLoopWarning {
                        src: NamedSource::new(path, src),
                        move_span: *move_span,
                        loop_span: Span {
                            start: while_stmt.span.start,
                            end: while_stmt.condition.span().end,
                            path: while_stmt.span.path,
                        },
                        var_name: var_name.to_string(),
                    });
                    // Record and print warning
                    self.warnings.push(warn);
                }

                // If any variables were deleted inside the loop, that's an error
                // because the loop could run multiple times, causing use-after-free
                if let Some((var_name, delete_span)) = deleted_in_loop.first() {
                    // Emit a warning instead of an error: deleting a variable inside a loop
                    // could be UB if loop executes multiple times. We already marked it conditionally.
                    let path = delete_span.path;
                    let src = utils::get_file_content(path).unwrap_or_default();
                    let warn = HirWarning::MoveInLoop(MoveInLoopWarning {
                        src: NamedSource::new(path, src),
                        move_span: *delete_span,
                        loop_span: Span {
                            start: while_stmt.span.start,
                            end: while_stmt.condition.span().end,
                            path: while_stmt.span.path,
                        },
                        var_name: var_name.to_string(),
                    });
                    self.warnings.push(warn);
                }

                // Prepend any pending statements (temporary extractions from condition)
                let mut result = condition_pending_statements;
                result.push(HirStatement::While(
                    crate::atlas_c::atlas_hir::stmt::HirWhileStmt {
                        span: while_stmt.span,
                        condition: transformed_condition,
                        body: while_stmt.body.clone(),
                    },
                ));
                Ok(result)
            }
            HirStatement::Break(span) => Ok(vec![HirStatement::Break(*span)]),
            HirStatement::Continue(span) => Ok(vec![HirStatement::Continue(*span)]),
            HirStatement::Block(block) => {
                self.scope_map.new_scope();
                self.transform_block(block)?;
                self.scope_map.end_scope();
                Ok(vec![HirStatement::Block(block.clone())])
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

            HirExpr::Call(call) => {
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

                        HirExpr::FieldAccess(HirFieldAccessExpr {
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

                // Use ownership analysis to determine per-argument semantics
                let call_info = self.analyze_call_ownership_semantics(call);

                let mut transformed_args = Vec::new();
                for (i, arg) in call.args.iter().enumerate() {
                    if call_info.moved_args.contains(&i) {
                        transformed_args.push(self.transform_moving_argument(arg)?);
                    } else if call_info.copied_args.contains(&i) {
                        transformed_args.push(self.transform_copying_argument(arg)?);
                    } else if call_info.taken_args.contains(&i) {
                        transformed_args.push(self.transform_taking_argument(arg)?);
                    } else {
                        // Default: infer from parameter type (references don't consume)
                        let is_ref_param = call.args_ty.get(i).is_some_and(|ty| {
                            matches!(ty, HirTy::ReadOnlyReference(_) | HirTy::MutableReference(_))
                        });
                        let consumes_ownership = !is_ref_param;
                        transformed_args
                            .push(self.transform_expr_ownership(arg, consumes_ownership)?);
                    }
                }

                Ok(HirExpr::Call(HirFunctionCallExpr {
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

                Ok(HirExpr::HirBinaryOperation(HirBinaryOpExpr {
                    span: binop.span,
                    op: binop.op.clone(),
                    op_span: binop.op_span,
                    lhs: Box::new(transformed_lhs),
                    rhs: Box::new(transformed_rhs),
                    ty: binop.ty,
                }))
            }
            HirExpr::Unary(unary) => {
                // For unary expressions, determine if inner should consume ownership
                let inner_consumes = match &unary.op {
                    // Reference operators don't consume ownership (borrow)
                    Some(HirUnaryOp::AsRef) => false,
                    Some(HirUnaryOp::Deref) => false,
                    // Neg and Not are typically for primitives - don't consume
                    Some(HirUnaryOp::Neg) => false,
                    Some(HirUnaryOp::Not) => false,
                    // No operator means this is just a wrapper - propagate the consuming flag
                    None => is_ownership_consuming,
                };

                let transformed_inner =
                    self.transform_expr_ownership(&unary.expr, inner_consumes)?;

                let unary_expr = HirExpr::Unary(UnaryOpExpr {
                    span: unary.span,
                    op: unary.op.clone(),
                    expr: Box::new(transformed_inner),
                    ty: unary.ty,
                });

                // Special case: Deref in an ownership-consuming context with a copyable result type
                // needs to produce a copy of the object, not just read the pointer.
                // This handles cases like `*this.field` in _copy methods where field is an object.
                if is_ownership_consuming && matches!(unary.op, Some(HirUnaryOp::Deref)) {
                    let is_copyable = self.is_type_copyable(unary.ty);

                    // If we're dereferencing in an ownership-consuming context and the type is not copyable,
                    // this is an error - you can't move out of a reference
                    if !is_copyable {
                        let path = unary.span.path;
                        let src = utils::get_file_content(path).unwrap_or_default();
                        return Err(HirError::CannotMoveOutOfReference(
                            CannotMoveOutOfReferenceError {
                                span: unary.span,
                                ty_name: Self::get_type_name(unary.ty),
                                src: NamedSource::new(path, src),
                            },
                        ));
                    }

                    if is_copyable && !Self::is_primitive_type(unary.ty) {
                        // Check for recursive copy in copy constructors
                        // When dereferencing in a copy constructor, we're copying the dereferenced value
                        if let Some(method_ctx) = &self.current_method_context
                            && method_ctx.method_name == COPY_CONSTRUCTOR_MANGLED_NAME
                            && let Some(return_ty) = &method_ctx.return_ty
                            // Check if the dereferenced type matches the type we're constructing
                            && self.types_match(unary.ty, return_ty)
                        {
                            let path = unary.span.path;
                            let src = utils::get_file_content(path).unwrap_or_default();
                            return Err(HirError::RecursiveCopyConstructor(
                                RecursiveCopyConstructorError {
                                    copy_span: unary.span,
                                    method_span: method_ctx.method_span,
                                    type_name: Self::get_type_name(unary.ty),
                                    src: NamedSource::new(path, src),
                                },
                            ));
                        }

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
                    transformed_args.push(self.transform_expr_ownership(arg, true)?);
                }

                Ok(HirExpr::NewObj(HirNewObjExpr {
                    span: new_obj.span,
                    ty: new_obj.ty,
                    args: transformed_args,
                    args_ty: new_obj.args_ty.clone(),
                    is_copy_constructor_call: new_obj.is_copy_constructor_call,
                }))
            }
            HirExpr::ObjLiteral(obj_lit) => {
                let mut transformed_fields = Vec::new();
                for field in obj_lit.fields.iter() {
                    let transformed_value = self.transform_expr_ownership(&field.value, true)?;
                    transformed_fields.push(HirFieldInit {
                        span: field.span,
                        name: field.name,
                        name_span: field.name_span,
                        ty: field.ty,
                        value: Box::new(transformed_value),
                    });
                }

                Ok(HirExpr::ObjLiteral(HirObjLiteralExpr {
                    span: obj_lit.span,
                    ty: obj_lit.ty,
                    fields: transformed_fields,
                }))
            }
            HirExpr::ListLiteral(list) => {
                let mut transformed_items = Vec::new();
                for item in list.items.iter() {
                    transformed_items.push(self.transform_expr_ownership(item, true)?);
                }

                Ok(HirExpr::ListLiteral(HirListLiteralExpr {
                    span: list.span,
                    items: transformed_items,
                    ty: list.ty,
                }))
            }
            HirExpr::FieldAccess(field) => {
                // Check if the target expression creates a temporary value that needs to be freed
                // This catches method chaining like: foo.bar().baz() where bar() returns a temporary
                // Instead of warning, we extract the temporary into a variable
                let transformed_target =
                    if Self::should_warn_about_temporary_in_method_chain(&field.target) {
                        // Extract the temporary into a variable
                        let (temp_name, let_stmt) =
                            self.extract_temporary(*field.target.clone(), field.target.span())?;
                        self.pending_statements.push(let_stmt);

                        // Use the temporary variable instead
                        HirExpr::Ident(HirIdentExpr {
                            span: field.target.span(),
                            name: temp_name,
                            ty: field.target.ty(),
                        })
                    } else {
                        self.transform_expr_ownership(&field.target, false)?
                    };

                let field_access_expr = HirExpr::FieldAccess(HirFieldAccessExpr {
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
                        // Non-copyable field being consumed: mark the root as moved (if identifiable)
                        // and return the field access expression (no Move node).
                        if let HirExpr::FieldAccess(fa) = &field_access_expr {
                            if let HirExpr::Ident(ident) = fa.target.as_ref() {
                                // Mark the root variable as moved-from; the field value is being taken.
                                self.scope_map.mark_as_moved(ident.name, field.span);
                            }
                        }

                        Ok(field_access_expr)
                    }
                } else {
                    Ok(field_access_expr)
                }
            }
            HirExpr::Indexing(indexing) => {
                let transformed_target = self.transform_expr_ownership(&indexing.target, false)?;
                let transformed_index = self.transform_expr_ownership(&indexing.index, false)?;
                let indexing_expr = HirExpr::Indexing(HirIndexingExpr {
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
                // Instead of warning, we extract the temporary into a variable
                let transformed = if Self::should_warn_about_temporary_in_cast(&cast.expr) {
                    // Extract the temporary into a variable
                    let (temp_name, let_stmt) =
                        self.extract_temporary(*cast.expr.clone(), cast.expr.span())?;
                    self.pending_statements.push(let_stmt);

                    // Use the temporary variable instead
                    HirExpr::Ident(HirIdentExpr {
                        span: cast.expr.span(),
                        name: temp_name,
                        ty: cast.expr.ty(),
                    })
                } else {
                    self.transform_expr_ownership(&cast.expr, is_ownership_consuming)?
                };

                Ok(HirExpr::Casting(HirCastExpr {
                    span: cast.span,
                    expr: Box::new(transformed),
                    ty: cast.ty,
                }))
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
                    // If the variable is already deleted or moved, emit an error
                    //self.check_variable_valid(ident)?;

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
            // Explicit move wrapper removed: ownership pass no longer creates Move nodes
            // Explicit copy expression from user code: copy<>(x)
            //
            // NB: This is not yet implemented in the parser, so users can't write it directly
            // Though I do plan to add a copy expression in the future for explicit copies
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

    /// Transform an argument that should be moved into the callee.
    fn transform_moving_argument(&mut self, arg: &HirExpr<'hir>) -> HirResult<HirExpr<'hir>> {
        // Argument must be an lvalue (ident, field access, deref, index)

        if let HirExpr::Ident(ident) = arg {
            // Get variable data
            if let Some(var_data) = self.scope_map.get(ident.name) {
                // Warn if already moved-from
                if var_data.status.is_moved_from() {
                    if let VarStatus::MovedFrom { move_span }
                    | VarStatus::ConditionallyMoved { move_span } = &var_data.status
                    {
                        let move_span = *move_span;
                        self.emit_use_after_move_warning(ident, move_span);
                    }
                } else {
                    // Error if deleted
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
                }
            }

            // Mark as moved-from (will still be deleted at scope end)
            self.scope_map.mark_as_moved(ident.name, ident.span);

            // Return the ident as-is - no Move node needed
            Ok(HirExpr::Ident(ident.clone()))
        } else if matches!(
            arg,
            HirExpr::FieldAccess(_) | HirExpr::Indexing(_) | HirExpr::Unary(_)
        ) {
            // Can move from fields, array elements, derefs
            // Transform normally (non-consuming since we're just reading the lvalue)
            let transformed = self.transform_expr_ownership(arg, false)?;

            // For complex lvalues, we can't mark MovedFrom, but that's okay
            // The move constructor handles the actual data transfer
            Ok(transformed)
        } else {
            // Cannot move from rvalue (temporary values)
            let path = arg.span().path;
            let src = utils::get_file_content(path).unwrap_or_default();
            Err(HirError::CannotMoveFromRvalue(CannotMoveFromRvalueError {
                span: arg.span(),
                src: NamedSource::new(path, src),
                hint: "std::moveable parameters require an lvalue (variable, field, etc.)"
                    .to_string(),
            }))
        }
    }

    /// Transform an argument that should be copied for the callee.
    fn transform_copying_argument(&mut self, arg: &HirExpr<'hir>) -> HirResult<HirExpr<'hir>> {
        let ty = arg.ty();

        // Check if type is copyable
        if !self.is_type_copyable(ty) {
            let path = arg.span().path;
            let src = utils::get_file_content(path).unwrap_or_default();
            return Err(HirError::TypeNotCopyable(TypeNotCopyableError {
                span: arg.span(),
                type_name: Self::get_type_name(ty),
                src: NamedSource::new(path, src),
            }));
        }

        // Transform inner expression (non-consuming - we're copying)
        let transformed = self.transform_expr_ownership(arg, false)?;

        // Check for recursive copy in copy constructors
        if let Some(method_ctx) = &self.current_method_context {
            if method_ctx.method_name == COPY_CONSTRUCTOR_MANGLED_NAME {
                if let Some(return_ty) = &method_ctx.return_ty {
                    if self.types_match(ty, return_ty) {
                        let path = arg.span().path;
                        let src = utils::get_file_content(path).unwrap_or_default();
                        return Err(HirError::RecursiveCopyConstructor(
                            RecursiveCopyConstructorError {
                                copy_span: arg.span(),
                                method_span: method_ctx.method_span,
                                type_name: Self::get_type_name(ty),
                                src: NamedSource::new(path, src),
                            },
                        ));
                    }
                }
            }
        }

        // Primitives don't need Copy wrapper (bitwise copy is implicit)
        if Self::is_primitive_type(ty) {
            return Ok(transformed);
        }

        // Wrap in Copy expression
        Ok(HirExpr::Copy(HirCopyExpr {
            span: arg.span(),
            source_name: "<copied>",
            expr: Box::new(transformed),
            ty,
        }))
    }

    /// Transform an argument that is "taken" by the callee (consumed by the callee's default)
    fn transform_taking_argument(&mut self, arg: &HirExpr<'hir>) -> HirResult<HirExpr<'hir>> {
        // For now, treat this like move (mark as moved-from)
        // The std::take function itself handles replacing with default

        if let HirExpr::Ident(ident) = arg {
            // Mark as moved-from
            self.scope_map.mark_as_moved(ident.name, ident.span);

            // Return as-is
            Ok(HirExpr::Ident(ident.clone()))
        } else {
            // For complex lvalues, transform normally
            self.transform_expr_ownership(arg, false)
        }
    }

    /// Transform an identifier in an ownership-consuming context
    ///
    /// Applies COPY-biased lowering:
    /// - If copyable: emit COPY
    /// - If not copyable: require explicit move (error on implicit move)
    fn transform_ownership_consuming_ident(
        &mut self,
        ident: &HirIdentExpr<'hir>,
    ) -> HirResult<HirExpr<'hir>> {
        // 1. Get variable data
        let var_data = match self.scope_map.get(ident.name) {
            Some(data) => data.clone(),
            None => return Ok(HirExpr::Ident(ident.clone())),
        };

        // 2. Check for use-after-delete (still a compile error)
        if let VarStatus::Deleted { delete_span } = &var_data.status {
            let path = ident.span.path;
            let src = utils::get_file_content(path).unwrap_or_default();

            if let ReferenceOrigin::Variable(origin_name) = &var_data.origin {
                return Err(HirError::LifetimeDependencyViolation(
                    LifetimeDependencyViolationError {
                        value_name: ident.name.to_string(),
                        origin_name: origin_name.to_string(),
                        origin_invalidation_span: *delete_span,
                        access_span: ident.span,
                        src: NamedSource::new(path, src),
                    },
                ));
            }

            return Err(HirError::TryingToAccessADeletedValue(
                TryingToAccessADeletedValueError {
                    delete_span: *delete_span,
                    access_span: ident.span,
                    src: NamedSource::new(path, src),
                },
            ));
        }

        // 3. NEW: Warn on use-after-move (not error, just UB warning)
        if var_data.status.is_moved_from() {
            if let VarStatus::MovedFrom { move_span }
            | VarStatus::ConditionallyMoved { move_span } = &var_data.status
            {
                self.emit_use_after_move_warning(ident, *move_span);
            }
            // Continue compilation - moved-from is valid but undefined behavior
        }

        // 4. If this variable is marked as Returned, treat it as moved for the return
        //    (do not generate a `Copy` even if the type is copyable).
        if matches!(var_data.status, VarStatus::Returned { .. }) {
            return Ok(HirExpr::Ident(ident.clone()));
        }

        // 5. References don't transfer ownership
        if var_data.kind == VarKind::Reference {
            return Ok(HirExpr::Ident(ident.clone()));
        }

        // 6. Primitives are always bitwise copied
        if var_data.kind == VarKind::Primitive {
            return Ok(HirExpr::Ident(ident.clone()));
        }

        // 6. NEW: C++-like semantics for objects
        if var_data.is_copyable {
            // Copyable: generate COPY (no last-use optimization)
            Ok(HirExpr::Copy(HirCopyExpr {
                span: ident.span,
                source_name: ident.name,
                expr: Box::new(HirExpr::Ident(ident.clone())),
                ty: ident.ty,
            }))
        } else {
            // Non-copyable: ERROR - user must use std::move explicitly
            let path = ident.span.path;
            let src = utils::get_file_content(path).unwrap_or_default();
            Err(HirError::CannotImplicitlyCopyNonCopyableValue(
                CannotImplicitlyCopyNonCopyableValueError {
                    var_name: ident.name.to_string(),
                    span: ident.span,
                    ty_name: Self::get_type_name(ident.ty),
                    src: NamedSource::new(path, src),
                },
            ))
        }
    }

    /// Transform an expression for return context (always moves) and mark returned variables
    ///
    /// TODO: if the return expression is a function/method call or constructor, we can optimize
    /// away the copy by marking the argument as "returned" in the callee instead of copying here.
    fn transform_expr_for_return(
        &mut self,
        expr: &HirExpr<'hir>,
        return_span: Span,
    ) -> HirResult<HirExpr<'hir>> {
        // If returning a simple variable (or a no-op unary wrapping an ident), mark it Returned
        // Marking before transformation ensures the ownership transform treats this as a move
        // (no implicit `Copy`) and allows returning non-copyable objects without explicit `std::move`.
        match expr {
            HirExpr::Ident(ident) => {
                if let Some(var) = self.scope_map.get_mut(ident.name) {
                    var.status = VarStatus::Returned { return_span };
                }
            }
            HirExpr::Unary(unary) if unary.op.is_none() => {
                if let HirExpr::Ident(ident) = unary.expr.as_ref() {
                    if let Some(var) = self.scope_map.get_mut(ident.name) {
                        var.status = VarStatus::Returned { return_span };
                    }
                }
            }
            _ => {}
        }

        // Now transform as ownership-consuming. Because returned variables are already marked
        // as `Returned`, `transform_ownership_consuming_ident` will avoid generating `Copy`.
        let transformed = self.transform_expr_ownership(expr, true)?;

        Ok(transformed)
    }

    /// NEW: Emit warning for use-after-move (moved-from access)
    fn emit_use_after_move_warning(&mut self, ident: &HirIdentExpr<'hir>, move_span: Span) {
        let path = ident.span.path;
        let src = utils::get_file_content(path).unwrap_or_default();
        let warning: ErrReport = HirWarning::UseAfterMove(UseAfterMoveWarning {
            src: NamedSource::new(path, src),
            access_span: ident.span,
            move_span,
            var_name: ident.name.to_string(),
        })
        .into();
        // For now keep parity with other passes: print the diagnostic report to stderr
        eprintln!("{:?}", warning);
    }

    // =========================================================================
    // Phase 4: Destructor Insertion
    // =========================================================================

    /// Generate delete statements for all owned variables in current scope
    fn generate_scope_destructors(&self, scope_end_span: Span) -> Vec<HirStatement<'hir>> {
        let mut deletes = Vec::new();
        // Iterate all variables declared in the current scope and delete those
        // whose status indicates they should be deleted at scope end.
        if let Some(scope) = self.scope_map.scopes.last() {
            for (_name, var) in scope.var_status.iter() {
                // Skip references and primitives
                if var.kind == VarKind::Reference || var.kind == VarKind::Primitive {
                    continue;
                }
                // Skip unions - user-managed
                if self.type_is_union(var.ty) {
                    continue;
                }
                if var.status.should_delete_at_scope_end() {
                    deletes.push(Self::create_delete_stmt(var.name, var.ty, scope_end_span));
                }
            }
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

    /// Create a delete statement for a general lvalue expression (field, indexing, deref, ident)
    fn create_delete_for_expr(expr: &HirExpr<'hir>, span: Span) -> HirStatement<'hir> {
        HirStatement::Expr(HirExprStmt {
            span,
            expr: HirExpr::Delete(HirDeleteExpr {
                span,
                expr: Box::new(expr.clone()),
            }),
        })
    }

    /// Helper to check whether a type is a union (user-managed).
    fn type_is_union(&self, ty: &HirTy<'hir>) -> bool {
        match ty {
            HirTy::Named(named) => self.hir_signature.unions.contains_key(named.name),
            HirTy::Generic(g) => {
                let mangled =
                    MonomorphizationPass::generate_mangled_name(self.hir_arena, g, "union");
                self.hir_signature.unions.contains_key(mangled)
            }
            _ => false,
        }
    }

    /// Handle assignment expressions and insert delete statements when needed.
    /// Returns `Ok(Some(stmts))` when the assignment was handled (statements
    /// already produced), or `Ok(None)` to fall back to normal handling.
    fn handle_assignment_with_deletes(
        &mut self,
        assign_stmt: &HirAssignStmt<'hir>,
    ) -> HirResult<Option<Vec<HirStatement<'hir>>>> {
        // Decide whether this LHS needs special handling. Use precise check:
        // - The LHS type must need memory management, and
        // - The root variable(s) (if any) must be owned (so deleting makes sense)
        let lhs_ty = assign_stmt.dst.ty();
        // Skip unions entirely - they're user-managed and must not be auto-deleted
        if self.type_is_union(lhs_ty) {
            return Ok(None);
        }
        if !Self::type_needs_memory_management(lhs_ty) {
            return Ok(None);
        }

        // Collect root idents from the LHS (e.g., for `this.field` -> root is `this` . `*var` -> root is `var`)
        let mut roots: Vec<&str> = Vec::new();
        fn collect_roots<'a>(expr: &'a HirExpr<'a>, out: &mut Vec<&'a str>) {
            match expr {
                HirExpr::Ident(ident) => out.push(ident.name),
                HirExpr::ThisLiteral(_) => out.push("this"),
                HirExpr::Unary(unary) => collect_roots(&unary.expr, out),
                HirExpr::FieldAccess(field) => collect_roots(&field.target, out),
                HirExpr::Indexing(idx) => collect_roots(&idx.target, out),
                HirExpr::Copy(cp) => collect_roots(&cp.expr, out),
                _ => {}
            }
        }
        collect_roots(&assign_stmt.dst, &mut roots);

        // Ensure at least one root (if any roots exist) is currently owned; if there are no roots
        // (e.g., assignment into a temporary expression) we still proceed because the LHS type
        // itself needs management.
        let mut has_owned_root = false;
        for r in &roots {
            if let Some(v) = self.scope_map.get(r)
                && v.status == VarStatus::Owned
            {
                has_owned_root = true;
                break;
            }
        }
        if !roots.is_empty() && !has_owned_root {
            return Ok(None);
        }

        // Special-case: when inside a constructor, do not emit deletes for assignments
        // whose root is `this` (initial field initialization). Constructors initialize
        // fields; deleting a previous value for the initial assignment is incorrect.
        if let Some(ctx) = &self.current_method_context
            && ctx.is_constructor
            && roots.contains(&"this")
        {
            return Ok(None);
        }

        // Collect vars used in RHS to see if it references any root of the LHS
        let mut used = Vec::new();
        Self::collect_vars_used_in_expr(&assign_stmt.val, &mut used);

        // Transform RHS first (it may consume ownership)
        let transformed_rhs = self.transform_expr_ownership(&assign_stmt.val, true)?;

        // Start with any pending temporaries
        let mut stmts: Vec<HirStatement<'hir>> = Vec::new();
        let mut pending = self.pending_statements.clone();
        stmts.append(&mut pending);

        let rhs_uses_lhs_root = roots.iter().any(|r| used.contains(r));

        if rhs_uses_lhs_root {
            // RHS uses the LHS -> evaluate RHS into a temporary
            let tmp_name = format!("__assign_tmp_{}", self.temp_var_counter);
            self.temp_var_counter += 1;
            let tmp_name_intern = self.hir_arena.names().get(&tmp_name);

            // Insert temp variable into scope map
            let tmp_ty = transformed_rhs.ty();
            let tmp_kind = self.classify_type_kind(tmp_ty);
            let tmp_is_copyable = self.is_type_copyable(tmp_ty);
            let tmp_var = VarData::new(
                tmp_name_intern,
                assign_stmt.span,
                tmp_kind,
                tmp_ty,
                tmp_is_copyable,
                self.current_stmt_index,
            );
            self.scope_map.insert(tmp_name_intern, tmp_var);

            // let __tmp = <transformed_rhs>;
            stmts.push(HirStatement::Const(HirVariableStmt {
                span: assign_stmt.span,
                name: tmp_name_intern,
                name_span: assign_stmt.span,
                ty: tmp_ty,
                ty_span: Some(assign_stmt.span),
                value: transformed_rhs,
            }));

            // delete lhs (use expr-based delete so we can delete fields/indexes)
            stmts.push(Self::create_delete_for_expr(
                &assign_stmt.dst,
                assign_stmt.span,
            ));

            // dst = __tmp
            let assign_after = HirStatement::Assign(HirAssignStmt {
                span: assign_stmt.span,
                dst: assign_stmt.dst.clone(),
                val: HirExpr::Ident(HirIdentExpr {
                    span: assign_stmt.span,
                    name: tmp_name_intern,
                    ty: tmp_ty,
                }),
                ty: assign_stmt.ty,
            });
            stmts.push(assign_after);

            // After assignment, mark all root idents of the LHS as Owned again
            for r in roots.iter() {
                if let Some(v) = self.scope_map.get_mut(r) {
                    v.status = VarStatus::Owned;
                }
            }

            Ok(Some(stmts))
        } else {
            // RHS does not reference LHS -> safe to delete old value first
            let mut result = self.pending_statements.clone();
            // delete old value (use expr-based delete so we can delete fields/indexes)
            result.push(Self::create_delete_for_expr(
                &assign_stmt.dst,
                assign_stmt.span,
            ));

            // Now create the transformed assignment using transformed_rhs
            let new_assign = HirStatement::Assign(HirAssignStmt {
                span: assign_stmt.span,
                dst: assign_stmt.dst.clone(),
                val: transformed_rhs,
                ty: assign_stmt.ty,
            });
            result.push(new_assign);

            // Re-establish ownership on LHS roots (assignment gives root variables ownership)
            for r in roots.iter() {
                if let Some(v) = self.scope_map.get_mut(r) {
                    v.status = VarStatus::Owned;
                }
            }

            Ok(Some(result))
        }
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
                VarStatus::MovedFrom { move_span } => {
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
                    // If the variable has a known origin, use the more descriptive lifetime error
                    if let ReferenceOrigin::Variable(origin_name) = &var_data.origin {
                        return Err(HirError::LifetimeDependencyViolation(
                            LifetimeDependencyViolationError {
                                value_name: ident.name.to_string(),
                                origin_name: origin_name.to_string(),
                                origin_invalidation_span: *delete_span,
                                access_span: ident.span,
                                src: NamedSource::new(path, src),
                            },
                        ));
                    }

                    // Fall back to generic deleted value error
                    return Err(HirError::TryingToAccessADeletedValue(
                        TryingToAccessADeletedValueError {
                            delete_span: *delete_span,
                            access_span: ident.span,
                            src: NamedSource::new(path, src),
                        },
                    ));
                }
                VarStatus::Returned { return_span } => {
                    let path = ident.span.path;
                    let src = utils::get_file_content(path).unwrap_or_default();
                    return Err(HirError::TryingToAccessAMovedValue(
                        TryingToAccessAMovedValueError {
                            move_span: *return_span,
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
    fn _is_indexing_expr(expr: &HirExpr<'hir>) -> bool {
        match expr {
            HirExpr::Indexing(_) => true,
            // Unwrap unary wrappers (parser sometimes wraps expressions)
            HirExpr::Unary(unary) if unary.op.is_none() => Self::_is_indexing_expr(&unary.expr),
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

    /// Conservative check whether a type is copyable (bitwise copy)
    fn is_type_copyable(&self, ty: &HirTy<'hir>) -> bool {
        match ty {
            HirTy::Boolean(_)
            | HirTy::Int64(_)
            | HirTy::Float64(_)
            | HirTy::Char(_)
            | HirTy::UInt64(_)
            | HirTy::Unit(_)
            | HirTy::String(_) => true,
            HirTy::Named(n) => {
                if let Some(struct_sig) = self.hir_signature.structs.get(n.name) {
                    return struct_sig.copy_constructor.is_some();
                } else {
                    //potentially a union
                    if self.hir_signature.unions.contains_key(n.name) {
                        return true;
                    }
                }
                false
            }
            HirTy::Generic(g) => {
                let mangled =
                    MonomorphizationPass::generate_mangled_name(self.hir_arena, g, "struct");
                if let Some(struct_sig) = self.hir_signature.structs.get(mangled) {
                    return struct_sig.copy_constructor.is_some();
                } else {
                    let mangled =
                        MonomorphizationPass::generate_mangled_name(self.hir_arena, g, "union");
                    //potentially a union
                    if self.hir_signature.unions.contains_key(mangled) {
                        return true;
                    }
                }
                false
            }
            // Lists and objects are not considered copyable by default here
            _ => false,
        }
    }

    /// Helper: is this a primitive type
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
                return method_sig.modifier == HirStructMethodModifier::Consuming;
            }
        }
        false
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
                    Some(HirUnaryOp::AsRef) => {
                        // The reference points into the inner expression
                        self.get_origin_from_lvalue(&unary.expr)
                    }
                    Some(HirUnaryOp::Deref) => {
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

            // Copy wrapper - look inside
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
            HirExpr::Unary(unary) if matches!(unary.op, Some(HirUnaryOp::Deref)) => {
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

            // Copy wrapper
            HirExpr::Copy(cp) => self.get_origin_from_lvalue(&cp.expr),

            // Everything else: no clear origin (literals, temporaries, complex expressions)
            _ => ReferenceOrigin::None,
        }
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
            (HirTy::Named(n), HirTy::Generic(g)) | (HirTy::Generic(g), HirTy::Named(n)) => {
                let name1 = n.name.to_string();
                let name2 =
                    MonomorphizationPass::generate_mangled_name(self.hir_arena, g, "struct");
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

    /// Check if a variable being returned has dependencies on local variables
    /// that will go out of scope. This prevents returning objects that contain
    /// references to local data.
    fn check_return_with_local_dependencies(
        &self,
        var_data: &VarData<'hir>,
        return_span: Span,
    ) -> Option<HirError> {
        // Helper to check if an origin variable will be deleted when the function returns
        let will_be_deleted = |origin_name: &'hir str| -> Option<Span> {
            if let Some(origin_var) = self.scope_map.get(origin_name) {
                // The origin will be deleted when we return if:
                // 1. It's not a primitive or reference (those don't need deletion)
                // 2. It's owned (not already moved/deleted)
                // 3. It's declared before or at the same time as the returned variable
                //    (indicating it's a local, not the returned value itself)

                let is_declared_before_or_with =
                    origin_var.declaration_stmt_index <= var_data.declaration_stmt_index;
                let needs_deletion = origin_var.kind != VarKind::Primitive
                    && origin_var.kind != VarKind::Reference
                    && origin_var.status.is_valid();

                // If the origin was declared at the same time or before the returned variable,
                // and it's different from the returned variable, it will be deleted
                if is_declared_before_or_with && origin_name != var_data.name && needs_deletion {
                    return Some(origin_var.span);
                }
            }
            None
        };

        // Check if the variable has any dependencies that will be deleted
        match &var_data.origin {
            ReferenceOrigin::Variable(origin_name) => {
                if let Some(dep_span) = will_be_deleted(origin_name) {
                    let path = return_span.path;
                    let src = utils::get_file_content(path).unwrap_or_default();
                    return Some(HirError::ReturningValueWithLocalLifetimeDependency(
                        ReturningValueWithLocalLifetimeDependencyError {
                            value_name: var_data.name.to_string(),
                            origin_name: origin_name.to_string(),
                            origin_declaration_span: dep_span,
                            return_span,
                            src: NamedSource::new(path, src),
                        },
                    ));
                }
            }
            ReferenceOrigin::Union(origins) => {
                // Check all origins in the union
                for origin in origins {
                    if let ReferenceOrigin::Variable(origin_name) = origin
                        && let Some(dep_span) = will_be_deleted(origin_name)
                    {
                        let path = return_span.path;
                        let src = utils::get_file_content(path).unwrap_or_default();
                        return Some(HirError::ReturningValueWithLocalLifetimeDependency(
                            ReturningValueWithLocalLifetimeDependencyError {
                                value_name: var_data.name.to_string(),
                                origin_name: origin_name.to_string(),
                                origin_declaration_span: dep_span,
                                return_span,
                                src: NamedSource::new(path, src),
                            },
                        ));
                    }
                }
            }
            ReferenceOrigin::None => {}
        }
        None
    }

    /// Compute lifetime dependencies for objects created via constructors.
    /// If the constructor takes reference arguments, the constructed object's lifetime
    /// is tied to those references.
    fn compute_object_lifetime_dependencies(&self, expr: &HirExpr<'hir>) -> ReferenceOrigin<'hir> {
        match expr {
            // Handle wrapped expressions
            HirExpr::Unary(unary) if unary.op.is_none() => {
                self.compute_object_lifetime_dependencies(&unary.expr)
            }
            // For NewObj, check all constructor arguments
            HirExpr::NewObj(new_obj) => {
                let mut combined_origin = ReferenceOrigin::None;
                for arg in &new_obj.args {
                    // If argument is a reference type, track its origin
                    if arg.ty().is_ref() {
                        let arg_origin = self.compute_reference_origin(arg);
                        combined_origin = combined_origin.merge(arg_origin);
                    }
                }
                combined_origin
            }
            // For ObjLiteral (union constructors), check field initializers
            HirExpr::ObjLiteral(obj_lit) => {
                let mut combined_origin = ReferenceOrigin::None;
                for field_init in &obj_lit.fields {
                    if field_init.value.ty().is_ref() {
                        let arg_origin = self.compute_reference_origin(&field_init.value);
                        combined_origin = combined_origin.merge(arg_origin);
                    }
                }
                combined_origin
            }
            // Function calls that return objects might also capture references from arguments
            HirExpr::Call(call) => {
                let mut combined_origin = ReferenceOrigin::None;
                for arg in &call.args {
                    if arg.ty().is_ref() {
                        let arg_origin = self.compute_reference_origin(arg);
                        combined_origin = combined_origin.merge(arg_origin);
                    }
                }
                combined_origin
            }
            _ => ReferenceOrigin::None,
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
                    .is_some_and(|s| s.copy_constructor.is_some())
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

    /// Checks if a block (recursively) contains a `delete this` statement
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
            HirStatement::Block(block) => Self::block_contains_delete_this(block),
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
            // Also check Copy wrapper in case transform wrapped the delete
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
            HirStatement::Block(block) => Self::block_transfers_this_ownership(block),
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
            // Copy/Move expressions - check if the inner expression needs management
            HirExpr::Copy(copy_expr) => Self::should_warn_about_temporary_in_cast(&copy_expr.expr),
            // Field access - if it returns a managed type, it might need extraction
            HirExpr::FieldAccess(field) => {
                !field.ty.is_ref() && Self::type_needs_memory_management(field.ty)
            }
            // Nested casts - the inner cast creates a temporary that needs to be extracted
            HirExpr::Casting(cast) => {
                !cast.ty.is_ref() && Self::type_needs_memory_management(cast.ty)
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

    /// Generate a unique temporary variable name
    fn generate_temp_var_name(&mut self) -> &'hir str {
        let name = format!("__atlas77_temp_{}", self.temp_var_counter);
        self.temp_var_counter += 1;
        self.hir_arena.names().get(&name)
    }

    /// Extract temporaries from method chains in an expression recursively
    /// This handles expressions like: obj.method1().method2().method3()
    /// Returns a transformed expression with temporaries extracted
    fn extract_temporaries_from_expr(&mut self, expr: HirExpr<'hir>) -> HirResult<HirExpr<'hir>> {
        match expr {
            // Unwrap Unary expressions with no operator (grouping/parentheses)
            HirExpr::Unary(unary) if unary.op.is_none() => {
                let processed_inner = self.extract_temporaries_from_expr(*unary.expr.clone())?;
                Ok(HirExpr::Unary(UnaryOpExpr {
                    span: unary.span,
                    op: None,
                    expr: Box::new(processed_inner),
                    ty: unary.ty,
                }))
            }
            HirExpr::Call(mut call) => {
                // First, recursively process the callee to extract any nested temporaries
                let processed_callee = self.extract_temporaries_from_expr(*call.callee.clone())?;
                call.callee = Box::new(processed_callee);

                // Then check if the call itself creates a temporary in a method chain context
                // This happens when the call returns a value that will be used for field access/method call
                Ok(HirExpr::Call(call))
            }
            // For field access, check if the target is a call chain that needs extraction
            HirExpr::FieldAccess(mut field) => {
                // First recursively process the target
                let processed_target = self.extract_temporaries_from_expr(*field.target.clone())?;

                // Then check if this processed target needs extraction
                if Self::should_warn_about_temporary_in_method_chain(&processed_target) {
                    let (temp_name, let_stmt) =
                        self.extract_temporary(processed_target.clone(), processed_target.span())?;
                    self.pending_statements.push(let_stmt);

                    field.target = Box::new(HirExpr::Ident(HirIdentExpr {
                        span: processed_target.span(),
                        name: temp_name,
                        ty: processed_target.ty(),
                    }));
                } else {
                    field.target = Box::new(processed_target);
                }
                Ok(HirExpr::FieldAccess(field))
            }
            // For nested casts, extract inner cast if needed
            HirExpr::Casting(mut cast) => {
                // First recursively process the inner expression
                let processed_inner = self.extract_temporaries_from_expr(*cast.expr.clone())?;

                // Check if this processed inner expression needs extraction
                if Self::should_warn_about_temporary_in_cast(&processed_inner) {
                    let (temp_name, let_stmt) =
                        self.extract_temporary(processed_inner.clone(), processed_inner.span())?;
                    self.pending_statements.push(let_stmt);

                    cast.expr = Box::new(HirExpr::Ident(HirIdentExpr {
                        span: processed_inner.span(),
                        name: temp_name,
                        ty: processed_inner.ty(),
                    }));
                } else {
                    cast.expr = Box::new(processed_inner);
                }
                Ok(HirExpr::Casting(cast))
            }
            // For other expressions, just return as-is
            _ => Ok(expr),
        }
    }

    /// Extract a temporary expression into a variable, returning the variable name and let statement
    /// This is used to properly manage the lifetime of temporary values in casts and method chains
    fn extract_temporary(
        &mut self,
        expr: HirExpr<'hir>,
        span: Span,
    ) -> HirResult<(&'hir str, HirStatement<'hir>)> {
        let temp_name = self.generate_temp_var_name();
        let ty = expr.ty();

        // Transform the expression (it might need COPY/MOVE inserted)
        let transformed_expr = self.transform_expr_ownership(&expr, true)?;

        // Register the temporary variable
        let kind = self.classify_type_kind(ty);
        let is_copyable = self.is_type_copyable(ty);

        let var_data = VarData::new(
            temp_name,
            span,
            kind,
            ty,
            is_copyable,
            self.current_stmt_index,
        );
        self.scope_map.insert(temp_name, var_data);

        // Create the let statement
        let let_stmt = HirStatement::Let(HirVariableStmt {
            span,
            name: temp_name,
            name_span: span,
            ty,
            ty_span: Some(span),
            value: transformed_expr,
        });

        Ok((temp_name, let_stmt))
    }
}
