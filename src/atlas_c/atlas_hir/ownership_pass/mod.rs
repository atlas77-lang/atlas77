//! # Ownership Pass
//!
//! The ownership pass enforces memory safety through:
//! - **Type-based classification** (Trivial/Resource/View)
//! - **Strict reference lifetime tracking** (compile errors on use-after-free)
//! - **Explicit move semantics** (warnings on use-after-move)
//! - **Automatic destructor insertion** (RAII cleanup)
//!
//! ## Design Philosophy
//! - **References = Rust-level safety:** Use-after-free is compile error
//! - **Values = C++-level safety:** Use-after-move is warning (UB)
//! - **Explicit over implicit:** No automatic moves, require `std::move()`
//! - **Zero-cost abstractions:** Trivial types bypass tracking entirely

pub(crate) mod context;

use crate::atlas_c::atlas_hir::arena::HirArena;
use crate::atlas_c::atlas_hir::error::{
    BorrowConflictError, CannotBorrowAsMutableWhileSharedBorrowExistsError,
    CannotBorrowAsSharedWhileMutableBorrowExistsError, CannotDeleteOutOfLoopError,
    CannotMutateWhileBorrowedError, DoubleMoveError, HirError, HirResult,
    ReturningReferenceToLocalVariableError, TryingToAccessADeletedValueError,
};
use crate::atlas_c::atlas_hir::expr::{HirExpr, HirIdentExpr};
use crate::atlas_c::atlas_hir::item::{HirStruct, HirStructConstructor, HirStructMethod};
use crate::atlas_c::atlas_hir::signature::HirModuleSignature;
use crate::atlas_c::atlas_hir::stmt::HirStatement;
use crate::atlas_c::atlas_hir::ty::{HirReferenceKind, HirTy};
use crate::atlas_c::atlas_hir::warning::{MoveInLoopWarning, UseAfterMoveWarning};
use crate::atlas_c::atlas_hir::{HirFunction, HirModule};
use crate::atlas_c::utils::{self, Span};
use context::{
    BorrowInfo, BorrowKind, LifetimeScope, OwnershipFunction, OwnershipVariable, TypeCategory,
    VarId, VarStatus,
};
use miette::{ErrReport, NamedSource};
use std::collections::HashMap;

/// The ownership pass analyzes and transforms the HIR to enforce memory safety.
pub struct OwnershipPass<'hir> {
    /// Module signature for type lookups
    signature: HirModuleSignature<'hir>,
    /// HIR arena for allocations
    arena: &'hir HirArena<'hir>,
    /// Current function context
    current_function: OwnershipFunction<'hir>,
    /// Collected warnings
    warnings: Vec<ErrReport>,
    /// Current function name (for error messages)
    current_func_name: Option<&'hir str>,
    /// Current class name (for method analysis)
    current_class_name: Option<&'hir str>,
}

impl<'hir> OwnershipPass<'hir> {
    pub fn new(signature: HirModuleSignature<'hir>, arena: &'hir HirArena<'hir>) -> Self {
        Self {
            signature,
            arena,
            current_function: OwnershipFunction::new(),
            warnings: Vec::new(),
            current_func_name: None,
            current_class_name: None,
        }
    }

    /// Run the ownership pass on the module.
    /// Returns the modified module, or an error if ownership rules are violated.
    pub fn run(
        &mut self,
        hir: &'hir mut HirModule<'hir>,
    ) -> Result<&'hir mut HirModule<'hir>, (&'hir mut HirModule<'hir>, HirError)> {
        self.signature = hir.signature.clone();

        // Analyze all functions
        for (name, func) in hir.body.functions.iter_mut() {
            self.current_func_name = Some(name);
            if let Err(e) = self.analyze_function(func) {
                self.emit_warnings();
                return Err((hir, e));
            }
        }

        // Analyze all struct methods
        for (name, class) in hir.body.structs.iter_mut() {
            self.current_class_name = Some(name);
            if let Err(e) = self.analyze_class(class) {
                self.emit_warnings();
                return Err((hir, e));
            }
        }

        // Emit all collected warnings
        self.emit_warnings();

        Ok(hir)
    }

    /// Emit all collected warnings
    fn emit_warnings(&self) {
        for warning in &self.warnings {
            // Use miette's error formatting for nice display
            eprintln!("{:?}", warning);
        }
    }

    // =========================================================================
    // Type Classification
    // =========================================================================

    /// Classify a type into Trivial, Resource, or View
    fn classify_type(&self, ty: &HirTy<'hir>) -> TypeCategory {
        match ty {
            // Primitives are trivial
            HirTy::Integer(_)
            | HirTy::Float(_)
            | HirTy::UnsignedInteger(_)
            | HirTy::Boolean(_)
            | HirTy::Unit(_)
            | HirTy::Char(_) => TypeCategory::Trivial,

            // Raw pointers are trivial (no automatic cleanup)
            HirTy::PtrTy(_) => TypeCategory::Trivial,

            // References are view types (lifetime-tracked, no ownership)
            HirTy::Reference(_) => TypeCategory::View,

            // Strings are resources (own heap memory)
            HirTy::String(_) => TypeCategory::Resource,

            // Slices are view types (don't own their data)
            HirTy::Slice(_) => TypeCategory::View,

            // Inline arrays: trivial if element is trivial
            HirTy::InlineArray(arr) => self.classify_type(arr.inner),

            // Named types: check if struct has destructor
            HirTy::Named(named) => self.classify_named_type(named.name),

            // Generic types: check if struct has destructor
            HirTy::Generic(generic) => self.classify_named_type(generic.name),

            // Function types are trivial (just function pointers)
            HirTy::Function(_) => TypeCategory::Trivial,

            // Uninitialized types are trivial
            HirTy::Uninitialized(_) => TypeCategory::Trivial,
        }
    }

    /// Classify a named type (struct) based on its properties
    fn classify_named_type(&self, name: &str) -> TypeCategory {
        if let Some(struct_sig) = self.signature.structs.get(name) {
            // If the struct has a user-defined destructor, it's a resource type
            if struct_sig.had_user_defined_destructor {
                return TypeCategory::Resource;
            }

            // If the struct has a move constructor, it should be tracked as a resource
            // (move semantics require tracking ownership state)
            if struct_sig.had_user_defined_move_constructor {
                return TypeCategory::Resource;
            }

            // If the struct has a copy constructor, it's also a resource
            // (explicit copy semantics indicate non-trivial data)
            if struct_sig.had_user_defined_copy_constructor {
                return TypeCategory::Resource;
            }

            // If any field is a resource, the struct is a resource
            for field in struct_sig.fields.values() {
                if matches!(self.classify_type(field.ty), TypeCategory::Resource) {
                    return TypeCategory::Resource;
                }
            }

            // All fields trivial => struct is trivial
            TypeCategory::Trivial
        } else {
            // Unknown type, assume trivial
            TypeCategory::Trivial
        }
    }

    /// Check if a type is trivial (can be bitwise copied, no cleanup needed)
    fn is_trivial(&self, ty: &HirTy<'hir>) -> bool {
        matches!(self.classify_type(ty), TypeCategory::Trivial)
    }

    /// Check if a type is a resource (owns heap resources, needs cleanup)
    fn is_resource(&self, ty: &HirTy<'hir>) -> bool {
        matches!(self.classify_type(ty), TypeCategory::Resource)
    }

    /// Check if a type has a destructor
    fn has_destructor(&self, ty: &HirTy<'hir>) -> bool {
        match ty {
            HirTy::Named(named) => {
                if let Some(struct_sig) = self.signature.structs.get(named.name) {
                    struct_sig.destructor.is_some()
                } else {
                    false
                }
            }
            HirTy::Generic(generic) => {
                if let Some(struct_sig) = self.signature.structs.get(generic.name) {
                    struct_sig.destructor.is_some()
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    /// Check if a type has a copy constructor
    fn has_copy_constructor(&self, ty: &HirTy<'hir>) -> bool {
        match ty {
            HirTy::Named(named) => {
                if let Some(struct_sig) = self.signature.structs.get(named.name) {
                    struct_sig.had_user_defined_copy_constructor
                        || struct_sig.copy_constructor.is_some()
                } else {
                    false
                }
            }
            HirTy::Generic(generic) => {
                if let Some(struct_sig) = self.signature.structs.get(generic.name) {
                    struct_sig.had_user_defined_copy_constructor
                        || struct_sig.copy_constructor.is_some()
                } else {
                    false
                }
            }
            // Primitives are trivially copyable
            _ => self.is_trivial(ty),
        }
    }

    /// Check if a type has a move constructor
    fn has_move_constructor(&self, ty: &HirTy<'hir>) -> bool {
        match ty {
            HirTy::Named(named) => {
                if let Some(struct_sig) = self.signature.structs.get(named.name) {
                    struct_sig.had_user_defined_move_constructor
                        || struct_sig.move_constructor.is_some()
                } else {
                    false
                }
            }
            HirTy::Generic(generic) => {
                if let Some(struct_sig) = self.signature.structs.get(generic.name) {
                    struct_sig.had_user_defined_move_constructor
                        || struct_sig.move_constructor.is_some()
                } else {
                    false
                }
            }
            // Primitives are trivially moveable
            _ => self.is_trivial(ty),
        }
    }

    // =========================================================================
    // Function Analysis
    // =========================================================================

    fn analyze_function(&mut self, func: &mut HirFunction<'hir>) -> HirResult<()> {
        // Reset function context
        self.current_function = OwnershipFunction::new();

        // Enter function scope
        self.current_function.enter_scope(func.span, false);

        // Register parameters
        for param in &func.signature.params {
            let var_id = self.current_function.create_var_id();
            let category = self.classify_type(param.ty);
            self.current_function.declare_variable(OwnershipVariable {
                name: param.name,
                var_id,
                ty: param.ty,
                category,
                status: VarStatus::Owned,
                declaration_span: param.span,
                is_param: true,
                lifetime: LifetimeScope {
                    scope_id: self.current_function.current_scope_id(),
                    can_escape: true, // Parameters can escape (returned)
                },
                refs_locals: Vec::new(),
            });
        }

        // Analyze body statements
        for stmt in &mut func.body.statements {
            self.analyze_statement(stmt)?;
        }

        // Exit function scope
        self.current_function.exit_scope();

        Ok(())
    }

    fn analyze_class(&mut self, class: &mut HirStruct<'hir>) -> HirResult<()> {
        // Analyze each method
        for method in &mut class.methods {
            self.current_func_name = Some(method.name);
            self.analyze_method(method)?;
        }

        // Analyze constructor
        self.current_func_name = Some("constructor");
        self.analyze_constructor(&mut class.constructor)?;

        // Analyze copy constructor if present
        if let Some(copy_ctor) = &mut class.copy_constructor {
            self.current_func_name = Some("_copy");
            self.analyze_constructor(copy_ctor)?;
        }

        // Analyze move constructor if present
        if let Some(move_ctor) = &mut class.move_constructor {
            self.current_func_name = Some("_move");
            self.analyze_constructor(move_ctor)?;
        }

        // Analyze destructor if present
        if let Some(destructor) = &mut class.destructor {
            self.current_func_name = Some("_destroy");
            self.analyze_constructor(destructor)?;
        }

        Ok(())
    }

    fn analyze_method(&mut self, method: &mut HirStructMethod<'hir>) -> HirResult<()> {
        // Reset function context
        self.current_function = OwnershipFunction::new();

        // Enter method scope
        self.current_function.enter_scope(method.span, false);

        // Register parameters (including implicit 'this')
        for param in &method.signature.params {
            let var_id = self.current_function.create_var_id();
            let category = self.classify_type(param.ty);
            self.current_function.declare_variable(OwnershipVariable {
                name: param.name,
                var_id,
                ty: param.ty,
                category,
                status: VarStatus::Owned,
                declaration_span: param.span,
                is_param: true,
                lifetime: LifetimeScope {
                    scope_id: self.current_function.current_scope_id(),
                    can_escape: true,
                },
                refs_locals: Vec::new(),
            });
        }

        // Analyze body statements
        for stmt in &mut method.body.statements {
            self.analyze_statement(stmt)?;
        }

        // Exit method scope
        self.current_function.exit_scope();

        Ok(())
    }

    fn analyze_constructor(&mut self, ctor: &mut HirStructConstructor<'hir>) -> HirResult<()> {
        // Reset function context
        self.current_function = OwnershipFunction::new();

        // Enter constructor scope
        self.current_function.enter_scope(ctor.span, false);

        // Register parameters
        for param in &ctor.params {
            let var_id = self.current_function.create_var_id();
            let category = self.classify_type(param.ty);
            self.current_function.declare_variable(OwnershipVariable {
                name: param.name,
                var_id,
                ty: param.ty,
                category,
                status: VarStatus::Owned,
                declaration_span: param.span,
                is_param: true,
                lifetime: LifetimeScope {
                    scope_id: self.current_function.current_scope_id(),
                    can_escape: true,
                },
                refs_locals: Vec::new(),
            });
        }

        // Analyze body statements
        for stmt in &mut ctor.body.statements {
            self.analyze_statement(stmt)?;
        }

        // Exit constructor scope
        self.current_function.exit_scope();

        Ok(())
    }

    // =========================================================================
    // Statement Analysis
    // =========================================================================

    fn analyze_statement(&mut self, stmt: &mut HirStatement<'hir>) -> HirResult<()> {
        match stmt {
            HirStatement::Let(let_stmt) => {
                // First analyze the value expression
                self.analyze_expr(&mut let_stmt.value)?;

                // Get reference origins if the value is a reference
                let refs_locals = self.get_reference_origins(&let_stmt.value);

                // If this is a reference type, check for borrow conflicts and record the borrow
                if let HirTy::Reference(ref_ty) = let_stmt.ty {
                    let borrow_kind = match ref_ty.kind {
                        HirReferenceKind::Mutable => BorrowKind::Mutable,
                        HirReferenceKind::ReadOnly => BorrowKind::Shared,
                        HirReferenceKind::Moveable => BorrowKind::Mutable, // Moveable refs are exclusive
                    };

                    // Check for borrow conflicts on each origin variable
                    for &origin_var_id in &refs_locals {
                        if let Some(conflict) = self
                            .current_function
                            .check_borrow_conflict(origin_var_id, borrow_kind)
                        {
                            let conflict_span = conflict.span;
                            let conflict_kind = conflict.kind;
                            let origin_name = self
                                .current_function
                                .get_variable(origin_var_id)
                                .map(|v| v.name)
                                .unwrap_or("unknown");

                            let path = let_stmt.span.path;
                            let src = utils::get_file_content(path).unwrap();

                            match (borrow_kind, conflict_kind) {
                                (BorrowKind::Mutable, BorrowKind::Shared) => {
                                    return Err(HirError::CannotBorrowAsMutableWhileSharedBorrowExists(
                                        CannotBorrowAsMutableWhileSharedBorrowExistsError {
                                            var_name: origin_name.to_string(),
                                            mutable_borrow_span: let_stmt.span,
                                            shared_borrow_span: conflict_span,
                                            src: NamedSource::new(path, src),
                                        },
                                    ));
                                }
                                (BorrowKind::Shared, BorrowKind::Mutable) => {
                                    return Err(HirError::CannotBorrowAsSharedWhileMutableBorrowExists(
                                        CannotBorrowAsSharedWhileMutableBorrowExistsError {
                                            var_name: origin_name.to_string(),
                                            shared_borrow_span: let_stmt.span,
                                            mutable_borrow_span: conflict_span,
                                            src: NamedSource::new(path, src),
                                        },
                                    ));
                                }
                                (BorrowKind::Mutable, BorrowKind::Mutable) => {
                                    return Err(HirError::BorrowConflict(BorrowConflictError {
                                        var_name: origin_name.to_string(),
                                        new_borrow_kind: "mutable (&)".to_string(),
                                        existing_borrow_kind: "mutable (&)".to_string(),
                                        new_borrow_span: let_stmt.span,
                                        existing_borrow_span: conflict_span,
                                        src: NamedSource::new(path, src),
                                    }));
                                }
                                // Shared + Shared is always OK
                                _ => {}
                            }
                        }
                    }
                }

                // Register the new variable
                let var_id = self.current_function.create_var_id();
                let category = self.classify_type(let_stmt.ty);
                self.current_function.declare_variable(OwnershipVariable {
                    name: let_stmt.name,
                    var_id,
                    ty: let_stmt.ty,
                    category,
                    status: VarStatus::Owned,
                    declaration_span: let_stmt.span,
                    is_param: false,
                    lifetime: LifetimeScope {
                        scope_id: self.current_function.current_scope_id(),
                        can_escape: false, // Local variables cannot escape
                    },
                    refs_locals: refs_locals.clone(),
                });

                // If this is a reference type, record the borrow on origin variables
                if let HirTy::Reference(ref_ty) = let_stmt.ty {
                    let borrow_kind = match ref_ty.kind {
                        HirReferenceKind::Mutable => BorrowKind::Mutable,
                        HirReferenceKind::ReadOnly => BorrowKind::Shared,
                        HirReferenceKind::Moveable => BorrowKind::Mutable,
                    };

                    for &origin_var_id in &refs_locals {
                        self.current_function.record_borrow(
                            origin_var_id,
                            BorrowInfo {
                                ref_var: var_id,
                                kind: borrow_kind,
                                active: true,
                                span: let_stmt.span,
                            },
                        );
                    }
                }
            }

            HirStatement::Const(const_stmt) => {
                // Analyze the value expression
                self.analyze_expr(&mut const_stmt.value)?;

                // Register the constant
                let var_id = self.current_function.create_var_id();
                let category = self.classify_type(const_stmt.ty);
                self.current_function.declare_variable(OwnershipVariable {
                    name: const_stmt.name,
                    var_id,
                    ty: const_stmt.ty,
                    category,
                    status: VarStatus::Owned,
                    declaration_span: const_stmt.span,
                    is_param: false,
                    lifetime: LifetimeScope {
                        scope_id: self.current_function.current_scope_id(),
                        can_escape: false,
                    },
                    refs_locals: Vec::new(),
                });
            }

            HirStatement::Assign(assign_stmt) => {
                // Analyze the value expression first
                self.analyze_expr(&mut assign_stmt.val)?;

                // Check if the destination variable is currently borrowed
                // (cannot mutate a variable while it is borrowed)
                if let HirExpr::Ident(ident) = &assign_stmt.dst {
                    if let Some(var_id) = self.current_function.lookup_variable_id(ident.name) {
                        if let Some(borrows) = self.current_function.is_borrowed(var_id) {
                            // Find the first active borrow to report
                            if let Some(active_borrow) =
                                borrows.iter().find(|b| b.active)
                            {
                                let borrow_span = active_borrow.span;
                                let path = assign_stmt.span.path;
                                let src = utils::get_file_content(path).unwrap();
                                return Err(HirError::CannotMutateWhileBorrowed(
                                    CannotMutateWhileBorrowedError {
                                        var_name: ident.name.to_string(),
                                        assign_span: assign_stmt.span,
                                        borrow_span,
                                        src: NamedSource::new(path, src),
                                    },
                                ));
                            }
                        }
                    }
                }

                // Analyze the destination
                self.analyze_expr(&mut assign_stmt.dst)?;
            }

            HirStatement::Expr(expr_stmt) => {
                self.analyze_expr(&mut expr_stmt.expr)?;
            }

            HirStatement::Return(ret_stmt) => {
                self.analyze_expr(&mut ret_stmt.value)?;

                // Check for returning references to local variables
                self.check_return_lifetime(&ret_stmt.value, ret_stmt.span)?;
            }

            HirStatement::IfElse(if_stmt) => {
                // Analyze condition
                self.analyze_expr(&mut if_stmt.condition)?;

                // Save current state for branch analysis
                let state_before = self.current_function.clone_var_states();

                // Enter then branch scope
                self.current_function
                    .enter_scope(if_stmt.then_branch.span, false);

                // Analyze then branch
                for stmt in &mut if_stmt.then_branch.statements {
                    self.analyze_statement(stmt)?;
                }

                // Exit then branch scope
                self.current_function.exit_scope();

                // Save then branch state
                let state_after_then = self.current_function.clone_var_states();

                // Restore state for else branch
                self.current_function
                    .restore_var_states(state_before.clone());

                // Analyze else branch if present
                if let Some(else_branch) = &mut if_stmt.else_branch {
                    self.current_function.enter_scope(else_branch.span, false);

                    for stmt in &mut else_branch.statements {
                        self.analyze_statement(stmt)?;
                    }

                    self.current_function.exit_scope();
                }

                let state_after_else = self.current_function.clone_var_states();

                // Merge branch states
                self.merge_branch_states(state_after_then, state_after_else);
            }

            HirStatement::While(while_stmt) => {
                // Analyze condition
                self.analyze_expr(&mut while_stmt.condition)?;

                // Save state before loop
                let state_before = self.current_function.clone_var_states();

                // Enter loop scope
                self.current_function
                    .enter_scope(while_stmt.span, true /* is_loop */);

                // Analyze body
                for stmt in &mut while_stmt.body.statements {
                    self.analyze_statement(stmt)?;
                }

                // Exit loop scope
                self.current_function.exit_scope();

                // Check for moves inside loop and update to conditional moves
                self.check_moves_in_loop(state_before);
            }

            HirStatement::Block(block) => {
                self.current_function.enter_scope(block.span, false);

                for stmt in &mut block.statements {
                    self.analyze_statement(stmt)?;
                }

                self.current_function.exit_scope();
            }

            HirStatement::Break(_) | HirStatement::Continue(_) => {
                // No ownership implications
            }
        }

        Ok(())
    }

    // =========================================================================
    // Expression Analysis
    // =========================================================================

    fn analyze_expr(&mut self, expr: &mut HirExpr<'hir>) -> HirResult<()> {
        match expr {
            // Literals don't have ownership implications
            HirExpr::IntegerLiteral(_)
            | HirExpr::FloatLiteral(_)
            | HirExpr::UnsignedIntegerLiteral(_)
            | HirExpr::BooleanLiteral(_)
            | HirExpr::UnitLiteral(_)
            | HirExpr::CharLiteral(_)
            | HirExpr::StringLiteral(_)
            | HirExpr::NullLiteral(_) => {}

            HirExpr::Ident(ident) => {
                self.check_variable_access(ident)?;
            }

            HirExpr::ThisLiteral(_) => {
                // 'this' is always valid
            }

            HirExpr::HirBinaryOperation(bin_op) => {
                self.analyze_expr(&mut bin_op.lhs)?;
                self.analyze_expr(&mut bin_op.rhs)?;
            }

            HirExpr::Unary(unary) => {
                self.analyze_expr(&mut unary.expr)?;
            }

            HirExpr::Call(call) => {
                // Analyze callee
                self.analyze_expr(&mut call.callee)?;

                // Analyze arguments
                for arg in &mut call.args {
                    self.analyze_expr(arg)?;
                }

                // Check for std::move calls
                self.check_move_call(call)?;
            }

            HirExpr::FieldAccess(field_access) => {
                self.analyze_expr(&mut field_access.target)?;
            }

            HirExpr::StaticAccess(_) => {
                // Static access doesn't involve ownership
            }

            HirExpr::Indexing(indexing) => {
                self.analyze_expr(&mut indexing.target)?;
                self.analyze_expr(&mut indexing.index)?;
            }

            HirExpr::Casting(cast) => {
                self.analyze_expr(&mut cast.expr)?;
            }

            HirExpr::ListLiteral(list) => {
                for item in &mut list.items {
                    self.analyze_expr(item)?;
                }
            }

            HirExpr::NewArray(new_arr) => {
                self.analyze_expr(&mut new_arr.size)?;
            }

            HirExpr::NewObj(new_obj) => {
                for arg in &mut new_obj.args {
                    self.analyze_expr(arg)?;
                }
            }

            HirExpr::ObjLiteral(obj_lit) => {
                for field in &mut obj_lit.fields {
                    self.analyze_expr(&mut field.value)?;
                }
            }

            HirExpr::Delete(delete) => {
                self.analyze_expr(&mut delete.expr)?;
                self.check_delete_expr(delete)?;
            }

            HirExpr::Copy(copy_expr) => {
                self.analyze_expr(&mut copy_expr.expr)?;
            }

            HirExpr::IntrinsicCall(intrinsic) => {
                for arg in &mut intrinsic.args {
                    self.analyze_expr(arg)?;
                }
            }
        }

        Ok(())
    }

    // =========================================================================
    // Ownership Checks
    // =========================================================================

    /// Check if accessing a variable is valid
    fn check_variable_access(&mut self, ident: &HirIdentExpr<'hir>) -> HirResult<()> {
        if let Some(var_id) = self.current_function.lookup_variable_id(ident.name) {
            if let Some(var) = self.current_function.get_variable(var_id) {
                // Trivial types don't need tracking
                if matches!(var.category, TypeCategory::Trivial) {
                    return Ok(());
                }

                match &var.status {
                    VarStatus::Moved { move_span } => {
                        // Use-after-move is a warning (UB in C++ terms)
                        let warning =
                            self.use_after_move_warning(ident.name, ident.span, *move_span);
                        self.warnings.push(warning);
                    }
                    VarStatus::ConditionallyMoved { move_span } => {
                        // Possible use-after-move is also a warning
                        let warning =
                            self.use_after_move_warning(ident.name, ident.span, *move_span);
                        self.warnings.push(warning);
                    }
                    VarStatus::Deleted { delete_span } => {
                        // Use-after-delete is an error
                        let path = ident.span.path;
                        let src = utils::get_file_content(path).unwrap();
                        return Err(HirError::TryingToAccessADeletedValue(
                            TryingToAccessADeletedValueError {
                                delete_span: *delete_span,
                                access_span: ident.span,
                                src: NamedSource::new(path, src),
                            },
                        ));
                    }
                    VarStatus::Owned | VarStatus::Borrowed { .. } => {
                        // Valid access
                    }
                }
            }
        }
        Ok(())
    }

    /// Check function calls for moveable reference parameters and mark variables as moved.
    ///
    /// When a variable is passed to a parameter with type `T&&` (moveable reference),
    /// that variable should be marked as moved. This is independent of what the function
    /// is called - any function taking a moveable reference will trigger a move.
    fn check_move_call(
        &mut self,
        call: &mut crate::atlas_c::atlas_hir::expr::HirFunctionCallExpr<'hir>,
    ) -> HirResult<()> {
        // Iterate through arguments and their expected types
        for (i, arg) in call.args.iter().enumerate() {
            // Get the expected parameter type
            let Some(param_ty) = call.args_ty.get(i) else {
                continue;
            };

            // Check if this parameter expects a moveable reference
            let is_moveable_ref = matches!(
                param_ty,
                HirTy::Reference(r) if r.kind == HirReferenceKind::Moveable
            );

            if !is_moveable_ref {
                continue;
            }

            // The argument is being passed to a moveable reference parameter.
            // Extract the variable being moved (handles both `var` and `&var` patterns).
            let var_name = match arg {
                HirExpr::Ident(ident) => Some((ident.name, ident.span)),
                HirExpr::Unary(unary) => {
                    // Handle &var being passed as T&&
                    if let HirExpr::Ident(ident) = unary.expr.as_ref() {
                        Some((ident.name, ident.span))
                    } else {
                        None
                    }
                }
                _ => None,
            };

            let Some((name, arg_span)) = var_name else {
                continue;
            };

            let Some(var_id) = self.current_function.lookup_variable_id(name) else {
                continue;
            };

            // Collect the variable IDs to mark as moved
            // This includes the variable itself and any variables it references
            let mut vars_to_mark_moved = vec![var_id];

            // Check if already moved
            if let Some(var) = self.current_function.get_variable(var_id) {
                if let VarStatus::Moved { move_span } = &var.status {
                    // Double move warning - valid in C++ but potentially dangerous
                    let warning = self.use_after_move_warning(name, arg_span, *move_span);
                    self.warnings.push(warning);
                }

                // Check if moving in a loop
                if self.current_function.is_in_loop() {
                    if let Some(loop_span) = self.current_function.get_innermost_loop_span() {
                        let warning = self.move_in_loop_warning(name, arg_span, loop_span);
                        self.warnings.push(warning);
                    }
                }

                // If this variable is a reference, also mark the referenced variables as moved
                if !var.refs_locals.is_empty() {
                    vars_to_mark_moved.extend(var.refs_locals.iter().copied());
                }
            }

            // Mark all variables as moved (the reference itself and any variables it references)
            for vid in vars_to_mark_moved {
                self.current_function.mark_moved(vid, arg_span);
            }
        }

        Ok(())
    }

    /// Check delete expressions
    fn check_delete_expr(
        &mut self,
        delete: &crate::atlas_c::atlas_hir::expr::HirDeleteExpr<'hir>,
    ) -> HirResult<()> {
        // Check if deleting in a loop
        if self.current_function.is_in_loop() {
            if let HirExpr::Ident(ident) = delete.expr.as_ref() {
                if let Some(loop_span) = self.current_function.get_innermost_loop_span() {
                    let path = delete.span.path;
                    let src = utils::get_file_content(path).unwrap();
                    return Err(HirError::CannotDeleteOutOfLoop(
                        CannotDeleteOutOfLoopError {
                            loop_span,
                            delete_span: delete.span,
                            var_name: ident.name.to_string(),
                            src: NamedSource::new(path, src),
                        },
                    ));
                }
            }
        }

        // Mark variable as deleted if it's an identifier
        if let HirExpr::Ident(ident) = delete.expr.as_ref() {
            if let Some(var_id) = self.current_function.lookup_variable_id(ident.name) {
                self.current_function.mark_deleted(var_id, delete.span);
            }
        }

        Ok(())
    }

    /// Check return statement for reference lifetime issues
    fn check_return_lifetime(&self, expr: &HirExpr<'hir>, return_span: Span) -> HirResult<()> {
        let ty = expr.ty();

        // Only check reference types
        if !matches!(ty, HirTy::Reference(_)) {
            return Ok(());
        }

        // Get all local variables this expression references
        let local_refs = self.get_local_ref_targets(expr);

        for local_name in local_refs {
            if let Some(var_id) = self.current_function.lookup_variable_id(local_name) {
                if let Some(var) = self.current_function.get_variable(var_id) {
                    // If the variable cannot escape (is a local), error
                    if !var.lifetime.can_escape && !var.is_param {
                        let path = return_span.path;
                        let src = utils::get_file_content(path).unwrap();
                        return Err(HirError::ReturningReferenceToLocalVariable(
                            ReturningReferenceToLocalVariableError {
                                span: return_span,
                                var_name: local_name.to_string(),
                                src: NamedSource::new(path, src),
                            },
                        ));
                    }
                }
            }
        }

        Ok(())
    }

    /// Get all local variables that an expression references
    fn get_local_ref_targets(&self, expr: &HirExpr<'hir>) -> Vec<&'hir str> {
        match expr {
            HirExpr::Ident(ident) => {
                if let Some(var) = self.current_function.lookup_variable(ident.name) {
                    if !var.is_param {
                        return vec![ident.name];
                    }
                    // Include transitive references
                    let mut result = Vec::new();
                    for &origin_id in &var.refs_locals {
                        if let Some(origin_var) = self.current_function.get_variable(origin_id) {
                            if !origin_var.is_param {
                                result.push(origin_var.name);
                            }
                        }
                    }
                    return result;
                }
                Vec::new()
            }
            HirExpr::Unary(unary) => {
                // &x creates a reference to x
                if unary.op == Some(crate::atlas_c::atlas_hir::expr::HirUnaryOp::AsRef) {
                    return self.get_local_ref_targets(&unary.expr);
                }
                Vec::new()
            }
            HirExpr::FieldAccess(field) => {
                // Field access inherits target's lifetime
                self.get_local_ref_targets(&field.target)
            }
            _ => Vec::new(),
        }
    }

    /// Get reference origins from an expression (for tracking what a reference points to)
    fn get_reference_origins(&self, expr: &HirExpr<'hir>) -> Vec<VarId> {
        match expr {
            HirExpr::Ident(ident) => {
                if let Some(var_id) = self.current_function.lookup_variable_id(ident.name) {
                    vec![var_id]
                } else {
                    Vec::new()
                }
            }
            HirExpr::Unary(unary) => {
                // &x creates a reference to x
                // Also handle implicit conversions (op = None) for moveable references
                if unary.op == Some(crate::atlas_c::atlas_hir::expr::HirUnaryOp::AsRef)
                    || unary.op.is_none()
                {
                    return self.get_reference_origins(&unary.expr);
                }
                Vec::new()
            }
            HirExpr::FieldAccess(field) => self.get_reference_origins(&field.target),
            _ => Vec::new(),
        }
    }

    /// Merge variable states from two branches (if/else)
    fn merge_branch_states(
        &mut self,
        then_states: HashMap<VarId, VarStatus>,
        else_states: HashMap<VarId, VarStatus>,
    ) {
        for (var_id, then_status) in then_states {
            let else_status = else_states
                .get(&var_id)
                .cloned()
                .unwrap_or(VarStatus::Owned);

            let merged = match (&then_status, &else_status) {
                (VarStatus::Moved { move_span: s1 }, VarStatus::Moved { .. }) => {
                    // Moved in both branches → definitely moved
                    VarStatus::Moved { move_span: *s1 }
                }
                (VarStatus::Moved { move_span }, _) | (_, VarStatus::Moved { move_span }) => {
                    // Moved in one branch → conditionally moved
                    VarStatus::ConditionallyMoved {
                        move_span: *move_span,
                    }
                }
                (VarStatus::Deleted { delete_span: s1 }, VarStatus::Deleted { .. }) => {
                    // Deleted in both branches → definitely deleted
                    VarStatus::Deleted { delete_span: *s1 }
                }
                (VarStatus::ConditionallyMoved { move_span }, _)
                | (_, VarStatus::ConditionallyMoved { move_span }) => {
                    VarStatus::ConditionallyMoved {
                        move_span: *move_span,
                    }
                }
                _ => VarStatus::Owned,
            };

            if let Some(var) = self.current_function.get_variable_mut(var_id) {
                var.status = merged;
            }
        }
    }

    /// Check for moves that occurred in a loop and upgrade them to conditional moves
    fn check_moves_in_loop(&mut self, state_before: HashMap<VarId, VarStatus>) {
        for (var_id, status_before) in state_before {
            if let Some(var) = self.current_function.get_variable(var_id) {
                if let VarStatus::Moved { move_span } = &var.status {
                    // Was moved inside loop
                    if !matches!(status_before, VarStatus::Moved { .. }) {
                        // Upgrade to conditionally moved
                        self.current_function
                            .mark_conditionally_moved(var_id, *move_span);
                    }
                }
            }
        }
    }

    // =========================================================================
    // Warning Generation
    // =========================================================================

    fn use_after_move_warning(
        &self,
        var_name: &str,
        access_span: Span,
        move_span: Span,
    ) -> ErrReport {
        let path = access_span.path;
        let src = utils::get_file_content(path).unwrap();
        UseAfterMoveWarning {
            src: NamedSource::new(path, src),
            access_span,
            move_span,
            var_name: var_name.to_string(),
        }
        .into()
    }

    fn move_in_loop_warning(&self, var_name: &str, move_span: Span, loop_span: Span) -> ErrReport {
        let path = move_span.path;
        let src = utils::get_file_content(path).unwrap();
        MoveInLoopWarning {
            src: NamedSource::new(path, src),
            move_span,
            loop_span,
            var_name: var_name.to_string(),
        }
        .into()
    }
}
