// TEMPORARY DESIGN (v0.8.0 MVP)
//
// This ownership pass uses raw pointers (*T, *const T) for all borrowing.
// In v0.9+, references (&T, &const T) will be added with lifetime tracking.
//
// Current constructor signatures:
//   - Copy: Foo(from: *const Foo)
//   - Move: Foo(from: *Foo)
//
// Future constructor signatures:
//   - Copy: Foo(from: &const Foo)
//   - Move: Foo(from: &Foo)
//
// This is intentionally simplified to establish MVP baseline.

use crate::atlas_c::atlas_hir::arena::HirArena;
use crate::atlas_c::atlas_hir::error::{
    HirError, HirResult, TryingToAccessADeletedValueError, TypeIsNotCopyableError,
    TypeIsNotMoveableError,
};
use crate::atlas_c::atlas_hir::expr::{
    HirCopyExpr, HirDeleteExpr, HirExpr, HirFieldAccessExpr, HirFunctionCallExpr, HirIdentExpr,
    HirUnaryOp,
};
use crate::atlas_c::atlas_hir::item::{HirStruct, HirStructConstructor, HirStructMethod};
use crate::atlas_c::atlas_hir::signature::HirModuleSignature;
use crate::atlas_c::atlas_hir::stmt::{HirBlock, HirExprStmt, HirStatement};
use crate::atlas_c::atlas_hir::ty::HirTy;
use crate::atlas_c::atlas_hir::warning::UseAfterMoveWarning;
use crate::atlas_c::atlas_hir::{HirFunction, HirModule};
use crate::atlas_c::utils::{self, Span};
use miette::{ErrReport, NamedSource};
use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone)]
enum MoveReason {
    ExplicitMoveCall,
    MoveConstructorCall,
    MutablePointerParameter,
}

#[derive(Debug, Clone)]
enum VarState<'hir> {
    Valid,
    Moved {
        move_span: Span,
        reason: MoveReason,
    },
    Deleted {
        delete_span: Span,
    },
    PartiallyMoved {
        moved_fields: HashSet<&'hir str>,
        move_span: Span,
    },
    ConditionallyMoved {
        move_span: Span,
    },
}

#[derive(Debug, Clone)]
struct VarInfo<'hir> {
    name: &'hir str,
    ty: &'hir HirTy<'hir>,
    state: VarState<'hir>,
    is_param: bool,
}

#[derive(Debug, Clone, Default)]
struct ScopeState<'hir> {
    vars: HashMap<&'hir str, VarInfo<'hir>>,
}

pub struct OwnershipPass<'hir> {
    signature: HirModuleSignature<'hir>,
    _arena: &'hir HirArena<'hir>,
    scopes: Vec<ScopeState<'hir>>,
    warnings: Vec<ErrReport>,
    emitted_ctor_warning: bool,
}

impl<'hir> OwnershipPass<'hir> {
    pub fn new(signature: HirModuleSignature<'hir>, arena: &'hir HirArena<'hir>) -> Self {
        Self {
            signature,
            _arena: arena,
            scopes: Vec::new(),
            warnings: Vec::new(),
            emitted_ctor_warning: false,
        }
    }

    pub fn run(
        &mut self,
        hir: &'hir mut HirModule<'hir>,
    ) -> Result<&'hir mut HirModule<'hir>, (&'hir mut HirModule<'hir>, HirError)> {
        self.signature = hir.signature.clone();

        for (_, func) in hir.body.functions.iter_mut() {
            if let Err(e) = self.analyze_function(func) {
                self.emit_warnings();
                return Err((hir, e));
            }
        }

        for (_, class) in hir.body.structs.iter_mut() {
            if let Err(e) = self.analyze_class(class) {
                self.emit_warnings();
                return Err((hir, e));
            }
        }

        self.emit_warnings();
        Ok(hir)
    }

    fn emit_warnings(&self) {
        for warning in &self.warnings {
            eprintln!("{:?}", warning);
        }
    }

    fn enter_scope(&mut self) {
        self.scopes.push(ScopeState::default());
    }

    fn exit_scope(&mut self) {
        let _ = self.scopes.pop();
    }

    fn declare_var(&mut self, name: &'hir str, ty: &'hir HirTy<'hir>, is_param: bool) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.vars.insert(
                name,
                VarInfo {
                    name,
                    ty,
                    state: VarState::Valid,
                    is_param,
                },
            );
        }
    }

    fn get_var(&self, name: &'hir str) -> Option<&VarInfo<'hir>> {
        self.scopes.iter().rev().find_map(|s| s.vars.get(name))
    }

    fn get_var_mut(&mut self, name: &'hir str) -> Option<&mut VarInfo<'hir>> {
        self.scopes
            .iter_mut()
            .rev()
            .find_map(|s| s.vars.get_mut(name))
    }

    fn mark_moved(&mut self, name: &'hir str, span: Span, reason: MoveReason) {
        if let Some(var) = self.get_var_mut(name) {
            var.state = VarState::Moved {
                move_span: span,
                reason,
            };
        }
    }

    fn mark_field_moved(&mut self, base: &'hir str, field: &'hir str, span: Span) {
        if let Some(var) = self.get_var_mut(base) {
            match &mut var.state {
                VarState::PartiallyMoved {
                    moved_fields,
                    move_span,
                } => {
                    moved_fields.insert(field);
                    *move_span = span;
                }
                VarState::Valid | VarState::ConditionallyMoved { .. } => {
                    let mut fields = HashSet::new();
                    fields.insert(field);
                    var.state = VarState::PartiallyMoved {
                        moved_fields: fields,
                        move_span: span,
                    };
                }
                _ => {}
            }
        }
    }

    fn mark_deleted(&mut self, name: &'hir str, span: Span) {
        if let Some(var) = self.get_var_mut(name) {
            var.state = VarState::Deleted { delete_span: span };
        }
    }

    fn is_trivial(&self, ty: &HirTy<'hir>) -> bool {
        matches!(
            ty,
            HirTy::Integer(_)
                | HirTy::Float(_)
                | HirTy::UnsignedInteger(_)
                | HirTy::Boolean(_)
                | HirTy::Unit(_)
                | HirTy::Char(_)
                | HirTy::PtrTy(_)
        )
    }

    fn is_copyable(&self, ty: &HirTy<'hir>) -> bool {
        if self.is_trivial(ty) {
            return true;
        }

        match ty {
            HirTy::String(_) => true,
            HirTy::InlineArray(arr) => self.is_copyable(arr.inner),
            HirTy::Named(named) => self
                .signature
                .structs
                .get(named.name)
                .is_some_and(|s| s.copy_constructor.is_some()),
            HirTy::Generic(generic) => self
                .signature
                .structs
                .get(generic.name)
                .is_some_and(|s| s.copy_constructor.is_some()),
            _ => false,
        }
    }

    fn should_auto_delete_type(&self, ty: &HirTy<'hir>) -> bool {
        match ty {
            HirTy::String(_) => true,
            HirTy::Named(named) => self
                .signature
                .structs
                .get(named.name)
                .is_some_and(|s| s.destructor.is_some()),
            HirTy::Generic(generic) => self
                .signature
                .structs
                .get(generic.name)
                .is_some_and(|s| s.destructor.is_some()),
            HirTy::InlineArray(arr) => self.should_auto_delete_type(arr.inner),
            _ => false,
        }
    }

    fn analyze_function(&mut self, func: &mut HirFunction<'hir>) -> HirResult<()> {
        self.scopes.clear();
        self.enter_scope();

        for param in &func.signature.params {
            self.declare_var(param.name, param.ty, true);
        }

        self.transform_block(&mut func.body)?;
        self.exit_scope();
        Ok(())
    }

    fn analyze_class(&mut self, class: &mut HirStruct<'hir>) -> HirResult<()> {
        self.validate_ctor_notice(class);

        for method in &mut class.methods {
            self.analyze_method(method)?;
        }

        self.analyze_constructor(&mut class.constructor)?;
        if let Some(copy_ctor) = &mut class.copy_constructor {
            self.analyze_constructor(copy_ctor)?;
        }
        if let Some(move_ctor) = &mut class.move_constructor {
            self.analyze_constructor(move_ctor)?;
        }
        if let Some(destructor) = &mut class.destructor {
            self.analyze_constructor(destructor)?;
        }

        Ok(())
    }

    fn validate_ctor_notice(&mut self, class: &HirStruct<'hir>) {
        let has_temporary_ctor_shapes =
            class.copy_constructor.is_some() || class.move_constructor.is_some();
        if has_temporary_ctor_shapes && !self.emitted_ctor_warning {
            eprintln!("┌─────────────────────────────────────────────────┐");
            eprintln!("│ NOTICE: Temporary Constructor Signatures        │");
            eprintln!("├─────────────────────────────────────────────────┤");
            eprintln!("│ Copy: Foo(from: *const Foo)                     │");
            eprintln!("│ Move: Foo(from: *Foo)                           │");
            eprintln!("│                                                 │");
            eprintln!("│ These will change to use references in v0.9+    │");
            eprintln!("└─────────────────────────────────────────────────┘");
            self.emitted_ctor_warning = true;
        }
    }

    fn analyze_method(&mut self, method: &mut HirStructMethod<'hir>) -> HirResult<()> {
        self.scopes.clear();
        self.enter_scope();

        for param in &method.signature.params {
            self.declare_var(param.name, param.ty, true);
        }

        self.transform_block(&mut method.body)?;
        self.exit_scope();
        Ok(())
    }

    fn analyze_constructor(&mut self, ctor: &mut HirStructConstructor<'hir>) -> HirResult<()> {
        self.scopes.clear();
        self.enter_scope();

        for param in &ctor.params {
            self.declare_var(param.name, param.ty, true);
        }

        self.transform_block(&mut ctor.body)?;
        self.exit_scope();
        Ok(())
    }

    fn transform_block(&mut self, block: &mut HirBlock<'hir>) -> HirResult<()> {
        self.enter_scope();

        let old_statements = std::mem::take(&mut block.statements);
        let mut new_statements = Vec::with_capacity(old_statements.len() + 4);

        for mut stmt in old_statements {
            self.analyze_statement(&mut stmt)?;

            if matches!(stmt, HirStatement::Return(_)) {
                let mut deletes = self.build_scope_auto_deletes(stmt.span());
                new_statements.append(&mut deletes);
                new_statements.push(stmt);
            } else {
                new_statements.push(stmt);
            }
        }

        let mut tail_deletes = self.build_scope_auto_deletes(block.span);
        new_statements.append(&mut tail_deletes);

        block.statements = new_statements;
        self.exit_scope();
        Ok(())
    }

    fn analyze_statement(&mut self, stmt: &mut HirStatement<'hir>) -> HirResult<()> {
        match stmt {
            HirStatement::Let(let_stmt) => {
                self.analyze_expr(&mut let_stmt.value)?;
                self.copy_by_default(&mut let_stmt.value, let_stmt.ty, let_stmt.span)?;
                self.declare_var(let_stmt.name, let_stmt.ty, false);
            }
            HirStatement::Const(const_stmt) => {
                self.analyze_expr(&mut const_stmt.value)?;
                self.copy_by_default(&mut const_stmt.value, const_stmt.ty, const_stmt.span)?;
                self.declare_var(const_stmt.name, const_stmt.ty, false);
            }
            HirStatement::Assign(assign_stmt) => {
                self.analyze_expr(&mut assign_stmt.dst)?;
                self.analyze_expr(&mut assign_stmt.val)?;
                self.copy_by_default(&mut assign_stmt.val, assign_stmt.ty, assign_stmt.span)?;
            }
            HirStatement::Expr(expr_stmt) => {
                self.analyze_expr(&mut expr_stmt.expr)?;
            }
            HirStatement::Return(ret_stmt) => {
                self.analyze_expr(&mut ret_stmt.value)?;
                self.copy_by_default(&mut ret_stmt.value, ret_stmt.ty, ret_stmt.span)?;
            }
            HirStatement::IfElse(if_stmt) => {
                self.analyze_expr(&mut if_stmt.condition)?;

                let before = self.scopes.clone();

                self.transform_block(&mut if_stmt.then_branch)?;
                let then_state = self.scopes.clone();

                self.scopes = before.clone();
                if let Some(else_branch) = &mut if_stmt.else_branch {
                    self.transform_block(else_branch)?;
                }
                let else_state = self.scopes.clone();

                self.merge_branch_states(&before, &then_state, &else_state);
            }
            HirStatement::While(while_stmt) => {
                self.analyze_expr(&mut while_stmt.condition)?;
                let before = self.scopes.clone();
                self.transform_block(&mut while_stmt.body)?;
                let after = self.scopes.clone();
                self.merge_loop_states(&before, &after);
            }
            HirStatement::Block(block) => {
                self.transform_block(block)?;
            }
            HirStatement::Break(_) | HirStatement::Continue(_) => {}
        }

        Ok(())
    }

    fn analyze_expr(&mut self, expr: &mut HirExpr<'hir>) -> HirResult<()> {
        match expr {
            HirExpr::Ident(ident) => self.check_variable_access(ident)?,

            HirExpr::HirBinaryOperation(bin_op) => {
                self.analyze_expr(&mut bin_op.lhs)?;
                self.analyze_expr(&mut bin_op.rhs)?;
            }

            HirExpr::Unary(unary) => {
                self.analyze_expr(&mut unary.expr)?;
            }

            HirExpr::Call(call) => {
                self.analyze_expr(&mut call.callee)?;
                for arg in &mut call.args {
                    self.analyze_expr(arg)?;
                }
                self.apply_call_ownership(call)?;
            }

            HirExpr::FieldAccess(field_access) => {
                self.analyze_expr(&mut field_access.target)?;
                self.check_field_access_state(field_access);
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

            HirExpr::Delete(delete_expr) => {
                self.analyze_expr(&mut delete_expr.expr)?;
                if let Some((name, _)) = self.extract_move_target(&delete_expr.expr) {
                    self.mark_deleted(name, delete_expr.span);
                }
            }

            HirExpr::Copy(copy_expr) => {
                self.analyze_expr(&mut copy_expr.expr)?;
            }

            HirExpr::IntrinsicCall(intrinsic) => {
                for arg in &mut intrinsic.args {
                    self.analyze_expr(arg)?;
                }
            }

            HirExpr::IntegerLiteral(_)
            | HirExpr::FloatLiteral(_)
            | HirExpr::UnsignedIntegerLiteral(_)
            | HirExpr::BooleanLiteral(_)
            | HirExpr::UnitLiteral(_)
            | HirExpr::CharLiteral(_)
            | HirExpr::StringLiteral(_)
            | HirExpr::NullLiteral(_)
            | HirExpr::ThisLiteral(_)
            | HirExpr::StaticAccess(_) => {}
        }

        Ok(())
    }

    fn check_variable_access(&mut self, ident: &HirIdentExpr<'hir>) -> HirResult<()> {
        if let Some(var) = self.get_var(ident.name) {
            match &var.state {
                VarState::Moved { move_span, reason } => {
                    let warning =
                        self.use_after_move_warning(ident.name, ident.span, *move_span, reason);
                    self.warnings.push(warning);
                }
                VarState::ConditionallyMoved { move_span } => {
                    let warning = self.use_after_move_warning(
                        ident.name,
                        ident.span,
                        *move_span,
                        &MoveReason::MutablePointerParameter,
                    );
                    self.warnings.push(warning);
                }
                VarState::PartiallyMoved { move_span, .. } => {
                    let warning = self.use_after_move_warning(
                        ident.name,
                        ident.span,
                        *move_span,
                        &MoveReason::MoveConstructorCall,
                    );
                    self.warnings.push(warning);
                }
                VarState::Deleted { delete_span } => {
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
                VarState::Valid => {}
            }
        }

        Ok(())
    }

    fn check_field_access_state(&mut self, field_access: &HirFieldAccessExpr<'hir>) {
        if let HirExpr::Ident(base_ident) = field_access.target.as_ref() {
            if let Some(var) = self.get_var(base_ident.name) {
                if let VarState::PartiallyMoved {
                    moved_fields,
                    move_span,
                } = &var.state
                {
                    if moved_fields.contains(field_access.field.name) {
                        let warning = self.use_after_move_warning(
                            base_ident.name,
                            field_access.span,
                            *move_span,
                            &MoveReason::MoveConstructorCall,
                        );
                        self.warnings.push(warning);
                    }
                }
            }
        }
    }

    fn apply_call_ownership(&mut self, call: &mut HirFunctionCallExpr<'hir>) -> HirResult<()> {
        let move_reason = if self.is_std_move_call(call) {
            Some(MoveReason::ExplicitMoveCall)
        } else if self.is_move_constructor_call(call) {
            Some(MoveReason::MoveConstructorCall)
        } else {
            None
        };

        for idx in 0..call.args.len() {
            let param_ty = call.args_ty.get(idx).copied();
            let is_mut_ptr_param = matches!(param_ty, Some(HirTy::PtrTy(p)) if !p.is_const);

            let should_move = move_reason.is_some() || is_mut_ptr_param;

            if should_move {
                self.ensure_not_moving_from_const_ptr(&call.args[idx])?;

                if let Some((var_name, field_name)) = self.extract_move_target(&call.args[idx]) {
                    if let Some(field) = field_name {
                        self.mark_field_moved(var_name, field, call.args[idx].span());
                    } else {
                        self.mark_moved(
                            var_name,
                            call.args[idx].span(),
                            move_reason
                                .clone()
                                .unwrap_or(MoveReason::MutablePointerParameter),
                        );
                    }
                }
                continue;
            }

            if !matches!(param_ty, Some(HirTy::PtrTy(_))) {
                let fallback_ty = call.args[idx].ty();
                let arg_span = call.args[idx].span();
                self.copy_by_default(&mut call.args[idx], fallback_ty, arg_span)?;
            }
        }

        Ok(())
    }

    fn ensure_not_moving_from_const_ptr(&self, expr: &HirExpr<'hir>) -> HirResult<()> {
        if let HirExpr::Unary(unary) = expr {
            if unary.op == Some(HirUnaryOp::Deref)
                && matches!(unary.expr.ty(), HirTy::PtrTy(p) if p.is_const)
            {
                let path = expr.span().path;
                let src = utils::get_file_content(path).unwrap();
                return Err(HirError::TypeIsNotMoveable(TypeIsNotMoveableError {
                    span: expr.span(),
                    ty_name: format!("{}", unary.expr.ty()),
                    src: NamedSource::new(path, src),
                }));
            }
        }

        Ok(())
    }

    fn extract_move_target(&self, expr: &HirExpr<'hir>) -> Option<(&'hir str, Option<&'hir str>)> {
        match expr {
            HirExpr::Ident(ident) => Some((ident.name, None)),
            HirExpr::FieldAccess(field_access) => {
                if let HirExpr::Ident(base) = field_access.target.as_ref() {
                    Some((base.name, Some(field_access.field.name)))
                } else {
                    None
                }
            }
            HirExpr::Unary(unary)
                if unary.op == Some(HirUnaryOp::AsRef) || unary.op == Some(HirUnaryOp::Deref) =>
            {
                self.extract_move_target(&unary.expr)
            }
            _ => None,
        }
    }

    // TODO: Remove "is_std" and replace it by "is_intrinsic"
    fn is_std_move_call(&self, call: &HirFunctionCallExpr<'hir>) -> bool {
        match call.callee.as_ref() {
            HirExpr::Ident(ident) => ident.name == "move",
            HirExpr::StaticAccess(access) => access.field.name == "move",
            _ => false,
        }
    }

    fn is_move_constructor_call(&self, call: &HirFunctionCallExpr<'hir>) -> bool {
        match call.callee.as_ref() {
            HirExpr::Ident(ident) => ident.name == "__move_ctor",
            HirExpr::StaticAccess(access) => access.field.name == "__move_ctor",
            _ => false,
        }
    }

    fn copy_by_default(
        &mut self,
        expr: &mut HirExpr<'hir>,
        expected_ty: &'hir HirTy<'hir>,
        span: Span,
    ) -> HirResult<()> {
        if self.is_explicit_move_expr(expr) || matches!(expr, HirExpr::Copy(_)) {
            return Ok(());
        }

        let source_ty = expr.ty();
        if self.is_trivial(source_ty) || self.is_trivial(expected_ty) {
            return Ok(());
        }

        if !self.is_copyable(source_ty) {
            let path = span.path;
            let src = utils::get_file_content(path).unwrap();
            return Err(HirError::TypeIsNotCopyable(TypeIsNotCopyableError {
                span,
                type_name: format!("{}", source_ty),
                src: NamedSource::new(path, src),
            }));
        }

        if !self.should_wrap_copy_expr(expr) {
            return Ok(());
        }

        let source_name = self.extract_copy_source_name(expr).unwrap_or("<tmp>");
        let old = expr.clone();
        *expr = HirExpr::Copy(HirCopyExpr {
            span,
            source_name,
            expr: Box::new(old),
            ty: source_ty,
        });

        Ok(())
    }

    fn should_wrap_copy_expr(&self, expr: &HirExpr<'hir>) -> bool {
        matches!(
            expr,
            HirExpr::Ident(_)
                | HirExpr::FieldAccess(_)
                | HirExpr::Indexing(_)
                | HirExpr::Unary(_)
                | HirExpr::ThisLiteral(_)
        )
    }

    fn extract_copy_source_name(&self, expr: &HirExpr<'hir>) -> Option<&'hir str> {
        match expr {
            HirExpr::Ident(ident) => Some(ident.name),
            HirExpr::FieldAccess(field) => {
                if let HirExpr::Ident(base) = field.target.as_ref() {
                    Some(base.name)
                } else {
                    None
                }
            }
            HirExpr::Unary(unary) => self.extract_copy_source_name(&unary.expr),
            _ => None,
        }
    }

    fn is_explicit_move_expr(&self, expr: &HirExpr<'hir>) -> bool {
        matches!(expr, HirExpr::Call(call) if self.is_std_move_call(call) || self.is_move_constructor_call(call))
    }

    fn build_scope_auto_deletes(&mut self, span: Span) -> Vec<HirStatement<'hir>> {
        let mut deletes = Vec::new();
        let mut names_to_delete: Vec<&'hir str> = Vec::new();

        if let Some(scope) = self.scopes.last() {
            for var in scope.vars.values() {
                if var.is_param {
                    continue;
                }
                if !self.should_auto_delete_type(var.ty) {
                    continue;
                }
                if matches!(var.state, VarState::Deleted { .. }) {
                    continue;
                }
                names_to_delete.push(var.name);
            }
        }

        for name in names_to_delete {
            if let Some(var) = self.get_var(name) {
                let ident = HirExpr::Ident(HirIdentExpr {
                    name,
                    span,
                    ty: var.ty,
                });
                deletes.push(HirStatement::Expr(HirExprStmt {
                    span,
                    expr: HirExpr::Delete(HirDeleteExpr {
                        span,
                        expr: Box::new(ident),
                    }),
                }));
            }
            self.mark_deleted(name, span);
        }

        deletes
    }

    fn merge_branch_states(
        &mut self,
        before: &[ScopeState<'hir>],
        then_state: &[ScopeState<'hir>],
        else_state: &[ScopeState<'hir>],
    ) {
        let depth = before.len().min(then_state.len()).min(else_state.len());

        for idx in 0..depth {
            let names: Vec<&'hir str> = before[idx].vars.keys().copied().collect();
            for name in names {
                let then_var = then_state[idx].vars.get(name);
                let else_var = else_state[idx].vars.get(name);

                let (Some(then_var), Some(else_var)) = (then_var, else_var) else {
                    continue;
                };

                let merged = match (&then_var.state, &else_var.state) {
                    (VarState::Moved { move_span, reason }, VarState::Moved { .. }) => {
                        VarState::Moved {
                            move_span: *move_span,
                            reason: reason.clone(),
                        }
                    }
                    (VarState::Deleted { delete_span }, VarState::Deleted { .. }) => {
                        VarState::Deleted {
                            delete_span: *delete_span,
                        }
                    }
                    (VarState::Moved { move_span, .. }, _)
                    | (_, VarState::Moved { move_span, .. }) => VarState::ConditionallyMoved {
                        move_span: *move_span,
                    },
                    _ => VarState::Valid,
                };

                if let Some(var) = self.scopes[idx].vars.get_mut(name) {
                    var.state = merged;
                }
            }
        }
    }

    fn merge_loop_states(&mut self, before: &[ScopeState<'hir>], after: &[ScopeState<'hir>]) {
        let depth = before.len().min(after.len());

        for idx in 0..depth {
            let names: Vec<&'hir str> = before[idx].vars.keys().copied().collect();
            for name in names {
                let before_var = before[idx].vars.get(name);
                let after_var = after[idx].vars.get(name);

                let (Some(before_var), Some(after_var)) = (before_var, after_var) else {
                    continue;
                };

                if matches!(before_var.state, VarState::Valid)
                    && let VarState::Moved { move_span, .. } = after_var.state
                    && let Some(current) = self.scopes[idx].vars.get_mut(name)
                {
                    current.state = VarState::ConditionallyMoved { move_span };
                }
            }
        }
    }

    fn use_after_move_warning(
        &self,
        var_name: &'hir str,
        access_span: Span,
        move_span: Span,
        reason: &MoveReason,
    ) -> ErrReport {
        let reason_str = match reason {
            MoveReason::ExplicitMoveCall => "explicit move call",
            MoveReason::MoveConstructorCall => "move constructor call",
            MoveReason::MutablePointerParameter => "mutable pointer parameter",
        };

        let path = access_span.path;
        let src = utils::get_file_content(path).unwrap();
        let mut report: ErrReport = UseAfterMoveWarning {
            src: NamedSource::new(path, src),
            access_span,
            move_span,
            var_name: var_name.to_string(),
        }
        .into();

        report = report.wrap_err(format!(
            "undefined behavior risk: variable was moved via {}",
            reason_str
        ));
        report
    }
}
