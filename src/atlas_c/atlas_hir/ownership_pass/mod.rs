use std::collections::HashMap;

use miette::NamedSource;

use crate::atlas_c::{
    atlas_hir::{
        HirModule,
        arena::HirArena,
        error::{
            CannotMoveFromRvalueError, HirError, HirResult, OwnershipAnalysisFailedError,
            TryingToAccessAConsumedValueError, TryingToAccessADeletedValueError,
            TryingToAccessAMovedValueError, TryingToAccessAPotentiallyConsumedValueError,
            TryingToAccessAPotentiallyDeletedValueError, TryingToAccessAPotentiallyMovedValueError,
            TypeIsNotTriviallyCopyableError,
        },
        expr::{HirDeleteExpr, HirExpr, HirIdentExpr, HirUnaryOp},
        monomorphization_pass::MonomorphizationPass,
        pretty_print::HirPrettyPrinter,
        signature::{HirFunctionParameterSignature, HirModuleSignature, HirStructMethodModifier},
        stmt::{HirAssignStmt, HirBlock, HirExprStmt, HirStatement},
        ty::HirTy,
        warning::{HirWarning, UnusedResultFromFunctionWarning},
    },
    utils::{self, Span},
};

#[derive(Debug, Clone, PartialEq, Eq)]
enum OwnershipState {
    Alive,
    Deleted(Vec<Span>),
    Moved(Vec<Span>),
    Consumed(Vec<Span>),
    ConditionallyDeleted(Vec<Span>),
    ConditionallyMoved(Vec<Span>),
    ConditionallyConsumed(Vec<Span>),
}

#[derive(Debug, Clone, Copy)]
struct LocalVar<'hir> {
    name: &'hir str,
    ty: &'hir HirTy<'hir>,
    is_compiler_temp: bool,
}

#[derive(Debug, Clone, Default)]
struct ScopeFrame<'hir> {
    locals: Vec<LocalVar<'hir>>,
    states: HashMap<&'hir str, OwnershipState>,
}

pub struct HirOwnershipPass<'hir> {
    _hir_arena: &'hir HirArena<'hir>,
    signature: HirModuleSignature<'hir>,
    errors: Vec<HirError>,
    pub warnings: Vec<HirWarning>,
}

impl<'hir> HirOwnershipPass<'hir> {
    pub fn new(hir_arena: &'hir HirArena<'hir>, signature: &HirModuleSignature<'hir>) -> Self {
        Self {
            _hir_arena: hir_arena,
            signature: signature.clone(),
            errors: Vec::new(),
            warnings: Vec::new(),
        }
    }

    pub fn run(&mut self, hir_module: &mut HirModule<'hir>) -> HirResult<()> {
        self.errors.clear();

        for function in hir_module.body.functions.values_mut() {
            self.run_ownership_for_body(&mut function.body, &function.signature.params);
        }

        for strukt in hir_module.body.structs.values_mut() {
            for method in &mut strukt.methods {
                self.run_ownership_for_method_body(
                    &mut method.body,
                    &method.signature.params,
                    method.signature.modifier.clone(),
                    self._hir_arena
                        .types()
                        .get_named_ty(strukt.name, strukt.name_span),
                );
            }

            for operator in &mut strukt.operators {
                self.run_ownership_for_body(&mut operator.body, &operator.signature.params);
            }

            if let Some(destructor) = &mut strukt.destructor {
                self.run_ownership_for_body(&mut destructor.body, &[]);
            }
        }

        if !self.errors.is_empty() {
            let errors = std::mem::take(&mut self.errors);
            return Err(HirError::OwnershipAnalysisFailed(
                OwnershipAnalysisFailedError {
                    error_count: errors.len(),
                    errors,
                },
            ));
        }

        Ok(())
    }

    fn run_ownership_for_method_body(
        &mut self,
        body: &mut HirBlock<'hir>,
        params: &[HirFunctionParameterSignature<'hir>],
        method_modifier: HirStructMethodModifier,
        struct_ty: &'hir HirTy<'hir>,
    ) {
        let mut scope_stack = vec![ScopeFrame::default()];
        match method_modifier {
            HirStructMethodModifier::Consuming => {
                self.register_local(
                    &mut scope_stack,
                    LocalVar {
                        name: "this",
                        ty: struct_ty,
                        is_compiler_temp: false,
                    },
                );
            }
            HirStructMethodModifier::Const => {
                self.register_local(
                    &mut scope_stack,
                    LocalVar {
                        name: "this",
                        ty: self
                            ._hir_arena
                            .types()
                            .get_ptr_ty(struct_ty, true, Span::default()),
                        is_compiler_temp: false,
                    },
                );
            }
            HirStructMethodModifier::Mutable => {
                self.register_local(
                    &mut scope_stack,
                    LocalVar {
                        name: "this",
                        ty: self
                            ._hir_arena
                            .types()
                            .get_ptr_ty(struct_ty, false, Span::default()),
                        is_compiler_temp: false,
                    },
                );
            }
            HirStructMethodModifier::Static => {}
        }
        for param in params {
            self.register_local(
                &mut scope_stack,
                LocalVar {
                    name: param.name,
                    ty: param.ty,
                    is_compiler_temp: self.is_compiler_temp_name(param.name),
                },
            );
        }

        let transformed_body = self.transform_block(body.clone(), &mut scope_stack);

        *body = transformed_body;
    }

    fn run_ownership_for_body(
        &mut self,
        body: &mut HirBlock<'hir>,
        params: &[HirFunctionParameterSignature<'hir>],
    ) {
        let mut scope_stack = vec![ScopeFrame::default()];
        for param in params {
            self.register_local(
                &mut scope_stack,
                LocalVar {
                    name: param.name,
                    ty: param.ty,
                    is_compiler_temp: self.is_compiler_temp_name(param.name),
                },
            );
        }

        let transformed_body = self.transform_block(body.clone(), &mut scope_stack);

        *body = transformed_body;
    }

    fn transform_block(
        &mut self,
        block: HirBlock<'hir>,
        scope_stack: &mut Vec<ScopeFrame<'hir>>,
    ) -> HirBlock<'hir> {
        scope_stack.push(ScopeFrame::default());

        let mut statements = Vec::with_capacity(block.statements.len());
        for statement in block.statements {
            match statement {
                HirStatement::Block(inner) => {
                    let transformed = self.transform_block(inner, scope_stack);
                    statements.push(HirStatement::Block(transformed));
                }
                HirStatement::Return(ret) => {
                    if let Some(expr) = &ret.value {
                        self.validate_expr(expr, scope_stack);

                        let excluded = self.returned_identifier_name(expr);
                        statements.extend(self.collect_scope_drops(scope_stack, excluded));
                    }
                    statements.push(HirStatement::Return(ret));
                }
                HirStatement::Expr(expr_stmt) => {
                    self.validate_expr(&expr_stmt.expr, scope_stack);
                    if let Some((name, span)) = self.deleted_identifier(&expr_stmt.expr) {
                        self.mark_deleted(scope_stack, name, span);
                    }

                    if self.should_auto_delete(expr_stmt.expr.ty())
                        && let HirExpr::Call(c) = self.strip_noop_unary(&expr_stmt.expr)
                    {
                        let path = expr_stmt.span.path;
                        let src = utils::get_file_content(path).unwrap();
                        let mut pretty_printer = HirPrettyPrinter::new();
                        pretty_printer.print_expr(&c.callee);
                        let func_name = pretty_printer.get_output();
                        self.warnings.push(HirWarning::UnusedResultFromFunction(
                            UnusedResultFromFunctionWarning {
                                func_name,
                                span: expr_stmt.span,
                                src: NamedSource::new(path, src),
                            },
                        ));
                        statements.push(HirStatement::Expr(HirExprStmt {
                            span: expr_stmt.span,
                            expr: HirExpr::Delete(HirDeleteExpr {
                                span: expr_stmt.span,
                                expr: Box::new(expr_stmt.expr),
                            }),
                        }));
                    } else {
                        statements.push(HirStatement::Expr(expr_stmt));
                    }
                }
                HirStatement::Let(let_stmt) => {
                    self.validate_expr(&let_stmt.value, scope_stack);
                    self.record_result(self.ensure_identifier_copy_allowed(
                        scope_stack,
                        &let_stmt.value,
                        Some(let_stmt.name),
                    ));

                    let consumed_temp = self.consumed_compiler_temp_from_value(
                        scope_stack,
                        &let_stmt.value,
                        let_stmt.name,
                        let_stmt.ty,
                    );
                    self.register_local(
                        scope_stack,
                        LocalVar {
                            name: let_stmt.name,
                            ty: let_stmt.ty,
                            is_compiler_temp: self.is_compiler_temp_name(let_stmt.name),
                        },
                    );
                    if let Some((temp_name, temp_span)) = consumed_temp {
                        self.mark_deleted(scope_stack, temp_name, temp_span);
                    }
                    statements.push(HirStatement::Let(let_stmt));
                }
                HirStatement::Const(const_stmt) => {
                    self.validate_expr(&const_stmt.value, scope_stack);
                    self.record_result(self.ensure_identifier_copy_allowed(
                        scope_stack,
                        &const_stmt.value,
                        Some(const_stmt.name),
                    ));

                    let consumed_temp = self.consumed_compiler_temp_from_value(
                        scope_stack,
                        &const_stmt.value,
                        const_stmt.name,
                        const_stmt.ty,
                    );
                    self.register_local(
                        scope_stack,
                        LocalVar {
                            name: const_stmt.name,
                            ty: const_stmt.ty,
                            is_compiler_temp: self.is_compiler_temp_name(const_stmt.name),
                        },
                    );
                    if let Some((temp_name, temp_span)) = consumed_temp {
                        self.mark_deleted(scope_stack, temp_name, temp_span);
                    }
                    statements.push(HirStatement::Const(const_stmt));
                }
                HirStatement::Assign(assign_stmt) => {
                    self.validate_expr(&assign_stmt.dst, scope_stack);
                    self.validate_expr(&assign_stmt.val, scope_stack);
                    let dst_name = match self.strip_noop_unary(&assign_stmt.dst) {
                        HirExpr::Ident(id) => Some(id.name),
                        _ => None,
                    };
                    self.record_result(self.ensure_identifier_copy_allowed(
                        scope_stack,
                        &assign_stmt.val,
                        dst_name,
                    ));
                    let consumed_temp =
                        self.consumed_compiler_temp_from_assign(scope_stack, &assign_stmt);
                    if let Some(delete_stmt) =
                        self.pre_delete_before_assign(scope_stack, &assign_stmt)
                    {
                        statements.push(delete_stmt);
                    }
                    self.mark_assigned_alive(scope_stack, &assign_stmt);
                    if let Some((temp_name, temp_span)) = consumed_temp {
                        self.mark_deleted(scope_stack, temp_name, temp_span);
                    }

                    statements.push(HirStatement::Assign(assign_stmt));
                }
                HirStatement::IfElse(mut if_else) => {
                    self.validate_expr(&if_else.condition, scope_stack);
                    let mut then_stack = scope_stack.clone();
                    if_else.then_branch =
                        self.transform_block(if_else.then_branch, &mut then_stack);
                    let then_terminates =
                        self.statements_guaranteed_return(&if_else.then_branch.statements);

                    let mut else_stack: Option<Vec<ScopeFrame<'hir>>> = None;
                    let mut else_terminates = false;
                    if let Some(else_branch) = if_else.else_branch.take() {
                        let mut local_else_stack = scope_stack.clone();
                        let transformed_else =
                            self.transform_block(else_branch, &mut local_else_stack);
                        else_terminates =
                            self.statements_guaranteed_return(&transformed_else.statements);
                        if_else.else_branch = Some(transformed_else);
                        else_stack = Some(local_else_stack);
                    }

                    self.merge_control_flow_states(
                        scope_stack,
                        &then_stack,
                        else_stack.as_deref(),
                        then_terminates,
                        else_terminates,
                    );
                    statements.push(HirStatement::IfElse(if_else));
                }
                HirStatement::While(mut while_stmt) => {
                    self.validate_expr(&while_stmt.condition, scope_stack);
                    let mut loop_stack = scope_stack.clone();
                    while_stmt.body = self.transform_block(while_stmt.body, &mut loop_stack);
                    self.merge_control_flow_states(scope_stack, &loop_stack, None, false, false);
                    statements.push(HirStatement::While(while_stmt));
                }
                HirStatement::Break(span) => statements.push(HirStatement::Break(span)),
                HirStatement::Continue(span) => statements.push(HirStatement::Continue(span)),
            }
        }

        // Block exit RAII: destroy surviving locals declared in this scope in reverse order.
        // When the block has a guaranteed return path at its tail (direct return or
        // if/else where both branches return), return handling already emits required
        // drops and preserves returned ownership transfer.
        if !self.statements_guaranteed_return(&statements)
            && let Some(frame) = scope_stack.last()
        {
            let mut tail_drops = Vec::new();
            for local in frame.locals.iter().rev() {
                if !matches!(frame.states.get(local.name), Some(OwnershipState::Alive)) {
                    continue;
                }
                if self.should_auto_delete_local(local) {
                    tail_drops.push(self.delete_stmt_for(block.span, local.name, local.ty));
                }
            }
            statements.extend(tail_drops);
        }

        scope_stack.pop();
        HirBlock {
            span: block.span,
            statements,
        }
    }

    fn statements_guaranteed_return(&self, statements: &[HirStatement<'hir>]) -> bool {
        match statements.last() {
            Some(stmt) => self.statement_guaranteed_return(stmt),
            None => false,
        }
    }

    fn statement_guaranteed_return(&self, stmt: &HirStatement<'hir>) -> bool {
        match stmt {
            HirStatement::Return(_) => true,
            HirStatement::Block(block) => self.statements_guaranteed_return(&block.statements),
            HirStatement::IfElse(if_else) => {
                let then_returns =
                    self.statements_guaranteed_return(&if_else.then_branch.statements);
                let else_returns = if_else.else_branch.as_ref().is_some_and(|else_block| {
                    self.statements_guaranteed_return(&else_block.statements)
                });
                then_returns && else_returns
            }
            _ => false,
        }
    }

    fn record_result(&mut self, result: HirResult<()>) {
        if let Err(err) = result {
            self.errors.push(err);
        }
    }

    fn should_auto_delete(&self, ty: &'hir HirTy<'hir>) -> bool {
        match ty {
            HirTy::PtrTy(_) => false,
            HirTy::Named(named) => self
                .signature
                .structs
                .get(named.name)
                .is_some_and(|sig| sig.destructor.is_some()),
            HirTy::Generic(generic) => {
                let sig = self
                    .signature
                    .structs
                    .get(generic.name)
                    .copied()
                    .or_else(|| {
                        self.signature
                            .structs
                            .values()
                            .find(|sig| {
                                sig.pre_mangled_ty.is_some_and(|pre| {
                                    pre.name == generic.name && pre.inner == generic.inner
                                })
                            })
                            .copied()
                    });
                sig.is_some_and(|sig| sig.destructor.is_some())
            }
            HirTy::InlineArray(arr) => self.should_auto_delete(arr.inner),
            _ => false,
        }
    }

    fn should_auto_delete_local(&self, local: &LocalVar<'hir>) -> bool {
        self.should_auto_delete(local.ty)
    }

    fn ensure_identifier_copy_allowed(
        &self,
        scope_stack: &[ScopeFrame<'hir>],
        value: &HirExpr<'hir>,
        dst_name: Option<&'hir str>,
    ) -> HirResult<()> {
        let (src_name, src_span) = match self.strip_noop_unary(value) {
            HirExpr::Ident(id) => (id.name, id.span),
            HirExpr::ThisLiteral(t) => ("this", t.span),
            _ => return Ok(()),
        };

        if dst_name.is_some_and(|dst| dst == src_name) {
            return Ok(());
        }

        let Some(src_local) = self.find_local(scope_stack, src_name) else {
            return Ok(());
        };

        if let Some(state) = self.find_state(scope_stack, src_name)
            && !matches!(state, OwnershipState::Alive)
        {
            return Ok(());
        }

        // Compiler temporaries can transfer ownership without explicit copy().
        if src_local.is_compiler_temp {
            return Ok(());
        }

        if src_local.ty.is_trivially_copyable(&self.signature) {
            return Ok(());
        }

        let path = src_span.path;
        let src_text = utils::get_file_content(path).unwrap_or_default();
        let name = if let Some(sig) = self.signature.structs.get(src_local.name) {
            if let Some(pre) = sig.pre_mangled_ty {
                format!("{}", HirTy::Generic(pre.clone()))
            } else {
                format!("{}", src_local.ty)
            }
        } else {
            format!("{}", src_local.ty)
        };
        Err(HirError::TypeIsNotTriviallyCopyable(
            TypeIsNotTriviallyCopyableError {
                src: NamedSource::new(path, src_text),
                span: src_span,
                type_name: name,
            },
        ))
    }

    fn consumed_compiler_temp_from_assign(
        &self,
        scope_stack: &[ScopeFrame<'hir>],
        assign: &HirAssignStmt<'hir>,
    ) -> Option<(&'hir str, crate::atlas_c::utils::Span)> {
        let dst_name = match self.strip_noop_unary(&assign.dst) {
            HirExpr::Ident(id) => id.name,
            HirExpr::ThisLiteral(_) => "this",
            _ => return None,
        };
        let (src_name, src_span) = match self.strip_noop_unary(&assign.val) {
            HirExpr::Ident(id) => (id.name, id.span),
            HirExpr::ThisLiteral(t) => ("this", t.span),
            _ => return None,
        };

        if src_name == dst_name {
            return None;
        }

        let src_local = self.find_local(scope_stack, src_name)?;
        if !src_local.is_compiler_temp {
            return None;
        }

        let dst_local = self.find_local(scope_stack, dst_name)?;
        if dst_local.is_compiler_temp {
            return None;
        }

        if !std::ptr::eq(src_local.ty, dst_local.ty) {
            return None;
        }

        Some((src_name, src_span))
    }

    fn consumed_compiler_temp_from_value(
        &self,
        scope_stack: &[ScopeFrame<'hir>],
        value: &HirExpr<'hir>,
        dst_name: &'hir str,
        dst_ty: &'hir HirTy<'hir>,
    ) -> Option<(&'hir str, crate::atlas_c::utils::Span)> {
        let (src_name, src_span) = match self.strip_noop_unary(value) {
            HirExpr::Ident(id) => (id.name, id.span),
            HirExpr::ThisLiteral(t) => ("this", t.span),
            _ => return None,
        };

        if src_name == dst_name {
            return None;
        }

        let src_local = self.find_local(scope_stack, src_name)?;
        if !src_local.is_compiler_temp {
            return None;
        }

        if self.is_compiler_temp_name(dst_name) {
            return None;
        }

        if !std::ptr::eq(src_local.ty, dst_ty) {
            return None;
        }

        Some((src_name, src_span))
    }

    fn find_local<'a>(
        &self,
        scope_stack: &'a [ScopeFrame<'hir>],
        name: &'hir str,
    ) -> Option<&'a LocalVar<'hir>> {
        for frame in scope_stack.iter().rev() {
            if let Some(local) = frame.locals.iter().rev().find(|v| v.name == name) {
                return Some(local);
            }
        }
        None
    }

    fn find_state(
        &self,
        scope_stack: &[ScopeFrame<'hir>],
        name: &'hir str,
    ) -> Option<OwnershipState> {
        for frame in scope_stack.iter().rev() {
            if let Some(state) = frame.states.get(name).cloned() {
                return Some(state);
            }
        }
        None
    }

    fn returned_identifier_name(&self, expr: &HirExpr<'hir>) -> Option<&'hir str> {
        match self.strip_noop_unary(expr) {
            HirExpr::Ident(id) => Some(id.name),
            HirExpr::ThisLiteral(_) => Some("this"),
            _ => None,
        }
    }

    fn consuming_method_receiver(
        &self,
        expr: &HirExpr<'hir>,
        scope_stack: &[ScopeFrame<'hir>],
    ) -> Option<(&'hir str, crate::atlas_c::utils::Span, &'hir HirTy<'hir>)> {
        let HirExpr::Call(call) = self.strip_noop_unary(expr) else {
            return None;
        };

        let HirExpr::FieldAccess(field_access) = self.strip_noop_unary(&call.callee) else {
            return None;
        };

        let receiver = self.strip_noop_unary(&field_access.target);
        let HirExpr::Ident(receiver_id) = receiver else {
            return None;
        };

        let receiver_local = self.find_local(scope_stack, receiver_id.name)?;
        let class_name = self.class_name_from_receiver_ty(field_access.target.ty())?;
        let class = self.signature.structs.get(class_name)?;
        let method = class.methods.get(field_access.field.name)?;
        if method.modifier != HirStructMethodModifier::Consuming {
            return None;
        }

        Some((receiver_local.name, field_access.span, receiver_local.ty))
    }

    fn class_name_from_receiver_ty(&self, ty: &'hir HirTy<'hir>) -> Option<&'hir str> {
        match ty {
            HirTy::Named(named) => Some(named.name),
            HirTy::Generic(generic) => {
                let mangled =
                    MonomorphizationPass::generate_mangled_name(self._hir_arena, generic, "struct");
                self.signature
                    .structs
                    .contains_key(mangled)
                    .then_some(mangled)
            }
            HirTy::PtrTy(ptr) => self.class_name_from_receiver_ty(ptr.inner),
            _ => None,
        }
    }

    fn validate_expr(&mut self, expr: &HirExpr<'hir>, scope_stack: &mut Vec<ScopeFrame<'hir>>) {
        match self.strip_noop_unary(expr) {
            HirExpr::Ident(id) => {
                self.record_result(self.validate_identifier_use(scope_stack, id.name, id.span))
            }
            HirExpr::ThisLiteral(t) => {
                self.record_result(self.validate_identifier_use(scope_stack, "this", t.span));
            }
            HirExpr::Delete(del) => self.validate_expr(&del.expr, scope_stack),
            HirExpr::Unary(unary) => self.validate_expr(&unary.expr, scope_stack),
            HirExpr::Casting(cast) => self.validate_expr(&cast.expr, scope_stack),
            HirExpr::HirBinaryOperation(binary) => {
                self.validate_expr(&binary.lhs, scope_stack);
                self.validate_expr(&binary.rhs, scope_stack);
            }
            HirExpr::Call(call) => {
                self.validate_expr(&call.callee, scope_stack);
                for arg in &call.args {
                    self.validate_expr(arg, scope_stack);
                    self.record_result(self.ensure_identifier_copy_allowed(scope_stack, arg, None));
                    self.mark_compiler_temp_consumed(scope_stack, arg);
                }
                if let Some((name, span, _ty)) = self.consuming_method_receiver(expr, scope_stack) {
                    self.mark_consumed(scope_stack, name, span);
                }
            }
            HirExpr::ListLiteral(list) => {
                for item in &list.items {
                    self.validate_expr(item, scope_stack);
                }
            }
            HirExpr::ListLiteralWithSize(list) => {
                self.validate_expr(&list.item, scope_stack);
                // list.size > 1, we need to ensure the type isn't being moved into the list multiple times.
                let size = list.size_as_usize().unwrap_or(0);
                if size > 1 {
                    self.record_result(self.ensure_identifier_copy_allowed(
                        scope_stack,
                        &list.item,
                        None,
                    ));
                    // size > 1 needs N independent copies — do NOT mark the temp consumed here.
                    // ensure_identifier_copy_allowed's `is_compiler_temp` bypass is itself wrong
                    // for this branch (a bare move can't produce N copies); that's a second,
                    // separate bug to fix at some point — not covered by today's patch.
                } else {
                    self.mark_compiler_temp_consumed(scope_stack, &list.item);
                }
            }
            HirExpr::ObjLiteral(obj) => {
                for field in &obj.fields {
                    self.validate_expr(&field.value, scope_stack);
                    self.record_result(self.ensure_identifier_copy_allowed(
                        scope_stack,
                        &field.value,
                        None,
                    ));
                    self.mark_compiler_temp_consumed(scope_stack, &field.value);
                }
            }
            HirExpr::FieldAccess(field) => self.validate_expr(&field.target, scope_stack),
            HirExpr::Indexing(indexing) => {
                self.validate_expr(&indexing.target, scope_stack);
                self.validate_expr(&indexing.index, scope_stack);
            }
            HirExpr::StaticAccess(_) => {}
            HirExpr::IntrinsicCall(intrinsic) => {
                for arg in &intrinsic.args {
                    self.validate_expr(arg, scope_stack);
                }
                if intrinsic.name == "std::move"
                    && let Some(first_arg) = intrinsic.args.first()
                {
                    let res = self.validate_move_argument(scope_stack, first_arg);
                    self.record_result(res);
                }
            }
            HirExpr::FloatLiteral(_)
            | HirExpr::CharLiteral(_)
            | HirExpr::IntegerLiteral(_)
            | HirExpr::UnitLiteral(_)
            | HirExpr::BooleanLiteral(_)
            | HirExpr::UnsignedIntegerLiteral(_)
            | HirExpr::StringLiteral(_)
            | HirExpr::NullLiteral(_) => {}
        }
    }

    fn validate_identifier_use(
        &self,
        scope_stack: &[ScopeFrame<'hir>],
        name: &'hir str,
        access_span: crate::atlas_c::utils::Span,
    ) -> HirResult<()> {
        for frame in scope_stack.iter().rev() {
            if let Some(state) = frame.states.get(name) {
                let path = access_span.path;
                let src = utils::get_file_content(path).unwrap_or_default();
                match state {
                    OwnershipState::Alive => return Ok(()),
                    OwnershipState::Deleted(spans) => {
                        return Err(HirError::TryingToAccessADeletedValue(
                            TryingToAccessADeletedValueError {
                                delete_span: spans.first().copied().unwrap_or(access_span),
                                access_span,
                                src: NamedSource::new(path, src),
                            },
                        ));
                    }
                    OwnershipState::Moved(spans) => {
                        return Err(HirError::TryingToAccessAMovedValue(
                            TryingToAccessAMovedValueError {
                                move_span: spans.first().copied().unwrap_or(access_span),
                                access_span,
                                src: NamedSource::new(path, src),
                            },
                        ));
                    }
                    OwnershipState::Consumed(spans) => {
                        return Err(HirError::TryingToAccessAConsumedValue(
                            TryingToAccessAConsumedValueError {
                                consume_spans: spans.clone(),
                                access_span,
                                src: NamedSource::new(path, src),
                            },
                        ));
                    }
                    OwnershipState::ConditionallyMoved(spans) => {
                        return Err(HirError::TryingToAccessAPotentiallyMovedValue(
                            TryingToAccessAPotentiallyMovedValueError {
                                move_span: spans.first().copied().unwrap_or(access_span),
                                access_span,
                                src: NamedSource::new(path, src),
                            },
                        ));
                    }
                    OwnershipState::ConditionallyDeleted(spans) => {
                        return Err(HirError::TryingToAccessAPotentiallyDeletedValue(
                            TryingToAccessAPotentiallyDeletedValueError {
                                delete_span: spans.first().copied().unwrap_or(access_span),
                                access_span,
                                src: NamedSource::new(path, src),
                            },
                        ));
                    }
                    OwnershipState::ConditionallyConsumed(spans) => {
                        return Err(HirError::TryingToAccessAPotentiallyConsumedValue(
                            TryingToAccessAPotentiallyConsumedValueError {
                                consume_spans: spans.clone(),
                                access_span,
                                src: NamedSource::new(path, src),
                            },
                        ));
                    }
                }
            }
        }
        Ok(())
    }

    fn validate_move_argument(
        &mut self,
        scope_stack: &mut [ScopeFrame<'hir>],
        arg: &HirExpr<'hir>,
    ) -> HirResult<()> {
        let stripped = self.strip_noop_unary(arg);
        let (id_name, id_span) = match stripped {
            HirExpr::Ident(id) => (id.name, id.span),
            HirExpr::ThisLiteral(t) => ("this", t.span),
            _ => {
                let path = arg.span().path;
                let src = utils::get_file_content(path).unwrap_or_default();
                return Err(HirError::CannotMoveFromRvalue(CannotMoveFromRvalueError {
                    src: NamedSource::new(path, src),
                    span: arg.span(),
                    hint: "`std::move` only accepts local variables; assign the expression to a local first".to_string(),
                }));
            }
        };

        if self.find_local(scope_stack, id_name).is_none() {
            let path = id_span.path;
            let src = utils::get_file_content(path).unwrap_or_default();
            return Err(HirError::CannotMoveFromRvalue(CannotMoveFromRvalueError {
                src: NamedSource::new(path, src),
                span: id_span,
                hint: "`std::move` only accepts local variables".to_string(),
            }));
        }

        if self.is_compiler_temp_name(id_name) {
            let path = id_span.path;
            let src = utils::get_file_content(path).unwrap_or_default();
            return Err(HirError::CannotMoveFromRvalue(CannotMoveFromRvalueError {
                src: NamedSource::new(path, src),
                span: id_span,
                hint: "`std::move` cannot be used with compiler temporaries (`__tmpN`); move from a named local variable".to_string(),
            }));
        }

        self.mark_moved(scope_stack, id_name, id_span);
        Ok(())
    }

    fn merge_control_flow_states(
        &self,
        base_stack: &mut [ScopeFrame<'hir>],
        then_stack: &[ScopeFrame<'hir>],
        else_stack: Option<&[ScopeFrame<'hir>]>,
        then_terminates: bool,
        else_terminates: bool,
    ) {
        for (i, base_frame) in base_stack.iter_mut().enumerate() {
            let Some(then_frame) = then_stack.get(i) else {
                continue;
            };
            let else_frame = else_stack.and_then(|stack| stack.get(i));
            let names: Vec<&'hir str> = base_frame.states.keys().copied().collect();

            for name in names {
                let base_state = base_frame
                    .states
                    .get(name)
                    .cloned()
                    .unwrap_or(OwnershipState::Alive);
                let then_state = then_frame
                    .states
                    .get(name)
                    .cloned()
                    .unwrap_or_else(|| base_state.clone());
                let else_state = else_frame
                    .and_then(|frame| frame.states.get(name).cloned())
                    .unwrap_or_else(|| base_state.clone());

                let merged = if then_terminates && !else_terminates {
                    else_state
                } else if !then_terminates && else_terminates {
                    then_state
                } else if then_terminates && else_terminates {
                    // No path reaches the join point; keep base state unchanged to avoid
                    // introducing conditional moved/deleted noise into unreachable code.
                    base_state
                } else {
                    self.merge_join_state(base_state, then_state, else_state)
                };
                base_frame.states.insert(name, merged);
            }
        }
    }

    fn merge_join_state(
        &self,
        base: OwnershipState,
        then_state: OwnershipState,
        else_state: OwnershipState,
    ) -> OwnershipState {
        if then_state == else_state {
            return then_state;
        }

        if matches!(then_state, OwnershipState::Alive) {
            return self.conditionalize_state(else_state).unwrap_or(base);
        }
        if matches!(else_state, OwnershipState::Alive) {
            return self.conditionalize_state(then_state).unwrap_or(base);
        }

        if self.is_delete_family(&then_state) && self.is_delete_family(&else_state) {
            let spans =
                self.combine_spans(self.state_spans(&then_state), self.state_spans(&else_state));
            return OwnershipState::Deleted(spans);
        }
        if self.is_move_family(&then_state) && self.is_move_family(&else_state) {
            let spans =
                self.combine_spans(self.state_spans(&then_state), self.state_spans(&else_state));
            return OwnershipState::Moved(spans);
        }

        OwnershipState::Consumed(
            self.combine_spans(self.state_spans(&then_state), self.state_spans(&else_state)),
        )
    }

    fn conditionalize_state(&self, state: OwnershipState) -> Option<OwnershipState> {
        match state {
            OwnershipState::Alive => None,
            OwnershipState::Deleted(spans) => Some(OwnershipState::ConditionallyDeleted(spans)),
            OwnershipState::ConditionallyDeleted(spans) => {
                Some(OwnershipState::ConditionallyDeleted(spans))
            }
            OwnershipState::Moved(spans) => Some(OwnershipState::ConditionallyMoved(spans)),
            OwnershipState::ConditionallyMoved(spans) => {
                Some(OwnershipState::ConditionallyMoved(spans))
            }
            OwnershipState::Consumed(spans) => Some(OwnershipState::ConditionallyConsumed(spans)),
            OwnershipState::ConditionallyConsumed(spans) => {
                Some(OwnershipState::ConditionallyConsumed(spans))
            }
        }
    }

    fn state_spans(&self, state: &OwnershipState) -> Vec<crate::atlas_c::utils::Span> {
        match state {
            OwnershipState::Alive => Vec::new(),
            OwnershipState::Deleted(spans) | OwnershipState::Moved(spans) => spans.clone(),
            OwnershipState::Consumed(spans)
            | OwnershipState::ConditionallyDeleted(spans)
            | OwnershipState::ConditionallyMoved(spans)
            | OwnershipState::ConditionallyConsumed(spans) => spans.clone(),
        }
    }

    fn combine_spans(
        &self,
        mut a: Vec<crate::atlas_c::utils::Span>,
        b: Vec<crate::atlas_c::utils::Span>,
    ) -> Vec<crate::atlas_c::utils::Span> {
        for span in b {
            if !a.contains(&span) {
                a.push(span);
            }
        }
        a
    }

    fn is_delete_family(&self, state: &OwnershipState) -> bool {
        matches!(
            state,
            OwnershipState::Deleted(_) | OwnershipState::ConditionallyDeleted(_)
        )
    }

    fn is_move_family(&self, state: &OwnershipState) -> bool {
        matches!(
            state,
            OwnershipState::Moved(_) | OwnershipState::ConditionallyMoved(_)
        )
    }

    fn collect_scope_drops(
        &self,
        scope_stack: &[ScopeFrame<'hir>],
        excluded_name: Option<&'hir str>,
    ) -> Vec<HirStatement<'hir>> {
        let mut drops = Vec::new();
        for frame in scope_stack.iter().rev() {
            for local in frame.locals.iter().rev() {
                if Some(local.name) == excluded_name {
                    continue;
                }
                if !matches!(frame.states.get(local.name), Some(OwnershipState::Alive)) {
                    continue;
                }
                if self.should_auto_delete_local(local) {
                    drops.push(self.delete_stmt_for(
                        crate::atlas_c::utils::Span::default(),
                        local.name,
                        local.ty,
                    ));
                }
            }
        }
        drops
    }

    fn delete_stmt_for(
        &self,
        span: crate::atlas_c::utils::Span,
        name: &'hir str,
        ty: &'hir HirTy<'hir>,
    ) -> HirStatement<'hir> {
        HirStatement::Expr(HirExprStmt {
            span,
            expr: HirExpr::Delete(HirDeleteExpr {
                span,
                expr: Box::new(HirExpr::Ident(HirIdentExpr { name, span, ty })),
            }),
        })
    }

    fn deleted_identifier(
        &self,
        expr: &HirExpr<'hir>,
    ) -> Option<(&'hir str, crate::atlas_c::utils::Span)> {
        match self.strip_noop_unary(expr) {
            HirExpr::Delete(delete) => match self.strip_noop_unary(&delete.expr) {
                HirExpr::Ident(id) => Some((id.name, id.span)),
                HirExpr::ThisLiteral(t) => Some(("this", t.span)),
                _ => None,
            },
            _ => None,
        }
    }

    fn mark_deleted(
        &self,
        scope_stack: &mut [ScopeFrame<'hir>],
        name: &'hir str,
        delete_span: crate::atlas_c::utils::Span,
    ) {
        for frame in scope_stack.iter_mut().rev() {
            if frame.states.contains_key(name) {
                frame
                    .states
                    .insert(name, OwnershipState::Deleted(vec![delete_span]));
                return;
            }
        }
    }

    fn mark_moved(
        &self,
        scope_stack: &mut [ScopeFrame<'hir>],
        name: &'hir str,
        move_span: crate::atlas_c::utils::Span,
    ) {
        for frame in scope_stack.iter_mut().rev() {
            if frame.states.contains_key(name) {
                frame
                    .states
                    .insert(name, OwnershipState::Moved(vec![move_span]));
                return;
            }
        }
    }

    fn mark_consumed(
        &self,
        scope_stack: &mut [ScopeFrame<'hir>],
        name: &'hir str,
        consumed_span: crate::atlas_c::utils::Span,
    ) {
        for frame in scope_stack.iter_mut().rev() {
            if frame.states.contains_key(name) {
                frame
                    .states
                    .insert(name, OwnershipState::Consumed(vec![consumed_span]));
                return;
            }
        }
    }

    fn mark_assigned_alive(
        &self,
        scope_stack: &mut [ScopeFrame<'hir>],
        assign: &HirAssignStmt<'hir>,
    ) {
        if let HirExpr::Ident(id) = self.strip_noop_unary(&assign.dst) {
            for frame in scope_stack.iter_mut().rev() {
                if frame.states.contains_key(id.name) {
                    frame.states.insert(id.name, OwnershipState::Alive);
                    return;
                }
            }
        } else if let HirExpr::ThisLiteral(_) = self.strip_noop_unary(&assign.dst) {
            for frame in scope_stack.iter_mut().rev() {
                if frame.states.contains_key("this") {
                    frame.states.insert("this", OwnershipState::Alive);
                    return;
                }
            }
        }
    }

    /// After an argument/field/item expression has passed `ensure_identifier_copy_allowed`,
    /// if it turned out to be a bare compiler temporary being handed off by value, record
    /// that hand-off so scope-exit drop insertion doesn't also try to delete it.
    fn mark_compiler_temp_consumed(
        &self,
        scope_stack: &mut [ScopeFrame<'hir>],
        expr: &HirExpr<'hir>,
    ) {
        let (name, span) = match self.strip_noop_unary(expr) {
            HirExpr::Ident(id) => (id.name, id.span),
            HirExpr::ThisLiteral(t) => ("this", t.span),
            _ => return,
        };

        let Some(local) = self.find_local(scope_stack, name) else {
            return;
        };
        if !local.is_compiler_temp {
            return;
        }
        if !matches!(
            self.find_state(scope_stack, name),
            Some(OwnershipState::Alive)
        ) {
            return;
        }

        self.mark_moved(scope_stack, name, span);
    }

    fn pre_delete_before_assign(
        &self,
        scope_stack: &[ScopeFrame<'hir>],
        assign: &HirAssignStmt<'hir>,
    ) -> Option<HirStatement<'hir>> {
        let ident = match self.strip_noop_unary(&assign.dst) {
            HirExpr::Ident(id) => id,
            _ => return None,
        };

        for frame in scope_stack.iter().rev() {
            let Some(local) = frame.locals.iter().rev().find(|v| v.name == ident.name) else {
                continue;
            };
            if !self.should_auto_delete_local(local) {
                return None;
            }
            if !matches!(frame.states.get(ident.name), Some(OwnershipState::Alive)) {
                return None;
            }
            return Some(self.delete_stmt_for(assign.span, ident.name, local.ty));
        }
        None
    }

    fn register_local(&self, scope_stack: &mut [ScopeFrame<'hir>], var: LocalVar<'hir>) {
        if let Some(frame) = scope_stack.last_mut() {
            frame.locals.push(var);
            frame.states.insert(var.name, OwnershipState::Alive);
        }
    }

    fn is_compiler_temp_name(&self, name: &str) -> bool {
        name.starts_with("__tmp")
    }

    fn strip_noop_unary<'a>(&self, mut expr: &'a HirExpr<'hir>) -> &'a HirExpr<'hir> {
        while let HirExpr::Unary(unary) = expr {
            if unary.op == Some(HirUnaryOp::AsRef) || unary.op == Some(HirUnaryOp::Deref) {
                break;
            }
            if unary.op.is_some() {
                break;
            }
            expr = &unary.expr;
        }
        expr
    }
}
