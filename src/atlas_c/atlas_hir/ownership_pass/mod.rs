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
        signature::{HirFunctionParameterSignature, HirModuleSignature},
        stmt::{HirAssignStmt, HirBlock, HirExprStmt, HirStatement},
        ty::HirTy,
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
}

impl<'hir> HirOwnershipPass<'hir> {
    pub fn new(hir_arena: &'hir HirArena<'hir>, signature: &HirModuleSignature<'hir>) -> Self {
        Self {
            _hir_arena: hir_arena,
            signature: signature.clone(),
            errors: Vec::new(),
        }
    }

    pub fn run(&mut self, hir_module: &mut HirModule<'hir>) -> HirResult<()> {
        self.errors.clear();

        for function in hir_module.body.functions.values_mut() {
            self.run_ownership_for_body(&mut function.body, &function.signature.params);
        }

        for strukt in hir_module.body.structs.values_mut() {
            for method in &mut strukt.methods {
                self.run_ownership_for_body(&mut method.body, &method.signature.params);
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

        *body = self.transform_block(body.clone(), &mut scope_stack);
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
                    statements.push(HirStatement::Expr(expr_stmt));
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

    fn is_implicitly_copyable(&self, ty: &'hir HirTy<'hir>) -> bool {
        match ty {
            HirTy::Integer(_)
            | HirTy::UnsignedInteger(_)
            | HirTy::Float(_)
            | HirTy::Boolean(_)
            | HirTy::Char(_)
            | HirTy::Unit(_)
            | HirTy::String(_)
            | HirTy::LiteralInteger(_)
            | HirTy::LiteralUnsignedInteger(_)
            | HirTy::LiteralFloat(_)
            | HirTy::PtrTy(_)
            | HirTy::Function(_)
            | HirTy::Slice(_) => true,
            HirTy::InlineArray(arr) => self.is_implicitly_copyable(arr.inner),
            HirTy::Named(named) => self
                .signature
                .structs
                .get(named.name)
                .is_some_and(|sig| sig.is_trivially_copyable),
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
                sig.is_some_and(|sig| sig.is_trivially_copyable)
            }
            _ => false,
        }
    }

    fn ensure_identifier_copy_allowed(
        &self,
        scope_stack: &[ScopeFrame<'hir>],
        value: &HirExpr<'hir>,
        dst_name: Option<&'hir str>,
    ) -> HirResult<()> {
        let src = match self.strip_noop_unary(value) {
            HirExpr::Ident(id) => id,
            _ => return Ok(()),
        };

        if dst_name.is_some_and(|dst| dst == src.name) {
            return Ok(());
        }

        let Some(src_local) = self.find_local(scope_stack, src.name) else {
            return Ok(());
        };

        if let Some(state) = self.find_state(scope_stack, src.name)
            && !matches!(state, OwnershipState::Alive)
        {
            return Ok(());
        }

        // Compiler temporaries can transfer ownership without explicit copy().
        if src_local.is_compiler_temp {
            return Ok(());
        }

        if self.is_implicitly_copyable(src_local.ty) {
            return Ok(());
        }

        let path = src.span.path;
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
                span: src.span,
                type_name: name,
            },
        ))
    }

    fn consumed_compiler_temp_from_assign(
        &self,
        scope_stack: &[ScopeFrame<'hir>],
        assign: &HirAssignStmt<'hir>,
    ) -> Option<(&'hir str, crate::atlas_c::utils::Span)> {
        let dst = match self.strip_noop_unary(&assign.dst) {
            HirExpr::Ident(id) => id,
            _ => return None,
        };
        let src = match self.strip_noop_unary(&assign.val) {
            HirExpr::Ident(id) => id,
            _ => return None,
        };

        if src.name == dst.name {
            return None;
        }

        let src_local = self.find_local(scope_stack, src.name)?;
        if !src_local.is_compiler_temp {
            return None;
        }

        let dst_local = self.find_local(scope_stack, dst.name)?;
        if dst_local.is_compiler_temp {
            return None;
        }

        if !std::ptr::eq(src_local.ty, dst_local.ty) {
            return None;
        }

        Some((src.name, src.span))
    }

    fn consumed_compiler_temp_from_value(
        &self,
        scope_stack: &[ScopeFrame<'hir>],
        value: &HirExpr<'hir>,
        dst_name: &'hir str,
        dst_ty: &'hir HirTy<'hir>,
    ) -> Option<(&'hir str, crate::atlas_c::utils::Span)> {
        let src = match self.strip_noop_unary(value) {
            HirExpr::Ident(id) => id,
            _ => return None,
        };

        if src.name == dst_name {
            return None;
        }

        let src_local = self.find_local(scope_stack, src.name)?;
        if !src_local.is_compiler_temp {
            return None;
        }

        if self.is_compiler_temp_name(dst_name) {
            return None;
        }

        if !std::ptr::eq(src_local.ty, dst_ty) {
            return None;
        }

        Some((src.name, src.span))
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
            _ => None,
        }
    }

    fn validate_expr(&mut self, expr: &HirExpr<'hir>, scope_stack: &mut Vec<ScopeFrame<'hir>>) {
        match self.strip_noop_unary(expr) {
            HirExpr::Ident(id) => {
                self.record_result(self.validate_identifier_use(scope_stack, id.name, id.span))
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
                }
            }
            HirExpr::ListLiteral(list) => {
                for item in &list.items {
                    self.validate_expr(item, scope_stack);
                }
            }
            HirExpr::ListLiteralWithSize(list) => {
                // list.size > 1, we need to ensure the type isn't being moved into the list multiple times.
                let size = list.size_as_usize().unwrap_or(0);
                if size > 1 {
                    self.validate_expr(&list.item, scope_stack);
                    self.record_result(self.ensure_identifier_copy_allowed(
                        scope_stack,
                        &list.item,
                        None,
                    ));
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
            HirExpr::ThisLiteral(_)
            | HirExpr::FloatLiteral(_)
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
        let HirExpr::Ident(id) = stripped else {
            let path = arg.span().path;
            let src = utils::get_file_content(path).unwrap_or_default();
            return Err(HirError::CannotMoveFromRvalue(CannotMoveFromRvalueError {
                src: NamedSource::new(path, src),
                span: arg.span(),
                hint: "`std::move` only accepts local variables; assign the expression to a local first".to_string(),
            }));
        };

        if self.find_local(scope_stack, id.name).is_none() {
            let path = id.span.path;
            let src = utils::get_file_content(path).unwrap_or_default();
            return Err(HirError::CannotMoveFromRvalue(CannotMoveFromRvalueError {
                src: NamedSource::new(path, src),
                span: id.span,
                hint: "`std::move` only accepts local variables".to_string(),
            }));
        }

        if self.is_compiler_temp_name(id.name) {
            let path = id.span.path;
            let src = utils::get_file_content(path).unwrap_or_default();
            return Err(HirError::CannotMoveFromRvalue(CannotMoveFromRvalueError {
                src: NamedSource::new(path, src),
                span: id.span,
                hint: "`std::move` cannot be used with compiler temporaries (`__tmpN`); move from a named local variable".to_string(),
            }));
        }

        self.mark_moved(scope_stack, id.name, id.span);
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
        }
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
