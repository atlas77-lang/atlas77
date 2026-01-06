mod context;

use super::{
    HirFunction, HirModule, HirModuleSignature, arena::HirArena, expr, stmt::HirStatement,
};
use crate::atlas_c::atlas_hir::item::HirStructConstructor;
use crate::atlas_c::atlas_hir::signature::{
    HirFunctionParameterSignature, HirFunctionSignature, HirStructMethodModifier, HirVisibility,
};
use crate::atlas_c::atlas_hir::{
    error::{
        AccessingClassFieldOutsideClassError, AccessingPrivateConstructorError,
        AccessingPrivateFieldError, AccessingPrivateFunctionError, AccessingPrivateFunctionOrigin,
        AccessingPrivateStructError, AccessingPrivateStructOrigin,
        CallingNonConstMethodOnConstReferenceError, CallingNonConstMethodOnConstReferenceOrigin,
        CanOnlyConstructStructsError, EmptyListLiteralError, FieldKind, HirError, HirResult,
        IllegalOperationError, IllegalUnaryOperationError, InvalidSpecialMethodSignatureError,
        NotEnoughArgumentsError, NotEnoughArgumentsOrigin, ReturningReferenceToLocalVariableError,
        TryingToAccessFieldOnNonObjectTypeError,
        TryingToCreateAnUnionWithMoreThanOneActiveFieldError,
        TryingToCreateAnUnionWithMoreThanOneActiveFieldOrigin, TryingToIndexNonIndexableTypeError,
        TryingToMutateConstReferenceError, TypeMismatchActual, TypeMismatchError,
        UnknownFieldError, UnknownIdentifierError, UnknownMethodError, UnknownTypeError,
        UnsupportedExpr,
    },
    expr::{
        HirBinaryOperator, HirExpr, HirFunctionCallExpr, HirIdentExpr, HirUnaryOp,
        HirUnsignedIntegerLiteralExpr,
    },
    item::{HirStruct, HirStructMethod},
    monomorphization_pass::MonomorphizationPass,
    ty::{HirGenericTy, HirNamedTy, HirTy, HirTyId},
    type_check_pass::context::{ContextFunction, ContextVariable},
    warning::{HirWarning, TryingToCastToTheSameTypeWarning},
};
use crate::atlas_c::utils;
use crate::atlas_c::utils::Span;
use miette::{ErrReport, NamedSource};
use std::collections::HashMap;

pub struct TypeChecker<'hir> {
    arena: &'hir HirArena<'hir>,
    ///Keep track of the scopes and variables
    ///
    /// Should be rework in the future, variables should only be represented as (usize, usize)
    ///  (i.e. (scope, var_name) var_name being in the arena)
    context_functions: Vec<HashMap<String, ContextFunction<'hir>>>,
    signature: HirModuleSignature<'hir>,
    current_func_name: Option<&'hir str>,
    current_class_name: Option<&'hir str>,
    //TODO: Move this to the MonomorphizationPass in the future
    extern_monomorphized: HashMap<
        (&'hir str, Vec<&'hir HirTy<'hir>>, Vec<&'hir HirTy<'hir>>),
        &'hir HirFunctionSignature<'hir>,
    >,
}

impl<'hir> TypeChecker<'hir> {
    pub fn new(arena: &'hir HirArena<'hir>) -> Self {
        Self {
            arena,
            context_functions: vec![],
            signature: HirModuleSignature::default(),
            current_func_name: None,
            current_class_name: None,
            extern_monomorphized: HashMap::new(),
        }
    }

    pub fn check(
        &mut self,
        hir: &'hir mut HirModule<'hir>,
    ) -> HirResult<&'hir mut HirModule<'hir>> {
        self.signature = hir.signature.clone();
        for func in &mut hir.body.functions {
            self.current_func_name = Some(func.0);
            self.check_func(func.1)?;
        }
        for class in &mut hir.body.structs {
            self.current_class_name = Some(class.0);
            self.check_class(class.1)?;
        }
        Ok(hir)
    }

    pub fn check_class(&mut self, class: &mut HirStruct<'hir>) -> HirResult<()> {
        for method in &mut class.methods {
            self.current_class_name = Some(class.name);
            self.current_func_name = Some(method.name);
            self.context_functions.push(HashMap::new());
            self.check_method(method)?;
        }
        self.current_func_name = Some("constructor");
        self.check_constructor(&mut class.constructor)?;
        self.current_func_name = Some("destructor");
        self.check_destructor(&mut class.destructor)?;
        Ok(())
    }

    fn check_destructor(&mut self, destructor: &mut HirStructConstructor<'hir>) -> HirResult<()> {
        self.context_functions.push(HashMap::new());
        self.context_functions
            .last_mut()
            .unwrap()
            .insert(String::from("destructor"), ContextFunction::new());
        for param in &destructor.params {
            self.context_functions
                .last_mut()
                .unwrap()
                .get_mut("destructor")
                .unwrap()
                .insert(
                    param.name,
                    ContextVariable {
                        _name: param.name,
                        _name_span: param.span,
                        ty: param.ty,
                        _ty_span: param.ty_span,
                        _is_mut: false,
                        is_param: true,
                        refs_locals: vec![],
                    },
                );
        }
        for stmt in &mut destructor.body.statements {
            self.check_stmt(stmt)?;
        }
        //Because it is a destructor we don't keep it in the `context_functions`
        self.context_functions.pop();
        Ok(())
    }

    fn check_constructor(&mut self, constructor: &mut HirStructConstructor<'hir>) -> HirResult<()> {
        self.context_functions.push(HashMap::new());
        self.context_functions
            .last_mut()
            .unwrap()
            .insert(String::from("constructor"), ContextFunction::new());
        for param in &constructor.params {
            self.context_functions
                .last_mut()
                .unwrap()
                .get_mut("constructor")
                .unwrap()
                .insert(
                    param.name,
                    ContextVariable {
                        _name: param.name,
                        _name_span: param.span,
                        ty: param.ty,
                        _ty_span: param.ty_span,
                        _is_mut: false,
                        is_param: true,
                        refs_locals: vec![],
                    },
                );
        }
        for stmt in &mut constructor.body.statements {
            self.check_stmt(stmt)?;
        }
        //Because it is a constructor we don't keep it in the `context_functions`
        self.context_functions.pop();
        Ok(())
    }

    fn check_method(&mut self, method: &mut HirStructMethod<'hir>) -> HirResult<()> {
        self.check_special_method_signature(method.name, method)?;
        self.context_functions.push(HashMap::new());
        self.context_functions.last_mut().unwrap().insert(
            self.current_func_name.unwrap().to_string(),
            ContextFunction::new(),
        );
        for param in &method.signature.params {
            self.context_functions
                .last_mut()
                .unwrap()
                .get_mut(self.current_func_name.unwrap())
                .unwrap()
                .insert(
                    param.name,
                    ContextVariable {
                        _name: param.name,
                        _name_span: param.span,
                        ty: param.ty,
                        _ty_span: param.ty_span,
                        _is_mut: false,
                        is_param: true,
                        refs_locals: vec![],
                    },
                );
        }
        for stmt in &mut method.body.statements {
            self.check_stmt(stmt)?;
        }
        //Because it is a method we don't keep it in the `context_functions`
        self.context_functions.pop();
        Ok(())
    }

    fn check_special_method_signature(
        &mut self,
        name: &str,
        method: &HirStructMethod<'hir>,
    ) -> HirResult<()> {
        match name {
            // fun _copy(&const this) -> CurrentClass
            "_copy" => {
                if method.signature.modifier != HirStructMethodModifier::Const {
                    return Err(HirError::InvalidSpecialMethodSignature(
                        InvalidSpecialMethodSignatureError {
                            span: method.signature.span,
                            method_name: name.to_string(),
                            expected: "a const modifier".to_string(),
                            actual: format!("{:?}", method.signature.modifier),
                            src: NamedSource::new(
                                method.signature.span.path,
                                utils::get_file_content(method.signature.span.path).unwrap(),
                            ),
                        },
                    ));
                }
                // Methods don't inherently have a parameter for `this`, it is implicit
                // The fist parameter is determined by the modifier (in this case const)
                if !method.signature.params.is_empty() {
                    return Err(HirError::InvalidSpecialMethodSignature(
                        InvalidSpecialMethodSignatureError {
                            span: method.signature.span,
                            method_name: name.to_string(),
                            expected: "1 parameter (&const this)".to_string(),
                            actual: format!("{} parameters", method.signature.params.len() + 1), // + 1 for implicit this
                            src: NamedSource::new(
                                method.signature.span.path,
                                utils::get_file_content(method.signature.span.path).unwrap(),
                            ),
                        },
                    ));
                }
                match self.is_equivalent_ty(
                    &method.signature.return_ty,
                    method.signature.span,
                    self.arena
                        .types()
                        .get_named_ty(self.current_class_name.unwrap(), method.signature.span),
                    method.signature.return_ty_span.unwrap(),
                ) {
                    Ok(_) => {}
                    Err(_) => {
                        return Err(HirError::InvalidSpecialMethodSignature(
                            InvalidSpecialMethodSignatureError {
                                span: method
                                    .signature
                                    .return_ty_span
                                    .unwrap_or(method.signature.span),
                                method_name: name.to_string(),
                                expected: format!(
                                    "return type to be {}",
                                    self.current_class_name.unwrap()
                                ),
                                actual: format!("{}", method.signature.return_ty),
                                src: NamedSource::new(
                                    method.signature.span.path,
                                    utils::get_file_content(method.signature.span.path).unwrap(),
                                ),
                            },
                        ));
                    }
                }
            }
            _ => {}
        }
        Ok(())
    }

    pub fn check_func(&mut self, func: &mut HirFunction<'hir>) -> HirResult<()> {
        self.context_functions.push(HashMap::new());
        self.context_functions.last_mut().unwrap().insert(
            self.current_func_name.unwrap().to_string(),
            ContextFunction::new(),
        );
        for param in &func.signature.params {
            self.context_functions
                .last_mut()
                .unwrap()
                .get_mut(self.current_func_name.unwrap())
                .unwrap()
                .insert(
                    param.name,
                    ContextVariable {
                        _name: param.name,
                        _name_span: param.span,
                        ty: param.ty,
                        _ty_span: param.ty_span,
                        _is_mut: false,
                        is_param: true,
                        refs_locals: vec![],
                    },
                );
        }
        for stmt in &mut func.body.statements {
            self.check_stmt(stmt)?;
        }

        Ok(())
    }

    pub fn check_stmt(&mut self, stmt: &mut HirStatement<'hir>) -> HirResult<()> {
        match stmt {
            HirStatement::Expr(e) => {
                self.check_expr(&mut e.expr)?;
                Ok(())
            }
            HirStatement::Return(r) => {
                let actual_ret_ty = self.check_expr(&mut r.value)?;

                // Check for returning a reference to a local variable (directly or through a struct)
                let local_refs = self.get_local_ref_targets(&r.value);
                if let Some(local_var_name) = local_refs.first() {
                    let path = r.span.path;
                    let src = utils::get_file_content(path).unwrap();
                    return Err(HirError::ReturningReferenceToLocalVariable(
                        ReturningReferenceToLocalVariableError {
                            span: r.value.span(),
                            var_name: local_var_name.to_string(),
                            src: NamedSource::new(path, src),
                        },
                    ));
                }

                let mut expected_ret_ty = self.arena.types().get_uninitialized_ty();
                let mut span = Span::default();
                if self.current_class_name.is_some() {
                    //This means we're in a class method
                    let class = self
                        .signature
                        .structs
                        .get(self.current_class_name.unwrap())
                        .unwrap();
                    let method = class.methods.get(self.current_func_name.unwrap()).unwrap();
                    expected_ret_ty = self.arena.intern(method.clone().return_ty);
                    span = method.return_ty_span.unwrap_or(r.span);
                } else if self.current_func_name.is_some() {
                    //This means we're in a standalone function
                    let func_ret_from = self
                        .signature
                        .functions
                        .get(self.current_func_name.unwrap())
                        .unwrap();
                    expected_ret_ty = self.arena.intern(func_ret_from.return_ty.clone());
                    span = func_ret_from.return_ty_span.unwrap_or(r.span);
                }
                self.is_equivalent_ty(expected_ret_ty, span, actual_ret_ty, r.value.span())
            }
            HirStatement::While(w) => {
                let cond_ty = self.check_expr(&mut w.condition)?;
                self.is_equivalent_ty(
                    cond_ty,
                    w.condition.span(),
                    self.arena.types().get_boolean_ty(),
                    w.condition.span(),
                )?;
                //there should be just "self.context.new_scope()" and "self.context.end_scope()"
                self.context_functions
                    .last_mut()
                    .unwrap()
                    .get_mut(self.current_func_name.unwrap())
                    .unwrap()
                    .new_scope();
                for stmt in &mut w.body.statements {
                    self.check_stmt(stmt)?;
                }
                self.context_functions
                    .last_mut()
                    .unwrap()
                    .get_mut(self.current_func_name.unwrap())
                    .unwrap()
                    .end_scope();

                Ok(())
            }
            HirStatement::IfElse(i) => {
                let cond_ty = self.check_expr(&mut i.condition)?;
                self.is_equivalent_ty(
                    cond_ty,
                    i.condition.span(),
                    self.arena.types().get_boolean_ty(),
                    i.condition.span(),
                )?;

                self.context_functions
                    .last_mut()
                    .unwrap()
                    .get_mut(self.current_func_name.unwrap())
                    .unwrap()
                    .new_scope();
                for stmt in &mut i.then_branch.statements {
                    self.check_stmt(stmt)?;
                }
                self.context_functions
                    .last_mut()
                    .unwrap()
                    .get_mut(self.current_func_name.unwrap())
                    .unwrap()
                    .end_scope();
                if let Some(else_branch) = &mut i.else_branch {
                    self.context_functions
                        .last_mut()
                        .unwrap()
                        .get_mut(self.current_func_name.unwrap())
                        .unwrap()
                        .new_scope();
                    for stmt in &mut else_branch.statements {
                        self.check_stmt(stmt)?;
                    }
                    self.context_functions
                        .last_mut()
                        .unwrap()
                        .get_mut(self.current_func_name.unwrap())
                        .unwrap()
                        .end_scope();
                }
                Ok(())
            }
            HirStatement::Const(c) => {
                let expr_ty = self.check_expr(&mut c.value)?;
                let const_ty = if c.ty == self.arena.types().get_uninitialized_ty() {
                    //Need inference
                    expr_ty
                } else {
                    self.is_equivalent_ty(
                        expr_ty,
                        c.value.span(),
                        c.ty,
                        c.ty_span.unwrap_or(c.name_span),
                    )?;
                    c.ty
                };

                // Check if the const is being assigned a reference to a local variable
                let refs_locals = self.get_local_ref_targets(&c.value);

                self.context_functions
                    .last_mut()
                    .unwrap()
                    .get_mut(self.current_func_name.unwrap())
                    .unwrap()
                    .insert(
                        c.name,
                        ContextVariable {
                            _name: c.name,
                            _name_span: c.name_span,
                            ty: const_ty,
                            _ty_span: c.ty_span.unwrap_or(c.name_span),
                            _is_mut: false,
                            is_param: false,
                            refs_locals,
                        },
                    );

                self.is_equivalent_ty(
                    expr_ty,
                    c.value.span(),
                    const_ty,
                    c.ty_span.unwrap_or(c.name_span),
                )
            }
            HirStatement::Let(l) => {
                let expr_ty = self.check_expr(&mut l.value)?;
                let var_ty = if l.ty == self.arena.types().get_uninitialized_ty() {
                    //Need inference
                    expr_ty
                } else {
                    self.is_equivalent_ty(
                        l.ty,
                        l.ty_span.unwrap_or(l.name_span),
                        expr_ty,
                        l.value.span(),
                    )?;
                    l.ty
                };
                l.ty = var_ty;

                // Check if the let is being assigned a reference to a local variable
                let refs_locals = self.get_local_ref_targets(&l.value);

                self.context_functions
                    .last_mut()
                    .unwrap()
                    .get_mut(self.current_func_name.unwrap())
                    .unwrap()
                    .insert(
                        l.name,
                        ContextVariable {
                            _name: l.name,
                            _name_span: l.name_span,
                            ty: var_ty,
                            _ty_span: l.ty_span.unwrap_or(l.name_span),
                            _is_mut: true,
                            is_param: false,
                            refs_locals,
                        },
                    );
                self.is_equivalent_ty(
                    var_ty,
                    l.ty_span.unwrap_or(l.name_span),
                    expr_ty,
                    l.value.span(),
                )
            }
            _ => Err(HirError::UnsupportedExpr(UnsupportedExpr {
                span: stmt.span(),
                expr: format!("{:?}", stmt),
                src: NamedSource::new(
                    stmt.span().path,
                    utils::get_file_content(stmt.span().path).unwrap(),
                ),
            })),
        }
    }
    pub fn check_expr(&mut self, expr: &mut HirExpr<'hir>) -> HirResult<&'hir HirTy<'hir>> {
        match expr {
            HirExpr::IntegerLiteral(_) => Ok(self.arena.types().get_integer64_ty()),
            HirExpr::FloatLiteral(_) => Ok(self.arena.types().get_float64_ty()),
            HirExpr::UnsignedIntegerLiteral(_) => Ok(self.arena.types().get_uint64_ty()),
            HirExpr::BooleanLiteral(_) => Ok(self.arena.types().get_boolean_ty()),
            HirExpr::UnitLiteral(_) => Ok(self.arena.types().get_unit_ty()),
            HirExpr::CharLiteral(_) => Ok(self.arena.types().get_char_ty()),
            HirExpr::StringLiteral(_) => Ok(self.arena.types().get_str_ty()),
            HirExpr::Delete(del_expr) => {
                let ty = self.check_expr(&mut del_expr.expr)?;
                let name = match self.get_class_name_of_type(ty) {
                    Some(n) => n,
                    None => {
                        return Ok(self.arena.types().get_unit_ty());
                    }
                };
                let class = match self.signature.structs.get(name) {
                    Some(c) => *c,
                    None => {
                        return Ok(self.arena.types().get_unit_ty());
                    }
                };
                if class.destructor.vis != HirVisibility::Public {
                    Err(Self::accessing_private_constructor_err(
                        &del_expr.span,
                        "destructor",
                    ))
                } else {
                    Ok(self.arena.types().get_unit_ty())
                }
            }
            HirExpr::ThisLiteral(s) => {
                let class_name = match self.current_class_name {
                    Some(class_name) => class_name,
                    None => {
                        let path = expr.span().path;
                        let src = utils::get_file_content(path).unwrap();
                        return Err(HirError::AccessingClassFieldOutsideClass(
                            AccessingClassFieldOutsideClassError {
                                span: expr.span(),
                                src: NamedSource::new(path, src),
                            },
                        ));
                    }
                };
                let class = self.signature.structs.get(class_name).unwrap();
                let self_ty = self
                    .arena
                    .types()
                    .get_named_ty(class.name, class.declaration_span);
                let function_name = match self.current_func_name {
                    Some(func_name) => func_name,
                    None => {
                        let path = expr.span().path;
                        let src = utils::get_file_content(path).unwrap();
                        return Err(HirError::AccessingClassFieldOutsideClass(
                            AccessingClassFieldOutsideClassError {
                                span: expr.span(),
                                src: NamedSource::new(path, src),
                            },
                        ));
                    }
                };
                //early return for constructor and destructor
                if function_name == "constructor" || function_name == "destructor" {
                    s.ty = self_ty;
                    return Ok(self_ty);
                }
                let method = class.methods.get(function_name).unwrap();
                match method.modifier {
                    HirStructMethodModifier::Const => {
                        let readonly_self_ty =
                            self.arena.types().get_readonly_reference_ty(self_ty);
                        s.ty = readonly_self_ty;
                        Ok(readonly_self_ty)
                    }
                    HirStructMethodModifier::Mutable | HirStructMethodModifier::None => {
                        // Both &this (mutable reference) and this (ownership) use the same
                        // object type inside the method body - the difference is only in
                        // ownership semantics at the call site
                        s.ty = self_ty;
                        Ok(self_ty)
                    }
                    HirStructMethodModifier::Static => {
                        let path = expr.span().path;
                        let src = utils::get_file_content(path).unwrap();
                        Err(HirError::AccessingClassFieldOutsideClass(
                            AccessingClassFieldOutsideClassError {
                                span: expr.span(),
                                src: NamedSource::new(path, src),
                            },
                        ))
                    }
                }
            }
            HirExpr::Unary(u) => {
                let ty = self.check_expr(&mut u.expr)?;
                match u.op {
                    Some(expr::HirUnaryOp::Neg) => {
                        if !TypeChecker::is_arithmetic_type(ty) {
                            return Err(Self::illegal_unary_operation_err(
                                ty,
                                u.expr.span(),
                                "negation operation",
                            ));
                        }
                        u.ty = ty;
                        Ok(ty)
                    }
                    Some(HirUnaryOp::AsRef) => {
                        let ref_ty = self.arena.types().get_ref_ty(ty);
                        u.ty = ref_ty;
                        Ok(ref_ty)
                    }
                    Some(HirUnaryOp::Deref) => match ty {
                        HirTy::MutableReference(r) => {
                            u.ty = r.inner;
                            Ok(r.inner)
                        }
                        HirTy::ReadOnlyReference(r) => {
                            u.ty = r.inner;
                            Ok(r.inner)
                        }
                        _ => Err(Self::illegal_unary_operation_err(
                            ty,
                            u.expr.span(),
                            "dereference operation",
                        )),
                    },
                    _ => {
                        u.ty = ty;
                        Ok(ty)
                    }
                }
            }
            HirExpr::Casting(c) => {
                //This should be reworked when operator overloading is added
                let expr_ty = self.check_expr(&mut c.expr)?;
                let can_cast = matches!(
                    expr_ty,
                    HirTy::Int64(_)
                        | HirTy::Float64(_)
                        | HirTy::UInt64(_)
                        | HirTy::Boolean(_)
                        | HirTy::Char(_)
                        | HirTy::String(_)
                );
                if !can_cast {
                    return Err(Self::type_mismatch_err(
                        &format!("{}", expr_ty),
                        &c.expr.span(),
                        "int64, float64, uint64, bool, char or str",
                        &c.expr.span(),
                    ));
                }
                if self
                    .is_equivalent_ty(expr_ty, c.expr.span(), c.ty, c.span)
                    .is_ok()
                {
                    Self::trying_to_cast_to_the_same_type_warning(&c.span, &format!("{}", c.ty));
                    // Unwrap the redundant cast by replacing the casting expression with the inner expression
                    *expr = (*c.expr).clone();
                    return Ok(expr_ty);
                }

                Ok(c.ty)
            }
            HirExpr::Indexing(indexing_expr) => {
                let path = indexing_expr.span.path;
                let target = self.check_expr(&mut indexing_expr.target)?;
                let index = self.check_expr(&mut indexing_expr.index)?;
                self.is_equivalent_ty(
                    index,
                    indexing_expr.index.span(),
                    self.arena.types().get_uint64_ty(),
                    indexing_expr.index.span(),
                )?;

                match target {
                    HirTy::List(l) => {
                        indexing_expr.ty = l.inner;
                        Ok(l.inner)
                    }
                    HirTy::String(_) => {
                        indexing_expr.ty = self.arena.types().get_char_ty();
                        Ok(self.arena.types().get_char_ty())
                    }
                    _ => {
                        let src = utils::get_file_content(path).unwrap();
                        Err(HirError::TryingToIndexNonIndexableType(
                            TryingToIndexNonIndexableTypeError {
                                span: indexing_expr.span,
                                ty: format!("{}", target),
                                src: NamedSource::new(path, src),
                            },
                        ))
                    }
                }
            }
            HirExpr::HirBinaryOperation(b) => {
                let lhs = self.check_expr(&mut b.lhs)?;
                b.ty = lhs;
                let rhs = self.check_expr(&mut b.rhs)?;
                let is_equivalent = self.is_equivalent_ty(lhs, b.lhs.span(), rhs, b.rhs.span());
                if is_equivalent.is_err() {
                    return Err(Self::illegal_operation_err(
                        lhs,
                        rhs,
                        b.span,
                        "binary operation",
                    ));
                }

                match b.op {
                    HirBinaryOperator::Add
                    | HirBinaryOperator::Sub
                    | HirBinaryOperator::Mul
                    | HirBinaryOperator::Div
                    | HirBinaryOperator::Mod => {
                        if !TypeChecker::is_arithmetic_type(lhs) {
                            return Err(Self::illegal_operation_err(
                                lhs,
                                rhs,
                                b.span,
                                "arithmetic operation",
                            ));
                        }
                        Ok(lhs)
                    }
                    HirBinaryOperator::And | HirBinaryOperator::Or => {
                        if HirTyId::from(lhs) != HirTyId::compute_boolean_ty_id() {
                            return Err(Self::illegal_operation_err(
                                lhs,
                                rhs,
                                b.span,
                                "logical operation",
                            ));
                        }
                        Ok(lhs)
                    }
                    HirBinaryOperator::Eq | HirBinaryOperator::Neq => {
                        if !self.is_equality_comparable(lhs) {
                            return Err(Self::illegal_operation_err(
                                lhs,
                                rhs,
                                b.span,
                                "equality comparison",
                            ));
                        }
                        Ok(self.arena.types().get_boolean_ty())
                    }
                    HirBinaryOperator::Gt
                    | HirBinaryOperator::Gte
                    | HirBinaryOperator::Lt
                    | HirBinaryOperator::Lte => {
                        if !TypeChecker::is_orderable_type(lhs) {
                            return Err(Self::illegal_operation_err(
                                lhs,
                                rhs,
                                b.span,
                                "ordering comparison",
                            ));
                        }
                        Ok(self.arena.types().get_boolean_ty())
                    }
                }
            }
            HirExpr::ListLiteral(l) => {
                let path = l.span.path;
                if l.items.is_empty() {
                    let src = utils::get_file_content(path).unwrap();
                    return Err(HirError::EmptyListLiteral(EmptyListLiteralError {
                        span: l.span,
                        src: NamedSource::new(path, src),
                    }));
                }
                let ty = self.check_expr(&mut l.items[0])?;
                for e in &mut l.items {
                    let e_ty = self.check_expr(e)?;
                    self.is_equivalent_ty(e_ty, e.span(), ty, l.span)?;
                }
                l.ty = self.arena.types().get_list_ty(ty);
                Ok(l.ty)
            }
            HirExpr::NewArray(a) => {
                let size_ty = self.check_expr(a.size.as_mut())?;
                let size_ty_id = HirTyId::from(size_ty);
                if size_ty_id != HirTyId::compute_uint64_ty_id()
                    && size_ty_id != HirTyId::compute_integer64_ty_id()
                {
                    return Err(Self::type_mismatch_err(
                        &format!("{}", size_ty),
                        &a.size.span(),
                        &format!(
                            "{} or {}",
                            self.arena.types().get_uint64_ty(),
                            self.arena.types().get_integer64_ty()
                        ),
                        &a.size.span(),
                    ));
                }
                Ok(a.ty)
            }
            HirExpr::ObjLiteral(obj_lit) => {
                // Only support union literals for now & there is no constructor for unions
                // It's just `name { .field = value, ... }`
                let union_ty;
                let union_signature = if let HirTy::Named(n) = obj_lit.ty {
                    union_ty = n;
                    let tmp = match self.signature.unions.get(n.name) {
                        Some(c) => c,
                        None => {
                            return Err(Self::unknown_type_err(n.name, &obj_lit.span));
                        }
                    };
                    *tmp
                } else if let HirTy::Generic(g) = obj_lit.ty {
                    let name =
                        MonomorphizationPass::mangle_generic_object_name(self.arena, g, "union");
                    union_ty = self.arena.intern(HirNamedTy { name, span: g.span })
                        as &'hir HirNamedTy<'hir>;
                    let tmp = match self.signature.unions.get(name) {
                        Some(c) => c,
                        None => {
                            return Err(Self::unknown_type_err(name, &obj_lit.span));
                        }
                    };
                    *tmp
                } else {
                    let path = obj_lit.span.path;
                    let src = utils::get_file_content(path).unwrap();
                    return Err(HirError::CanOnlyConstructStructs(
                        CanOnlyConstructStructsError {
                            span: obj_lit.span,
                            src: NamedSource::new(path, src),
                        },
                    ));
                };

                if union_signature.name_span.path != obj_lit.span.path
                    && union_signature.vis != HirVisibility::Public
                {
                    let origin_path = union_signature.name_span.path;
                    let origin_src = utils::get_file_content(origin_path).unwrap();
                    let obj_path = obj_lit.span.path;
                    let obj_src = utils::get_file_content(obj_path).unwrap();
                    return Err(HirError::AccessingPrivateStruct(
                        AccessingPrivateStructError {
                            name: union_ty.name.to_owned(),
                            span: obj_lit.span,
                            src: NamedSource::new(obj_path, obj_src),
                            origin: AccessingPrivateStructOrigin {
                                span: union_signature.name_span,
                                src: NamedSource::new(origin_path, origin_src),
                            },
                        },
                    ));
                }

                if obj_lit.fields.len() > 1 {
                    let origin = TryingToCreateAnUnionWithMoreThanOneActiveFieldOrigin {
                        span: obj_lit.span,
                        src: NamedSource::new(
                            union_signature.name_span.path,
                            utils::get_file_content(union_signature.name_span.path).unwrap(),
                        ),
                    };
                    return Err(HirError::TryingToCreateAnUnionWithMoreThanOneActiveField(
                        TryingToCreateAnUnionWithMoreThanOneActiveFieldError {
                            span: obj_lit.span,
                            src: NamedSource::new(
                                obj_lit.span.path,
                                utils::get_file_content(obj_lit.span.path).unwrap(),
                            ),
                            origin,
                        },
                    ));
                }

                for field in &mut obj_lit.fields {
                    let field_signature = match union_signature.variants.get(field.name) {
                        Some(f) => f,
                        None => {
                            return Err(Self::unknown_field_err(
                                field.name,
                                union_ty.name,
                                &field.span,
                            ));
                        }
                    };
                    let field_ty = self.check_expr(&mut field.value)?;
                    let is_equivalent = self.is_equivalent_ty(
                        field_ty,
                        field.value.span(),
                        field_signature.ty,
                        field_signature.span,
                    );
                    if is_equivalent.is_err() {
                        return Err(Self::type_mismatch_err(
                            &format!("{}", field_ty),
                            &field.value.span(),
                            &format!("{}", field_signature.ty),
                            &field_signature.span,
                        ));
                    }
                }

                Ok(obj_lit.ty)
            }
            HirExpr::NewObj(obj) => {
                let struct_ty;
                let struct_signature = if let HirTy::Named(n) = obj.ty {
                    struct_ty = n;
                    let tmp = match self.signature.structs.get(n.name) {
                        Some(c) => c,
                        None => {
                            return Err(Self::unknown_type_err(n.name, &obj.span));
                        }
                    };
                    *tmp
                } else if let HirTy::Generic(g) = obj.ty {
                    let name =
                        MonomorphizationPass::mangle_generic_object_name(self.arena, g, "struct");
                    struct_ty = self.arena.intern(HirNamedTy { name, span: g.span })
                        as &'hir HirNamedTy<'hir>;
                    let tmp = match self.signature.structs.get(name) {
                        Some(c) => c,
                        None => {
                            return Err(Self::unknown_type_err(name, &obj.span));
                        }
                    };
                    *tmp
                } else {
                    let path = obj.span.path;
                    let src = utils::get_file_content(path).unwrap();
                    return Err(HirError::CanOnlyConstructStructs(
                        CanOnlyConstructStructsError {
                            span: obj.span,
                            src: NamedSource::new(path, src),
                        },
                    ));
                };
                if struct_signature.name_span.path != obj.span.path
                    && struct_signature.vis != HirVisibility::Public
                {
                    let origin_path = struct_signature.name_span.path;
                    let origin_src = utils::get_file_content(origin_path).unwrap();
                    let obj_path = obj.span.path;
                    let obj_src = utils::get_file_content(obj_path).unwrap();
                    return Err(HirError::AccessingPrivateStruct(
                        AccessingPrivateStructError {
                            name: struct_ty.name.to_owned(),
                            span: obj.span,
                            src: NamedSource::new(obj_path, obj_src),
                            origin: AccessingPrivateStructOrigin {
                                span: struct_signature.name_span,
                                src: NamedSource::new(origin_path, origin_src),
                            },
                        },
                    ));
                }
                if struct_signature.constructor.vis != HirVisibility::Public
                    && self.current_class_name != Some(struct_ty.name)
                {
                    let path = obj.span.path;
                    let src = utils::get_file_content(path).unwrap();
                    return Err(HirError::AccessingPrivateConstructor(
                        AccessingPrivateConstructorError {
                            span: obj.span,
                            kind: String::from("constructor"),
                            src: NamedSource::new(path, src),
                        },
                    ));
                }
                if struct_signature.constructor.params.len() != obj.args.len() {
                    return Err(Self::type_mismatch_err(
                        &format!("{} parameter(s)", obj.args.len()),
                        &obj.span,
                        &format!("{} argument(s)", struct_signature.constructor.params.len()),
                        &struct_signature.constructor.span,
                    ));
                }
                for (param, arg) in struct_signature
                    .constructor
                    .params
                    .iter()
                    .zip(obj.args.iter_mut())
                {
                    let arg_ty = self.check_expr(arg)?;
                    let is_equivalent =
                        self.is_equivalent_ty(arg_ty, arg.span(), param.ty, param.span);
                    if is_equivalent.is_err() {
                        return Err(Self::type_mismatch_err(
                            &format!("{}", arg_ty),
                            &arg.span(),
                            &format!("{}", param.ty),
                            &param.span,
                        ));
                    }
                }
                obj.ty = self
                    .arena
                    .types()
                    .get_named_ty(struct_ty.name, struct_ty.span);
                Ok(obj.ty)
            }
            HirExpr::Call(func_expr) => {
                let path = func_expr.span.path;

                let callee = func_expr.callee.as_mut();
                match callee {
                    HirExpr::Ident(i) => {
                        let name = if func_expr.generics.is_empty() {
                            i.name
                        } else {
                            MonomorphizationPass::mangle_generic_object_name(
                                self.arena,
                                &HirGenericTy {
                                    name: i.name,
                                    //Need to go from Vec<&T> to Vec<T>
                                    inner: func_expr
                                        .generics
                                        .iter()
                                        .map(|g| (*g).clone())
                                        .collect(),
                                    span: i.span,
                                },
                                "function",
                            )
                        };
                        let func = match self.signature.functions.get(name) {
                            Some(f) => *f,
                            None => {
                                return Err(Self::unknown_type_err(name, &i.span));
                            }
                        };

                        if func.span.path != path && func.vis != HirVisibility::Public {
                            let origin_path = func.span.path;
                            let origin_src = utils::get_file_content(origin_path).unwrap();
                            let call_path = func_expr.span.path;
                            let call_src = utils::get_file_content(call_path).unwrap();
                            return Err(HirError::AccessingPrivateFunction(
                                AccessingPrivateFunctionError {
                                    name: name.to_string(),
                                    span: func_expr.span,
                                    src: NamedSource::new(call_path, call_src),
                                    origin: AccessingPrivateFunctionOrigin {
                                        span: func.span,
                                        src: NamedSource::new(origin_path, origin_src),
                                    },
                                },
                            ));
                        }

                        if func.params.len() != func_expr.args.len() {
                            return Err(Self::not_enough_arguments_err(
                                "function".to_string(),
                                func.params.len(),
                                &func.span,
                                func_expr.args.len(),
                                &func_expr.span,
                            ));
                        }

                        //Only check if it's an external function with generics (e.g. `extern foo<T>(a: T) -> T`)
                        if func.is_external && !func.generics.is_empty() {
                            return self.check_extern_fn(name, func_expr, func);
                        }

                        for (param, arg) in func.params.iter().zip(func_expr.args.iter_mut()) {
                            let arg_ty = self.check_expr(arg)?;
                            self.is_equivalent_ty(param.ty, param.span, arg_ty, arg.span())?;
                        }
                        func_expr.ty = self.arena.intern(func.return_ty.clone());
                        Ok(self.arena.intern(func.return_ty.clone()))
                    }
                    HirExpr::FieldAccess(field_access) => {
                        let target_ty = self.check_expr(&mut field_access.target)?;
                        let name = match self.get_class_name_of_type(target_ty) {
                            Some(n) => n,
                            None => {
                                let path = field_access.span.path;
                                let src = utils::get_file_content(path).unwrap();
                                return Err(HirError::TryingToAccessFieldOnNonObjectType(
                                    TryingToAccessFieldOnNonObjectTypeError {
                                        span: field_access.span,
                                        src: NamedSource::new(path, src),
                                        ty: format!("{}", target_ty),
                                    },
                                ));
                            }
                        };
                        let class = match self.signature.structs.get(name) {
                            Some(c) => *c,
                            None => {
                                return Err(Self::unknown_type_err(name, &field_access.span));
                            }
                        };
                        if class.declaration_span.path != path && class.vis != HirVisibility::Public
                        {
                            let origin_path = class.declaration_span.path;
                            let origin_src = utils::get_file_content(origin_path).unwrap();
                            let access_path = field_access.span.path;
                            let access_src = utils::get_file_content(access_path).unwrap();
                            return Err(HirError::AccessingPrivateStruct(
                                AccessingPrivateStructError {
                                    name: name.to_owned(),
                                    span: field_access.span,
                                    src: NamedSource::new(access_path, access_src),
                                    origin: AccessingPrivateStructOrigin {
                                        span: class.declaration_span,
                                        src: NamedSource::new(origin_path, origin_src),
                                    },
                                },
                            ));
                        }
                        let method = class
                            .methods
                            .iter()
                            .find(|m| *m.0 == field_access.field.name);

                        if let Some((_, method_signature)) = method {
                            //Check if you're currently in the class, if not check is the method public
                            if self.current_class_name != Some(name)
                                && method_signature.vis != HirVisibility::Public
                            {
                                let src = utils::get_file_content(path).unwrap();
                                return Err(HirError::AccessingPrivateField(
                                    AccessingPrivateFieldError {
                                        span: field_access.span,
                                        kind: FieldKind::Function,
                                        src: NamedSource::new(path, src),
                                        field_name: field_access.field.name.to_string(),
                                    },
                                ));
                            }

                            if method_signature.modifier == HirStructMethodModifier::Static {
                                let src = utils::get_file_content(path).unwrap();
                                return Err(HirError::UnsupportedExpr(UnsupportedExpr {
                                    span: field_access.span,
                                    expr: "Static method call on instance".to_string(),
                                    src: NamedSource::new(path, src),
                                }));
                            }

                            if method_signature.params.len() != func_expr.args.len() {
                                return Err(Self::not_enough_arguments_err(
                                    "method".to_string(),
                                    method_signature.params.len(),
                                    &method_signature.span,
                                    func_expr.args.len(),
                                    &func_expr.span,
                                ));
                            }

                            if self.is_const_ty(target_ty)
                                && method_signature.modifier != HirStructMethodModifier::Const
                            {
                                return Err(Self::calling_non_const_method_on_const_reference_err(
                                    &method_signature.span,
                                    &field_access.span,
                                ));
                            }

                            for (param, arg) in method_signature
                                .params
                                .iter()
                                .zip(func_expr.args.iter_mut())
                            {
                                let arg_ty = self.check_expr(arg)?;
                                self.is_equivalent_ty(param.ty, param.span, arg_ty, arg.span())?;
                            }
                            field_access.ty = self.arena.intern(method_signature.return_ty.clone());
                            func_expr.ty = self.arena.intern(method_signature.return_ty.clone());
                            field_access.field.ty =
                                self.arena.intern(method_signature.return_ty.clone());

                            Ok(func_expr.ty)
                        } else {
                            Err(Self::unknown_method_err(
                                field_access.field.name,
                                name,
                                &field_access.span,
                            ))
                        }
                    }
                    HirExpr::StaticAccess(static_access) => {
                        let name = match static_access.target {
                            HirTy::Named(n) => n.name,
                            HirTy::Generic(g) => MonomorphizationPass::mangle_generic_object_name(
                                self.arena, g, "struct",
                            ),
                            _ => {
                                let path = static_access.span.path;
                                let src = utils::get_file_content(path).unwrap();
                                return Err(HirError::CanOnlyConstructStructs(
                                    CanOnlyConstructStructsError {
                                        span: static_access.span,
                                        src: NamedSource::new(path, src),
                                    },
                                ));
                            }
                        };
                        let class = match self.signature.structs.get(name) {
                            Some(c) => *c,
                            None => {
                                return Err(Self::unknown_type_err(name, &static_access.span));
                            }
                        };
                        let func = class
                            .methods
                            .iter()
                            .find(|m| *m.0 == static_access.field.name);
                        if let Some((_, method_signature)) = func {
                            if method_signature.modifier == HirStructMethodModifier::None
                                || method_signature.modifier == HirStructMethodModifier::Const
                            {
                                let src = utils::get_file_content(path).unwrap();
                                return Err(HirError::UnsupportedExpr(UnsupportedExpr {
                                    span: static_access.span,
                                    expr: "Instance method call".to_string(),
                                    src: NamedSource::new(path, src),
                                }));
                            }
                            if method_signature.params.len() != func_expr.args.len() {
                                return Err(Self::not_enough_arguments_err(
                                    "static method".to_string(),
                                    method_signature.params.len(),
                                    &method_signature.span,
                                    func_expr.args.len(),
                                    &func_expr.span,
                                ));
                            }
                            for (param, arg) in method_signature
                                .params
                                .iter()
                                .zip(func_expr.args.iter_mut())
                            {
                                let arg_ty = self.check_expr(arg)?;
                                self.is_equivalent_ty(arg_ty, arg.span(), param.ty, param.span)?;
                            }

                            static_access.ty =
                                self.arena.intern(method_signature.return_ty.clone());
                            func_expr.ty = self.arena.intern(method_signature.return_ty.clone());
                            static_access.field.ty =
                                self.arena.intern(method_signature.return_ty.clone());

                            Ok(func_expr.ty)
                        } else {
                            Err(Self::unknown_method_err(
                                static_access.field.name,
                                name,
                                &static_access.span,
                            ))
                        }
                    }
                    _ => Err(HirError::UnsupportedExpr(UnsupportedExpr {
                        span: func_expr.span,
                        expr: "Function call on non-identifier expression".to_string(),
                        src: NamedSource::new(path, utils::get_file_content(path).unwrap()),
                    })),
                }
            }
            HirExpr::Assign(a) => {
                let rhs = self.check_expr(&mut a.rhs)?;
                let lhs = self.check_expr(&mut a.lhs)?;
                //Todo needs a special rule for `this.field = value`, because you can assign once to a const field

                //Check if lhs is a const reference
                if lhs.is_const() {
                    //Check if we're in a constructor
                    if self.current_func_name == Some("constructor")
                        && self.current_class_name.is_some()
                    {
                        self.is_equivalent_ty(lhs, a.lhs.span(), rhs, a.rhs.span())?;
                        return Ok(lhs);
                    } else {
                        return Err(Self::trying_to_mutate_const_reference(&a.lhs.span(), lhs));
                    }
                }
                //Let's check if we are dereferencing a const reference
                if let HirExpr::Unary(unary_expr) = &*a.lhs {
                    if let Some(HirUnaryOp::Deref) = &unary_expr.op {
                        let deref_target_ty = self.check_expr(&mut unary_expr.expr.clone())?;
                        if deref_target_ty.is_const() {
                            //Check if we're in a constructor
                            if self.current_func_name == Some("constructor")
                                && self.current_class_name.is_some()
                            {
                                self.is_equivalent_ty(lhs, a.lhs.span(), rhs, a.rhs.span())?;
                                return Ok(lhs);
                            } else {
                                return Err(Self::trying_to_mutate_const_reference(
                                    &a.lhs.span(),
                                    deref_target_ty,
                                ));
                            }
                        }
                    }
                }
                self.is_equivalent_ty(lhs, a.lhs.span(), rhs, a.rhs.span())?;
                Ok(lhs)
            }
            HirExpr::Ident(i) => {
                let ctx_var = self.get_ident_ty(i)?;
                Ok(ctx_var.ty)
            }
            HirExpr::FieldAccess(field_access) => {
                let target_ty = self.check_expr(&mut field_access.target)?;
                let name = match self.get_class_name_of_type(target_ty) {
                    Some(n) => n,
                    None => {
                        let path = field_access.span.path;
                        let src = utils::get_file_content(path).unwrap();
                        return Err(HirError::TryingToAccessFieldOnNonObjectType(
                            TryingToAccessFieldOnNonObjectTypeError {
                                span: field_access.span,
                                ty: format!("{}", target_ty),
                                src: NamedSource::new(path, src),
                            },
                        ));
                    }
                };
                let class = match self.signature.structs.get(name) {
                    Some(c) => *c,
                    None => {
                        // We might be trying to access an union variant
                        if let Some(union_signature) = self.signature.unions.get(name) {
                            let variant = union_signature
                                .variants
                                .iter()
                                .find(|v| *v.0 == field_access.field.name);
                            match variant {
                                Some((_, var)) => {
                                    field_access.ty = var.ty;
                                    field_access.field.ty = var.ty;
                                    return Ok(var.ty);
                                }
                                None => {
                                    return Err(Self::unknown_field_err(
                                        field_access.field.name,
                                        name,
                                        &field_access.span,
                                    ));
                                }
                            }
                        }
                        return Err(Self::unknown_type_err(name, &field_access.span));
                    }
                };
                let field = class
                    .fields
                    .iter()
                    .find(|f| *f.0 == field_access.field.name);
                if let Some((_, field_signature)) = field {
                    if self.current_class_name != Some(name)
                        && field_signature.vis != HirVisibility::Public
                    {
                        let path = field_access.span.path;
                        let src = utils::get_file_content(path).unwrap();
                        return Err(HirError::AccessingPrivateField(
                            AccessingPrivateFieldError {
                                span: field_access.span,
                                kind: FieldKind::Field,
                                src: NamedSource::new(path, src),
                                field_name: field_access.field.name.to_string(),
                            },
                        ));
                    }
                    if self.is_const_ty(target_ty) {
                        field_access.ty = self
                            .arena
                            .types()
                            .get_readonly_reference_ty(field_signature.ty);
                        field_access.field.ty = self
                            .arena
                            .types()
                            .get_readonly_reference_ty(field_signature.ty);
                    } else {
                        field_access.ty = field_signature.ty;
                        field_access.field.ty = field_signature.ty;
                    }
                    match field_access.field.ty {
                        HirTy::Named(n) => {
                            if self.signature.enums.contains_key(n.name) {
                                Ok(self.arena.types().get_uint64_ty())
                            } else {
                                Ok(field_access.field.ty)
                            }
                        }
                        _ => Ok(field_access.field.ty),
                    }
                } else {
                    Err(Self::unknown_field_err(
                        field_access.field.name,
                        name,
                        &field_access.span,
                    ))
                }
            }
            HirExpr::StaticAccess(static_access) => {
                let name = match self.get_class_name_of_type(static_access.target) {
                    Some(n) => n,
                    None => {
                        let path = static_access.span.path;
                        let src = utils::get_file_content(path).unwrap();
                        return Err(HirError::TryingToAccessFieldOnNonObjectType(
                            TryingToAccessFieldOnNonObjectTypeError {
                                span: static_access.span,
                                ty: format!("{}", static_access.target),
                                src: NamedSource::new(path, src),
                            },
                        ));
                    }
                };

                let class = match self.signature.structs.get(name) {
                    Some(c) => *c,
                    None => {
                        //We might be trying to access an enum variant
                        if let Some(enum_signature) = self.signature.enums.get(name) {
                            let variant = enum_signature
                                .variants
                                .iter()
                                .find(|v| v.name == static_access.field.name);
                            match variant {
                                Some(var) => {
                                    let replaced_expr = HirExpr::UnsignedIntegerLiteral(
                                        HirUnsignedIntegerLiteralExpr {
                                            value: var.value,
                                            span: static_access.span,
                                            ty: self.arena.types().get_uint64_ty(),
                                        },
                                    );
                                    *expr = replaced_expr;
                                    return Ok(self.arena.types().get_uint64_ty());
                                }
                                None => {
                                    return Err(Self::unknown_field_err(
                                        static_access.field.name,
                                        name,
                                        &static_access.span,
                                    ));
                                }
                            }
                        }
                        return Err(Self::unknown_type_err(name, &static_access.span));
                    }
                };
                let constant = class
                    .constants
                    .iter()
                    .find(|f| *f.0 == static_access.field.name);
                if let Some((_, const_signature)) = constant {
                    static_access.field.ty = const_signature.ty;
                    static_access.ty = const_signature.ty;
                    Ok(const_signature.ty)
                } else {
                    Err(Self::unknown_field_err(
                        static_access.field.name,
                        name,
                        &static_access.span,
                    ))
                }
            }
            // Move expressions: type-check the inner expression
            HirExpr::Move(move_expr) => {
                let ty = self.check_expr(&mut move_expr.expr)?;
                move_expr.ty = ty;
                Ok(ty)
            }
            // Copy expressions: type-check the inner expression
            HirExpr::Copy(copy_expr) => {
                let ty = self.check_expr(&mut copy_expr.expr)?;
                copy_expr.ty = ty;
                Ok(ty)
            }
        }
    }

    fn check_extern_fn(
        &mut self,
        name: &'hir str,
        call_expr: &mut HirFunctionCallExpr<'hir>,
        signature: &'hir HirFunctionSignature<'hir>,
    ) -> HirResult<&'hir HirTy<'hir>> {
        let args_ty = call_expr
            .args
            .iter_mut()
            .map(|a| self.check_expr(a))
            .collect::<HirResult<Vec<_>>>()?;

        // Create cache key including explicit generics to distinguish calls like default::<int64>() from default::<string>()
        let explicit_generics = call_expr.generics.clone();
        let monomorphized =
            self.extern_monomorphized
                .get(&(name, args_ty.clone(), explicit_generics.clone()));
        if let Some(m) = monomorphized {
            return Ok(self.arena.intern(m.return_ty.clone()));
        }
        //Contains the name + the actual type of that generic
        let mut generics: Vec<(&'hir str, &'hir HirTy<'hir>)> = Vec::new();
        let mut params = vec![];

        // Check if explicit generic type arguments are provided (e.g., `default::<Int64>()`)
        if !call_expr.generics.is_empty() {
            // Use explicit type arguments from the function call
            if !signature.generics.is_empty() {
                for (generic_param, concrete_ty) in
                    signature.generics.iter().zip(call_expr.generics.iter())
                {
                    generics.push((generic_param.generic_name, concrete_ty));
                }
            }
            // Still need to create params even with explicit generics
            for (param, _arg) in signature.params.iter().zip(args_ty.iter()) {
                // Substitute the generic with the concrete type while preserving type constructors
                let param_ty = if let Some(generic_name) = Self::get_generic_name(param.ty) {
                    if let Some((_, concrete_ty)) = generics.iter().find(|(name, _)| *name == generic_name) {
                        self.get_generic_ret_ty(param.ty, concrete_ty)
                    } else {
                        param.ty
                    }
                } else {
                    param.ty
                };
                
                let param_sign: HirFunctionParameterSignature = HirFunctionParameterSignature {
                    name: param.name,
                    name_span: param.name_span,
                    span: param.span,
                    ty: param_ty,
                    ty_span: param.ty_span,
                };
                params.push(param_sign);
            }
        } else if !signature.params.is_empty() {
            // Try to infer generic types from arguments if no explicit type arguments provided
            for (i, (param, arg)) in signature.params.iter().zip(args_ty.iter()).enumerate() {
                //This only take the name of the generic type (e.g. `T` in `extern foo<T>(a: T) -> T`)
                //So `extern foo<T>(a: [T]) -> T` won't be correctly type checked

                if let Some(name) = Self::get_generic_name(param.ty) {
                    let ty = if let Some(ty) = Self::get_generic_ty(param.ty, arg) {
                        ty
                    } else {
                        return Err(Self::type_mismatch_err(
                            &format!("{}", arg),
                            &call_expr.args[i].span(),
                            &format!("{}", param.ty),
                            &param.span,
                        ));
                    };
                    generics.push((name, ty));
                }
            }
            // Now substitute the inferred generics into parameter types
            for (param, _arg) in signature.params.iter().zip(args_ty.iter()) {
                // Find the concrete type for this parameter's generic
                let param_ty = if let Some(generic_name) = Self::get_generic_name(param.ty) {
                    if let Some((_, concrete_ty)) = generics.iter().find(|(name, _)| *name == generic_name) {
                        // Substitute the generic with the concrete type while preserving type constructors
                        self.get_generic_ret_ty(param.ty, concrete_ty)
                    } else {
                        param.ty
                    }
                } else {
                    param.ty
                };
                
                let param_sign: HirFunctionParameterSignature = HirFunctionParameterSignature {
                    name: param.name,
                    name_span: param.name_span,
                    span: param.span,
                    ty: param_ty,
                    ty_span: param.ty_span,
                };
                params.push(param_sign);
            }
        } else if call_expr.generics.is_empty() {
            // Parameterless function with no explicit type arguments - error
            if !signature.generics.is_empty() {
                return Err(Self::type_mismatch_err(
                    "parameterless generic function",
                    &call_expr.span,
                    "explicit type arguments (e.g., `function::<Int64>()`)",
                    &signature.span,
                ));
            }
        }

        let mut monomorphized = signature.clone();
        monomorphized.params = params;
        if let Some(name) =
            Self::get_generic_name(self.arena.intern(monomorphized.return_ty.clone()))
        {
            let actual_generic_ty = match generics.iter().find(|(n, _)| *n == name) {
                Some((_, ty)) => *ty,
                None => {
                    return Err(Self::type_mismatch_err(
                        &format!("{}", monomorphized.return_ty),
                        &monomorphized.return_ty_span.unwrap_or(monomorphized.span),
                        "concrete type (check explicit type arguments match generic parameters)",
                        &signature.span,
                    ));
                }
            };
            let return_ty = self.get_generic_ret_ty(
                self.arena.intern(monomorphized.return_ty.clone()),
                actual_generic_ty,
            );

            monomorphized.return_ty = return_ty.clone();
        };

        monomorphized.generics = vec![];
        let signature = self.arena.intern(monomorphized);
        self.extern_monomorphized
            .insert((name, args_ty, explicit_generics), signature);
        Ok(self.arena.intern(signature.return_ty.clone()))
    }

    fn get_generic_name(ty: &'hir HirTy<'hir>) -> Option<&'hir str> {
        match ty {
            HirTy::List(l) => Self::get_generic_name(l.inner),
            HirTy::ReadOnlyReference(r) => Self::get_generic_name(r.inner),
            HirTy::MutableReference(m) => Self::get_generic_name(m.inner),
            HirTy::Named(n) => Some(n.name),
            _ => None,
        }
    }

    fn get_generic_ret_ty(
        &self,
        ty: &'hir HirTy<'hir>,
        actual_generic_ty: &'hir HirTy<'hir>,
    ) -> &'hir HirTy<'hir> {
        match ty {
            HirTy::List(l) => self
                .arena
                .types()
                .get_list_ty(self.get_generic_ret_ty(l.inner, actual_generic_ty)),
            HirTy::Named(_) => actual_generic_ty,
            HirTy::ReadOnlyReference(r) => self
                .arena
                .types()
                .get_readonly_reference_ty(self.get_generic_ret_ty(r.inner, actual_generic_ty)),
            HirTy::MutableReference(r) => self
                .arena
                .types()
                .get_ref_ty(self.get_generic_ret_ty(r.inner, actual_generic_ty)),
            _ => actual_generic_ty,
        }
    }

    /// Return the type of the generic after monormophization
    /// e.g. `foo<T>(a: T) -> T` with `foo(42)` will return `int64`
    fn get_generic_ty(
        ty: &'hir HirTy<'hir>,
        given_ty: &'hir HirTy<'hir>,
    ) -> Option<&'hir HirTy<'hir>> {
        match (ty, given_ty) {
            (HirTy::List(l1), HirTy::List(l2)) => Self::get_generic_ty(l1.inner, l2.inner),
            (HirTy::ReadOnlyReference(r1), HirTy::ReadOnlyReference(r2)) => {
                Self::get_generic_ty(r1.inner, r2.inner)
            }
            // A mutable reference can be used where a read-only reference is expected
            (HirTy::ReadOnlyReference(r1), HirTy::MutableReference(m2)) => {
                Self::get_generic_ty(r1.inner, m2.inner)
            }
            (HirTy::MutableReference(m1), HirTy::MutableReference(m2)) => {
                Self::get_generic_ty(m1.inner, m2.inner)
            }
            (HirTy::Named(_), _) => Some(given_ty),
            (HirTy::ReadOnlyReference(r1), _) => Self::get_generic_ty(r1.inner, given_ty),
            (HirTy::MutableReference(m1), _) => Self::get_generic_ty(m1.inner, given_ty),
            (_, HirTy::ReadOnlyReference(r2)) => Self::get_generic_ty(ty, r2.inner),
            (_, HirTy::MutableReference(m2)) => Self::get_generic_ty(ty, m2.inner),
            _ => None,
        }
    }

    fn get_ident_ty(&mut self, i: &mut HirIdentExpr<'hir>) -> HirResult<&ContextVariable<'hir>> {
        if let Some(ctx_var) = self
            .context_functions
            .last()
            .unwrap()
            .get(self.current_func_name.unwrap())
            .unwrap()
            .get(i.name)
        {
            i.ty = ctx_var.ty;
            Ok(ctx_var)
        } else {
            Err(Self::unknown_identifier_err(i.name, &i.span))
        }
    }

    fn is_const_ty(&self, ty: &HirTy<'_>) -> bool {
        matches!(ty, HirTy::ReadOnlyReference(_))
    }

    /// Check if two types are equivalent, considering generics and references
    ///
    /// - ty1 is the expected type
    /// - ty2 is the actual type
    ///
    /// We need a way to handle &primitive and &const primitive being equivalent to primitive
    fn is_equivalent_ty(
        &self,
        ty1: &HirTy<'_>,
        ty1_span: Span,
        ty2: &HirTy<'_>,
        ty2_span: Span,
    ) -> HirResult<()> {
        match (ty1, ty2) {
            // Let's handle ref to primitive types

            //(HirTy::Int64(_), HirTy::UInt64(_)) | (HirTy::UInt64(_), HirTy::Int64(_)) => Ok(()),
            (HirTy::Generic(g), HirTy::Named(n)) | (HirTy::Named(n), HirTy::Generic(g)) => {
                if MonomorphizationPass::mangle_generic_object_name(self.arena, g, "struct")
                    == n.name
                {
                    Ok(())
                } else {
                    Err(Self::type_mismatch_err(
                        &format!("{}", ty1),
                        &ty1_span,
                        &format!("{}", ty2),
                        &ty2_span,
                    ))
                }
            }
            //Check for enums
            (HirTy::Named(n), HirTy::UInt64(_)) | (HirTy::UInt64(_), HirTy::Named(n)) => {
                if self.signature.enums.contains_key(n.name) {
                    Ok(())
                } else {
                    Err(Self::type_mismatch_err(
                        &format!("{}", ty1),
                        &ty1_span,
                        &format!("{}", ty2),
                        &ty2_span,
                    ))
                }
            }
            (HirTy::ReadOnlyReference(read_only1), HirTy::ReadOnlyReference(read_only2)) => {
                self.is_equivalent_ty(read_only1.inner, ty1_span, read_only2.inner, ty2_span)
            }
            (HirTy::ReadOnlyReference(r1), HirTy::MutableReference(m2)) => {
                // A mutable reference (&T) can be coerced to a read-only reference (&const T)
                // ty1 = &const T (expected), ty2 = &T (actual) - this is valid
                self.is_equivalent_ty(r1.inner, ty1_span, m2.inner, ty2_span)
            }
            (HirTy::MutableReference(mutable1), HirTy::MutableReference(mutable2)) => {
                self.is_equivalent_ty(mutable1.inner, ty1_span, mutable2.inner, ty2_span)
            }
            // NOTE: Removed implicit value-to-reference coercion.
            // Previously, `&const T` could match `T` and `&mut T` could match `T`.
            // This caused issues with ownership: the type checker would accept `len(list)`
            // when `len` expects `&const [T]`, but the ownership pass didn't know about
            // the implicit borrow and would try to move/consume the list.
            // Now users must explicitly write `len(&list)` to borrow.
            _ => {
                if HirTyId::from(ty1) == HirTyId::from(ty2) {
                    Ok(())
                } else {
                    Err(Self::type_mismatch_err(
                        &format!("{}", ty1),
                        &ty1_span,
                        &format!("{}", ty2),
                        &ty2_span,
                    ))
                }
            }
        }
    }

    fn get_class_name_of_type(&self, ty: &HirTy<'hir>) -> Option<&'hir str> {
        match ty {
            HirTy::Named(n) => Some(n.name),
            HirTy::Generic(g) => {
                // Need to handle union and struct generics
                let name;
                if self.signature.structs.contains_key(
                    MonomorphizationPass::mangle_generic_object_name(self.arena, g, "struct"),
                ) {
                    name =
                        MonomorphizationPass::mangle_generic_object_name(self.arena, g, "struct");
                    return Some(name);
                } else {
                    name = MonomorphizationPass::mangle_generic_object_name(self.arena, g, "union");
                    if self.signature.unions.contains_key(name) {
                        return Some(name);
                    }
                }
                None
            }
            HirTy::ReadOnlyReference(read_only) => self.get_class_name_of_type(read_only.inner),
            HirTy::MutableReference(mutable) => self.get_class_name_of_type(mutable.inner),
            _ => None,
        }
    }

    fn trying_to_mutate_const_reference(span: &Span, ty: &HirTy<'_>) -> HirError {
        let path = span.path;
        let src = utils::get_file_content(path).unwrap();
        HirError::TryingToMutateConstReference(TryingToMutateConstReferenceError {
            span: *span,
            ty: ty.to_string(),
            src: NamedSource::new(path, src),
        })
    }

    #[inline(always)]
    fn type_mismatch_err(
        actual_type: &str,
        actual_loc: &Span,
        expected_type: &str,
        expected_loc: &Span,
    ) -> HirError {
        let actual_path = actual_loc.path;
        let actual_src = utils::get_file_content(actual_path).unwrap();
        let actual_err = TypeMismatchActual {
            actual_ty: actual_type.to_string(),
            span: *actual_loc,
            src: NamedSource::new(actual_path, actual_src),
        };

        let expected_path = expected_loc.path;
        let expected_src = utils::get_file_content(expected_path).unwrap();
        let expected_err = TypeMismatchError {
            expected_ty: expected_type.to_string(),
            span: *expected_loc,
            src: NamedSource::new(expected_path, expected_src),
            actual: actual_err,
        };
        HirError::TypeMismatch(expected_err)
    }

    fn calling_non_const_method_on_const_reference_err(
        declaration_span: &Span,
        call_span: &Span,
    ) -> HirError {
        let declaration_path = declaration_span.path;
        let declaration_src = utils::get_file_content(declaration_path).unwrap();
        let origin = CallingNonConstMethodOnConstReferenceOrigin {
            method_span: *declaration_span,
            src: NamedSource::new(declaration_path, declaration_src),
        };

        let call_path = call_span.path;
        let call_src = utils::get_file_content(call_path).unwrap();
        HirError::CallingNonConstMethodOnConstReference(
            CallingNonConstMethodOnConstReferenceError {
                call_span: *call_span,
                src: NamedSource::new(call_path, call_src),
                origin,
            },
        )
    }

    #[inline(always)]
    fn unknown_type_err(name: &str, span: &Span) -> HirError {
        let path = span.path;
        let src = utils::get_file_content(path).unwrap();
        HirError::UnknownType(UnknownTypeError {
            name: name.to_string(),
            span: *span,
            src: NamedSource::new(path, src),
        })
    }

    #[inline(always)]
    fn unknown_identifier_err(name: &str, span: &Span) -> HirError {
        let path = span.path;
        let src = utils::get_file_content(path).unwrap();
        HirError::UnknownIdentifier(UnknownIdentifierError {
            name: name.to_string(),
            span: *span,
            src: NamedSource::new(path, src),
        })
    }

    #[inline(always)]
    fn unknown_field_err(field_name: &str, ty_name: &str, span: &Span) -> HirError {
        let path = span.path;
        let src = utils::get_file_content(path).unwrap();
        HirError::UnknownField(UnknownFieldError {
            field_name: field_name.to_string(),
            ty_name: ty_name.to_string(),
            span: *span,
            src: NamedSource::new(path, src),
        })
    }

    #[inline(always)]
    fn unknown_method_err(method_name: &str, ty_name: &str, span: &Span) -> HirError {
        let path = span.path;
        let src = utils::get_file_content(path).unwrap();
        HirError::UnknownMethod(UnknownMethodError {
            method_name: method_name.to_string(),
            ty_name: ty_name.to_string(),
            span: *span,
            src: NamedSource::new(path, src),
        })
    }

    fn accessing_private_constructor_err(span: &Span, kind: &str) -> HirError {
        let path = span.path;
        let src = utils::get_file_content(path).unwrap();
        HirError::AccessingPrivateConstructor(AccessingPrivateConstructorError {
            span: *span,
            kind: kind.to_string(),
            src: NamedSource::new(path, src),
        })
    }

    fn not_enough_arguments_err(
        kind: String,
        expected: usize,
        expected_span: &Span,
        found: usize,
        found_span: &Span,
    ) -> HirError {
        let expected_path = expected_span.path;
        let expected_src = utils::get_file_content(expected_path).unwrap();
        let origin = NotEnoughArgumentsOrigin {
            expected,
            span: *expected_span,
            src: NamedSource::new(expected_path, expected_src),
        };

        let found_path = found_span.path;
        let found_src = utils::get_file_content(found_path).unwrap();
        HirError::NotEnoughArguments(NotEnoughArgumentsError {
            kind,
            found,
            span: *found_span,
            src: NamedSource::new(found_path, found_src),
            origin,
        })
    }

    fn illegal_unary_operation_err(ty: &HirTy, expr_span: Span, operation: &str) -> HirError {
        let path = expr_span.path;
        let src = utils::get_file_content(path).unwrap();
        HirError::IllegalUnaryOperation(IllegalUnaryOperationError {
            operation: operation.to_string(),
            expr_span,
            src: NamedSource::new(path, src),
            ty: ty.to_string(),
        })
    }

    fn illegal_operation_err(
        ty1: &HirTy,
        ty2: &HirTy,
        expr_span: Span,
        operation: &str,
    ) -> HirError {
        let path = expr_span.path;
        let src = utils::get_file_content(path).unwrap();
        HirError::IllegalOperation(IllegalOperationError {
            operation: operation.to_string(),
            expr_span,
            src: NamedSource::new(path, src),
            ty1: ty1.to_string(),
            ty2: ty2.to_string(),
        })
    }

    fn trying_to_cast_to_the_same_type_warning(span: &Span, ty: &str) {
        let path = span.path;
        let src = utils::get_file_content(path).unwrap();
        let warning: ErrReport =
            HirWarning::TryingToCastToTheSameType(TryingToCastToTheSameTypeWarning {
                span: *span,
                src: NamedSource::new(path, src),
                ty: ty.to_string(),
            })
            .into();
        eprintln!("{:?}", warning);
    }

    /// Check if the expression is a reference (`&expr`) to a local variable,
    /// or an identifier that holds a reference to a local variable.
    /// Returns the name of the local variable if it is, None otherwise.
    ///
    /// This is used to detect when a function is trying to return a reference
    /// to a local variable, which would be a dangling reference.
    /// Get all local variables that the expression references (directly or transitively).
    /// Returns a list of local variable names if any, empty vec otherwise.
    ///
    /// This is used to detect when a function is trying to return a reference
    /// to a local variable, which would be a dangling reference.
    fn get_local_ref_targets(&self, expr: &HirExpr<'hir>) -> Vec<&'hir str> {
        match expr {
            HirExpr::Unary(u) => {
                if matches!(u.op, Some(HirUnaryOp::AsRef)) {
                    // Check what we're taking a reference to
                    match u.expr.as_ref() {
                        HirExpr::Ident(ident) => {
                            // Check if this is a local variable (not a function parameter)
                            if self.is_local_variable(ident.name) {
                                return vec![ident.name];
                            }
                        }
                        HirExpr::FieldAccess(fa) => {
                            // Check if the base object is a local variable
                            if let HirExpr::Ident(ident) = fa.target.as_ref() {
                                if self.is_local_variable(ident.name) {
                                    return vec![ident.name];
                                }
                            }
                        }
                        _ => {}
                    }
                    vec![]
                } else if u.op.is_none() {
                    // No op - just unwrap and recurse (parser sometimes wraps in Unary with no op)
                    self.get_local_ref_targets(u.expr.as_ref())
                } else {
                    vec![]
                }
            }
            HirExpr::Ident(ident) => {
                // Check if this identifier holds references to local variables
                self.get_refs_locals(ident.name)
            }
            HirExpr::NewObj(new_obj) => {
                // Check if any constructor argument is a reference to a local
                let mut refs = vec![];
                for arg in &new_obj.args {
                    refs.extend(self.get_local_ref_targets(arg));
                }
                refs
            }
            _ => vec![],
        }
    }

    /// Get the local variables that a variable references (if any)
    fn get_refs_locals(&self, name: &str) -> Vec<&'hir str> {
        if let Some(context_map) = self.context_functions.last() {
            if let Some(func_name) = self.current_func_name {
                if let Some(context_func) = context_map.get(func_name) {
                    if let Some(var) = context_func.get_variable(name) {
                        return var.refs_locals.clone();
                    }
                }
            }
        }
        vec![]
    }

    /// Check if a variable name refers to a local variable (not a function parameter)
    fn is_local_variable(&self, name: &str) -> bool {
        // Get the current function's context
        if let Some(context_map) = self.context_functions.last() {
            if let Some(func_name) = self.current_func_name {
                if let Some(context_func) = context_map.get(func_name) {
                    // Check if the variable is in the local scope
                    if let Some(var) = context_func.get_variable(name) {
                        // If it's a parameter, it's not local
                        return !var.is_param;
                    }
                }
            }
        }
        // If we can't determine, assume it's local (conservative)
        true
    }

    /// + - * / %
    fn is_arithmetic_type(ty: &HirTy) -> bool {
        matches!(
            ty,
            HirTy::Int64(_) | HirTy::UInt64(_) | HirTy::Float64(_) | HirTy::Char(_)
        )
    }

    /// == !=
    fn is_equality_comparable(&self, ty: &HirTy) -> bool {
        match ty {
            HirTy::Int64(_)
            | HirTy::UInt64(_)
            | HirTy::Float64(_)
            | HirTy::Char(_)
            | HirTy::Boolean(_)
            | HirTy::Unit(_)
            // You can compare references for equality
            | HirTy::ReadOnlyReference(_)
            | HirTy::MutableReference(_) => true,
            HirTy::Named(n) => self.signature.enums.contains_key(n.name),
            _ => false,
        }
    }

    fn is_orderable_type(ty: &HirTy) -> bool {
        matches!(
            ty,
            HirTy::Int64(_) | HirTy::UInt64(_) | HirTy::Float64(_) | HirTy::Char(_)
        )
    }
}
