mod context;

use super::{
    HirFunction, HirModule, HirModuleSignature,
    arena::HirArena,
    error::{FunctionTypeMismatchError, HirError, HirResult, TypeMismatchError, UnknownTypeError},
    expr,
    expr::{HirBinaryOperator, HirExpr},
    stmt::HirStatement,
    ty::{HirTy, HirTyId},
};
use crate::atlas_c::atlas_hir::item::{HirStruct, HirStructMethod};
use crate::atlas_c::atlas_hir::monomorphization_pass::MonomorphizationPass;
use crate::atlas_c::atlas_hir::signature::{
    HirFunctionParameterSignature, HirFunctionSignature, HirStructMethodModifier, HirVisibility,
};
use crate::atlas_c::atlas_hir::ty::HirNamedTy;
use crate::atlas_c::atlas_hir::type_check_pass::context::{ContextFunction, ContextVariable};
use crate::atlas_c::atlas_hir::warning::{DeletingReferenceIsUnstableWarning, HirWarning};
use crate::atlas_c::atlas_hir::{
    expr::{HirFunctionCallExpr, HirIdentExpr},
    item::HirStructConstructor,
};
use crate::atlas_c::utils::Span;
use crate::atlas_c::{
    atlas_hir::error::{
        AccessingClassFieldOutsideClassError, AccessingPrivateConstructorError,
        AccessingPrivateFieldError, AccessingPrivateFunctionError, AccessingPrivateFunctionOrigin,
        AccessingPrivateStructError, AccessingPrivateStructOrigin, CanOnlyConstructStructsError,
        CannotDeletePrimitiveTypeError, ConstTyToNonConstTyError, EmptyListLiteralError, FieldKind,
        IllegalOperationError, TryingToIndexNonIndexableTypeError, TypeMismatchActual,
        UnsupportedExpr,
    },
    utils,
};
use miette::{ErrReport, NamedSource, SourceOffset, SourceSpan};
use std::collections::HashMap;
use std::path::PathBuf;

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
    extern_monomorphized:
        HashMap<(&'hir str, Vec<&'hir HirTy<'hir>>), &'hir HirFunctionSignature<'hir>>,
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

    pub fn check(&mut self, hir: &mut HirModule<'hir>) -> HirResult<()> {
        self.signature = hir.signature.clone();
        for func in &mut hir.body.functions {
            self.current_func_name = Some(func.0);
            self.check_func(func.1)?;
        }
        for class in &mut hir.body.structs {
            self.current_class_name = Some(class.0);
            self.check_class(class.1)?;
        }
        Ok(())
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
                        _name_span: param.span.clone(),
                        ty: param.ty,
                        _ty_span: param.ty_span.clone(),
                        _is_mut: false,
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
                        _name_span: param.span.clone(),
                        ty: param.ty,
                        _ty_span: param.ty_span.clone(),
                        _is_mut: false,
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

    pub fn check_method(&mut self, method: &mut HirStructMethod<'hir>) -> HirResult<()> {
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
                        _name_span: param.span.clone(),
                        ty: param.ty,
                        _ty_span: param.ty_span.clone(),
                        _is_mut: false,
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
                        _name_span: param.span.clone(),
                        ty: param.ty,
                        _ty_span: param.ty_span.clone(),
                        _is_mut: false,
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
                    span = method.return_ty_span.clone().unwrap_or(r.span.clone());
                } else if self.current_func_name.is_some() {
                    //This means we're in a standalone function
                    let func_ret_from = self
                        .signature
                        .functions
                        .get(self.current_func_name.unwrap())
                        .unwrap();
                    expected_ret_ty = self.arena.intern(func_ret_from.return_ty.clone());
                    span = func_ret_from
                        .return_ty_span
                        .clone()
                        .unwrap_or(r.span.clone());
                }
                self.is_equivalent_ty(actual_ret_ty, r.value.span(), expected_ret_ty, span)
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
                let const_ty = c.ty.unwrap_or(expr_ty);
                c.ty = Some(const_ty);
                self.context_functions
                    .last_mut()
                    .unwrap()
                    .get_mut(self.current_func_name.unwrap())
                    .unwrap()
                    .insert(
                        c.name,
                        ContextVariable {
                            _name: c.name,
                            _name_span: c.name_span.clone(),
                            ty: const_ty,
                            _ty_span: c.ty_span.clone().unwrap_or(c.name_span.clone()),
                            _is_mut: false,
                        },
                    );

                self.is_equivalent_ty(
                    expr_ty,
                    c.value.span(),
                    const_ty,
                    c.ty_span.clone().unwrap_or(c.name_span.clone()),
                )
            }
            HirStatement::Let(l) => {
                let expr_ty = self.check_expr(&mut l.value)?;
                let var_ty = l.ty.unwrap_or(expr_ty);
                l.ty = Some(var_ty);
                self.context_functions
                    .last_mut()
                    .unwrap()
                    .get_mut(self.current_func_name.unwrap())
                    .unwrap()
                    .insert(
                        l.name,
                        ContextVariable {
                            _name: l.name,
                            _name_span: l.name_span.clone(),
                            ty: var_ty,
                            _ty_span: l.ty_span.clone().unwrap_or(l.name_span.clone()),
                            _is_mut: true,
                        },
                    );
                self.is_equivalent_ty(
                    expr_ty,
                    l.value.span(),
                    var_ty,
                    l.ty_span.clone().unwrap_or(l.name_span.clone()),
                )
            }
            _ => {
                todo!("TypeChecker::check_stmt: {:?}", stmt)
            }
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
            HirExpr::Delete(del) => {
                let ty = self.check_expr(&mut del.expr)?;
                if Self::is_primitive_type(ty) {
                    let path = del.span.path;
                    let src = utils::get_file_content(&path).unwrap();
                    return Err(HirError::CannotDeletePrimitiveType(
                        CannotDeletePrimitiveTypeError {
                            span: SourceSpan::new(
                                SourceOffset::from(del.expr.span().start),
                                del.expr.span().end - del.expr.span().start,
                            ),
                            ty: format!("{}", ty),
                            src: NamedSource::new(path, src),
                        },
                    ));
                } else {
                    let mut name = "";
                    if let HirTy::Named(n) = ty {
                        name = n.name;
                    } else if let HirTy::Generic(g) = ty {
                        name = MonomorphizationPass::mangle_generic_struct_name(self.arena, g);
                    } else if let HirTy::Reference(_) = ty {
                        Self::deleting_ref_is_unstable_warning(&del.span);
                    } else {
                        return Ok(self.arena.types().get_unit_ty());
                    }
                    let class = match self.signature.structs.get(name) {
                        Some(c) => *c,
                        None => {
                            return Ok(self.arena.types().get_unit_ty());
                        }
                    };
                    if class.destructor.vis != HirVisibility::Public {
                        Err(Self::accessing_private_constructor_err(
                            &del.span,
                            "destructor",
                        ))
                    } else {
                        Ok(self.arena.types().get_unit_ty())
                    }
                }
            }
            HirExpr::ThisLiteral(s) => {
                let class_name = match self.current_class_name {
                    Some(class_name) => class_name,
                    None => {
                        let path = expr.span().path;
                        let src = utils::get_file_content(&path).unwrap();
                        return Err(HirError::AccessingClassFieldOutsideClass(
                            AccessingClassFieldOutsideClassError {
                                span: SourceSpan::new(
                                    SourceOffset::from(expr.span().start),
                                    expr.span().end - expr.span().start,
                                ),
                                src: NamedSource::new(path, src),
                            },
                        ));
                    }
                };
                let class = self.signature.structs.get(class_name).unwrap();
                let self_ty = self
                    .arena
                    .types()
                    .get_named_ty(class.name, class.declaration_span.clone());
                s.ty = self_ty;
                Ok(self_ty)
            }
            HirExpr::Unary(u) => {
                let ty = self.check_expr(&mut u.expr)?;
                match u.op {
                    Some(expr::HirUnaryOp::Neg) => {
                        self.is_equivalent_ty(
                            ty,
                            u.expr.span(),
                            self.arena.types().get_integer64_ty(),
                            u.expr.span(),
                        )?;
                        u.ty = ty;
                        Ok(ty)
                    }
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
                        let src = utils::get_file_content(&path).unwrap();
                        Err(HirError::TryingToIndexNonIndexableType(
                            TryingToIndexNonIndexableTypeError {
                                span: SourceSpan::new(
                                    SourceOffset::from(indexing_expr.target.span().start),
                                    indexing_expr.target.span().end
                                        - indexing_expr.target.span().start,
                                ),
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
                        if !TypeChecker::is_equality_comparable(lhs) {
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
                    let src = utils::get_file_content(&path).unwrap();
                    return Err(HirError::EmptyListLiteral(EmptyListLiteralError {
                        span: SourceSpan::new(
                            SourceOffset::from(l.span.start),
                            l.span.end - l.span.start,
                        ),
                        src: NamedSource::new(path, src),
                    }));
                }
                let ty = self.check_expr(&mut l.items[0])?;
                for e in &mut l.items {
                    let e_ty = self.check_expr(e)?;
                    if HirTyId::from(e_ty) != HirTyId::from(ty) {
                        return Err(Self::type_mismatch_err(
                            &format!("{}", e_ty),
                            &e.span(),
                            &format!("{}", ty),
                            &l.span,
                        ));
                    }
                }
                l.ty = self.arena.types().get_list_ty(ty);
                Ok(self.arena.types().get_list_ty(ty))
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
                    let name = MonomorphizationPass::mangle_generic_struct_name(self.arena, g);
                    struct_ty = self.arena.intern(HirNamedTy {
                        name,
                        span: g.span.clone(),
                    }) as &'hir HirNamedTy<'hir>;
                    let tmp = match self.signature.structs.get(name) {
                        Some(c) => c,
                        None => {
                            return Err(Self::unknown_type_err(name, &obj.span));
                        }
                    };
                    *tmp
                } else {
                    let path = obj.span.path;
                    let src = utils::get_file_content(&path).unwrap();
                    return Err(HirError::CanOnlyConstructStructs(
                        CanOnlyConstructStructsError {
                            span: SourceSpan::new(
                                SourceOffset::from(obj.span.start),
                                obj.span.end - obj.span.start,
                            ),
                            src: NamedSource::new(path, src),
                        },
                    ));
                };
                if struct_signature.name_span.path != obj.span.path
                    && struct_signature.vis != HirVisibility::Public
                {
                    let origin_path = struct_signature.name_span.path;
                    let origin_src = utils::get_file_content(&origin_path).unwrap();
                    let obj_path = obj.span.path;
                    let obj_src = utils::get_file_content(&obj_path).unwrap();
                    return Err(HirError::AccessingPrivateStruct(
                        AccessingPrivateStructError {
                            name: struct_ty.name.to_owned(),
                            span: obj.span.clone(),
                            src: NamedSource::new(obj_path, obj_src),
                            origin: AccessingPrivateStructOrigin {
                                span: struct_signature.name_span.clone(),
                                src: NamedSource::new(origin_path, origin_src),
                            },
                        },
                    ));
                }
                if struct_signature.constructor.vis != HirVisibility::Public
                    && self.current_class_name != Some(struct_ty.name)
                {
                    let path = obj.span.path;
                    let src = utils::get_file_content(&path).unwrap();
                    return Err(HirError::AccessingPrivateConstructor(
                        AccessingPrivateConstructorError {
                            span: SourceSpan::new(
                                SourceOffset::from(obj.span.start),
                                obj.span.end - obj.span.start,
                            ),
                            kind: String::from("constructor"),
                            src: NamedSource::new(path, src),
                        },
                    ));
                }
                if struct_signature.constructor.params.len() != obj.args.len() {
                    return Err(Self::type_mismatch_err(
                        &format!(
                            "{} parameter(s)",
                            struct_signature.constructor.params.len()
                        ),
                        &obj.span,
                        &format!("{} argument(s)", obj.args.len()),
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
                        self.is_equivalent_ty(arg_ty, arg.span(), param.ty, param.span.clone());
                    if !is_equivalent.is_ok() {
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
                    .get_named_ty(struct_ty.name, struct_ty.span.clone());
                Ok(obj.ty)
            }
            HirExpr::Call(func_expr) => {
                let path = func_expr.span.path;

                let callee = func_expr.callee.as_mut();
                match callee {
                    HirExpr::Ident(i) => {
                        let name = i.name;
                        let func = match self.signature.functions.get(name) {
                            Some(f) => *f,
                            None => {
                                return Err(Self::unknown_type_err(name, &i.span));
                            }
                        };

                        if func.span.path != path && func.vis != HirVisibility::Public {
                            let origin_path = func.span.path;
                            let origin_src = utils::get_file_content(&origin_path).unwrap();
                            let call_path = func_expr.span.path;
                            let call_src = utils::get_file_content(&call_path).unwrap();
                            return Err(HirError::AccessingPrivateFunction(
                                AccessingPrivateFunctionError {
                                    name: name.to_string(),
                                    span: func_expr.span.clone(),
                                    src: NamedSource::new(call_path, call_src),
                                    origin: AccessingPrivateFunctionOrigin {
                                        span: func.span.clone(),
                                        src: NamedSource::new(origin_path, origin_src),
                                    },
                                },
                            ));
                        }

                        if func.params.len() != func_expr.args.len() {
                            let path = expr.span().path;
                            let src = utils::get_file_content(&path).unwrap();
                            return Err(HirError::FunctionTypeMismatch(
                                FunctionTypeMismatchError {
                                    expected_ty: format!("{:?}", func),
                                    span: SourceSpan::new(
                                        SourceOffset::from(func.span.start),
                                        func.span.end - func.span.start,
                                    ),
                                    src: NamedSource::new(path, src),
                                },
                            ));
                        }

                        //Only check if it's an external function with generics (e.g. `extern foo<T>(a: T) -> T`)
                        if func.is_external && func.generics.is_some() {
                            return self.check_extern_fn(name, func_expr, func);
                        }

                        for (param, arg) in func.params.iter().zip(func_expr.args.iter_mut()) {
                            let arg_ty = self.check_expr(arg)?;
                            self.is_equivalent_ty(
                                arg_ty,
                                arg.span(),
                                param.ty,
                                param.span.clone(),
                            )?;
                        }

                        Ok(self.arena.intern(func.return_ty.clone()))
                    }
                    //todo: Check if the field access try to access public/private functions
                    HirExpr::FieldAccess(field_access) => {
                        let target_ty = self.check_expr(&mut field_access.target)?;
                        let name;
                        if let HirTy::Named(n) = target_ty {
                            name = n.name;
                        } else if let HirTy::Generic(g) = target_ty {
                            name = MonomorphizationPass::mangle_generic_struct_name(self.arena, g);
                        } else {
                            let path = field_access.span.path;
                            let src = utils::get_file_content(&path).unwrap();
                            return Err(HirError::CanOnlyConstructStructs(
                                CanOnlyConstructStructsError {
                                    span: SourceSpan::new(
                                        SourceOffset::from(field_access.span.start),
                                        field_access.span.end - field_access.span.start,
                                    ),
                                    src: NamedSource::new(path, src),
                                },
                            ));
                        }
                        let class = match self.signature.structs.get(name) {
                            Some(c) => *c,
                            None => {
                                return Err(Self::unknown_type_err(name, &field_access.span));
                            }
                        };
                        if class.declaration_span.path != path && class.vis != HirVisibility::Public
                        {
                            let origin_path = class.declaration_span.path;
                            let origin_src = utils::get_file_content(&origin_path).unwrap();
                            let access_path = field_access.span.path;
                            let access_src = utils::get_file_content(&access_path).unwrap();
                            return Err(HirError::AccessingPrivateStruct(
                                AccessingPrivateStructError {
                                    name: name.to_owned(),
                                    span: field_access.span.clone(),
                                    src: NamedSource::new(access_path, access_src),
                                    origin: AccessingPrivateStructOrigin {
                                        span: class.declaration_span.clone(),
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
                                let src = utils::get_file_content(&path).unwrap();
                                return Err(HirError::AccessingPrivateField(
                                    AccessingPrivateFieldError {
                                        span: SourceSpan::new(
                                            SourceOffset::from(expr.span().start),
                                            expr.span().end - expr.span().start,
                                        ),
                                        kind: FieldKind::Function,
                                        src: NamedSource::new(path, src),
                                    },
                                ));
                            }
                            if method_signature.modifier == HirStructMethodModifier::Static {
                                let src = utils::get_file_content(&path).unwrap();
                                return Err(HirError::UnsupportedExpr(UnsupportedExpr {
                                    span: SourceSpan::new(
                                        SourceOffset::from(field_access.span.start),
                                        field_access.span.end - field_access.span.start,
                                    ),
                                    expr: "Static method call".to_string(),
                                    src: NamedSource::new(path, src),
                                }));
                            }
                            if method_signature.params.len() != func_expr.args.len() {
                                let src = utils::get_file_content(&path).unwrap();
                                return Err(HirError::FunctionTypeMismatch(
                                    FunctionTypeMismatchError {
                                        expected_ty: format!("{:?}", method_signature),
                                        span: SourceSpan::new(
                                            SourceOffset::from(field_access.span.start),
                                            field_access.span.end - field_access.span.start,
                                        ),
                                        src: NamedSource::new(path, src),
                                    },
                                ));
                            }
                            for (param, arg) in method_signature
                                .params
                                .iter()
                                .zip(func_expr.args.iter_mut())
                            {
                                let arg_ty = self.check_expr(arg)?;
                                self.is_equivalent_ty(
                                    arg_ty,
                                    arg.span(),
                                    param.ty,
                                    param.span.clone(),
                                )?;
                            }
                            field_access.ty = self.arena.intern(method_signature.return_ty.clone());
                            func_expr.ty = self.arena.intern(method_signature.return_ty.clone());
                            field_access.field.ty =
                                self.arena.intern(method_signature.return_ty.clone());

                            Ok(func_expr.ty)
                        } else {
                            Err(Self::unknown_type_err(
                                field_access.field.name,
                                &field_access.span,
                            ))
                        }
                    }
                    HirExpr::StaticAccess(static_access) => {
                        let class = match self.signature.structs.get(static_access.target.name) {
                            Some(c) => *c,
                            None => {
                                return Err(Self::unknown_type_err(
                                    static_access.target.name,
                                    &static_access.span,
                                ));
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
                                let src = std::fs::read_to_string(PathBuf::from(&path))
                                    .unwrap_or_else(|_| panic!("{} is not a valid path", path));
                                return Err(HirError::UnsupportedExpr(UnsupportedExpr {
                                    span: SourceSpan::new(
                                        SourceOffset::from(static_access.span.start),
                                        static_access.span.end - static_access.span.start,
                                    ),
                                    expr: "Instance method call".to_string(),
                                    src: NamedSource::new(path, src),
                                }));
                            }
                            if method_signature.params.len() != func_expr.args.len() {
                                let src = std::fs::read_to_string(PathBuf::from(&path))
                                    .unwrap_or_else(|_| panic!("{} is not a valid path", path));
                                return Err(HirError::FunctionTypeMismatch(
                                    FunctionTypeMismatchError {
                                        expected_ty: format!("{:?}", method_signature),
                                        span: SourceSpan::new(
                                            SourceOffset::from(static_access.span.start),
                                            static_access.span.end - static_access.span.start,
                                        ),
                                        src: NamedSource::new(path, src),
                                    },
                                ));
                            }
                            for (param, arg) in method_signature
                                .params
                                .iter()
                                .zip(func_expr.args.iter_mut())
                            {
                                let arg_ty = self.check_expr(arg)?;
                                if HirTyId::from(arg_ty) != HirTyId::from(param.ty) {
                                    return Err(Self::type_mismatch_err(
                                        &format!("{}", arg_ty),
                                        &arg.span(),
                                        &format!("{}", param.ty),
                                        &param.span,
                                    ));
                                }
                            }

                            static_access.ty =
                                self.arena.intern(method_signature.return_ty.clone());
                            func_expr.ty = self.arena.intern(method_signature.return_ty.clone());
                            static_access.target.ty = self
                                .arena
                                .types()
                                .get_named_ty(class.name, class.declaration_span.clone());
                            static_access.field.ty =
                                self.arena.intern(method_signature.return_ty.clone());

                            Ok(func_expr.ty)
                        } else {
                            Err(Self::unknown_type_err(
                                static_access.field.name,
                                &static_access.span,
                            ))
                        }
                    }
                    _ => {
                        todo!()
                    }
                }
            }
            HirExpr::Assign(a) => {
                let rhs = self.check_expr(&mut a.rhs)?;
                let lhs = self.check_expr(&mut a.lhs)?;
                //Todo needs a special rule for `self.field = value`, because you can assign once to a const field
                if lhs.is_const() {
                    return Err(Self::type_mismatch_err(
                        &format!("{}", lhs),
                        &a.lhs.span(),
                        "non-const",
                        &a.lhs.span(),
                    ));
                }
                self.is_equivalent_ty(rhs, a.rhs.span(), lhs, a.lhs.span())?;
                Ok(lhs)
            }
            HirExpr::Ident(i) => {
                let ctx_var = self.get_ident_ty(i)?;
                Ok(ctx_var.ty)
            }
            HirExpr::FieldAccess(field_access) => {
                let target_ty = self.check_expr(&mut field_access.target)?;
                let name;
                if let HirTy::Named(n) = target_ty {
                    name = n.name;
                } else if let HirTy::Generic(g) = target_ty {
                    name = MonomorphizationPass::mangle_generic_struct_name(self.arena, g);
                } else {
                    let path = field_access.span.path;
                    let src = utils::get_file_content(&path).unwrap();
                    return Err(HirError::CanOnlyConstructStructs(
                        CanOnlyConstructStructsError {
                            span: SourceSpan::new(
                                SourceOffset::from(field_access.span.start),
                                field_access.span.end - field_access.span.start,
                            ),
                            src: NamedSource::new(path, src),
                        },
                    ));
                }
                let class = match self.signature.structs.get(name) {
                    Some(c) => *c,
                    None => {
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
                        let path = expr.span().path;
                        let src = utils::get_file_content(&path).unwrap();
                        return Err(HirError::AccessingPrivateField(
                            AccessingPrivateFieldError {
                                span: SourceSpan::new(
                                    SourceOffset::from(expr.span().start),
                                    expr.span().end - expr.span().start,
                                ),
                                kind: FieldKind::Field,
                                src: NamedSource::new(path, src),
                            },
                        ));
                    }
                    field_access.ty = field_signature.ty;
                    field_access.field.ty = field_signature.ty;
                    Ok(field_signature.ty)
                } else {
                    Err(Self::unknown_type_err(
                        field_access.field.name,
                        &field_access.span,
                    ))
                }
            }
            HirExpr::StaticAccess(static_access) => {
                let class = match self.signature.structs.get(static_access.target.name) {
                    Some(c) => *c,
                    None => {
                        return Err(Self::unknown_type_err(
                            static_access.target.name,
                            &static_access.span,
                        ));
                    }
                };
                let constant = class
                    .constants
                    .iter()
                    .find(|f| *f.0 == static_access.field.name);
                if let Some((_, const_signature)) = constant {
                    static_access.target.ty = self
                        .arena
                        .types()
                        .get_named_ty(class.name, class.declaration_span.clone());
                    static_access.field.ty = const_signature.ty;
                    static_access.ty = const_signature.ty;
                    Ok(const_signature.ty)
                } else {
                    Err(Self::unknown_type_err(
                        static_access.field.name,
                        &static_access.span,
                    ))
                }
            }
        }
    }

    fn check_extern_fn(
        &mut self,
        name: &'hir str,
        expr: &mut HirFunctionCallExpr<'hir>,
        signature: &'hir HirFunctionSignature<'hir>,
    ) -> HirResult<&'hir HirTy<'hir>> {
        let args_ty = expr
            .args
            .iter_mut()
            .map(|a| self.check_expr(a))
            .collect::<HirResult<Vec<_>>>()?;
        let monomorphized = self.extern_monomorphized.get(&(name, args_ty.clone()));
        if let Some(m) = monomorphized {
            return Ok(self.arena.intern(m.return_ty.clone()));
        }
        //Contains the name + the actual type of that generic
        let mut generics: Vec<(&'hir str, &'hir HirTy<'hir>)> = Vec::new();
        let mut params = vec![];
        for (i, (param, arg)) in signature.params.iter().zip(args_ty.iter()).enumerate() {
            //This only take the name of the generic type (e.g. `T` in `extern foo<T>(a: T) -> T`)
            //So `extern foo<T>(a: [T]) -> T` won't be correctly type checked

            if let Some(name) = Self::get_generic_name(param.ty) {
                let ty = if let Some(ty) = Self::get_generic_ty(param.ty, arg) {
                    ty
                } else {
                    return Err(Self::type_mismatch_err(
                        &format!("{}", arg),
                        &expr.args[i].span(),
                        &format!("{}", param.ty),
                        &param.span,
                    ));
                };
                generics.push((name, ty));
            }
            let param_sign: HirFunctionParameterSignature = HirFunctionParameterSignature {
                name: param.name,
                name_span: param.name_span.clone(),
                span: param.span.clone(),
                ty: arg,
                ty_span: param.ty_span.clone(),
            };
            params.push(param_sign);
        }

        let mut monomorphized = signature.clone();
        monomorphized.params = params;
        if let Some(name) =
            Self::get_generic_name(self.arena.intern(monomorphized.return_ty.clone()))
        {
            let actual_generic_ty = generics.iter().find(|(n, _)| *n == name).unwrap().1;
            let return_ty = self.get_generic_ret_ty(
                self.arena.intern(monomorphized.return_ty.clone()),
                actual_generic_ty,
            );

            monomorphized.return_ty = return_ty.clone();
        };

        monomorphized.generics = None;
        let signature = self.arena.intern(monomorphized);
        self.extern_monomorphized.insert((name, args_ty), signature);
        Ok(self.arena.intern(signature.return_ty.clone()))
    }

    fn get_generic_name(ty: &'hir HirTy<'hir>) -> Option<&'hir str> {
        match ty {
            HirTy::List(l) => Self::get_generic_name(l.inner),
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
            (HirTy::Named(_), _) => Some(given_ty),
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
            Err(Self::unknown_type_err(i.name, &i.span))
        }
    }
    fn is_equivalent_ty(
        &self,
        ty1: &HirTy<'_>,
        ty1_span: Span,
        ty2: &HirTy<'_>,
        ty2_span: Span,
    ) -> HirResult<()> {
        match (ty1, ty2) {
            (HirTy::Nullable(n1), HirTy::Nullable(n2)) => {
                self.is_equivalent_ty(n1.inner, ty1_span, n2.inner, ty2_span)
            }
            (HirTy::Const(c1), HirTy::Const(c2)) => {
                self.is_equivalent_ty(c1.inner, ty1_span, c2.inner, ty2_span)
            }
            (HirTy::Const(_), _) => {
                let path = ty1_span.path;
                let src = utils::get_file_content(&path).unwrap();
                Err(HirError::ConstTyToNonConstTy(ConstTyToNonConstTyError {
                    const_val: SourceSpan::new(
                        SourceOffset::from(ty1_span.start),
                        ty1_span.end - ty1_span.start,
                    ),
                    const_type: ty1.to_string(),
                    non_const_type: ty2.to_string(),
                    non_const_val: SourceSpan::new(
                        SourceOffset::from(ty2_span.start),
                        ty2_span.end - ty2_span.start,
                    ),
                    src: NamedSource::new(path, src),
                }))
            }
            (HirTy::Int64(_), HirTy::UInt64(_)) | (HirTy::UInt64(_), HirTy::Int64(_)) => Ok(()),
            (_, HirTy::Const(r2)) => self.is_equivalent_ty(ty1, ty1_span, r2.inner, ty2_span),
            (HirTy::Nullable(n1), _) => self.is_equivalent_ty(n1.inner, ty1_span, ty2, ty2_span),
            (_, HirTy::Nullable(n2)) => self.is_equivalent_ty(ty1, ty1_span, n2.inner, ty2_span),
            (HirTy::Generic(g), HirTy::Named(n)) | (HirTy::Named(n), HirTy::Generic(g)) => {
                if MonomorphizationPass::mangle_generic_struct_name(self.arena, g) == n.name {
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

    // I don't know if I'll keep `string` as a primitive type in the future.
    // Maybe it will become something like `str` like in Rust,
    // so I can have a `std::string` package without conflicts.
    fn is_primitive_type(ty: &HirTy) -> bool {
        matches!(
            ty,
            HirTy::Int64(_)
                | HirTy::UInt64(_)
                | HirTy::Float64(_)
                | HirTy::Char(_)
                | HirTy::Boolean(_)
                | HirTy::String(_)
                | HirTy::Unit(_)
        )
    }

    #[inline(always)]
    fn type_mismatch_err(
        actual_type: &str,
        actual_loc: &Span,
        expected_type: &str,
        expected_loc: &Span,
    ) -> HirError {
        let actual_path = actual_loc.path;
        let actual_src = utils::get_file_content(&actual_path).unwrap();
        let actual_err = TypeMismatchActual {
            actual_ty: actual_type.to_string(),
            span: actual_loc.clone(),
            src: NamedSource::new(actual_path, actual_src),
        };

        let expected_path = expected_loc.path;
        let expected_src = utils::get_file_content(&expected_path).unwrap();
        let expected_err = TypeMismatchError {
            expected_ty: expected_type.to_string(),
            span: expected_loc.clone(),
            src: NamedSource::new(expected_path, expected_src),
            actual: actual_err,
        };
        HirError::TypeMismatch(expected_err)
    }

    #[inline(always)]
    fn unknown_type_err(name: &str, span: &Span) -> HirError {
        let path = span.path;
        let src = utils::get_file_content(&path).unwrap();
        HirError::UnknownType(UnknownTypeError {
            name: name.to_string(),
            span: SourceSpan::new(SourceOffset::from(span.start), span.end - span.start),
            src: NamedSource::new(path, src),
        })
    }

    fn accessing_private_constructor_err(span: &Span, kind: &str) -> HirError {
        let path = span.path;
        let src = utils::get_file_content(&path).unwrap();
        HirError::AccessingPrivateConstructor(AccessingPrivateConstructorError {
            span: SourceSpan::new(SourceOffset::from(span.start), span.end - span.start),
            kind: kind.to_string(),
            src: NamedSource::new(path, src),
        })
    }

    #[inline(always)]
    fn deleting_ref_is_unstable_warning(span: &Span) {
        let path = span.path;
        let src = utils::get_file_content(&path).unwrap();
        let warning: ErrReport =
            HirWarning::DeletingReferenceIsUnstable(DeletingReferenceIsUnstableWarning {
                span: SourceSpan::new(SourceOffset::from(span.start), span.end - span.start),
                src: NamedSource::new(path, src),
            })
            .into();
        eprintln!("{:?}", warning);
    }
    fn illegal_operation_err(
        ty1: &HirTy,
        ty2: &HirTy,
        expr_span: Span,
        operation: &str,
    ) -> HirError {
        let path = expr_span.path;
        let src = utils::get_file_content(&path).unwrap();
        HirError::IllegalOperation(IllegalOperationError {
            operation: operation.to_string(),
            expr_span,
            src: NamedSource::new(path, src),
            ty1: ty1.to_string(),
            ty2: ty2.to_string(),
        })
    }
    /// + - * / %
    fn is_arithmetic_type(ty: &HirTy) -> bool {
        match ty {
            //char is kind of an alias for uint32, so it can be used in arithmetic operations (Unicode char)
            HirTy::Int64(_) | HirTy::UInt64(_) | HirTy::Float64(_) | HirTy::Char(_) => true,
            _ => false,
        }
    }
    /// == !=
    fn is_equality_comparable(ty: &HirTy) -> bool {
        match ty {
            HirTy::Int64(_)
            | HirTy::UInt64(_)
            | HirTy::Float64(_)
            | HirTy::Char(_)
            | HirTy::Boolean(_)
            | HirTy::Unit(_) => true,
            _ => false,
        }
    }
    fn is_orderable_type(ty: &HirTy) -> bool {
        match ty {
            HirTy::Int64(_) | HirTy::UInt64(_) | HirTy::Float64(_) | HirTy::Char(_) => true,
            _ => false,
        }
    }
}
