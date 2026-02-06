mod context;

use super::{
    HirFunction, HirModule, HirModuleSignature, arena::HirArena, expr, stmt::HirStatement,
};
use crate::atlas_c::atlas_hir::error::{
    AccessingPrivateUnionError, CallingConsumingMethodOnMutableReferenceError,
    CallingConsumingMethodOnMutableReferenceOrigin, ListIndexOutOfBoundsError,
    ReferenceToReferenceError, RvalueReferenceToLvalueReferenceError,
    StdNonCopyableStructCannotHaveCopyConstructorError,
    StructCannotHaveAFieldOfItsOwnTypeError, ThisStructDoesNotHaveACopyConstructorError,
    ThisStructDoesNotHaveAMoveConstructorError, TypeIsNotCopyableError, TypeIsNotMoveableError,
    UnionMustHaveAtLeastTwoVariantError, UnionVariantDefinedMultipleTimesError,
    VariableNameAlreadyDefinedError,
};
use crate::atlas_c::atlas_hir::item::{HirStructConstructor, HirUnion};
use crate::atlas_c::atlas_hir::pretty_print::HirPrettyPrinter;
use crate::atlas_c::atlas_hir::signature::{
    HirFunctionParameterSignature, HirFunctionSignature, HirStructMethodModifier,
    HirStructSignature, HirVisibility,
};
use crate::atlas_c::atlas_hir::ty::{HirReferenceKind, HirReferenceTy};
use crate::atlas_c::atlas_hir::{
    error::{
        AccessingClassFieldOutsideClassError, AccessingPrivateConstructorError,
        AccessingPrivateFieldError, AccessingPrivateFunctionError, AccessingPrivateFunctionOrigin,
        AccessingPrivateObjectOrigin, AccessingPrivateStructError,
        CallingNonConstMethodOnConstReferenceError, CallingNonConstMethodOnConstReferenceOrigin,
        CanOnlyConstructStructsError, EmptyListLiteralError, FieldKind, HirError, HirResult,
        IllegalOperationError, IllegalUnaryOperationError, MethodConstraintNotSatisfiedError,
        NotEnoughArgumentsError, NotEnoughArgumentsOrigin, ReturningReferenceToLocalVariableError,
        TryingToAccessFieldOnNonObjectTypeError,
        TryingToCreateAnUnionWithMoreThanOneActiveFieldError,
        TryingToCreateAnUnionWithMoreThanOneActiveFieldOrigin, TryingToIndexNonIndexableTypeError,
        TryingToMutateConstReferenceError, TypeMismatchActual, TypeMismatchError,
        UnknownFieldError, UnknownIdentifierError, UnknownMethodError, UnknownTypeError,
        UnsupportedExpr,
    },
    expr::{
        HirBinaryOperator, HirExpr, HirIdentExpr, HirNewObjExpr, HirUnaryOp,
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
use std::collections::{HashMap, HashSet};

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

    pub(crate) fn check(
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
        for hir_union in hir.body.unions.values_mut() {
            self.check_union(hir_union)?;
        }
        Ok(hir)
    }

    fn check_union(&mut self, hir_union: &HirUnion<'hir>) -> HirResult<()> {
        if hir_union.variants.len() <= 1 {
            let path = hir_union.span.path;
            let src = utils::get_file_content(path).unwrap();
            Err(HirError::UnionMustHaveAtLeastTwoVariant(
                UnionMustHaveAtLeastTwoVariantError {
                    union_name: hir_union.name.to_string(),
                    span: hir_union.name_span,
                    src: NamedSource::new(path, src),
                },
            ))
        } else {
            let mut variants = HashMap::new();
            for variant in &hir_union.variants {
                if let Some((_, v_span)) = variants.get_key_value(variant.ty) {
                    let path = hir_union.span.path;
                    let src = utils::get_file_content(path).unwrap();
                    return Err(HirError::UnionVariantDefinedMultipleTimes(
                        UnionVariantDefinedMultipleTimesError {
                            union_name: hir_union.name.to_string(),
                            variant_ty: format!("{}", variant.ty),
                            first_span: *v_span,
                            second_span: variant.span,
                            src: NamedSource::new(path, src),
                        },
                    ));
                } else {
                    variants.insert(variant.ty, variant.span);
                }
            }
            Ok(())
        }
    }

    pub fn check_class(&mut self, class: &mut HirStruct<'hir>) -> HirResult<()> {
        // Check for cyclic struct references (struct containing itself directly or indirectly)
        for field in &class.fields {
            let mut visited = HashSet::new();
            let mut cycle_path = Vec::new();

            if self.has_cyclic_reference(
                field.ty,
                &class.signature,
                &mut visited,
                &mut cycle_path,
                field.span,
            ) {
                let path = class.span.path;
                let src = utils::get_file_content(path).unwrap();
                return Err(HirError::StructCannotHaveAFieldOfItsOwnType(
                    StructCannotHaveAFieldOfItsOwnTypeError {
                        struct_name: if let Some(gen_ty) = class.pre_mangled_ty {
                            HirPrettyPrinter::generic_ty_str(gen_ty)
                        } else {
                            class.name.to_string()
                        },
                        struct_span: class.name_span,
                        cycle_path,
                        src: NamedSource::new(path, src),
                    },
                ));
            }
        }
        for method in &mut class.methods {
            self.current_class_name = Some(class.name);
            self.current_func_name = Some(method.name);
            self.context_functions.push(HashMap::new());
            self.check_method(method)?;
        }
        self.current_func_name = Some("constructor");
        self.check_constructor(&mut class.constructor)?;
        if let Some(copy_ctor) = &mut class.move_constructor {
            if class.flag.is_non_moveable() {
                let path = class.span.path;
                let src = utils::get_file_content(path).unwrap();
                return Err(HirError::StdNonCopyableStructCannotHaveCopyConstructor(
                    StdNonCopyableStructCannotHaveCopyConstructorError {
                        struct_name: class.name.to_string(),
                        copy_ctor_span: copy_ctor.signature.span,
                        flag_span: class.flag.span().unwrap(),
                        src: NamedSource::new(path, src),
                    },
                ));
            }
            self.current_func_name = Some("copy_ctor");
            self.check_copy_ctor(copy_ctor)?;
        }
        self.current_func_name = Some("destructor");
        self.check_destructor(class.destructor.as_mut().unwrap())?;
        Ok(())
    }

    fn check_destructor(&mut self, destructor: &mut HirStructConstructor<'hir>) -> HirResult<()> {
        self.context_functions.push(HashMap::new());
        self.context_functions
            .last_mut()
            .unwrap()
            .insert(String::from("destructor"), ContextFunction::new());
        for stmt in &mut destructor.body.statements {
            self.check_stmt(stmt)?;
        }
        //Because it is a destructor we don't keep it in the `context_functions`
        self.context_functions.pop();
        Ok(())
    }

    fn check_copy_ctor(&mut self, copy_ctor: &mut HirStructConstructor<'hir>) -> HirResult<()> {
        self.context_functions.push(HashMap::new());
        self.context_functions
            .last_mut()
            .unwrap()
            .insert(String::from("copy_ctor"), ContextFunction::new());
        for param in &copy_ctor.params {
            self.context_functions
                .last_mut()
                .unwrap()
                .get_mut("copy_ctor")
                .unwrap()
                .insert(
                    param.name,
                    ContextVariable {
                        name: param.name,
                        name_span: param.span,
                        ty: param.ty,
                        _ty_span: param.ty_span,
                        _is_mut: false,
                        is_param: true,
                        refs_locals: vec![],
                    },
                );
        }
        for stmt in &mut copy_ctor.body.statements {
            self.check_stmt(stmt)?;
        }
        //Because it is a copy constructor we don't keep it in the `context_functions`
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
                        name: param.name,
                        name_span: param.span,
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
                        name: param.name,
                        name_span: param.span,
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
        _method: &HirStructMethod<'hir>,
    ) -> HirResult<()> {
        match name {
            "display" => {
                // TODO: Implement display method signature checks
                Ok(())
            }
            "to_string" => {
                // TODO: Implement to_string method signature checks
                Ok(())
            }
            _ => Ok(()),
        }
    }

    fn check_func(&mut self, func: &mut HirFunction<'hir>) -> HirResult<()> {
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
                        name: param.name,
                        name_span: param.span,
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

    fn check_stmt(&mut self, stmt: &mut HirStatement<'hir>) -> HirResult<()> {
        match stmt {
            HirStatement::Expr(e) => {
                self.check_expr(&mut e.expr)?;
                Ok(())
            }
            HirStatement::Return(ret) => {
                let actual_ret_ty = self.check_expr(&mut ret.value)?;

                // Check for returning a reference to a local variable (directly or through a struct)
                let local_refs = self.get_local_ref_targets(&ret.value);
                if let Some(local_var_name) = local_refs.first() {
                    let path = ret.span.path;
                    let src = utils::get_file_content(path).unwrap();
                    return Err(HirError::ReturningReferenceToLocalVariable(
                        ReturningReferenceToLocalVariableError {
                            span: ret.value.span(),
                            var_name: local_var_name.to_string(),
                            src: NamedSource::new(path, src),
                        },
                    ));
                }

                let (expected_ret_ty, span) = if let Some(name) = self.current_class_name {
                    //This means we're in a class method
                    let class = self.signature.structs.get(name).unwrap();
                    let method = class.methods.get(self.current_func_name.unwrap()).unwrap();
                    (
                        self.arena.intern(method.clone().return_ty) as &_,
                        method.return_ty_span.unwrap_or(ret.span),
                    )
                } else if let Some(name) = self.current_func_name {
                    //This means we're in a standalone function
                    let func_ret_from = self.signature.functions.get(name).unwrap();
                    (
                        self.arena.intern(func_ret_from.return_ty.clone()) as &_,
                        func_ret_from.return_ty_span.unwrap_or(ret.span),
                    )
                } else {
                    (self.arena.types().get_uninitialized_ty(), ret.span)
                };
                self.is_equivalent_ty(expected_ret_ty, span, actual_ret_ty, ret.value.span())
            }
            HirStatement::While(w) => {
                let cond_ty = self.check_expr(&mut w.condition)?;
                self.is_equivalent_ty(
                    self.arena.types().get_boolean_ty(),
                    w.condition.span(),
                    cond_ty,
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
                    self.arena.types().get_boolean_ty(),
                    i.condition.span(),
                    cond_ty,
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
                        c.ty,
                        c.ty_span.unwrap_or(c.name_span),
                        expr_ty,
                        c.value.span(),
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
                            name: c.name,
                            name_span: c.name_span,
                            ty: const_ty,
                            _ty_span: c.ty_span.unwrap_or(c.name_span),
                            _is_mut: false,
                            is_param: false,
                            refs_locals,
                        },
                    );

                self.is_equivalent_ty(
                    const_ty,
                    c.ty_span.unwrap_or(c.name_span),
                    expr_ty,
                    c.value.span(),
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

                self.insert_new_variable(ContextVariable {
                    name: l.name,
                    name_span: l.name_span,
                    ty: var_ty,
                    _ty_span: l.ty_span.unwrap_or(l.name_span),
                    _is_mut: true,
                    is_param: false,
                    refs_locals,
                })?;

                self.is_equivalent_ty(
                    var_ty,
                    l.ty_span.unwrap_or(l.name_span),
                    expr_ty,
                    l.value.span(),
                )
            }
            HirStatement::Assign(assign) => {
                let dst_ty = self.check_expr(&mut assign.dst)?;
                let val_ty = self.check_expr(&mut assign.val)?;

                // Check if we are dereferencing a const reference (mutation through const ref)
                // This catches: `*const_ref = value`
                if let HirExpr::Unary(unary_expr) = &assign.dst
                    && let Some(HirUnaryOp::Deref) = &unary_expr.op
                {
                    let deref_target_ty = self.check_expr(&mut unary_expr.expr.clone())?;
                    if deref_target_ty.is_const() {
                        // Allow mutation in constructor for field initialization
                        if !(self.current_func_name == Some("constructor")
                            && self.current_class_name.is_some())
                        {
                            return Err(Self::trying_to_mutate_const_reference(
                                &assign.dst.span(),
                                deref_target_ty,
                            ));
                        }
                    }
                }
                assign.ty = dst_ty;

                // Note: We intentionally do NOT block assignments where lhs.is_const() is true
                // but there's no dereference. For example:
                //   let arr: [&const T] = ...;
                //   arr[i] = some_const_ref;  // This is OK - storing a const ref value
                // This is different from *const_ref = value (mutation through const ref)

                self.is_equivalent_ty(dst_ty, assign.dst.span(), val_ty, assign.val.span())?;
                Ok(())
            }
            HirStatement::Block(block) => {
                // We need to add a new scope for the block
                self.context_functions
                    .last_mut()
                    .unwrap()
                    .get_mut(self.current_func_name.unwrap())
                    .unwrap()
                    .new_scope();
                for stmt in &mut block.statements {
                    self.check_stmt(stmt)?;
                }
                // We end the scope after finishing the block
                self.context_functions
                    .last_mut()
                    .unwrap()
                    .get_mut(self.current_func_name.unwrap())
                    .unwrap()
                    .end_scope();
                Ok(())
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
    fn check_expr(&mut self, expr: &mut HirExpr<'hir>) -> HirResult<&'hir HirTy<'hir>> {
        match expr {
            HirExpr::IntegerLiteral(i) => Ok(i.ty),
            HirExpr::FloatLiteral(f) => Ok(f.ty),
            HirExpr::UnsignedIntegerLiteral(u) => Ok(u.ty),
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
                if class.destructor.as_ref().unwrap().vis != HirVisibility::Public {
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
                //early return for constructor, destructor, and copy constructor
                if function_name == "constructor"
                    || function_name == "destructor"
                    || function_name == "copy_ctor"
                    || function_name == "move_ctor"
                    || function_name == "default_ctor"
                {
                    s.ty = self_ty;
                    return Ok(self_ty);
                }
                let method = class.methods.get(function_name).unwrap();
                match method.modifier {
                    HirStructMethodModifier::Const => {
                        let readonly_self_ty = self.arena.types().get_ref_ty(
                            self_ty,
                            HirReferenceKind::ReadOnly,
                            method.span,
                        );
                        s.ty = readonly_self_ty;
                        Ok(readonly_self_ty)
                    }
                    HirStructMethodModifier::Mutable => {
                        let mutable_self_ty = self.arena.types().get_ref_ty(
                            self_ty,
                            HirReferenceKind::Mutable,
                            method.span,
                        );
                        s.ty = mutable_self_ty;
                        Ok(mutable_self_ty)
                    }
                    HirStructMethodModifier::Consuming => {
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
            // TODO: Null literal should either have a pointer type, or a nullptr_t
            HirExpr::NullLiteral(n) => {
                let ptr_ty = self.arena.types().get_unit_ty();
                n.ty = ptr_ty;
                Ok(ptr_ty)
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
                        // Forbid reference nesting: &(&T) is not allowed
                        if matches!(ty, HirTy::Reference(_)) {
                            let path = u.expr.span().path;
                            let src = utils::get_file_content(path).unwrap();
                            return Err(HirError::ReferenceToReference(
                                ReferenceToReferenceError {
                                    span: u.span,
                                    src: NamedSource::new(path, src),
                                },
                            ));
                        }
                        // & creates a mutable (exclusive) reference by default
                        let ref_ty = self.arena.types().get_ref_ty(
                            ty,
                            HirReferenceKind::Mutable,
                            u.span,
                        );
                        u.ty = ref_ty;
                        Ok(ref_ty)
                    }
                    Some(HirUnaryOp::Deref) => match ty {
                        HirTy::PtrTy(ptr) => {
                            u.ty = ptr.inner;
                            Ok(ptr.inner)
                        }
                        HirTy::Reference(ref_ty) => {
                            u.ty = ref_ty.inner;
                            Ok(ref_ty.inner)
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
                    HirTy::Integer(_)
                        | HirTy::Float(_)
                        | HirTy::UnsignedInteger(_)
                        | HirTy::Boolean(_)
                        | HirTy::Char(_)
                        | HirTy::String(_)
                        | HirTy::PtrTy(_)
                );
                if !can_cast && !c.target_ty.is_raw_ptr() {
                    return Err(Self::type_mismatch_err(
                        &format!("{}", expr_ty),
                        &c.expr.span(),
                        "int64, float64, uint64, bool, char, str or raw pointer",
                        &c.expr.span(),
                    ));
                }
                if self
                    .is_equivalent_ty(expr_ty, c.expr.span(), c.target_ty, c.span)
                    .is_ok()
                {
                    Self::trying_to_cast_to_the_same_type_warning(
                        &c.span,
                        &format!("{}", c.target_ty),
                    );
                    // Unwrap the redundant cast by replacing the casting expression with the inner expression
                    *expr = (*c.expr).clone();
                    return Ok(expr_ty);
                }

                Ok(c.target_ty)
            }
            HirExpr::Indexing(indexing_expr) => {
                let path = indexing_expr.span.path;
                let target = self.check_expr(&mut indexing_expr.target)?;
                let index = self.check_expr(&mut indexing_expr.index)?;
                self.is_equivalent_ty(
                    // we expect the biggest size, because smaller size can be implicitely casted to it
                    self.arena.types().get_uint_ty(64),
                    indexing_expr.index.span(),
                    index,
                    indexing_expr.index.span(),
                )?;
                match target {
                    HirTy::Slice(l) => {
                        indexing_expr.ty = l.inner;
                        Ok(l.inner)
                    }
                    HirTy::InlineArray(arr) => {
                        fn is_inbound(index: &HirExpr, size: usize) -> (bool, usize) {
                            match index {
                                HirExpr::IntegerLiteral(i) => {
                                    if i.value < 0 || (i.value as usize) >= size {
                                        (false, i.value as usize)
                                    } else {
                                        (true, i.value as usize)
                                    }
                                }
                                HirExpr::UnsignedIntegerLiteral(u) => {
                                    if (u.value as usize) >= size {
                                        (false, u.value as usize)
                                    } else {
                                        (true, u.value as usize)
                                    }
                                }
                                HirExpr::Unary(unary) if unary.op.is_none() => {
                                    is_inbound(&unary.expr, size)
                                }
                                // We let the runtime handle non-constant indices
                                _ => (true, 0),
                            }
                        }
                        if let (false, index) = is_inbound(&indexing_expr.index, arr.size) {
                            let src = utils::get_file_content(path).unwrap();
                            return Err(HirError::ListIndexOutOfBounds(
                                ListIndexOutOfBoundsError {
                                    span: indexing_expr.index.span(),
                                    index,
                                    size: arr.size,
                                    src: NamedSource::new(path, src),
                                },
                            ));
                        }
                        indexing_expr.ty = arr.inner;
                        Ok(arr.inner)
                    }
                    HirTy::String(_) => {
                        indexing_expr.ty = self.arena.types().get_char_ty();
                        Ok(self.arena.types().get_char_ty())
                    }
                    HirTy::PtrTy(ptr_ty) => {
                        indexing_expr.ty = ptr_ty.inner;
                        Ok(ptr_ty.inner)
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
                l.ty = self.arena.types().get_inline_arr_ty(ty, l.items.len());
                Ok(l.ty)
            }
            HirExpr::NewArray(a) => {
                let size_ty = self.check_expr(a.size.as_mut())?;
                self.is_equivalent_ty(
                    self.arena.types().get_uint_ty(64),
                    a.size.span(),
                    size_ty,
                    a.size.span(),
                )?;
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
                    let name = MonomorphizationPass::generate_mangled_name(self.arena, g, "union");
                    union_ty = self.arena.intern(HirNamedTy { name, span: g.span })
                        as &'hir HirNamedTy<'hir>;
                    let tmp = match self.signature.unions.get(name) {
                        Some(c) => c,
                        None => {
                            return Err(Self::unknown_type_err(
                                &HirPrettyPrinter::generic_ty_str(g),
                                &obj_lit.span,
                            ));
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
                    return Err(HirError::AccessingPrivateUnion(
                        AccessingPrivateUnionError {
                            name: if let Some(n) = union_signature.pre_mangled_ty {
                                HirPrettyPrinter::generic_ty_str(n)
                            } else {
                                union_ty.name.to_owned()
                            },
                            span: obj_lit.span,
                            src: NamedSource::new(obj_path, obj_src),
                            origin: AccessingPrivateObjectOrigin {
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
                    self.is_equivalent_ty(
                        field_signature.ty,
                        field_signature.span,
                        field_ty,
                        field.value.span(),
                    )?;
                }

                Ok(obj_lit.ty)
            }
            HirExpr::NewObj(new_obj_expr) => {
                let struct_ty;
                let struct_signature = if let HirTy::Named(n) = new_obj_expr.ty {
                    struct_ty = n;
                    let tmp = match self.signature.structs.get(n.name) {
                        Some(c) => c,
                        None => {
                            return Err(Self::unknown_type_err(n.name, &new_obj_expr.span));
                        }
                    };
                    *tmp
                } else if let HirTy::Generic(g) = new_obj_expr.ty {
                    let name = MonomorphizationPass::generate_mangled_name(self.arena, g, "struct");
                    struct_ty = self.arena.intern(HirNamedTy { name, span: g.span })
                        as &'hir HirNamedTy<'hir>;
                    let tmp = match self.signature.structs.get(name) {
                        Some(c) => c,
                        None => {
                            return Err(Self::unknown_type_err(name, &new_obj_expr.span));
                        }
                    };
                    *tmp
                } else {
                    let path = new_obj_expr.span.path;
                    let src = utils::get_file_content(path).unwrap();
                    return Err(HirError::CanOnlyConstructStructs(
                        CanOnlyConstructStructsError {
                            span: new_obj_expr.span,
                            src: NamedSource::new(path, src),
                        },
                    ));
                };
                if struct_signature.name_span.path != new_obj_expr.span.path
                    && struct_signature.vis != HirVisibility::Public
                {
                    let origin_path = struct_signature.name_span.path;
                    let origin_src = utils::get_file_content(origin_path).unwrap();
                    let obj_path = new_obj_expr.span.path;
                    let obj_src = utils::get_file_content(obj_path).unwrap();
                    return Err(HirError::AccessingPrivateStruct(
                        AccessingPrivateStructError {
                            name: if let Some(n) = struct_signature.pre_mangled_ty {
                                HirPrettyPrinter::generic_ty_str(n)
                            } else {
                                struct_ty.name.to_owned()
                            },
                            span: new_obj_expr.span,
                            src: NamedSource::new(obj_path, obj_src),
                            origin: AccessingPrivateObjectOrigin {
                                span: struct_signature.name_span,
                                src: NamedSource::new(origin_path, origin_src),
                            },
                        },
                    ));
                }
                let mut is_copy_ctor_call = false;
                if struct_signature.constructor.params.len() != new_obj_expr.args.len() {
                    if self.is_copy_constructor_call(new_obj_expr) {
                        is_copy_ctor_call = true;
                    } else {
                        return Err(Self::type_mismatch_err(
                            &format!("{} argument(s)", struct_signature.constructor.params.len()),
                            &struct_signature.constructor.span,
                            &format!("{} parameter(s)", new_obj_expr.args.len()),
                            &new_obj_expr.span,
                        ));
                    }
                }

                if !is_copy_ctor_call {
                    if struct_signature.constructor.vis != HirVisibility::Public
                        && self.current_class_name != Some(struct_ty.name)
                    {
                        let path = new_obj_expr.span.path;
                        let src = utils::get_file_content(path).unwrap();
                        return Err(HirError::AccessingPrivateConstructor(
                            AccessingPrivateConstructorError {
                                span: new_obj_expr.span,
                                kind: String::from("constructor"),
                                src: NamedSource::new(path, src),
                            },
                        ));
                    }
                    for (param, arg) in struct_signature
                        .constructor
                        .params
                        .iter()
                        .zip(new_obj_expr.args.iter_mut())
                    {
                        let arg_ty = self.check_expr(arg)?;
                        self.is_equivalent_ty(param.ty, param.span, arg_ty, arg.span())?;
                    }
                } else {
                    //Check copy constructor
                    if let Some(copy_ctor) = &struct_signature.copy_constructor {
                        // Check if copy constructor's where_clause constraints are satisfied
                        if !copy_ctor.is_constraint_satisfied {
                            let path = new_obj_expr.span.path;
                            let src = utils::get_file_content(path).unwrap();
                            return Err(HirError::MethodConstraintNotSatisfied(
                                MethodConstraintNotSatisfiedError {
                                    member_kind: "Copy constructor".to_string(),
                                    member_name: if let Some(n) = struct_signature.pre_mangled_ty {
                                        n.name.to_string()
                                    } else {
                                        struct_ty.name.to_string()
                                    },
                                    ty_name: if let Some(n) = struct_signature.pre_mangled_ty {
                                        HirPrettyPrinter::generic_ty_str(n)
                                    } else {
                                        struct_ty.name.to_string()
                                    },
                                    span: new_obj_expr.span,
                                    src: NamedSource::new(path, src),
                                },
                            ));
                        }

                        let param = &copy_ctor.params.first().unwrap();
                        let arg = new_obj_expr.args.first_mut().unwrap();
                        let arg_ty = self.check_expr(arg)?;
                        self.is_equivalent_ty(param.ty, param.span, arg_ty, arg.span())?;
                        new_obj_expr.is_copy_constructor_call = true;
                    } else {
                        return Err(HirError::ThisStructDoesNotHaveACopyConstructor(
                            ThisStructDoesNotHaveACopyConstructorError {
                                span: new_obj_expr.span,
                                src: NamedSource::new(
                                    new_obj_expr.span.path,
                                    utils::get_file_content(new_obj_expr.span.path).unwrap(),
                                ),
                            },
                        ));
                    }
                }
                new_obj_expr.ty = self
                    .arena
                    .types()
                    .get_named_ty(struct_ty.name, struct_ty.span);
                Ok(new_obj_expr.ty)
            }
            HirExpr::Call(func_expr) => {
                let path = func_expr.span.path;

                let callee = func_expr.callee.as_mut();
                match callee {
                    HirExpr::Ident(i) => {
                        // First check if the function is external by looking up the base name
                        let base_func = self.signature.functions.get(i.name);
                        let (is_external, is_intrinsic) = base_func
                            .map(|f| (f.is_external, f.is_intrinsic))
                            .unwrap_or((false, false));

                        // Only mangle the name if it's NOT external and has generics
                        let name = if func_expr.generics.is_empty() || is_external || is_intrinsic {
                            i.name
                        } else {
                            MonomorphizationPass::generate_mangled_name(
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
                                    name: if let Some(n) = func.pre_mangled_ty {
                                        HirPrettyPrinter::generic_ty_str(n)
                                    } else {
                                        name.to_string()
                                    },
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
                            let ty = self.arena.intern(self.check_extern_fn(
                                name,
                                &mut func_expr.generics,
                                &mut func_expr.args,
                                func_expr.span,
                                func,
                            )?);
                            func_expr.ty = ty;
                            return Ok(ty);
                        }

                        for (param, arg) in func.params.iter().zip(func_expr.args.iter_mut()) {
                            let arg_ty = self.check_expr(arg)?;
                            self.is_equivalent_ty(param.ty, param.span, arg_ty, arg.span())?;
                            // Store the expected parameter type for ownership analysis
                            func_expr.args_ty.push(param.ty);
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
                                    name: if let Some(n) = class.pre_mangled_ty {
                                        HirPrettyPrinter::generic_ty_str(n)
                                    } else {
                                        name.to_string()
                                    },
                                    span: field_access.span,
                                    src: NamedSource::new(access_path, access_src),
                                    origin: AccessingPrivateObjectOrigin {
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
                            // Check if method's where_clause constraints are satisfied
                            if !method_signature.is_constraint_satisfied {
                                let path = field_access.span.path;
                                let src = utils::get_file_content(path).unwrap();
                                return Err(HirError::MethodConstraintNotSatisfied(
                                    MethodConstraintNotSatisfiedError {
                                        member_kind: "method".to_string(),
                                        member_name: field_access.field.name.to_string(),
                                        ty_name: if let Some(n) = class.pre_mangled_ty {
                                            HirPrettyPrinter::generic_ty_str(n)
                                        } else {
                                            name.to_string()
                                        },
                                        span: field_access.span,
                                        src: NamedSource::new(path, src),
                                    },
                                ));
                            }

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

                            if self.is_readonly_ref_ty(target_ty)
                                && method_signature.modifier != HirStructMethodModifier::Const
                            {
                                return Err(Self::calling_non_const_method_on_const_reference_err(
                                    &method_signature.span,
                                    &field_access.span,
                                ));
                            }

                            if self.is_mutable_ref_ty(target_ty)
                                && method_signature.modifier == HirStructMethodModifier::Consuming
                            {
                                // Consuming methods take ownership of `this`, which is not
                                // possible through a reference. &const this and &this methods
                                // are fine to call on mutable references.
                                return Err(
                                    Self::calling_consuming_method_on_mutable_reference_err(
                                        &method_signature.span,
                                        &field_access.span,
                                    ),
                                );
                            }

                            for (param, arg) in method_signature
                                .params
                                .iter()
                                .zip(func_expr.args.iter_mut())
                            {
                                let arg_ty = self.check_expr(arg)?;
                                self.is_equivalent_ty(param.ty, param.span, arg_ty, arg.span())?;
                                // Store the expected parameter type for ownership analysis
                                func_expr.args_ty.push(param.ty);
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
                            HirTy::Generic(g) => {
                                MonomorphizationPass::generate_mangled_name(self.arena, g, "struct")
                            }
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
                            // Check if method's where_clause constraints are satisfied
                            if !method_signature.is_constraint_satisfied {
                                let path = static_access.span.path;
                                let src = utils::get_file_content(path).unwrap();
                                return Err(HirError::MethodConstraintNotSatisfied(
                                    MethodConstraintNotSatisfiedError {
                                        member_kind: "method".to_string(),
                                        member_name: static_access.field.name.to_string(),
                                        ty_name: if let Some(n) = class.pre_mangled_ty {
                                            HirPrettyPrinter::generic_ty_str(n)
                                        } else {
                                            name.to_string()
                                        },
                                        span: static_access.span,
                                        src: NamedSource::new(path, src),
                                    },
                                ));
                            }

                            if method_signature.modifier == HirStructMethodModifier::Consuming
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
                                self.is_equivalent_ty(param.ty, param.span, arg_ty, arg.span())?;
                                // Store the expected parameter type for ownership analysis
                                func_expr.args_ty.push(param.ty);
                            }

                            static_access.ty =
                                self.arena.intern(method_signature.return_ty.clone());
                            func_expr.ty = self.arena.intern(method_signature.return_ty.clone());
                            static_access.field.ty =
                                self.arena.intern(method_signature.return_ty.clone());

                            Ok(func_expr.ty)
                        } else {
                            // It might be one of the name for the constructors/destructor
                            // All possible names are: "__ctor", "copy", "__mov_ctor", "default", "__dtor"
                            match static_access.field.name {
                                "__ctor" => {
                                    if func_expr.args.len() != class.constructor.params.len() {
                                        return Err(Self::not_enough_arguments_err(
                                            "constructor".to_string(),
                                            class.constructor.params.len(),
                                            &static_access.span,
                                            func_expr.args.len(),
                                            &func_expr.span,
                                        ));
                                    }
                                    for (param, arg) in class
                                        .constructor
                                        .params
                                        .iter()
                                        .zip(func_expr.args.iter_mut())
                                    {
                                        let arg_ty = self.check_expr(arg)?;
                                        self.is_equivalent_ty(
                                            param.ty,
                                            param.span,
                                            arg_ty,
                                            arg.span(),
                                        )?;
                                        // Store the expected parameter type for ownership analysis
                                        func_expr.args_ty.push(param.ty);
                                    }
                                    let ret_ty = self
                                        .arena
                                        .types()
                                        .get_named_ty(name, static_access.field.span);
                                    func_expr.ty = ret_ty;
                                    Ok(ret_ty)
                                }
                                "copy" | "__cpy_ctor" => {
                                    if func_expr.args.len() != 1 {
                                        return Err(Self::not_enough_arguments_err(
                                            "copy constructor".to_string(),
                                            1,
                                            &static_access.span,
                                            func_expr.args.len(),
                                            &func_expr.span,
                                        ));
                                    }
                                    let (expected_ty, expected_span) =
                                        if let Some(copy_ctor) = &class.copy_constructor {
                                            // No need to do .first().unwrap() because we already checked args.len() == 1
                                            (copy_ctor.params[0].ty, copy_ctor.params[0].span)
                                        } else {
                                            let path = static_access.span.path;
                                            let src = utils::get_file_content(path).unwrap();
                                            return Err(
                                                HirError::ThisStructDoesNotHaveACopyConstructor(
                                                    ThisStructDoesNotHaveACopyConstructorError {
                                                        span: static_access.span,
                                                        src: NamedSource::new(path, src),
                                                    },
                                                ),
                                            );
                                        };
                                    let found_ty = self.check_expr(&mut func_expr.args[0])?;
                                    self.is_equivalent_ty(
                                        expected_ty,
                                        expected_span,
                                        found_ty,
                                        func_expr.args[0].span(),
                                    )?;
                                    // Store the expected parameter type for ownership analysis
                                    func_expr.args_ty.push(expected_ty);
                                    let ret_ty = self
                                        .arena
                                        .types()
                                        .get_named_ty(name, static_access.field.span);
                                    func_expr.ty = ret_ty;
                                    Ok(ret_ty)
                                }
                                "__mov_ctor" => {
                                    if func_expr.args.len() != 1 {
                                        return Err(Self::not_enough_arguments_err(
                                            "move constructor".to_string(),
                                            1,
                                            &static_access.span,
                                            func_expr.args.len(),
                                            &func_expr.span,
                                        ));
                                    }
                                    let (expected_ty, expected_span) =
                                        if let Some(move_ctor) = &class.move_constructor {
                                            // No need to do .first().unwrap() because we already checked args.len() == 1
                                            (move_ctor.params[0].ty, move_ctor.params[0].span)
                                        } else {
                                            let path = static_access.span.path;
                                            let src = utils::get_file_content(path).unwrap();
                                            return Err(
                                                HirError::ThisStructDoesNotHaveAMoveConstructor(
                                                    ThisStructDoesNotHaveAMoveConstructorError {
                                                        span: static_access.span,
                                                        src: NamedSource::new(path, src),
                                                    },
                                                ),
                                            );
                                        };
                                    let found_ty = self.check_expr(&mut func_expr.args[0])?;
                                    self.is_equivalent_ty(
                                        expected_ty,
                                        expected_span,
                                        found_ty,
                                        func_expr.args[0].span(),
                                    )?;
                                    // Store the expected parameter type for ownership analysis
                                    func_expr.args_ty.push(expected_ty);
                                    let ret_ty = self
                                        .arena
                                        .types()
                                        .get_named_ty(name, static_access.field.span);
                                    func_expr.ty = ret_ty;
                                    Ok(ret_ty)
                                }
                                "default" => {
                                    if func_expr.args.is_empty() {
                                        return Err(Self::not_enough_arguments_err(
                                            "default constructor".to_string(),
                                            0,
                                            &static_access.span,
                                            func_expr.args.len(),
                                            &func_expr.span,
                                        ));
                                    }
                                    let ret_ty = self
                                        .arena
                                        .types()
                                        .get_named_ty(name, static_access.field.span);
                                    func_expr.ty = ret_ty;
                                    Ok(ret_ty)
                                }
                                "__dtor" => {
                                    if func_expr.args.is_empty() {
                                        return Err(Self::not_enough_arguments_err(
                                            "destructor".to_string(),
                                            0,
                                            &static_access.span,
                                            func_expr.args.len(),
                                            &func_expr.span,
                                        ));
                                    }
                                    let unit_ty = self.arena.types().get_unit_ty();
                                    func_expr.ty = unit_ty;
                                    Ok(unit_ty)
                                }
                                _ => Err(Self::unknown_method_err(
                                    static_access.field.name,
                                    name,
                                    &static_access.span,
                                )),
                            }
                        }
                    }
                    _ => Err(HirError::UnsupportedExpr(UnsupportedExpr {
                        span: func_expr.span,
                        expr: "Function call on non-identifier expression".to_string(),
                        src: NamedSource::new(path, utils::get_file_content(path).unwrap()),
                    })),
                }
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
                                    // Preserve reference type from target_ty like struct field access does
                                    if self.is_readonly_ref_ty(target_ty) {
                                        field_access.ty = self.arena.types().get_ref_ty(
                                            var.ty,
                                            HirReferenceKind::ReadOnly,
                                            field_access.span,
                                        );
                                        field_access.field.ty = self.arena.types().get_ref_ty(
                                            var.ty,
                                            HirReferenceKind::ReadOnly,
                                            field_access.span,
                                        );
                                        return Ok(field_access.ty);
                                    } else if self.is_mutable_ref_ty(target_ty) {
                                        field_access.ty = self.arena.types().get_ref_ty(
                                            var.ty,
                                            HirReferenceKind::Mutable,
                                            field_access.span,
                                        );
                                        field_access.field.ty = self.arena.types().get_ref_ty(
                                            var.ty,
                                            HirReferenceKind::Mutable,
                                            field_access.span,
                                        );
                                        return Ok(field_access.ty);
                                    } else {
                                        field_access.ty = var.ty;
                                        field_access.field.ty = var.ty;
                                        return Ok(var.ty);
                                    }
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
                    if self.is_readonly_ref_ty(target_ty) {
                        field_access.ty = self.arena.types().get_ref_ty(
                            field_signature.ty,
                            HirReferenceKind::ReadOnly,
                            field_access.span,
                        );
                        field_access.field.ty = self.arena.types().get_ref_ty(
                            field_signature.ty,
                            HirReferenceKind::ReadOnly,
                            field_access.span,
                        );
                    } else if self.is_mutable_ref_ty(target_ty) {
                        field_access.ty = self.arena.types().get_ref_ty(
                            field_signature.ty,
                            HirReferenceKind::Mutable,
                            field_access.span,
                        );
                        field_access.field.ty = self.arena.types().get_ref_ty(
                            field_signature.ty,
                            HirReferenceKind::Mutable,
                            field_access.span,
                        );
                    } else {
                        field_access.ty = field_signature.ty;
                        field_access.field.ty = field_signature.ty;
                    }
                    match field_access.field.ty {
                        HirTy::Named(n) => {
                            if self.signature.enums.contains_key(n.name) {
                                Ok(self.arena.types().get_uint_ty(64))
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
                                            ty: self.arena.types().get_uint_ty(64),
                                        },
                                    );
                                    *expr = replaced_expr;
                                    return Ok(self.arena.types().get_uint_ty(64));
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
            // Copy expressions: type-check the inner expression
            HirExpr::Copy(copy_expr) => {
                let ty = self.check_expr(&mut copy_expr.expr)?;
                copy_expr.ty = ty;
                Ok(ty)
            }
            HirExpr::IntrinsicCall(intrinsic) => {
                // Intrinsic functions are defined as external functions with special handling
                //Let's just use the extern fn checker for now
                let signature = match self.signature.functions.get(intrinsic.name) {
                    Some(sig) => sig,
                    None => todo!(
                        "Add error when the intrinsic is not declared (shouldn't happen though)"
                    ),
                };
                self.check_extern_fn(
                    intrinsic.name,
                    &mut intrinsic.args_ty,
                    &mut intrinsic.args,
                    intrinsic.span,
                    signature,
                )
            }
        }
    }

    fn is_copy_constructor_call(&mut self, new_obj: &HirNewObjExpr<'hir>) -> bool {
        if new_obj.args.len() != 1 {
            eprintln!("Copy constructor must have exactly one argument");
            return false;
        }
        let arg_ty = self.check_expr(&mut new_obj.args[0].clone());
        if arg_ty.is_err() {
            eprintln!("Failed to check argument type for copy constructor");
            return false;
        }
        let struct_ty = {
            let struct_name = match new_obj.ty {
                HirTy::Named(n) => n.name,
                HirTy::Generic(g) => {
                    MonomorphizationPass::generate_mangled_name(self.arena, g, "struct")
                }
                _ => {
                    eprintln!("New object type is not named or generic");
                    return false;
                }
            };
            self.arena.types().get_ref_ty(
                self.arena.types().get_named_ty(struct_name, new_obj.span),
                HirReferenceKind::Mutable,
                new_obj.span,
            )
        };
        let arg_ty = arg_ty.unwrap();
        if self
            .is_equivalent_ty(struct_ty, new_obj.span, arg_ty, new_obj.args[0].span())
            .is_err()
        {
            eprintln!(
                "Argument type {} is not equivalent to struct type {} for copy constructor",
                arg_ty, struct_ty
            );
            return false;
        }
        true
    }

    fn check_extern_fn(
        &mut self,
        name: &'hir str,
        call_expr_generics: &mut Vec<&'hir HirTy<'hir>>,
        call_expr_args: &mut Vec<HirExpr<'hir>>,
        call_expr_span: Span,
        signature: &'hir HirFunctionSignature<'hir>,
    ) -> HirResult<&'hir HirTy<'hir>> {
        let args_ty = call_expr_args
            .iter_mut()
            .map(|a| self.check_expr(a))
            .collect::<HirResult<Vec<_>>>()?;

        // Create cache key including explicit generics to distinguish calls like default::<int64>() from default::<string>()
        let explicit_generics = call_expr_generics.clone();
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
        if !call_expr_generics.is_empty() {
            // Use explicit type arguments from the function call
            if !signature.generics.is_empty() {
                for (generic_param, concrete_ty) in
                    signature.generics.iter().zip(call_expr_generics.iter())
                {
                    generics.push((generic_param.generic_name, concrete_ty));
                }
            }
            // Still need to create params even with explicit generics
            for (param, _arg) in signature.params.iter().zip(args_ty.iter()) {
                // Substitute the generic with the concrete type while preserving type constructors
                let param_ty = if let Some(generic_name) = Self::get_generic_name(param.ty) {
                    if let Some((_, concrete_ty)) =
                        generics.iter().find(|(name, _)| *name == generic_name)
                    {
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
                            &call_expr_args[i].span(),
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
                    if let Some((_, concrete_ty)) =
                        generics.iter().find(|(name, _)| *name == generic_name)
                    {
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
        } else if call_expr_generics.is_empty() {
            // Parameterless function with no explicit type arguments - error
            if !signature.generics.is_empty() {
                return Err(Self::type_mismatch_err(
                    "parameterless generic function",
                    &call_expr_span,
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
                        &format!(
                            "generic parameter `{}` should be inferred from function arguments",
                            name
                        ),
                        &signature.span,
                        &format!(
                            "inferred type for `{}` from the return type `{}`",
                            name, monomorphized.return_ty
                        ),
                        &call_expr_span,
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
            HirTy::Slice(l) => Self::get_generic_name(l.inner),
            HirTy::Reference(r) => Self::get_generic_name(r.inner),
            HirTy::Named(n) => Some(n.name),
            HirTy::PtrTy(ptr_ty) => Self::get_generic_name(ptr_ty.inner),
            _ => None,
        }
    }

    fn get_generic_ret_ty(
        &self,
        ty: &'hir HirTy<'hir>,
        actual_generic_ty: &'hir HirTy<'hir>,
    ) -> &'hir HirTy<'hir> {
        match ty {
            HirTy::Slice(l) => self
                .arena
                .types()
                .get_slice_ty(self.get_generic_ret_ty(l.inner, actual_generic_ty)),
            HirTy::InlineArray(arr) => self.arena.types().get_inline_arr_ty(
                self.get_generic_ret_ty(arr.inner, actual_generic_ty),
                arr.size,
            ),
            HirTy::Named(_) => actual_generic_ty,
            HirTy::PtrTy(ptr_ty) => self
                .arena
                .types()
                .get_ptr_ty(self.get_generic_ret_ty(ptr_ty.inner, actual_generic_ty)),
            HirTy::Reference(r) => self.arena.types().get_ref_ty(
                self.get_generic_ret_ty(r.inner, actual_generic_ty),
                r.kind,
                r.span,
            ),
            _ => actual_generic_ty,
        }
    }

    /// Return the type of the generic after monormophization
    /// e.g. `foo<T>(a: T) -> T` with `foo(42)` will return `int64`
    /// e.g. `foo<T>(a: T&&) -> T` with `foo(value)` where value is int64&& will return `int64`
    fn get_generic_ty(
        ty: &'hir HirTy<'hir>,
        given_ty: &'hir HirTy<'hir>,
    ) -> Option<&'hir HirTy<'hir>> {
        match (ty, given_ty) {
            (HirTy::Slice(l1), HirTy::Slice(l2)) => Self::get_generic_ty(l1.inner, l2.inner),
            (HirTy::Reference(ref1), HirTy::Reference(ref2)) => {
                Self::get_generic_ty(ref1.inner, ref2.inner)
            }
            (HirTy::Named(_), _) => Some(given_ty),
            (HirTy::Reference(r1), _) => Self::get_generic_ty(r1.inner, given_ty),
            (_, HirTy::Reference(r2)) => Self::get_generic_ty(ty, r2.inner),
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

    fn is_readonly_ref_ty(&self, ty: &HirTy<'_>) -> bool {
        matches!(
            ty,
            HirTy::Reference(HirReferenceTy {
                kind: HirReferenceKind::ReadOnly,
                ..
            })
        )
    }

    fn is_mutable_ref_ty(&self, ty: &HirTy<'_>) -> bool {
        matches!(
            ty,
            HirTy::Reference(HirReferenceTy {
                kind: HirReferenceKind::Mutable,
                ..
            })
        )
    }

    /// Check if two types are equivalent, considering generics and references
    /// We need a way to handle &primitive and &const primitive being equivalent to primitive
    fn is_equivalent_ty(
        &self,
        expected_ty: &HirTy<'_>,
        expected_span: Span,
        found_ty: &HirTy<'_>,
        found_span: Span,
    ) -> HirResult<()> {
        match (expected_ty, found_ty) {
            //(HirTy::Int64(_), HirTy::UInt64(_)) | (HirTy::UInt64(_), HirTy::Int64(_)) => Ok(()),
            (HirTy::Generic(g), HirTy::Named(n)) | (HirTy::Named(n), HirTy::Generic(g)) => {
                if MonomorphizationPass::generate_mangled_name(self.arena, g, "struct") == n.name {
                    Ok(())
                } else {
                    Err(Self::type_mismatch_err(
                        &format!("{}", expected_ty),
                        &expected_span,
                        &format!("{}", found_ty),
                        &found_span,
                    ))
                }
            }
            //Check for enums
            (HirTy::Named(n), HirTy::UnsignedInteger(_))
            | (HirTy::UnsignedInteger(_), HirTy::Named(n)) => {
                if self.signature.enums.contains_key(n.name) {
                    Ok(())
                } else {
                    Err(Self::type_mismatch_err(
                        &format!("{}", expected_ty),
                        &expected_span,
                        &format!("{}", found_ty),
                        &found_span,
                    ))
                }
            }
            (HirTy::InlineArray(list1), HirTy::InlineArray(list2)) => {
                if list1.size != list2.size {
                    return Err(Self::type_mismatch_err(
                        &format!("{}", expected_ty),
                        &expected_span,
                        &format!("{}", found_ty),
                        &found_span,
                    ));
                }
                self.is_equivalent_ty(list1.inner, expected_span, list2.inner, found_span)
            }
            (HirTy::Integer(i), HirTy::Integer(j)) => {
                if i.size_in_bits >= j.size_in_bits {
                    Ok(())
                } else {
                    Err(Self::type_mismatch_err(
                        &format!("{}", expected_ty),
                        &expected_span,
                        &format!("{}", found_ty),
                        &found_span,
                    ))
                }
            }
            (HirTy::UnsignedInteger(i), HirTy::UnsignedInteger(j)) => {
                if i.size_in_bits >= j.size_in_bits {
                    Ok(())
                } else {
                    Err(Self::type_mismatch_err(
                        &format!("{}", expected_ty),
                        &expected_span,
                        &format!("{}", found_ty),
                        &found_span,
                    ))
                }
            }
            (HirTy::Float(i), HirTy::Float(j)) => {
                if i.size_in_bits >= j.size_in_bits {
                    Ok(())
                } else {
                    Err(Self::type_mismatch_err(
                        &format!("{}", expected_ty),
                        &expected_span,
                        &format!("{}", found_ty),
                        &found_span,
                    ))
                }
            }
            (HirTy::Reference(r1), HirTy::Reference(r2)) => {
                // A T& can become a T&& but not the other way around
                // A T& can become a const T& but not the other way around
                // A const T& can become a const T&& but not the other way around
                //TODO: Improve error messages here to specify which kind of reference mismatch occurred
                match (&r1.kind, &r2.kind) {
                    // A T&& cannot become a T&
                    (HirReferenceKind::Mutable, HirReferenceKind::Moveable) => {
                        return Err(HirError::RvalueReferenceToLvalueReferenceError(
                            RvalueReferenceToLvalueReferenceError {
                                r_val_span: found_span,
                                l_val_span: expected_span,
                                src: NamedSource::new(
                                    expected_span.path,
                                    utils::get_file_content(expected_span.path).unwrap(),
                                ),
                            },
                        ));
                    }
                    // A const T& cannot become a T&
                    (HirReferenceKind::Mutable, HirReferenceKind::ReadOnly) => {
                        return Err(Self::type_mismatch_err(
                            &format!("{}", expected_ty),
                            &expected_span,
                            &format!("{}", found_ty),
                            &found_span,
                        ));
                    }
                    // A T&& cannot become a const T&
                    (HirReferenceKind::ReadOnly, HirReferenceKind::Moveable) => {
                        return Err(Self::type_mismatch_err(
                            &format!("{}", expected_ty),
                            &expected_span,
                            &format!("{}", found_ty),
                            &found_span,
                        ));
                    }
                    _ => self.is_equivalent_ty(r1.inner, expected_span, r2.inner, found_span),
                }
            }
            // If it expects a value type, but found a reference, it should only works if the reference is copyable
            (expected, HirTy::Reference(r)) => {
                self.is_equivalent_ty(expected, expected_span, r.inner, found_span)?;
                // check if found is copyable
                match expected {
                    HirTy::Reference(HirReferenceTy {
                        kind: HirReferenceKind::Mutable,
                        inner,
                        ..
                    })
                    | HirTy::Reference(HirReferenceTy {
                        kind: HirReferenceKind::ReadOnly,
                        inner,
                        ..
                    }) => {
                        if inner.is_copyable(&self.signature) {
                            return Ok(());
                        }
                    }
                    _ => {
                        if expected.is_copyable(&self.signature) {
                            return Ok(());
                        }
                    }
                }
                return Err(HirError::TypeIsNotCopyable(TypeIsNotCopyableError {
                    span: found_span,
                    type_name: found_ty.to_string(),
                    src: NamedSource::new(
                        found_span.path,
                        utils::get_file_content(found_span.path).unwrap(),
                    ),
                }));
            }
            (HirTy::Reference(r), found) => {
                self.is_equivalent_ty(r.inner, expected_span, found_ty, found_span)?;
                if r.kind == HirReferenceKind::Moveable {
                    // check if found is moveable
                    if found.is_moveable(&self.signature) {
                        return Ok(());
                    } else {
                        return Err(HirError::TypeIsNotMoveable(TypeIsNotMoveableError {
                            span: found_span,
                            ty_name: found_ty.to_string(),
                            src: NamedSource::new(
                                found_span.path,
                                utils::get_file_content(found_span.path).unwrap(),
                            ),
                        }));
                    }
                }
                Ok(())
            }
            // TODO: Replace Unit type with a proper nullptr_t type
            (HirTy::PtrTy(_), HirTy::Unit(_)) => Ok(()),
            _ => {
                if HirTyId::from(expected_ty) == HirTyId::from(found_ty) {
                    Ok(())
                } else {
                    Err(Self::type_mismatch_err(
                        &format!("{}", expected_ty),
                        &expected_span,
                        &format!("{}", found_ty),
                        &found_span,
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
                if self
                    .signature
                    .structs
                    .contains_key(MonomorphizationPass::generate_mangled_name(
                        self.arena, g, "struct",
                    ))
                {
                    name = MonomorphizationPass::generate_mangled_name(self.arena, g, "struct");
                    return Some(name);
                } else {
                    name = MonomorphizationPass::generate_mangled_name(self.arena, g, "union");
                    if self.signature.unions.contains_key(name) {
                        return Some(name);
                    }
                }
                None
            }
            HirTy::Reference(r) => self.get_class_name_of_type(r.inner),
            _ => None,
        }
    }

    /// Check if a type has a cyclic reference to the target struct.
    /// This function:
    /// - Returns false for references (they don't cause infinite size)
    /// - Recursively checks fields of structs to detect indirect cycles
    /// - Uses a visited set to avoid infinite recursion
    /// - Collects the path of the cycle with labeled spans for clear error reporting
    fn has_cyclic_reference(
        &self,
        ty: &HirTy<'hir>,
        target_struct: &HirStructSignature<'hir>,
        visited: &mut HashSet<&'hir str>,
        cycle_path: &mut Vec<miette::LabeledSpan>,
        current_field_span: Span,
    ) -> bool {
        match ty {
            // Check named struct type
            HirTy::Named(named) => {
                // If it's the target struct, we found a cycle
                if named.name == target_struct.name {
                    let type_name = Self::get_type_display_name(ty);
                    cycle_path.push(miette::LabeledSpan::new_with_span(
                        Some(format!(
                            "field of type `{}` completes the cycle back to `{}`",
                            type_name, target_struct.name
                        )),
                        current_field_span,
                    ));
                    return true;
                }

                // Avoid infinite recursion by checking if we've already visited this struct
                if visited.contains(named.name) {
                    return false;
                }
                visited.insert(named.name);

                // Add current field to the path
                let type_name = Self::get_type_display_name(ty);
                let path_index = cycle_path.len();
                cycle_path.push(miette::LabeledSpan::new_with_span(
                    Some(format!("→ field of type `{}`", type_name)),
                    current_field_span,
                ));

                // Recursively check the fields of this struct
                if let Some(struct_def) = self.signature.structs.get(named.name) {
                    for field in struct_def.fields.values() {
                        if self.has_cyclic_reference(
                            field.ty,
                            target_struct,
                            visited,
                            cycle_path,
                            field.span,
                        ) {
                            return true;
                        }
                    }
                } else if let Some(union_def) = self.signature.unions.get(named.name) {
                    // If there is only one variant, we need to check it for cycles
                    if !union_def.variants.len() <= 1 {
                        for variant in union_def.variants.values() {
                            // If it's one variant and there are others, skip checking this variant to avoid false positives
                            if self.has_cyclic_reference(
                                variant.ty,
                                target_struct,
                                visited,
                                cycle_path,
                                variant.span,
                            ) {
                                return true;
                            }
                        }
                    }
                }

                // No cycle found through this path, remove it
                cycle_path.truncate(path_index);
                visited.remove(named.name);
                false
            }

            // Check generic struct type
            HirTy::Generic(generic) => {
                // Get the mangled name for this generic struct
                let mangled_name =
                    MonomorphizationPass::generate_mangled_name(self.arena, generic, "struct");

                // If the mangled name matches or resolves to the target struct, we found a cycle
                if mangled_name == target_struct.name {
                    let type_name = Self::get_type_display_name(ty);
                    cycle_path.push(miette::LabeledSpan::new_with_span(
                        Some(format!(
                            "field of type `{}` completes the cycle back to `{}`",
                            type_name,
                            if let Some(gen_ty) = target_struct.pre_mangled_ty {
                                HirPrettyPrinter::generic_ty_str(gen_ty)
                            } else {
                                target_struct.name.to_string()
                            }
                        )),
                        current_field_span,
                    ));
                    return true;
                }

                // Avoid infinite recursion
                if visited.contains(mangled_name) {
                    return false;
                }
                visited.insert(mangled_name);

                // Add current field to the path
                let type_name = Self::get_type_display_name(ty);
                let path_index = cycle_path.len();
                cycle_path.push(miette::LabeledSpan::new_with_span(
                    Some(format!("→ field of type `{}`", type_name)),
                    current_field_span,
                ));

                // Recursively check the fields of this generic struct
                if let Some(struct_def) = self.signature.structs.get(mangled_name) {
                    for field in struct_def.fields.values() {
                        if self.has_cyclic_reference(
                            field.ty,
                            target_struct,
                            visited,
                            cycle_path,
                            field.span,
                        ) {
                            return true;
                        }
                    }
                }

                // No cycle found through this path, remove it
                cycle_path.truncate(path_index);
                visited.remove(mangled_name);
                false
            }

            // Other types (primitives, lists, etc.) can't be cyclic
            _ => false,
        }
    }

    /// Get a human-readable display name for a type (for error messages)
    fn get_type_display_name(ty: &HirTy<'hir>) -> String {
        match ty {
            HirTy::Named(n) => n.name.to_string(),
            HirTy::Generic(g) => {
                let args = g
                    .inner
                    .iter()
                    .map(Self::get_type_display_name)
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{}<{}>", g.name, args)
            }
            HirTy::Boolean(_) => "bool".to_string(),
            HirTy::Integer(_) => "int64".to_string(),
            HirTy::Float(_) => "float64".to_string(),
            HirTy::Char(_) => "char".to_string(),
            HirTy::UnsignedInteger(_) => "uint64".to_string(),
            HirTy::String(_) => "string".to_string(),
            HirTy::Unit(_) => "unit".to_string(),
            HirTy::Slice(l) => format!("[{}]", Self::get_type_display_name(l.inner)),
            HirTy::Reference(r) => match r.kind {
                HirReferenceKind::Mutable => format!("{}&", Self::get_type_display_name(r.inner)),
                HirReferenceKind::ReadOnly => {
                    format!("const {}&", Self::get_type_display_name(r.inner))
                }
                HirReferenceKind::Moveable => format!("{}&&", Self::get_type_display_name(r.inner)),
            },
            HirTy::PtrTy(p) => format!("*{}", Self::get_type_display_name(p.inner)),
            HirTy::InlineArray(arr) => {
                format!("[{}; {}]", Self::get_type_display_name(arr.inner), arr.size)
            }
            _ => "<unknown>".to_string(),
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
        expected_type: &str,
        expected_loc: &Span,
        actual_type: &str,
        actual_loc: &Span,
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

    fn calling_consuming_method_on_mutable_reference_err(
        declaration_span: &Span,
        call_span: &Span,
    ) -> HirError {
        let declaration_path = declaration_span.path;
        let declaration_src = utils::get_file_content(declaration_path).unwrap();
        let origin = CallingConsumingMethodOnMutableReferenceOrigin {
            method_span: *declaration_span,
            src: NamedSource::new(declaration_path, declaration_src),
        };

        let call_path = call_span.path;
        let call_src = utils::get_file_content(call_path).unwrap();
        HirError::CallingConsumingMethodOnMutableReference(
            CallingConsumingMethodOnMutableReferenceError {
                call_span: *call_span,
                src: NamedSource::new(call_path, call_src),
                origin,
            },
        )
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
                            if let HirExpr::Ident(ident) = fa.target.as_ref()
                                && self.is_local_variable(ident.name)
                            {
                                return vec![ident.name];
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
        if let Some(context_map) = self.context_functions.last()
            && let Some(func_name) = self.current_func_name
            && let Some(context_func) = context_map.get(func_name)
            && let Some(var) = context_func.get_variable(name)
        {
            return var.refs_locals.clone();
        }
        vec![]
    }

    /// Check if a variable name refers to a local variable (not a function parameter)
    fn is_local_variable(&self, name: &str) -> bool {
        // Get the current function's context
        if let Some(context_map) = self.context_functions.last()
            && let Some(func_name) = self.current_func_name
            && let Some(context_func) = context_map.get(func_name)
            // Check if the variable is in the local scope
            && let Some(var) = context_func.get_variable(name)
        {
            // If it's a parameter, it's not local
            return !var.is_param;
        }

        // If we can't determine, assume it's local (conservative)
        true
    }

    fn insert_new_variable(&mut self, var: ContextVariable<'hir>) -> HirResult<()> {
        if let Some(context_map) = self.context_functions.last_mut()
            && let Some(context_func) = context_map.get_mut(self.current_func_name.unwrap())
        {
            // we need to check if a variable with the same name already exists in the current context
            if let Some(map) = context_func.get_variable(var.name) {
                return Err(HirError::VariableNameAlreadyDefined(
                    VariableNameAlreadyDefinedError {
                        name: var.name.to_string(),
                        first_definition_span: map.name_span,
                        second_definition_span: var.name_span,
                        src: NamedSource::new(
                            var.name_span.path,
                            utils::get_file_content(var.name_span.path).unwrap(),
                        ),
                    },
                ));
            }
            context_func.insert(var.name, var);
            Ok(())
        } else {
            Err(Self::unknown_identifier_err(var.name, &var.name_span))
        }
    }

    /// + - * / %
    fn is_arithmetic_type(ty: &HirTy) -> bool {
        matches!(
            ty,
            HirTy::Integer(_) | HirTy::UnsignedInteger(_) | HirTy::Float(_) | HirTy::Char(_)
        )
    }

    /// == !=
    fn is_equality_comparable(&self, ty: &HirTy) -> bool {
        match ty {
            HirTy::Integer(_)
            | HirTy::UnsignedInteger(_)
            | HirTy::Float(_)
            | HirTy::Char(_)
            | HirTy::Boolean(_)
            | HirTy::Unit(_) => true,
            HirTy::Named(n) => self.signature.enums.contains_key(n.name),
            _ => false,
        }
    }

    fn is_orderable_type(ty: &HirTy) -> bool {
        matches!(
            ty,
            HirTy::Integer(_) | HirTy::UnsignedInteger(_) | HirTy::Float(_) | HirTy::Char(_)
        )
    }
}
