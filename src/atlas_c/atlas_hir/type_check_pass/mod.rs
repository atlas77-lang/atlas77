mod context;

use super::{
    HirFunction, HirModule, HirModuleSignature,
    arena::HirArena,
    expr,
    stmt::{HirBlock, HirExprStmt, HirStatement},
};
use crate::atlas_c::atlas_hir::signature::{
    HirFunctionParameterSignature, HirFunctionSignature, HirMethodAttribute,
    HirOverloadableOperatorKind, HirStructDestructorSignature, HirStructFieldSignature,
    HirStructMethodModifier, HirStructSignature, HirVisibility,
};
use crate::atlas_c::atlas_hir::special_methods::{
    INTRINSIC_PRIMITIVE_COPY, INTRINSIC_PRIMITIVE_DEFAULT, INTRINSIC_PRIMITIVE_HASH,
    SpecialMethodKind, SpecialMethodReceiver, primitive_special_call_descriptor,
};
use crate::atlas_c::atlas_hir::{
    error::UnknownOverloadableOperatorError, pretty_print::HirPrettyPrinter,
};
use crate::atlas_c::atlas_hir::{
    error::{
        AccessingClassFieldOutsideClassError, AccessingPrivateDestructorError,
        AccessingPrivateFieldError, AccessingPrivateFunctionError, AccessingPrivateFunctionOrigin,
        AccessingPrivateObjectOrigin, AccessingPrivateStructError, AccessingPrivateUnionError,
        CallingConsumingMethodOnMutableReferenceError,
        CallingConsumingMethodOnMutableReferenceOrigin, CallingNonConstMethodOnConstReferenceError,
        CallingNonConstMethodOnConstReferenceOrigin, CanOnlyConstructStructsError,
        CannotAccessFieldOfPointersError, EmptyListLiteralError, FieldKind, HirError, HirResult,
        IllegalOperationError, IllegalUnaryOperationError, InvalidListSizeError,
        InvalidSpecialMethodSignatureError, ListIndexOutOfBoundsError,
        MethodConstraintNotSatisfiedError, NonConstantListSizeError, NotEnoughArgumentsError,
        NotEnoughArgumentsOrigin, OperatorIsNotImplementedForThisTypeError,
        OperatorMustUseConstThisModifierError,
        OperatorOverloadDoesNotHaveRequiredAmountOfArgsError, ReturningPointerToLocalVariableError,
        StructCannotHaveAFieldOfItsOwnTypeError, TryingToAccessFieldOnNonObjectTypeError,
        TryingToCreateAnUnionWithMoreThanOneActiveFieldError,
        TryingToCreateAnUnionWithMoreThanOneActiveFieldOrigin, TryingToIndexNonIndexableTypeError,
        TryingToMutateConstPointerError, TypeCheckFailedError, TypeIsNotCopyableError,
        TypeMismatchActual, TypeMismatchError, UnionMustHaveAtLeastTwoVariantError,
        UnionVariantDefinedMultipleTimesError, UnknownFieldError, UnknownIdentifierError,
        UnknownMethodError, UnknownTypeError, UnsupportedExpr, VariableNameAlreadyDefinedError,
    },
    expr::{
        HirBinaryOperator, HirDeleteExpr, HirExpr, HirFieldAccessExpr, HirIdentExpr,
        HirThisLiteral, HirUnaryOp, HirUnsignedIntegerLiteralExpr,
    },
    item::{HirStruct, HirStructDestructor, HirStructMethod, HirUnion},
    monomorphization_pass::{MethodMonomorphizationRequest, MonomorphizationPass},
    ty::{HirGenericTy, HirNamedTy, HirTy, HirTyId},
    type_check_pass::context::{ContextFunction, ContextVariable},
    warning::{
        HirWarning, TryingToCastToTheSameTypeWarning,
        UnionFieldCannotBeAutomaticallyDeletedWarning, UnsafeRawPointerStructWarning,
    },
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
    pending_method_monomorphization: Vec<MethodMonomorphizationRequest<'hir>>,
    errors: Vec<HirError>,
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
            pending_method_monomorphization: Vec::new(),
            errors: Vec::new(),
        }
    }

    pub fn take_method_monomorphization_requests(
        &mut self,
    ) -> Vec<MethodMonomorphizationRequest<'hir>> {
        std::mem::take(&mut self.pending_method_monomorphization)
    }

    fn enqueue_method_monomorphization_request(
        &mut self,
        owner_name: &'hir str,
        method_name: &'hir str,
        generic_args: Vec<&'hir HirTy<'hir>>,
        span: Span,
    ) {
        if self.pending_method_monomorphization.iter().any(|req| {
            req.owner_name == owner_name
                && req.method_name == method_name
                && req.generic_args.len() == generic_args.len()
                && req
                    .generic_args
                    .iter()
                    .zip(generic_args.iter())
                    .all(|(lhs, rhs)| std::ptr::eq(*lhs, *rhs))
        }) {
            return;
        }

        self.pending_method_monomorphization
            .push(MethodMonomorphizationRequest {
                owner_name,
                method_name,
                generic_args,
                span,
            });
    }

    /// Checks if the method exists and is instantiated. If not, enqueues a monomorphization request and returns true.
    fn maybe_enqueue_deferred_method_materialization(
        &mut self,
        owner_name: &'hir str,
        method_name: &'hir str,
        generic_args: Vec<&'hir HirTy<'hir>>,
        span: Span,
    ) -> bool {
        let owner_sig = match self.signature.structs.get(owner_name) {
            Some(sig) => *sig,
            None => return false,
        };

        let template_owner_name = owner_sig
            .pre_mangled_ty
            .map(|pre_mangled| pre_mangled.name)
            .unwrap_or(owner_name);

        let template_owner_sig = match self.signature.structs.get(template_owner_name) {
            Some(sig) => *sig,
            None => return false,
        };

        let template_method_sig = match template_owner_sig.methods.get(method_name) {
            Some(sig) => sig,
            None => return false,
        };

        match &template_method_sig.generics {
            Some(method_generics) if method_generics.len() != generic_args.len() => return false,
            None if !generic_args.is_empty() => return false,
            _ => {}
        }

        self.enqueue_method_monomorphization_request(owner_name, method_name, generic_args, span);
        true
    }

    pub fn check(&mut self, hir: &mut HirModule<'hir>) -> HirResult<()> {
        self.errors.clear();
        self.signature = hir.signature.clone();

        // Auto-destructor synthesis now happens after monomorphization, during type checking.
        self.synthesize_auto_destructors(hir);
        self.signature = hir.signature.clone();

        for (func_name, func) in &mut hir.body.functions {
            if !func.signature.is_instantiated && !func.signature.generics.is_empty() {
                continue;
            }
            self.current_func_name = Some(func_name);
            let result = self.check_func(func);
            self.record_result(result);
        }
        for (class_name, class) in &mut hir.body.structs {
            if !class.signature.is_instantiated && !class.signature.generics.is_empty() {
                continue;
            }
            self.current_class_name = Some(class_name);
            let result = self.check_class(class);
            self.record_result(result);
        }
        for hir_union in hir.body.unions.values_mut() {
            if !hir_union.signature.is_instantiated && !hir_union.signature.generics.is_empty() {
                continue;
            }
            let result = self.check_union(hir_union);
            self.record_result(result);
        }

        if !self.errors.is_empty() {
            let errors = std::mem::take(&mut self.errors);
            return Err(HirError::TypeCheckFailed(TypeCheckFailedError {
                error_count: errors.len(),
                errors,
            }));
        }

        Ok(())
    }

    fn record_result<T>(&mut self, result: HirResult<T>) {
        if let Err(err) = result {
            self.errors.push(err);
        }
    }

    fn class_family_name(&self, class_name: &'hir str) -> &'hir str {
        if let Some(sig) = self.signature.structs.get(class_name)
            && let Some(pre_mangled) = sig.pre_mangled_ty
        {
            return pre_mangled.name;
        }
        class_name
    }

    fn is_same_class_family(&self, lhs: &'hir str, rhs: &'hir str) -> bool {
        self.class_family_name(lhs) == self.class_family_name(rhs)
    }

    fn is_accessing_from_same_class_family(&self, target: &'hir str) -> bool {
        self.current_class_name
            .map(|current| self.is_same_class_family(current, target))
            .unwrap_or(false)
    }

    fn type_requires_drop(
        &self,
        field_span: Span,
        ty: &'hir HirTy<'hir>,
        requires_drop: &HashMap<&'hir str, bool>,
    ) -> bool {
        match ty {
            HirTy::PtrTy(_)
            | HirTy::Function(_)
            | HirTy::Slice(_)
            | HirTy::Unit(_)
            | HirTy::Boolean(_)
            | HirTy::Integer(_)
            | HirTy::Float(_)
            | HirTy::Char(_)
            | HirTy::UnsignedInteger(_)
            | HirTy::String(_)
            | HirTy::LiteralInteger(_)
            | HirTy::LiteralFloat(_)
            | HirTy::LiteralUnsignedInteger(_)
            | HirTy::Uninitialized(_)
            | HirTy::Error(_) => false,
            HirTy::InlineArray(arr) => {
                self.type_requires_drop(field_span, arr.inner, requires_drop)
            }
            HirTy::Named(n) => {
                if self.signature.enums.contains_key(n.name) {
                    return false;
                }
                if let Some(sig) = self.signature.structs.get(n.name)
                    && sig.destructor.is_some()
                {
                    return true;
                }
                if let Some(sig) = self.signature.unions.get(n.name) {
                    sig.variants.iter().for_each(|(_, variant)| {
                        if self.type_requires_drop(field_span, variant.ty, requires_drop) {
                            Self::union_field_cannot_be_automatically_deleted_warning(
                                &sig.name_span,
                                n.name,
                                variant.name,
                                &variant.span,
                                format!("{}", ty),
                                field_span,
                            );
                        }
                    });
                    return false;
                }
                requires_drop.get(n.name).copied().unwrap_or(false)
            }
            HirTy::Generic(g) => {
                if self.signature.enums.contains_key(g.name)
                    || self.signature.unions.contains_key(g.name)
                {
                    return false;
                }
                if let Some(sig) = self.signature.structs.get(g.name)
                    && sig.destructor.is_some()
                {
                    return true;
                }
                requires_drop.get(g.name).copied().unwrap_or(false)
            }
        }
    }

    fn synthesize_auto_destructors(&mut self, hir: &mut HirModule<'hir>) {
        let mut requires_drop: HashMap<&'hir str, bool> =
            hir.body.structs.keys().map(|name| (*name, false)).collect();

        for (name, strct) in &hir.body.structs {
            if strct.signature.had_user_defined_destructor || strct.destructor.is_some() {
                requires_drop.insert(*name, true);
            }
        }

        loop {
            let mut changed = false;
            for (name, strct) in &hir.body.structs {
                if strct.signature.had_user_defined_destructor || strct.destructor.is_some() {
                    continue;
                }
                let needs_drop = strct
                    .signature
                    .fields
                    .values()
                    .any(|field| self.type_requires_drop(field.span, field.ty, &requires_drop));
                if needs_drop && !requires_drop.get(name).copied().unwrap_or(false) {
                    requires_drop.insert(*name, true);
                    changed = true;
                }
            }

            if !changed {
                break;
            }
        }

        let to_generate: Vec<(&'hir str, Span, Vec<HirStructFieldSignature<'hir>>)> = hir
            .body
            .structs
            .iter()
            .filter(|(name, s)| {
                s.destructor.is_none() && requires_drop.get(*name).copied().unwrap_or(false)
            })
            .map(|(name, s)| {
                (
                    *name,
                    s.name_span,
                    s.signature.fields.values().cloned().collect::<Vec<_>>(),
                )
            })
            .collect();

        for (struct_name, struct_span, fields) in to_generate {
            let mut statements = Vec::new();
            for field in &fields {
                if !self.type_requires_drop(field.span, field.ty, &requires_drop) {
                    continue;
                }

                let delete_expr = HirExpr::Delete(HirDeleteExpr {
                    span: field.span,
                    expr: Box::new(HirExpr::FieldAccess(HirFieldAccessExpr {
                        span: field.span,
                        target: Box::new(HirExpr::ThisLiteral(HirThisLiteral {
                            span: field.span,
                            ty: self.arena.types().get_uninitialized_ty(),
                        })),
                        field: Box::new(HirIdentExpr {
                            span: field.span,
                            name: field.name,
                            ty: field.ty,
                        }),
                        ty: field.ty,
                        is_arrow: true,
                    })),
                });

                statements.push(HirStatement::Expr(HirExprStmt {
                    span: field.span,
                    expr: delete_expr,
                }));
            }

            if statements.is_empty() {
                continue;
            }

            let signature = HirStructDestructorSignature {
                span: struct_span,
                vis: HirVisibility::Public,
                where_clause: None,
                docstring: None,
            };
            let hir_dtor = HirStructDestructor {
                span: struct_span,
                signature: self.arena.intern(signature),
                body: HirBlock {
                    span: struct_span,
                    statements,
                },
                vis: HirVisibility::Public,
            };

            if let Some(strct) = hir.body.structs.get_mut(struct_name) {
                strct.signature.destructor = Some(hir_dtor.signature.clone());
                strct.destructor = Some(hir_dtor);
                if let Some(sig_ref) = hir.signature.structs.get_mut(struct_name) {
                    *sig_ref = self.arena.intern(strct.signature.clone());
                }
            }
        }

        // Recompute copyability based on the final (possibly synthesized) destructor state.
        // This keeps ownership checks in sync now that destructor synthesis happens here.
        for (struct_name, strct) in hir.body.structs.iter_mut() {
            let has_destructor = strct.destructor.is_some() || strct.signature.destructor.is_some();

            // Destructor-bearing types are never trivially copyable.
            // This keeps ownership/drop semantics coherent even if users applied
            // #[std::trivially_copyable] manually.
            let is_trivially_copyable = !(has_destructor || strct.signature.flag.is_non_copyable());

            strct.signature.is_trivially_copyable = is_trivially_copyable;
            if is_trivially_copyable {
                strct.signature.is_std_copyable = true;
            }

            if let Some(sig_ref) = hir.signature.structs.get_mut(struct_name) {
                *sig_ref = self.arena.intern(strct.signature.clone());
            }
        }
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

        let has_destructor = class.destructor.is_some() || class.signature.destructor.is_some();

        // Explicitly reject trivially copyable structs that define a destructor.
        // The combination is semantically contradictory and leads to dangerous
        // ownership behavior (copy + drop).
        if class.signature.flag.is_trivially_copyable() && has_destructor {
            let path = class.span.path;
            let src = utils::get_file_content(path).unwrap();
            return Err(HirError::InvalidSpecialMethodSignature(
                InvalidSpecialMethodSignatureError {
                    span: class.name_span,
                    expected: "types with a destructor cannot be #[std::trivially_copyable]"
                        .to_string(),
                    actual: "#[std::trivially_copyable] used on a struct defining a destructor"
                        .to_string(),
                    src: NamedSource::new(path, src),
                    method_name: class.name.to_string(),
                },
            ));
        }

        // Warn when a struct owns raw pointers but is not trivially copyable and has no custom destructor.
        if !class.signature.flag.is_trivially_copyable()
            && !class.signature.had_user_defined_destructor
            && let Some(pointer_field) = class
                .fields
                .iter()
                .find(|field| matches!(field.ty, HirTy::PtrTy(_)))
        {
            Self::non_trivially_copyable_struct_holds_a_raw_pointer_with_no_custom_destructor_warning(
                    class,
                    pointer_field.span,
                );
        }

        for method in &mut class.methods {
            if method.signature.generics.is_some() || !method.signature.is_instantiated {
                continue;
            }
            self.current_class_name = Some(class.name);
            self.current_func_name = Some(method.name);
            self.context_functions.push(HashMap::new());
            let result =
                self.check_method(method, class.signature.nullable_attribute_span.is_some());
            self.record_result(result);
        }

        for operator in &mut class.operators {
            if operator.signature.generics.is_some() || !operator.signature.is_instantiated {
                continue;
            }
            self.current_class_name = Some(class.name);
            self.current_func_name = Some(operator.name);
            self.context_functions.push(HashMap::new());
            let result = self.check_operator((class.name, class.name_span), operator);
            self.record_result(result);
        }

        if let Some(destructor) = class.destructor.as_mut() {
            self.current_func_name = Some("__dtor");
            let result = self.check_destructor(destructor);
            self.record_result(result);
        }

        Ok(())
    }

    fn check_destructor(&mut self, destructor: &mut HirStructDestructor<'hir>) -> HirResult<()> {
        self.context_functions.push(HashMap::new());
        self.context_functions
            .last_mut()
            .unwrap()
            .insert(String::from("__dtor"), ContextFunction::new());
        for stmt in &mut destructor.body.statements {
            let result = self.check_stmt(stmt);
            self.record_result(result);
        }
        //Because it is a destructor we don't keep it in the `context_functions`
        self.context_functions.pop();
        Ok(())
    }

    fn type_exists(&self, ty: &HirTy) -> bool {
        match ty {
            HirTy::Named(n) => {
                self.signature.structs.contains_key(n.name)
                    || self.signature.enums.contains_key(n.name)
                    || self.signature.unions.contains_key(n.name)
            }
            HirTy::Generic(g) => {
                (self.signature.structs.contains_key(g.name)
                    || self.signature.enums.contains_key(g.name)
                    || self.signature.unions.contains_key(g.name))
                    && g.inner.iter().all(|arg| self.type_exists(arg))
            }
            HirTy::PtrTy(ptr) => self.type_exists(ptr.inner),
            HirTy::Slice(_)
            | HirTy::Unit(_)
            | HirTy::Boolean(_)
            | HirTy::Integer(_)
            | HirTy::Float(_)
            | HirTy::Char(_)
            | HirTy::String(_)
            | HirTy::UnsignedInteger(_)
            | HirTy::LiteralInteger(_)
            | HirTy::LiteralFloat(_)
            | HirTy::LiteralUnsignedInteger(_)
            | HirTy::Function(_) => true,
            HirTy::InlineArray(arr) => self.type_exists(arr.inner),
            HirTy::Uninitialized(_) | HirTy::Error(_) => false,
        }
    }

    fn check_function_params(&mut self, params: &[HirFunctionParameterSignature<'_>]) -> bool {
        let mut res = true;
        for arg in params.iter() {
            if !self.type_exists(arg.ty) {
                let path = arg.span.path;
                let src = utils::get_file_content(path).unwrap();
                self.errors.push(HirError::UnknownType(UnknownTypeError {
                    span: arg.ty_span,
                    name: format!("{}", arg.ty),
                    src: NamedSource::new(path, src),
                }));
                res = false;
            }
        }
        res
    }

    fn check_operator(
        &mut self,
        (struct_name, struct_span): (&'hir str, Span),
        method: &mut HirStructMethod<'hir>,
    ) -> HirResult<()> {
        let op: HirOverloadableOperatorKind = method
            .name
            .try_into()
            .map_err(|_| Self::unknown_overloadable_operator(method.name, &struct_span))?;
        let path = method.span.path;
        let src = utils::get_file_content(path).unwrap();
        if method.signature.modifier != HirStructMethodModifier::Const {
            return Err(HirError::OperatorMustUseConstThisModifier(
                OperatorMustUseConstThisModifierError {
                    span: method.name_span,
                    src: NamedSource::new(path, src),
                },
            ));
        }
        if op.is_binary() {
            // Expect exactly two parameters: `this` and the other operand.
            if method.signature.params.len() != 1 {
                self.errors
                    .push(HirError::OperatorOverloadDoesNotHaveRequiredAmountOfArgs(
                        OperatorOverloadDoesNotHaveRequiredAmountOfArgsError {
                            kind: op.into(),
                            expected: 2,
                            span: method.name_span,
                            found: method.signature.params.len() + 1,
                            src: NamedSource::new(path, src),
                            context: " and one additional argument".into(),
                        },
                    ));
            } else {
                // Operator-specific parameter/return checks
                match op {
                    HirOverloadableOperatorKind::Shl | HirOverloadableOperatorKind::Shr => {
                        // RHS must be uint64
                        if let Err(err) = self.is_equivalent_ty(
                            self.arena.types().get_uint_ty(64),
                            method.signature.params[0].span,
                            method.signature.params[0].ty,
                            method.signature.params[0].span,
                        ) {
                            self.errors.push(err);
                        }
                    }
                    HirOverloadableOperatorKind::Eq
                    | HirOverloadableOperatorKind::NEq
                    | HirOverloadableOperatorKind::Gt
                    | HirOverloadableOperatorKind::Gte
                    | HirOverloadableOperatorKind::Lt
                    | HirOverloadableOperatorKind::Lte => {
                        if let Err(err) = self.is_equivalent_ty(
                            self.arena.types().get_boolean_ty(),
                            method
                                .signature
                                .return_ty_span
                                .unwrap_or(method.signature.span),
                            &method.signature.return_ty,
                            method
                                .signature
                                .return_ty_span
                                .unwrap_or(method.signature.span),
                        ) {
                            self.errors.push(err);
                        }
                    }
                    _ => {
                        if let Err(err) = self.is_equivalent_ty(
                            self.arena.types().get_ptr_ty(
                                self.arena.types().get_named_ty(struct_name, struct_span),
                                true,
                                struct_span,
                            ),
                            struct_span,
                            method.signature.params[0].ty,
                            method.signature.params[0].span,
                        ) {
                            self.errors.push(err);
                        }
                    }
                }
            }
        } else if op.is_unary() {
            // Expect exactly one parameter: `this`.
            if method.signature.params.len() != 1 {
                self.errors
                    .push(HirError::OperatorOverloadDoesNotHaveRequiredAmountOfArgs(
                        OperatorOverloadDoesNotHaveRequiredAmountOfArgsError {
                            kind: op.into(),
                            expected: 1,
                            span: method.name_span,
                            found: method.signature.params.len(),
                            src: NamedSource::new(path, src),
                            context: "".into(),
                        },
                    ));
            } else {
                // For `not`, ensure return type is boolean
                if matches!(op, HirOverloadableOperatorKind::Not) {
                    if let Err(err) = self.is_equivalent_ty(
                        self.arena.types().get_boolean_ty(),
                        method
                            .signature
                            .return_ty_span
                            .unwrap_or(method.signature.span),
                        &method.signature.return_ty,
                        method
                            .signature
                            .return_ty_span
                            .unwrap_or(method.signature.span),
                    ) {
                        self.errors.push(err);
                    }
                }
            }
        }

        self.check_method(method, false)
    }

    fn check_method(
        &mut self,
        method: &mut HirStructMethod<'hir>,
        is_nullable_type: bool,
    ) -> HirResult<()> {
        self.check_special_method_signature(method.name, method)?;
        self.check_nullable_method_attributes(method, is_nullable_type)?;
        self.context_functions.push(HashMap::new());
        self.context_functions.last_mut().unwrap().insert(
            self.current_func_name.unwrap().to_string(),
            ContextFunction::new(),
        );
        self.check_function_params(&method.signature.params);
        if !self.type_exists(&method.signature.return_ty) {
            let path = method.span.path;
            let src = utils::get_file_content(path).unwrap();
            self.errors.push(HirError::UnknownType(UnknownTypeError {
                name: format!("{}", method.signature.return_ty),
                span: method
                    .signature
                    .return_ty_span
                    .unwrap_or(method.signature.span),
                src: NamedSource::new(path, src),
            }));
        }
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
                        ptrs_to_locals: vec![],
                    },
                );
        }
        for stmt in &mut method.body.statements {
            let result = self.check_stmt(stmt);
            self.record_result(result);
        }
        //Because it is a method we don't keep it in the `context_functions`
        self.context_functions.pop();
        Ok(())
    }

    fn check_nullable_method_attributes(
        &mut self,
        method: &HirStructMethod<'hir>,
        is_nullable_type: bool,
    ) -> HirResult<()> {
        for attr in &method.signature.attributes {
            match attr {
                HirMethodAttribute::NullablePredicate { span, .. } => {
                    if !is_nullable_type {
                        let path = span.path;
                        let src = utils::get_file_content(path).unwrap();
                        return Err(HirError::InvalidSpecialMethodSignature(
                            InvalidSpecialMethodSignatureError {
                                span: *span,
                                expected: "enclosing struct must be marked #[std::nullable]"
                                    .to_string(),
                                actual: "struct is not nullable".to_string(),
                                src: NamedSource::new(path, src),
                                method_name: method.name.to_string(),
                            },
                        ));
                    }

                    if !matches!(method.signature.return_ty, HirTy::Boolean(_)) {
                        let path = span.path;
                        let src = utils::get_file_content(path).unwrap();
                        return Err(HirError::InvalidSpecialMethodSignature(
                            InvalidSpecialMethodSignatureError {
                                span: *span,
                                expected: "nullable predicate methods must return bool".to_string(),
                                actual: format!("returns {}", method.signature.return_ty),
                                src: NamedSource::new(path, src),
                                method_name: method.name.to_string(),
                            },
                        ));
                    }

                    if method.signature.modifier != HirStructMethodModifier::Const
                        && method.signature.modifier != HirStructMethodModifier::Mutable
                    {
                        let path = span.path;
                        let src = utils::get_file_content(path).unwrap();
                        return Err(HirError::InvalidSpecialMethodSignature(
                            InvalidSpecialMethodSignatureError {
                                span: *span,
                                expected: "nullable predicate methods must be non-consuming (&const this or &this)".to_string(),
                                actual: format!(
                                    "modifier {:?}",
                                    method.signature.modifier
                                ),
                                src: NamedSource::new(path, src),
                                method_name: method.name.to_string(),
                            },
                        ));
                    }
                }
                HirMethodAttribute::Nullable(span) => {
                    let path = span.path;
                    let src = utils::get_file_content(path).unwrap();
                    return Err(HirError::InvalidSpecialMethodSignature(
                        InvalidSpecialMethodSignatureError {
                            span: *span,
                            expected:
                                "use #[std::nullable] on the struct declaration instead of methods"
                                    .to_string(),
                            actual: "method-level #[std::nullable]".to_string(),
                            src: NamedSource::new(path, src),
                            method_name: method.name.to_string(),
                        },
                    ));
                }
                HirMethodAttribute::NullableGuarded(span)
                | HirMethodAttribute::NullableInfallible(span) => {
                    if !is_nullable_type {
                        let path = span.path;
                        let src = utils::get_file_content(path).unwrap();
                        return Err(HirError::InvalidSpecialMethodSignature(
                            InvalidSpecialMethodSignatureError {
                                span: *span,
                                expected: "enclosing struct must be marked #[std::nullable]"
                                    .to_string(),
                                actual: "struct is not nullable".to_string(),
                                src: NamedSource::new(path, src),
                                method_name: method.name.to_string(),
                            },
                        ));
                    }

                    if method.signature.modifier == HirStructMethodModifier::Static {
                        let path = span.path;
                        let src = utils::get_file_content(path).unwrap();
                        return Err(HirError::InvalidSpecialMethodSignature(
                            InvalidSpecialMethodSignatureError {
                                span: *span,
                                expected:
                                    "nullable method attributes cannot be applied to static methods"
                                        .to_string(),
                                actual: "modifier Static".to_string(),
                                src: NamedSource::new(path, src),
                                method_name: method.name.to_string(),
                            },
                        ));
                    }
                }
            }
        }

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
        self.check_function_params(&func.signature.params);
        if !self.type_exists(&func.signature.return_ty) {
            let path = func.span.path;
            let src = utils::get_file_content(path).unwrap();
            self.errors.push(HirError::UnknownType(UnknownTypeError {
                name: format!("{}", func.signature.return_ty),
                span: func.signature.return_ty_span.unwrap_or(func.signature.span),
                src: NamedSource::new(path, src),
            }));
        }
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
                        ptrs_to_locals: vec![],
                    },
                );
        }
        for stmt in &mut func.body.statements {
            let result = self.check_stmt(stmt);
            self.record_result(result);
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
                let (expected_ret_ty, span) = if let Some(name) = self.current_class_name {
                    //This means we're in a class method
                    let class = self.signature.structs.get(name).unwrap();
                    if let Some(method) = class.methods.get(self.current_func_name.unwrap()) {
                        (
                            self.arena.intern(method.clone().return_ty) as &_,
                            method.return_ty_span.unwrap_or(ret.span),
                        )
                    } else if let Some(op) = class.methods.get(self.current_func_name.unwrap()) {
                        (
                            self.arena.intern(op.clone().return_ty) as &_,
                            op.return_ty_span.unwrap_or(ret.span),
                        )
                    } else {
                        (self.arena.types().get_uninitialized_ty(), ret.span)
                    }
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

                if let Some(expr) = &mut ret.value {
                    self.retag_integer_literal_for_expected_ty(expected_ret_ty, expr);
                    let actual_ret_ty = self.check_expr(expr)?;

                    // Check for returning a reference to a local variable (directly or through a struct)
                    let local_refs = self.get_local_ptr_targets(expr);
                    if let Some(local_var_name) = local_refs.first() {
                        let path = ret.span.path;
                        let src = utils::get_file_content(path).unwrap();
                        return Err(HirError::ReturningReferenceToLocalVariable(
                            ReturningPointerToLocalVariableError {
                                span: expr.span(),
                                var_name: local_var_name.to_string(),
                                src: NamedSource::new(path, src),
                            },
                        ));
                    }
                    self.is_equivalent_ty(expected_ret_ty, span, actual_ret_ty, expr.span())
                } else {
                    // Void return, only valid if expected return type is unit
                    self.is_equivalent_ty(
                        expected_ret_ty,
                        span,
                        self.arena.types().get_unit_ty(),
                        ret.span,
                    )
                }
            }
            HirStatement::While(w) => {
                match self.check_expr(&mut w.condition) {
                    Ok(cond_ty) => {
                        self.record_result(self.is_equivalent_ty(
                            self.arena.types().get_boolean_ty(),
                            w.condition.span(),
                            cond_ty,
                            w.condition.span(),
                        ));
                    }
                    Err(err) => self.errors.push(err),
                }
                //there should be just "self.context.new_scope()" and "self.context.end_scope()"
                self.context_functions
                    .last_mut()
                    .unwrap()
                    .get_mut(self.current_func_name.unwrap())
                    .unwrap()
                    .new_scope();
                for stmt in &mut w.body.statements {
                    let result = self.check_stmt(stmt);
                    self.record_result(result);
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
                match self.check_expr(&mut i.condition) {
                    Ok(cond_ty) => {
                        self.record_result(self.is_equivalent_ty(
                            self.arena.types().get_boolean_ty(),
                            i.condition.span(),
                            cond_ty,
                            i.condition.span(),
                        ));
                    }
                    Err(err) => self.errors.push(err),
                }

                self.context_functions
                    .last_mut()
                    .unwrap()
                    .get_mut(self.current_func_name.unwrap())
                    .unwrap()
                    .new_scope();
                for stmt in &mut i.then_branch.statements {
                    let result = self.check_stmt(stmt);
                    self.record_result(result);
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
                        let result = self.check_stmt(stmt);
                        self.record_result(result);
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
                // Phase 1: Register the variable early with a fallback type to prevent cascading errors
                // If a declared type is available, use it; otherwise use an error type
                let fallback_ty = if self.type_exists(c.ty) {
                    c.ty
                } else {
                    self.arena.types().get_error_ty()
                };

                // Register the variable with fallback type immediately
                self.insert_new_variable(ContextVariable {
                    name: c.name,
                    name_span: c.name_span,
                    ty: fallback_ty,
                    _ty_span: c.ty_span.unwrap_or(c.name_span),
                    _is_mut: false,
                    is_param: false,
                    ptrs_to_locals: vec![], // Will be updated below if expr check succeeds
                })?;

                // Phase 2: Check the initializer expression
                if c.ty != self.arena.types().get_uninitialized_ty() {
                    self.retag_integer_literal_for_expected_ty(c.ty, &mut c.value);
                }

                // Try to check the expression; if it fails, record the error but continue
                let expr_ty = match self.check_expr(&mut c.value) {
                    Ok(ty) => ty,
                    Err(err) => {
                        self.errors.push(err);
                        // Fall back to the declared type or error type
                        fallback_ty
                    }
                };

                // Check if the const is being assigned a reference to a local variable
                let ptrs_to_locals = self.get_local_ptr_targets(&c.value);

                // Phase 3: Determine the actual variable type
                let const_ty = if c.ty == self.arena.types().get_uninitialized_ty() {
                    //Need inference
                    expr_ty
                } else {
                    match self.is_equivalent_ty(
                        c.ty,
                        c.ty_span.unwrap_or(c.name_span),
                        expr_ty,
                        c.value.span(),
                    ) {
                        Ok(_) => {}
                        Err(err) => {
                            // Variable is already registered with fallback type above
                            self.errors.push(err);
                        }
                    }
                    c.ty
                };

                // Phase 4: Update the registered variable with correct type and ptr_to_locals info
                // We need to update the variable in the context to have the correct type and pointers
                if let Some(context_map) = self.context_functions.last_mut()
                    && let Some(context_func) = context_map.get_mut(self.current_func_name.unwrap())
                {
                    context_func.insert(
                        c.name,
                        ContextVariable {
                            name: c.name,
                            name_span: c.name_span,
                            ty: const_ty,
                            _ty_span: c.ty_span.unwrap_or(c.name_span),
                            _is_mut: false,
                            is_param: false,
                            ptrs_to_locals,
                        },
                    );
                }

                Ok(())
            }
            HirStatement::Let(l) => {
                // Phase 1: Register the variable early with a fallback type to prevent cascading errors
                // If a declared type is available, use it; otherwise use an error type
                let fallback_ty = if self.type_exists(l.ty) {
                    l.ty
                } else {
                    self.arena.types().get_error_ty()
                };

                // Register the variable with fallback type immediately
                self.insert_new_variable(ContextVariable {
                    name: l.name,
                    name_span: l.name_span,
                    ty: fallback_ty,
                    _ty_span: l.ty_span.unwrap_or(l.name_span),
                    _is_mut: true,
                    is_param: false,
                    ptrs_to_locals: vec![], // Will be updated below if expr check succeeds
                })?;

                // Phase 2: Check the initializer expression
                if l.ty != self.arena.types().get_uninitialized_ty() {
                    self.retag_integer_literal_for_expected_ty(l.ty, &mut l.value);
                }

                // Try to check the expression; if it fails, record the error but continue
                let expr_ty = match self.check_expr(&mut l.value) {
                    Ok(ty) => ty,
                    Err(err) => {
                        self.errors.push(err);
                        // Fall back to the declared type or error type
                        fallback_ty
                    }
                };

                // Check if the let is being assigned a reference to a local variable
                let ptrs_to_locals = self.get_local_ptr_targets(&l.value);

                // Phase 3: Determine the actual variable type
                let was_uninitialized = l.ty == self.arena.types().get_uninitialized_ty();
                let var_ty = if was_uninitialized {
                    // Keep `let x = ...` inferable across type-check rounds when the
                    // initializer temporarily fails (e.g. before deferred method
                    // monomorphization materializes).
                    if matches!(expr_ty, HirTy::Error(_)) {
                        self.arena.types().get_uninitialized_ty()
                    } else {
                        expr_ty
                    }
                } else {
                    match self.is_equivalent_ty(
                        l.ty,
                        l.ty_span.unwrap_or(l.name_span),
                        expr_ty,
                        l.value.span(),
                    ) {
                        Ok(_) => {}
                        Err(err) => {
                            // Variable is already registered with fallback type above
                            self.errors.push(err);
                        }
                    }
                    l.ty
                };
                l.ty = var_ty;
                let context_var_ty =
                    if was_uninitialized && var_ty == self.arena.types().get_uninitialized_ty() {
                        fallback_ty
                    } else {
                        var_ty
                    };

                // Phase 4: Update the registered variable with correct type and ptr_to_locals info
                // We need to update the variable in the context to have the correct type and pointers
                if let Some(context_map) = self.context_functions.last_mut()
                    && let Some(context_func) = context_map.get_mut(self.current_func_name.unwrap())
                {
                    context_func.insert(
                        l.name,
                        ContextVariable {
                            name: l.name,
                            name_span: l.name_span,
                            ty: context_var_ty,
                            _ty_span: l.ty_span.unwrap_or(l.name_span),
                            _is_mut: true,
                            is_param: false,
                            ptrs_to_locals,
                        },
                    );
                }

                Ok(())
            }
            HirStatement::Assign(assign) => {
                let dst_ty = self.check_expr(&mut assign.dst)?;
                self.retag_integer_literal_for_expected_ty(dst_ty, &mut assign.val);
                let val_ty = self.check_expr(&mut assign.val)?;

                // Check if we are dereferencing a const reference (mutation through const ref)
                // This catches: `*const_ref = value`
                if let HirExpr::Unary(unary_expr) = &assign.dst
                    && let Some(HirUnaryOp::Deref) = &unary_expr.op
                    && self.is_const_ptr_ty(unary_expr.expr.ty())
                {
                    let path = assign.span.path;
                    let src = utils::get_file_content(path).unwrap();
                    return Err(HirError::TryingToMutateConstPointer(
                        TryingToMutateConstPointerError {
                            span: assign.span,
                            ty: dst_ty.to_string(),
                            src: NamedSource::new(path, src),
                        },
                    ));
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
                    let result = self.check_stmt(stmt);
                    self.record_result(result);
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
            HirExpr::StringLiteral(s) => Ok(s.ty),
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
                let Some(dtor) = class.destructor.as_ref() else {
                    return Ok(self.arena.types().get_unit_ty());
                };
                if dtor.vis != HirVisibility::Public {
                    Err(Self::accessing_private_destructor_err(
                        &del_expr.span,
                        class.name,
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

                if function_name == "__dtor" {
                    let mutable_self_ty =
                        self.arena
                            .types()
                            .get_ptr_ty(self_ty, false, class.declaration_span);
                    s.ty = mutable_self_ty;
                    return Ok(mutable_self_ty);
                }

                // Check both methods and operators
                let method = match class.methods.get(function_name) {
                    Some(method) => method,
                    None => {
                        // Try to find it in operators only if the name can actually be an operator.
                        if let Ok(op_name) = function_name.try_into() {
                            match class.operators.get(&op_name) {
                                Some(op_method) => op_method,
                                None => {
                                    let path = expr.span().path;
                                    let src = utils::get_file_content(path).unwrap();
                                    return Err(HirError::UnknownMethod(UnknownMethodError {
                                        method_name: function_name.to_string(),
                                        ty_name: class.name.to_string(),
                                        span: expr.span(),
                                        src: NamedSource::new(path, src),
                                    }));
                                }
                            }
                        } else {
                            let path = expr.span().path;
                            let src = utils::get_file_content(path).unwrap();
                            return Err(HirError::UnknownMethod(UnknownMethodError {
                                method_name: function_name.to_string(),
                                ty_name: class.name.to_string(),
                                span: expr.span(),
                                src: NamedSource::new(path, src),
                            }));
                        }
                    }
                };
                match method.modifier {
                    HirStructMethodModifier::Const => {
                        let readonly_self_ty = self.arena.types().get_ptr_ty(
                            self_ty,
                            true, // is_const = true for const methods
                            method.span,
                        );
                        s.ty = readonly_self_ty;
                        Ok(readonly_self_ty)
                    }
                    HirStructMethodModifier::Mutable => {
                        let mutable_self_ty = self.arena.types().get_ptr_ty(
                            self_ty,
                            false, // is_const = false for mutable methods
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
                if let Some(op) = u.op
                    && !ty.is_primitive()
                    // AsRef works a bit differently. It can work on non primitive types
                    && op != HirUnaryOp::AsRef
                {
                    u.ty = self.check_unary_operator_overloading((ty, u.span), op)?;
                    u.is_overloaded = true;
                    return Ok(u.ty);
                }
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
                        if let Ok(ty) =
                            self.check_unary_operator_overloading((ty, u.span), HirUnaryOp::AsRef)
                        {
                            u.ty = ty;
                            return Ok(ty);
                        }
                        let ptr_ty = self.arena.types().get_ptr_ty(ty, false, u.span); // is_const = false for &
                        u.ty = ptr_ty;
                        Ok(ptr_ty)
                    }
                    Some(HirUnaryOp::Deref) => match ty {
                        HirTy::PtrTy(ptr) => {
                            u.ty = ptr.inner;
                            Ok(ptr.inner)
                        }
                        _ => Err(Self::illegal_unary_operation_err(
                            ty,
                            u.expr.span(),
                            "dereference operation",
                        )),
                    },
                    Some(HirUnaryOp::Not) => {
                        // If it's a boolean, we do logical not, otherwise we do bitwise not
                        if HirTyId::from(ty) == HirTyId::compute_boolean_ty_id()
                            || TypeChecker::is_binary_comparable_type(ty)
                        {
                            u.ty = ty;
                            Ok(ty)
                        } else {
                            Err(Self::illegal_unary_operation_err(
                                ty,
                                u.expr.span(),
                                "logical or bitwise not operation",
                            ))
                        }
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
                    HirTy::Integer(_)
                        | HirTy::LiteralInteger(_)
                        | HirTy::Float(_)
                        | HirTy::LiteralFloat(_)
                        | HirTy::UnsignedInteger(_)
                        | HirTy::LiteralUnsignedInteger(_)
                        | HirTy::Boolean(_)
                        | HirTy::Char(_)
                        | HirTy::String(_)
                        | HirTy::PtrTy(_)
                        // function can cast, but it should be Undefined Behaviour
                        | HirTy::Function(_)
                );
                if !can_cast && !c.target_ty.is_ptr() {
                    return Err(Self::type_mismatch_err(
                        &format!("{}", expr_ty),
                        &c.expr.span(),
                        "int64, float64, uint64, bool, char, str or raw pointer",
                        &c.expr.span(),
                    ));
                }
                // Only warn for truly redundant casts (same concrete type), not for
                // compatibility-based conversions handled by `is_equivalent_ty`.
                if HirTyId::from(expr_ty) == HirTyId::from(c.target_ty) {
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
                        indexing_expr.ty = self.arena.types().get_str_ty();
                        Ok(self.arena.types().get_str_ty())
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
                let rhs = self.check_expr(&mut b.rhs)?;
                if !lhs.is_primitive() {
                    b.ty = self.check_operator_overloading(
                        (lhs, b.lhs.span()),
                        (rhs, b.rhs.span()),
                        b.op.into(),
                    )?;
                    b.is_overloaded = true;
                    return Ok(b.ty);
                }

                let is_integer_like = |ty: &HirTy<'hir>| {
                    matches!(
                        ty,
                        HirTy::Integer(_)
                            | HirTy::UnsignedInteger(_)
                            | HirTy::LiteralInteger(_)
                            | HirTy::LiteralUnsignedInteger(_)
                    )
                };

                // Pointer arithmetic: ptr +/- integer and integer + ptr.
                // This must be handled before generic type-equivalence checks.
                match b.op {
                    HirBinaryOperator::Add => {
                        if matches!(lhs, HirTy::PtrTy(_)) && is_integer_like(rhs) {
                            b.ty = lhs;
                            return Ok(lhs);
                        }
                        if is_integer_like(lhs) && matches!(rhs, HirTy::PtrTy(_)) {
                            b.ty = rhs;
                            return Ok(rhs);
                        }
                    }
                    HirBinaryOperator::Sub
                        if matches!(lhs, HirTy::PtrTy(_)) && is_integer_like(rhs) =>
                    {
                        b.ty = lhs;
                        return Ok(lhs);
                    }
                    _ => {}
                }

                b.ty = lhs;

                match b.op {
                    HirBinaryOperator::Add
                    | HirBinaryOperator::Sub
                    | HirBinaryOperator::Mul
                    | HirBinaryOperator::Div
                    | HirBinaryOperator::Mod => {
                        // For arithmetic ops, require both sides to be equivalent
                        if !TypeChecker::is_arithmetic_type(lhs) {
                            return Err(Self::illegal_operation_err(
                                lhs,
                                rhs,
                                b.span,
                                "arithmetic operation",
                            ));
                        }
                        self.is_equivalent_ty(lhs, b.lhs.span(), rhs, b.rhs.span())?;
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
                        self.is_equivalent_ty(lhs, b.lhs.span(), rhs, b.rhs.span())?;
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
                        self.is_equivalent_ty(lhs, b.lhs.span(), rhs, b.rhs.span())?;
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
                        self.is_equivalent_ty(lhs, b.lhs.span(), rhs, b.rhs.span())?;
                        Ok(self.arena.types().get_boolean_ty())
                    }
                    HirBinaryOperator::BinAnd
                    | HirBinaryOperator::BinOr
                    | HirBinaryOperator::BinXor => {
                        if !TypeChecker::is_binary_comparable_type(lhs) {
                            return Err(Self::illegal_operation_err(
                                lhs,
                                rhs,
                                b.span,
                                "binary comparison",
                            ));
                        }
                        self.is_equivalent_ty(lhs, b.lhs.span(), rhs, b.rhs.span())?;
                        Ok(lhs)
                    }
                    HirBinaryOperator::ShL | HirBinaryOperator::ShR => {
                        // Left-hand side must be shiftable (integer-like)
                        if !TypeChecker::is_shiftable_type(lhs) {
                            return Err(Self::illegal_operation_err(
                                lhs,
                                rhs,
                                b.span,
                                "shift operation",
                            ));
                        }
                        // RHS must be an unsigned integer (literal or typed)
                        let rhs_is_unsigned = matches!(
                            rhs,
                            HirTy::UnsignedInteger(_) | HirTy::LiteralUnsignedInteger(_)
                        );
                        if !rhs_is_unsigned {
                            return Err(Self::illegal_operation_err(
                                lhs,
                                rhs,
                                b.span,
                                "shift operation (rhs must be unsigned integer)",
                            ));
                        }
                        Ok(lhs)
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
            // Represent the `[expr; size]` syntax.
            HirExpr::ListLiteralWithSize(l) => {
                let path = l.span.path;
                let size = match self.strip_noop_unary(&l.size) {
                    HirExpr::IntegerLiteral(i) => {
                        if i.value <= 0 {
                            let src = utils::get_file_content(path).unwrap();
                            return Err(HirError::InvalidListSize(InvalidListSizeError {
                                span: l.span,
                                size: i.value as usize,
                                src: NamedSource::new(path, src),
                            }));
                        };
                        i.value as usize
                    }
                    HirExpr::UnsignedIntegerLiteral(u) => {
                        if u.value == 0 {
                            let src = utils::get_file_content(path).unwrap();
                            return Err(HirError::InvalidListSize(InvalidListSizeError {
                                span: l.span,
                                size: 0,
                                src: NamedSource::new(path, src),
                            }));
                        };
                        u.value as usize
                    }
                    _ => {
                        let src = utils::get_file_content(path).unwrap();
                        return Err(HirError::NonConstantListSize(NonConstantListSizeError {
                            span: l.span,
                            src: NamedSource::new(path, src),
                        }));
                    }
                };

                let ty = self.check_expr(&mut l.item)?;
                l.ty = self.arena.types().get_inline_arr_ty(ty, size);
                Ok(l.ty)
            }
            HirExpr::ObjLiteral(obj_lit) => {
                // Support both struct and union literals: `Type { .field = value, ... }`
                let target_ty;
                let mut struct_signature: Option<&HirStructSignature<'hir>> = None;
                let mut union_signature = None;

                if let HirTy::Named(n) = obj_lit.ty {
                    target_ty = n;
                    if let Some(sig) = self.signature.structs.get(n.name) {
                        struct_signature = Some(*sig);
                    } else if let Some(sig) = self.signature.unions.get(n.name) {
                        union_signature = Some(*sig);
                    } else {
                        return Err(Self::unknown_type_err(n.name, &obj_lit.span));
                    }
                } else if let HirTy::Generic(g) = obj_lit.ty {
                    let struct_name =
                        MonomorphizationPass::generate_mangled_name(self.arena, g, "struct");
                    let union_name =
                        MonomorphizationPass::generate_mangled_name(self.arena, g, "union");

                    if let Some(sig) = self.signature.structs.get(struct_name) {
                        target_ty = self.arena.intern(HirNamedTy {
                            name: struct_name,
                            span: g.span,
                        }) as &'hir HirNamedTy<'hir>;
                        struct_signature = Some(*sig);
                    } else if let Some(sig) = self.signature.unions.get(union_name) {
                        target_ty = self.arena.intern(HirNamedTy {
                            name: union_name,
                            span: g.span,
                        }) as &'hir HirNamedTy<'hir>;
                        union_signature = Some(*sig);
                    } else {
                        return Err(Self::unknown_type_err(
                            &HirPrettyPrinter::generic_ty_str(g),
                            &obj_lit.span,
                        ));
                    }
                } else {
                    let path = obj_lit.span.path;
                    let src = utils::get_file_content(path).unwrap();
                    return Err(HirError::CanOnlyConstructStructs(
                        CanOnlyConstructStructsError {
                            span: obj_lit.span,
                            src: NamedSource::new(path, src),
                        },
                    ));
                }

                if let Some(struct_signature) = struct_signature {
                    if struct_signature.name_span.path != obj_lit.span.path
                        && struct_signature.vis != HirVisibility::Public
                    {
                        let origin_path = struct_signature.name_span.path;
                        let origin_src = utils::get_file_content(origin_path).unwrap();
                        let obj_path = obj_lit.span.path;
                        let obj_src = utils::get_file_content(obj_path).unwrap();
                        return Err(HirError::AccessingPrivateStruct(
                            AccessingPrivateStructError {
                                name: if let Some(n) = struct_signature.pre_mangled_ty {
                                    HirPrettyPrinter::generic_ty_str(n)
                                } else {
                                    target_ty.name.to_owned()
                                },
                                span: obj_lit.span,
                                src: NamedSource::new(obj_path, obj_src),
                                origin: AccessingPrivateObjectOrigin {
                                    span: struct_signature.name_span,
                                    src: NamedSource::new(origin_path, origin_src),
                                },
                            },
                        ));
                    }

                    if struct_signature.name_span.path != obj_lit.span.path
                        && let Some((private_name, _)) = struct_signature
                            .fields
                            .iter()
                            .find(|(_, sig)| sig.vis != HirVisibility::Public)
                    {
                        let src = utils::get_file_content(obj_lit.span.path).unwrap();
                        return Err(HirError::AccessingPrivateField(
                            AccessingPrivateFieldError {
                                span: obj_lit.span,
                                kind: FieldKind::Field,
                                src: NamedSource::new(obj_lit.span.path, src),
                                field_name: (*private_name).to_string(),
                            },
                        ));
                    }

                    for field in &mut obj_lit.fields {
                        let field_signature = match struct_signature.fields.get(field.name) {
                            Some(f) => f,
                            None => {
                                return Err(Self::unknown_field_err(
                                    field.name,
                                    target_ty.name,
                                    &field.span,
                                ));
                            }
                        };

                        if field_signature.span.path != obj_lit.span.path
                            && field_signature.vis != HirVisibility::Public
                        {
                            let src = utils::get_file_content(obj_lit.span.path).unwrap();
                            return Err(HirError::AccessingPrivateField(
                                AccessingPrivateFieldError {
                                    span: field.span,
                                    kind: FieldKind::Field,
                                    src: NamedSource::new(obj_lit.span.path, src),
                                    field_name: field.name.to_string(),
                                },
                            ));
                        }

                        let field_ty = self.check_expr(&mut field.value)?;
                        self.is_equivalent_ty(
                            field_signature.ty,
                            field_signature.span,
                            field_ty,
                            field.value.span(),
                        )?;
                    }
                } else if let Some(union_signature) = union_signature {
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
                                    target_ty.name.to_owned()
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
                                    target_ty.name,
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
                }

                Ok(obj_lit.ty)
            }
            HirExpr::Call(func_expr) => {
                if func_expr.is_reference {
                    return self.check_generic_function_reference(func_expr);
                }

                if let Some(rewritten) = self.try_rewrite_primitive_special_call(func_expr)? {
                    *expr = rewritten;
                    return self.check_expr(expr);
                }

                let path = func_expr.span.path;
                let callee = func_expr.callee.as_mut();
                match callee {
                    HirExpr::Ident(i) => {
                        // First check if the function is external by looking up the base name
                        let base_func = self.signature.functions.get(i.name).copied();

                        let (is_external, is_intrinsic) = base_func
                            .map(|f| (f.is_external, f.is_intrinsic))
                            .unwrap_or((false, false));

                        // Only mangle the name if it's NOT external nor intrinsic and has generics
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
                                if let Some(mut static_access) =
                                    self.build_static_access_from_qualified_ident(i)
                                {
                                    let ty = self.check_call_on_static_access(
                                        &mut static_access,
                                        &mut func_expr.args,
                                        &mut func_expr.args_ty,
                                        &mut func_expr.generics,
                                        func_expr.span,
                                    )?;
                                    *callee = HirExpr::StaticAccess(static_access);
                                    func_expr.ty = ty;
                                    return Ok(ty);
                                }
                                if let Ok(var) = self.get_ident_ty(i) {
                                    return self.check_callable_ty_call(
                                        var.ty,
                                        i.span,
                                        "function pointer",
                                        func_expr,
                                    );
                                }
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
                            self.retag_integer_literal_for_expected_ty(param.ty, arg);
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

                        if target_ty.is_ptr() && !field_access.is_arrow {
                            let path = field_access.span.path;
                            let src = utils::get_file_content(path).unwrap();
                            return Err(HirError::CannotAccessFieldOfPointers(
                                CannotAccessFieldOfPointersError {
                                    span: field_access.span,
                                    src: NamedSource::new(path, src),
                                },
                            ));
                        }

                        if !target_ty.is_ptr() && field_access.is_arrow {
                            return Err(Self::illegal_operation_err(
                                target_ty,
                                self.arena.types().get_unit_ty(),
                                field_access.span,
                                "->",
                            ));
                        }

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
                        let lookup_method_name = if func_expr.generics.is_empty() {
                            field_access.field.name
                        } else {
                            MonomorphizationPass::generate_mangled_name(
                                self.arena,
                                &HirGenericTy {
                                    name: field_access.field.name,
                                    inner: func_expr
                                        .generics
                                        .iter()
                                        .map(|g| (*g).clone())
                                        .collect(),
                                    span: func_expr.span,
                                },
                                "method",
                            )
                        };

                        let method = if let Some(method) = class.methods.get(lookup_method_name) {
                            Some(method)
                        } else if let Ok(op_name) = lookup_method_name.try_into() {
                            class.operators.get(&op_name)
                        } else {
                            None
                        };

                        if let Some(method_sig) = method {
                            if !method_sig.is_instantiated {
                                self.maybe_enqueue_deferred_method_materialization(
                                    name,
                                    field_access.field.name,
                                    func_expr.generics.clone(),
                                    func_expr.span,
                                );
                            }
                        } else if !func_expr.generics.is_empty() {
                            self.maybe_enqueue_deferred_method_materialization(
                                name,
                                field_access.field.name,
                                func_expr.generics.clone(),
                                func_expr.span,
                            );
                            return Err(Self::unknown_method_err(
                                lookup_method_name,
                                name,
                                &field_access.span,
                            ));
                        }

                        if let Some(method_signature) = method {
                            field_access.field.name = lookup_method_name;
                            if !func_expr.generics.is_empty() {
                                func_expr.generics.clear();
                            }

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
                            if !self.is_accessing_from_same_class_family(name)
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

                            if self.is_const_ptr_ty(target_ty)
                                && method_signature.modifier != HirStructMethodModifier::Const
                            {
                                return Err(Self::calling_non_const_method_on_const_reference_err(
                                    &method_signature.span,
                                    &field_access.span,
                                ));
                            }

                            if self.is_mutable_ptr_ty(target_ty)
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
                                self.retag_integer_literal_for_expected_ty(param.ty, arg);
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
                        } else if let Some((_, field_signature)) = class
                            .fields
                            .iter()
                            .find(|f| *f.0 == field_access.field.name)
                        {
                            if !self.is_accessing_from_same_class_family(name)
                                && field_signature.vis != HirVisibility::Public
                            {
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

                            field_access.ty = field_signature.ty;
                            field_access.field.ty = field_signature.ty;
                            self.check_callable_ty_call(
                                field_signature.ty,
                                field_signature.span,
                                "function pointer field",
                                func_expr,
                            )
                        } else {
                            Err(Self::unknown_method_err(
                                field_access.field.name,
                                name,
                                &field_access.span,
                            ))
                        }
                    }
                    HirExpr::StaticAccess(static_access) => {
                        let ty = self.check_call_on_static_access(
                            static_access,
                            &mut func_expr.args,
                            &mut func_expr.args_ty,
                            &mut func_expr.generics,
                            func_expr.span,
                        )?;
                        func_expr.ty = ty;
                        Ok(ty)
                    }
                    _ => Err(HirError::UnsupportedExpr(UnsupportedExpr {
                        span: func_expr.span,
                        expr: "Function call on non-identifier expression".to_string(),
                        src: NamedSource::new(path, utils::get_file_content(path).unwrap()),
                    })),
                }
            }
            HirExpr::Ident(i) => {
                if let Some(func) = self.signature.functions.get(i.name).copied() {
                    if !func.generics.is_empty() {
                        return Err(Self::type_mismatch_err(
                            "generic function reference with explicit type arguments",
                            &i.span,
                            i.name,
                            &func.span,
                        ));
                    }

                    let fn_ty = self.arena.types().get_function_ty_with_spans(
                        func.params.iter().map(|param| param.ty).collect(),
                        func.params.iter().map(|param| param.ty_span).collect(),
                        self.arena.intern(func.return_ty.clone()),
                        func.return_ty_span.unwrap_or(func.span),
                        func.span,
                    );
                    i.ty = fn_ty;
                    return Ok(fn_ty);
                }

                if let Some(static_access) = self.build_static_access_from_qualified_ident(i) {
                    let mut rewritten = HirExpr::StaticAccess(static_access);
                    let ty = self.check_expr(&mut rewritten)?;
                    *expr = rewritten;
                    return Ok(ty);
                }
                let ctx_var = self.get_ident_ty(i)?;
                Ok(ctx_var.ty)
            }
            HirExpr::FieldAccess(field_access) => {
                // target_ty.field_ty
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
                                    // Preserve pointer type from target_ty like struct field access does
                                    if self.is_const_ptr_ty(target_ty) {
                                        field_access.ty = self.arena.types().get_ptr_ty(
                                            var.ty,
                                            true, // is_const
                                            field_access.span,
                                        );
                                        field_access.field.ty = self.arena.types().get_ptr_ty(
                                            var.ty,
                                            true, // is_const
                                            field_access.span,
                                        );
                                        return Ok(field_access.ty);
                                    } else if self.is_mutable_ptr_ty(target_ty) {
                                        field_access.ty = self.arena.types().get_ptr_ty(
                                            var.ty,
                                            false, // is_const
                                            field_access.span,
                                        );
                                        field_access.field.ty = self.arena.types().get_ptr_ty(
                                            var.ty,
                                            false, // is_const
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
                    if !self.is_accessing_from_same_class_family(name)
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
                    if target_ty.is_ptr() && !field_access.is_arrow {
                        let path = field_access.span.path;
                        let src = utils::get_file_content(path).unwrap();
                        return Err(HirError::CannotAccessFieldOfPointers(
                            CannotAccessFieldOfPointersError {
                                span: field_access.span,
                                src: NamedSource::new(path, src),
                            },
                        ));
                    }
                    if !target_ty.is_ptr() && field_access.is_arrow {
                        return Err(Self::illegal_unary_operation_err(
                            target_ty,
                            field_access.span,
                            "->",
                        ));
                    }

                    // Field access always evaluates to the field type.
                    // For pointers, `->` dereferences then accesses the field.
                    field_access.ty = field_signature.ty;
                    field_access.field.ty = field_signature.ty;
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
                        // We might be trying to access an enum variant
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
                } else if let Some(method_signature) = class.methods.get(static_access.field.name) {
                    if !method_signature.is_instantiated {
                        self.maybe_enqueue_deferred_method_materialization(
                            name,
                            static_access.field.name,
                            vec![],
                            static_access.span,
                        );
                        return Err(Self::unknown_method_err(
                            static_access.field.name,
                            name,
                            &static_access.span,
                        ));
                    }

                    if !self.is_accessing_from_same_class_family(name)
                        && method_signature.vis != HirVisibility::Public
                    {
                        let path = static_access.span.path;
                        let src = utils::get_file_content(path).unwrap();
                        return Err(HirError::AccessingPrivateField(
                            AccessingPrivateFieldError {
                                span: static_access.span,
                                kind: FieldKind::Function,
                                src: NamedSource::new(path, src),
                                field_name: static_access.field.name.to_string(),
                            },
                        ));
                    }

                    let mut params: Vec<&HirTy<'hir>> = Vec::new();
                    let mut param_spans: Vec<Span> = Vec::new();
                    if method_signature.modifier != HirStructMethodModifier::Static {
                        let this_ty = match method_signature.modifier {
                            HirStructMethodModifier::Const => self.arena.types().get_ptr_ty(
                                self.arena
                                    .types()
                                    .get_named_ty(name, class.declaration_span),
                                true,
                                method_signature.span,
                            ),
                            HirStructMethodModifier::Mutable => self.arena.types().get_ptr_ty(
                                self.arena
                                    .types()
                                    .get_named_ty(name, class.declaration_span),
                                false,
                                method_signature.span,
                            ),
                            HirStructMethodModifier::Consuming => self
                                .arena
                                .types()
                                .get_named_ty(name, class.declaration_span),
                            HirStructMethodModifier::Static => unreachable!(),
                        };
                        params.push(this_ty);
                        param_spans.push(method_signature.span);
                    }

                    params.extend(method_signature.params.iter().map(|p| p.ty));
                    param_spans.extend(method_signature.params.iter().map(|p| p.ty_span));

                    let fn_ty = self.arena.types().get_function_ty_with_spans(
                        params,
                        param_spans,
                        &method_signature.return_ty,
                        method_signature
                            .return_ty_span
                            .unwrap_or(method_signature.span),
                        method_signature.span,
                    );
                    static_access.field.ty = fn_ty;
                    static_access.ty = fn_ty;
                    Ok(fn_ty)
                } else {
                    Err(Self::unknown_field_err(
                        static_access.field.name,
                        name,
                        &static_access.span,
                    ))
                }
            }
            HirExpr::IntrinsicCall(intrinsic) => {
                match intrinsic.name {
                    INTRINSIC_PRIMITIVE_DEFAULT => {
                        if !intrinsic.args.is_empty() || intrinsic.args_ty.len() != 1 {
                            return Err(Self::type_mismatch_err(
                                "primitive default intrinsic arguments",
                                &intrinsic.span,
                                "exactly one type argument and zero value arguments",
                                &intrinsic.span,
                            ));
                        }
                        intrinsic.ty = intrinsic.args_ty[0];
                        return Ok(intrinsic.ty);
                    }
                    INTRINSIC_PRIMITIVE_COPY => {
                        if intrinsic.args.len() != 1 {
                            return Err(Self::not_enough_arguments_err(
                                "primitive copy intrinsic".to_string(),
                                1,
                                &intrinsic.span,
                                intrinsic.args.len(),
                                &intrinsic.span,
                            ));
                        }
                        let arg_ty = self.check_expr(&mut intrinsic.args[0])?;
                        intrinsic.args_ty = vec![arg_ty];
                        intrinsic.ty = arg_ty;
                        return Ok(arg_ty);
                    }
                    INTRINSIC_PRIMITIVE_HASH => {
                        if intrinsic.args.len() != 1 {
                            return Err(Self::not_enough_arguments_err(
                                "primitive hash intrinsic".to_string(),
                                1,
                                &intrinsic.span,
                                intrinsic.args.len(),
                                &intrinsic.span,
                            ));
                        }
                        let arg_ty = self.check_expr(&mut intrinsic.args[0])?;
                        intrinsic.args_ty = vec![arg_ty];
                        intrinsic.ty = self.arena.types().get_uint_ty(64);
                        return Ok(intrinsic.ty);
                    }
                    _ => {}
                }

                // Intrinsic functions are defined as external functions with special handling
                //Let's just use the extern fn checker for now
                let signature = match self.signature.functions.get(intrinsic.name) {
                    Some(sig) => sig,
                    None => {
                        return Err(Self::unknown_method_err(
                            intrinsic.name,
                            intrinsic.name,
                            &intrinsic.span,
                        ));
                    }
                };
                let ty = self.check_extern_fn(
                    intrinsic.name,
                    &mut intrinsic.args_ty,
                    &mut intrinsic.args,
                    intrinsic.span,
                    signature,
                )?;
                intrinsic.ty = ty;
                Ok(ty)
            }
        }
    }

    fn check_operator_overloading(
        &self,
        (lhs_ty, lhs_span): (&'hir HirTy<'hir>, Span),
        (rhs_ty, rhs_span): (&'hir HirTy<'hir>, Span),
        op: HirOverloadableOperatorKind,
    ) -> HirResult<&'hir HirTy<'hir>> {
        // Temporary solution:
        if matches!(
            op,
            HirOverloadableOperatorKind::Shl | HirOverloadableOperatorKind::Shr
        ) {
            let rhs_is_unsigned = matches!(
                rhs_ty,
                HirTy::UnsignedInteger(_) | HirTy::LiteralUnsignedInteger(_)
            );
            if !rhs_is_unsigned {
                return Err(Self::illegal_operation_err(
                    lhs_ty,
                    rhs_ty,
                    rhs_span,
                    "shift operation (rhs must be unsigned integer)",
                ));
            }
        } else {
            self.is_equivalent_ty(lhs_ty, lhs_span, rhs_ty, rhs_span)?
        };
        if let HirTy::Named(n) = lhs_ty
            && let Some(struct_signature) = self.signature.structs.get(n.name)
            && struct_signature.operators.contains_key(&op)
        {
            // Check if operator's where_clause constraints are satisfied
            if let Some(operator_signature) = struct_signature.operators.get(&op)
                && !operator_signature.is_constraint_satisfied
            {
                let path = rhs_span.path;
                let src = utils::get_file_content(path).unwrap();
                return Err(HirError::MethodConstraintNotSatisfied(
                    MethodConstraintNotSatisfiedError {
                        member_kind: "operator".to_string(),
                        member_name: format!("{:?}", op),
                        ty_name: n.name.to_string(),
                        span: rhs_span,
                        src: NamedSource::new(path, src),
                    },
                ));
            }
            return Ok(lhs_ty);
        } else if let HirTy::Generic(g) = lhs_ty {
            let mangled_name = MonomorphizationPass::generate_mangled_name(self.arena, g, "struct");
            if let Some(struct_signature) = self.signature.structs.get(mangled_name)
                && struct_signature.operators.contains_key(&op)
            {
                // Check if operator's where_clause constraints are satisfied
                if let Some(operator_signature) = struct_signature.operators.get(&op)
                    && !operator_signature.is_constraint_satisfied
                {
                    let path = rhs_span.path;
                    let src = utils::get_file_content(path).unwrap();
                    return Err(HirError::MethodConstraintNotSatisfied(
                        MethodConstraintNotSatisfiedError {
                            member_kind: "operator".to_string(),
                            member_name: format!("{:?}", op),
                            ty_name: HirPrettyPrinter::generic_ty_str(g),
                            span: rhs_span,
                            src: NamedSource::new(path, src),
                        },
                    ));
                }
                return Ok(lhs_ty);
            }
        }

        Err(HirError::OperatorIsNotImplementedForThisType(
            OperatorIsNotImplementedForThisTypeError {
                operator: format!("{:?}", op),
                ty: format!("{}", lhs_ty),
                span: rhs_span,
                src: NamedSource::new(
                    rhs_span.path,
                    utils::get_file_content(rhs_span.path).unwrap(),
                ),
            },
        ))
    }

    fn check_unary_operator_overloading(
        &self,
        (ty, span): (&'hir HirTy<'hir>, Span),
        op: HirUnaryOp,
    ) -> HirResult<&'hir HirTy<'hir>> {
        let op_kind = HirOverloadableOperatorKind::from(op);
        if let HirTy::Named(n) = ty
            && let Some(struct_signature) = self.signature.structs.get(n.name)
            && struct_signature.operators.contains_key(&op_kind)
        {
            // Check if operator's where_clause constraints are satisfied
            if let Some(operator_signature) = struct_signature.operators.get(&op_kind)
                && !operator_signature.is_constraint_satisfied
            {
                let path = span.path;
                let src = utils::get_file_content(path).unwrap();
                return Err(HirError::MethodConstraintNotSatisfied(
                    MethodConstraintNotSatisfiedError {
                        member_kind: "operator".to_string(),
                        member_name: format!("{:?}", op_kind),
                        ty_name: n.name.to_string(),
                        span,
                        src: NamedSource::new(path, src),
                    },
                ));
            }
            return Ok(ty);
        } else if let HirTy::Generic(g) = ty {
            let mangled_name = MonomorphizationPass::generate_mangled_name(self.arena, g, "struct");
            if let Some(struct_signature) = self.signature.structs.get(mangled_name)
                && struct_signature.operators.contains_key(&op_kind)
            {
                // Check if operator's where_clause constraints are satisfied
                if let Some(operator_signature) = struct_signature.operators.get(&op_kind)
                    && !operator_signature.is_constraint_satisfied
                {
                    let path = span.path;
                    let src = utils::get_file_content(path).unwrap();
                    return Err(HirError::MethodConstraintNotSatisfied(
                        MethodConstraintNotSatisfiedError {
                            member_kind: "operator".to_string(),
                            member_name: format!("{:?}", op_kind),
                            ty_name: HirPrettyPrinter::generic_ty_str(g),
                            span,
                            src: NamedSource::new(path, src),
                        },
                    ));
                }

                return Ok(ty);
            }
        }

        Err(HirError::OperatorIsNotImplementedForThisType(
            OperatorIsNotImplementedForThisTypeError {
                operator: format!("{:?}", op),
                ty: format!("{}", ty),
                span,
                src: NamedSource::new(span.path, utils::get_file_content(span.path).unwrap()),
            },
        ))
    }

    fn is_primitive_special_receiver_ty(&self, ty: &HirTy<'hir>) -> bool {
        matches!(
            ty,
            HirTy::Boolean(_)
                | HirTy::Integer(_)
                | HirTy::Float(_)
                | HirTy::Char(_)
                | HirTy::String(_)
                | HirTy::UnsignedInteger(_)
                | HirTy::Unit(_)
                | HirTy::LiteralInteger(_)
                | HirTy::LiteralUnsignedInteger(_)
                | HirTy::LiteralFloat(_)
                // Slices are just pointers under the hood, so we can support special methods on them as well
                | HirTy::Slice(_)
        )
    }

    fn primitive_special_call_supported_for_ty(
        &self,
        kind: SpecialMethodKind,
        ty: &HirTy<'hir>,
    ) -> bool {
        if !self.is_primitive_special_receiver_ty(ty) {
            return false;
        }
        match kind {
            SpecialMethodKind::Copy => true,
            SpecialMethodKind::Default => true,
            SpecialMethodKind::Hash => true,
        }
    }

    fn try_rewrite_primitive_special_call(
        &mut self,
        func_expr: &mut expr::HirFunctionCallExpr<'hir>,
    ) -> HirResult<Option<HirExpr<'hir>>> {
        if !func_expr.generics.is_empty() {
            return Ok(None);
        }

        match func_expr.callee.as_mut() {
            HirExpr::FieldAccess(field_access) => {
                let Some(descriptor) = primitive_special_call_descriptor(
                    field_access.field.name,
                    SpecialMethodReceiver::Instance,
                ) else {
                    return Ok(None);
                };

                if !func_expr.args.is_empty() {
                    return Ok(None);
                }

                let receiver_ty = self.check_expr(&mut field_access.target)?;
                if !self.primitive_special_call_supported_for_ty(descriptor.kind, receiver_ty) {
                    return Ok(None);
                }

                let return_ty = match descriptor.kind {
                    SpecialMethodKind::Copy => receiver_ty,
                    SpecialMethodKind::Hash => self.arena.types().get_uint_ty(64),
                    SpecialMethodKind::Default => receiver_ty,
                };

                Ok(Some(HirExpr::IntrinsicCall(expr::HirIntrinsicCallExpr {
                    span: func_expr.span,
                    name: descriptor.intrinsic_name,
                    args: vec![(*field_access.target).clone()],
                    args_ty: vec![receiver_ty],
                    ty: return_ty,
                })))
            }
            HirExpr::StaticAccess(static_access) => {
                let Some(descriptor) = primitive_special_call_descriptor(
                    static_access.field.name,
                    SpecialMethodReceiver::Static,
                ) else {
                    return Ok(None);
                };

                if !func_expr.args.is_empty() {
                    return Ok(None);
                }

                let target_ty = static_access.target;
                if !self.primitive_special_call_supported_for_ty(descriptor.kind, target_ty) {
                    return Ok(None);
                }

                Ok(Some(HirExpr::IntrinsicCall(expr::HirIntrinsicCallExpr {
                    span: func_expr.span,
                    name: descriptor.intrinsic_name,
                    args: vec![],
                    args_ty: vec![target_ty],
                    ty: target_ty,
                })))
            }
            _ => Ok(None),
        }
    }

    fn check_generic_function_reference(
        &mut self,
        func_expr: &mut expr::HirFunctionCallExpr<'hir>,
    ) -> HirResult<&'hir HirTy<'hir>> {
        let path = func_expr.span.path;
        let callee = func_expr.callee.as_mut();
        match callee {
            HirExpr::Ident(i) => {
                let base_func = self.signature.functions.get(i.name).copied();
                let is_external = base_func.map(|f| f.is_external).unwrap_or(false);

                let name = if func_expr.generics.is_empty() || is_external {
                    i.name
                } else {
                    MonomorphizationPass::generate_mangled_name(
                        self.arena,
                        &HirGenericTy {
                            name: i.name,
                            inner: func_expr.generics.iter().map(|g| (*g).clone()).collect(),
                            span: i.span,
                        },
                        "function",
                    )
                };

                let func = match self.signature.functions.get(name) {
                    Some(f) => *f,
                    None => {
                        return Err(Self::unknown_identifier_err(i.name, &i.span));
                    }
                };

                if !func.generics.is_empty() && func_expr.generics.is_empty() {
                    return Err(Self::type_mismatch_err(
                        "generic function reference with explicit type arguments",
                        &func_expr.span,
                        i.name,
                        &i.span,
                    ));
                }

                let mut generic_pairs: Vec<(&'hir str, &'hir HirTy<'hir>)> = Vec::new();
                for (generic_param, concrete_ty) in
                    func.generics.iter().zip(func_expr.generics.iter())
                {
                    generic_pairs.push((generic_param.generic_name, concrete_ty));
                }

                let params: Vec<&'hir HirTy<'hir>> = func
                    .params
                    .iter()
                    .map(|param| {
                        if let Some(generic_name) = Self::get_generic_name(param.ty) {
                            if let Some((_, concrete_ty)) =
                                generic_pairs.iter().find(|(name, _)| *name == generic_name)
                            {
                                self.get_generic_ret_ty(param.ty, concrete_ty)
                            } else {
                                param.ty
                            }
                        } else {
                            param.ty
                        }
                    })
                    .collect();

                let ret_ty = if let Some(generic_name) =
                    Self::get_generic_name(self.arena.intern(func.return_ty.clone()))
                {
                    if let Some((_, concrete_ty)) =
                        generic_pairs.iter().find(|(name, _)| *name == generic_name)
                    {
                        self.get_generic_ret_ty(
                            self.arena.intern(func.return_ty.clone()),
                            concrete_ty,
                        )
                    } else {
                        self.arena.intern(func.return_ty.clone())
                    }
                } else {
                    self.arena.intern(func.return_ty.clone())
                };

                let fn_ty = self.arena.types().get_function_ty_with_spans(
                    params,
                    func.params.iter().map(|p| p.ty_span).collect(),
                    ret_ty,
                    func.return_ty_span.unwrap_or(func.span),
                    func.span,
                );
                i.ty = fn_ty;
                func_expr.ty = fn_ty;
                Ok(fn_ty)
            }
            _ => Err(HirError::UnsupportedExpr(UnsupportedExpr {
                span: func_expr.span,
                expr: "Generic function reference on non-identifier expression".to_string(),
                src: NamedSource::new(path, utils::get_file_content(path).unwrap()),
            })),
        }
    }

    fn check_extern_fn(
        &mut self,
        name: &'hir str,
        call_expr_generics: &mut Vec<&'hir HirTy<'hir>>,
        call_expr_args: &mut Vec<HirExpr<'hir>>,
        call_expr_span: Span,
        signature: &'hir HirFunctionSignature<'hir>,
    ) -> HirResult<&'hir HirTy<'hir>> {
        if signature.params.len() != call_expr_args.len() {
            return Err(Self::not_enough_arguments_err(
                "external function".to_string(),
                signature.params.len(),
                &signature.span,
                call_expr_args.len(),
                &call_expr_span,
            ));
        }

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
                // Substitute the generic with the concrete type
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
                        // Substitute the generic with the concrete type
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
            let is_declared_generic = signature.generics.iter().any(|g| g.generic_name == name);
            if is_declared_generic {
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
            }
        };

        monomorphized.generics = vec![];

        // Generic extern calls still need full argument validation after substitution.
        for ((param, arg), arg_ty) in monomorphized
            .params
            .iter()
            .zip(call_expr_args.iter())
            .zip(args_ty.iter())
        {
            self.is_equivalent_ty(param.ty, param.span, arg_ty, arg.span())?;
        }

        let signature = self.arena.intern(monomorphized);
        self.extern_monomorphized
            .insert((name, args_ty, explicit_generics), signature);
        Ok(self.arena.intern(signature.return_ty.clone()))
    }

    fn get_generic_name(ty: &'hir HirTy<'hir>) -> Option<&'hir str> {
        match ty {
            HirTy::Slice(l) => Self::get_generic_name(l.inner),
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
            HirTy::PtrTy(ptr_ty) => self.arena.types().get_ptr_ty(
                self.get_generic_ret_ty(ptr_ty.inner, actual_generic_ty),
                ptr_ty.is_const,
                ptr_ty.span,
            ),
            _ => actual_generic_ty,
        }
    }

    /// Return the type of the generic after monormophization
    /// e.g. `foo<T>(a: T) -> T` with `foo(42)` will return `int64`
    /// e.g. `foo<T>(a: *T) -> T` with `foo(&value)` where value is int64 will return `int64`
    fn get_generic_ty(
        ty: &'hir HirTy<'hir>,
        given_ty: &'hir HirTy<'hir>,
    ) -> Option<&'hir HirTy<'hir>> {
        match (ty, given_ty) {
            (HirTy::Slice(l1), HirTy::Slice(l2)) => Self::get_generic_ty(l1.inner, l2.inner),
            (HirTy::PtrTy(ptr1), HirTy::PtrTy(ptr2)) => {
                Self::get_generic_ty(ptr1.inner, ptr2.inner)
            }
            (HirTy::Named(_), _) => Some(given_ty),
            (HirTy::PtrTy(p1), _) => Self::get_generic_ty(p1.inner, given_ty),
            (_, HirTy::PtrTy(p2)) => Self::get_generic_ty(ty, p2.inner),
            _ => None,
        }
    }

    fn check_callable_ty_call(
        &mut self,
        callable_ty: &'hir HirTy<'hir>,
        callable_span: Span,
        kind: &str,
        func_expr: &mut expr::HirFunctionCallExpr<'hir>,
    ) -> HirResult<&'hir HirTy<'hir>> {
        let HirTy::Function(function_ty) = callable_ty else {
            return Err(Self::illegal_operation_err(
                callable_ty,
                self.arena.types().get_unit_ty(),
                callable_span,
                "call expression",
            ));
        };

        if function_ty.params.len() != func_expr.args.len() {
            return Err(Self::not_enough_arguments_err(
                kind.to_string(),
                function_ty.params.len(),
                &function_ty.span,
                func_expr.args.len(),
                &func_expr.span,
            ));
        }

        for (idx, (param_ty, arg)) in function_ty
            .params
            .iter()
            .zip(func_expr.args.iter_mut())
            .enumerate()
        {
            let expected_span = function_ty
                .param_spans
                .get(idx)
                .copied()
                .unwrap_or(function_ty.span);
            self.retag_integer_literal_for_expected_ty(param_ty, arg);
            let arg_ty = self.check_expr(arg)?;
            self.is_equivalent_ty(param_ty, expected_span, arg_ty, arg.span())?;
            func_expr.args_ty.push(param_ty);
        }

        func_expr.ty = function_ty.ret_ty;
        Ok(function_ty.ret_ty)
    }

    fn build_static_access_from_qualified_ident(
        &self,
        ident: &HirIdentExpr<'hir>,
    ) -> Option<expr::HirStaticAccessExpr<'hir>> {
        let (target_name, field_name) = ident.name.rsplit_once("::")?;
        if target_name.is_empty() || field_name.is_empty() {
            return None;
        }

        let target_name_interned = self.arena.names().get(target_name);
        if !self.signature.structs.contains_key(target_name_interned)
            && !self.signature.enums.contains_key(target_name_interned)
        {
            return None;
        }

        let target = self
            .arena
            .types()
            .get_named_ty(target_name_interned, ident.span);
        Some(expr::HirStaticAccessExpr {
            span: ident.span,
            target,
            field: Box::new(HirIdentExpr {
                name: self.arena.names().get(field_name),
                span: ident.span,
                ty: self.arena.types().get_uninitialized_ty(),
            }),
            ty: self.arena.types().get_uninitialized_ty(),
        })
    }

    fn check_call_on_static_access(
        &mut self,
        static_access: &mut expr::HirStaticAccessExpr<'hir>,
        call_args: &mut Vec<HirExpr<'hir>>,
        call_args_ty: &mut Vec<&'hir HirTy<'hir>>,
        call_generics: &mut Vec<&'hir HirTy<'hir>>,
        call_span: Span,
    ) -> HirResult<&'hir HirTy<'hir>> {
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
        let lookup_method_name = if call_generics.is_empty() {
            static_access.field.name
        } else {
            MonomorphizationPass::generate_mangled_name(
                self.arena,
                &HirGenericTy {
                    name: static_access.field.name,
                    inner: call_generics.iter().map(|g| (*g).clone()).collect(),
                    span: call_span,
                },
                "method",
            )
        };

        let func = class.methods.get(lookup_method_name);

        if let Some(method_sig) = func
            && !method_sig.is_instantiated
        {
            self.maybe_enqueue_deferred_method_materialization(
                name,
                static_access.field.name,
                call_generics.clone(),
                call_span,
            );
        } else {
            self.maybe_enqueue_deferred_method_materialization(
                name,
                static_access.field.name,
                call_generics.clone(),
                call_span,
            );
        }
        if func.is_none()
            || !func
                .map(|method_sig| method_sig.is_instantiated)
                .unwrap_or(false)
        {
            self.maybe_enqueue_deferred_method_materialization(
                name,
                static_access.field.name,
                call_generics.clone(),
                call_span,
            );
            return Err(Self::unknown_method_err(
                lookup_method_name,
                name,
                &static_access.span,
            ));
        }

        if let Some(method_signature) = func {
            static_access.field.name = lookup_method_name;
            if !call_generics.is_empty() {
                call_generics.clear();
            }

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
                let path = static_access.span.path;
                let src = utils::get_file_content(path).unwrap();
                return Err(HirError::UnsupportedExpr(UnsupportedExpr {
                    span: static_access.span,
                    expr: "Instance method call".to_string(),
                    src: NamedSource::new(path, src),
                }));
            }
            if method_signature.params.len() != call_args.len() {
                return Err(Self::not_enough_arguments_err(
                    "static method".to_string(),
                    method_signature.params.len(),
                    &method_signature.span,
                    call_args.len(),
                    &call_span,
                ));
            }
            for (param, arg) in method_signature.params.iter().zip(call_args.iter_mut()) {
                self.retag_integer_literal_for_expected_ty(param.ty, arg);
                let arg_ty = self.check_expr(arg)?;
                self.is_equivalent_ty(param.ty, param.span, arg_ty, arg.span())?;
                call_args_ty.push(param.ty);
            }

            static_access.ty = self.arena.intern(method_signature.return_ty.clone());
            static_access.field.ty = self.arena.intern(method_signature.return_ty.clone());

            Ok(static_access.ty)
        } else if let Some((_, const_signature)) = class
            .constants
            .iter()
            .find(|f| *f.0 == static_access.field.name)
        {
            static_access.ty = const_signature.ty;
            static_access.field.ty = const_signature.ty;
            let HirTy::Function(function_ty) = const_signature.ty else {
                return Err(Self::illegal_operation_err(
                    const_signature.ty,
                    self.arena.types().get_unit_ty(),
                    static_access.span,
                    "call expression",
                ));
            };

            if function_ty.params.len() != call_args.len() {
                return Err(Self::not_enough_arguments_err(
                    "function pointer static field".to_string(),
                    function_ty.params.len(),
                    &function_ty.span,
                    call_args.len(),
                    &call_span,
                ));
            }

            for (idx, (param_ty, arg)) in function_ty
                .params
                .iter()
                .zip(call_args.iter_mut())
                .enumerate()
            {
                let expected_span = function_ty
                    .param_spans
                    .get(idx)
                    .copied()
                    .unwrap_or(function_ty.span);
                self.retag_integer_literal_for_expected_ty(param_ty, arg);
                let arg_ty = self.check_expr(arg)?;
                self.is_equivalent_ty(param_ty, expected_span, arg_ty, arg.span())?;
                call_args_ty.push(param_ty);
            }

            Ok(function_ty.ret_ty)
        } else {
            Err(Self::unknown_method_err(
                static_access.field.name,
                name,
                &static_access.span,
            ))
        }
    }

    fn get_ident_ty(&mut self, i: &mut HirIdentExpr<'hir>) -> HirResult<ContextVariable<'hir>> {
        if let Some(ctx_var) = self
            .context_functions
            .last()
            .unwrap()
            .get(self.current_func_name.unwrap())
            .unwrap()
            .get(i.name)
        {
            i.ty = ctx_var.ty;
            Ok(ctx_var.clone())
        } else {
            // Then it might just be a function name (e.g. `foo` instead of `foo()`)
            if let Some(func_signature) = self.signature.functions.get(i.name) {
                i.ty = self.arena.types().get_function_ty_with_spans(
                    func_signature.params.iter().map(|p| p.ty).collect(),
                    func_signature.params.iter().map(|p| p.ty_span).collect(),
                    &func_signature.return_ty,
                    func_signature.return_ty_span.unwrap_or(func_signature.span),
                    func_signature.span,
                );
                Ok(ContextVariable {
                    name: i.name,
                    name_span: i.span,
                    ty: i.ty,
                    _is_mut: false,
                    is_param: false,
                    _ty_span: i.span,
                    ptrs_to_locals: vec![],
                })
            } else {
                Err(Self::unknown_identifier_err(i.name, &i.span))
            }
        }
    }

    fn is_const_ptr_ty(&self, ty: &HirTy<'_>) -> bool {
        matches!(ty, HirTy::PtrTy(p) if p.is_const)
    }

    /// Retag integer literals to unsigned literals when an unsigned expected type is known
    /// and the value fits. This helps later passes rely on a concrete, context-aware type.
    fn retag_integer_literal_for_expected_ty(
        &mut self,
        expected_ty: &HirTy<'hir>,
        expr: &mut HirExpr<'hir>,
    ) {
        match expr {
            HirExpr::Unary(unary) if unary.op.is_none() => {
                self.retag_integer_literal_for_expected_ty(expected_ty, unary.expr.as_mut());
            }
            HirExpr::IntegerLiteral(i) => {
                let expected_uint = match expected_ty {
                    HirTy::UnsignedInteger(u) => u,
                    _ => return,
                };

                if i.value < 0 {
                    return;
                }

                let value = i64::cast_unsigned(i.value);
                let lit_uint_ty = self.arena.types().get_literal_uint_ty(value, i.span);

                // Only retag when assignment is already valid for the expected unsigned type.
                if self
                    .is_equivalent_ty(expected_ty, i.span, lit_uint_ty, i.span)
                    .is_ok()
                {
                    let concrete_expected =
                        self.arena.types().get_uint_ty(expected_uint.size_in_bits);
                    *expr = HirExpr::UnsignedIntegerLiteral(HirUnsignedIntegerLiteralExpr {
                        value,
                        span: i.span,
                        ty: concrete_expected,
                    });
                }
            }
            _ => {}
        }
    }

    fn is_mutable_ptr_ty(&self, ty: &HirTy<'_>) -> bool {
        matches!(ty, HirTy::PtrTy(p) if !p.is_const)
    }

    /// Check if two types are equivalent, considering generics and pointers
    fn is_equivalent_ty(
        &self,
        expected_ty: &HirTy<'_>,
        expected_span: Span,
        found_ty: &HirTy<'_>,
        found_span: Span,
    ) -> HirResult<()> {
        match (expected_ty, found_ty) {
            (HirTy::UnsignedInteger(_), HirTy::LiteralInteger(li)) => {
                // e.g.: If we expect a uint8 but found a `int16 /* 255 */` it should still be accepted because the literal value can fit into the expected type
                let lu = self
                    .arena
                    .types()
                    .get_literal_uint_ty(i64::cast_unsigned(li.value), li.span);
                self.is_equivalent_ty(expected_ty, expected_span, lu, found_span)
            }
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
            (HirTy::Slice(slice), HirTy::InlineArray(array)) => {
                self.is_equivalent_ty(slice.inner, expected_span, array.inner, found_span)
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
            (HirTy::UnsignedInteger(u), HirTy::LiteralUnsignedInteger(lu)) => {
                if u.size_in_bits >= lu.get_minimal_uint_ty().size_in_bits {
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
            (HirTy::Integer(i), HirTy::LiteralInteger(li)) => {
                if i.size_in_bits >= li.get_minimal_int_ty().size_in_bits {
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
            (HirTy::Float(f), HirTy::LiteralFloat(lf)) => {
                if f.size_in_bits >= lf.get_float_ty().size_in_bits {
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
            (HirTy::PtrTy(p), HirTy::String(_)) => {
                if p.inner == self.arena.types().get_uint_ty(8) {
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
            (HirTy::PtrTy(p1), HirTy::PtrTy(p2)) => {
                // Pointer compatibility rules:
                // - *T (mutable) can become *const T
                // - *const T cannot become *T (mutable)
                match (p1.is_const, p2.is_const) {
                    // *T expected, *const T found: cannot convert const to mutable
                    (false, true) => Err(Self::type_mismatch_err(
                        &format!("{}", expected_ty),
                        &expected_span,
                        &format!("{}", found_ty),
                        &found_span,
                    )),
                    // Both same constness, or *const T expected and *T found (mutable to const is OK)
                    _ => self.is_equivalent_ty(p1.inner, expected_span, p2.inner, found_span),
                }
            }
            // If it expects a value type, but found a pointer, dereference should be explicit
            (expected, HirTy::PtrTy(p)) => {
                self.is_equivalent_ty(expected, expected_span, p.inner, found_span)?;
                // check if found is copyable
                match expected {
                    HirTy::PtrTy(ptr) => {
                        if ptr.inner.is_copyable(&self.signature) {
                            return Ok(());
                        }
                    }
                    _ => {
                        if expected.is_copyable(&self.signature) {
                            return Ok(());
                        }
                    }
                }
                Err(HirError::TypeIsNotCopyable(TypeIsNotCopyableError {
                    span: found_span,
                    type_name: found_ty.to_string(),
                    src: NamedSource::new(
                        found_span.path,
                        utils::get_file_content(found_span.path).unwrap(),
                    ),
                }))
            }
            // TODO: Replace Unit type with a proper nullptr_t type
            (HirTy::PtrTy(_), HirTy::Unit(_)) => Ok(()),
            // We silently ignore those errors, because they arise from earlier issues. e.g. if a variable
            // is uninitialized, it will have type `!uninitialized` and any operation on it will be invalid,
            // but we don't want to flood the user with type mismatch errors in that case.
            // Similarly, if a variable failed type-checking, it will have type `!error`, and we suppress cascades.
            (HirTy::Uninitialized(_), _) | (_, HirTy::Uninitialized(_)) => Ok(()),
            (HirTy::Error(_), _) | (_, HirTy::Error(_)) => Ok(()),
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
            HirTy::PtrTy(p) => self.get_class_name_of_type(p.inner),
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
            HirTy::String(_) => "str".to_string(),
            HirTy::Unit(_) => "unit".to_string(),
            HirTy::Slice(l) => format!("[{}]", Self::get_type_display_name(l.inner)),
            HirTy::PtrTy(p) => {
                if p.is_const {
                    format!("*const {}", Self::get_type_display_name(p.inner))
                } else {
                    format!("*{}", Self::get_type_display_name(p.inner))
                }
            }
            HirTy::InlineArray(arr) => {
                format!("[{}; {}]", Self::get_type_display_name(arr.inner), arr.size)
            }
            _ => "<unknown>".to_string(),
        }
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

    fn unknown_overloadable_operator(op: &str, span: &Span) -> HirError {
        let path = span.path;
        let src = utils::get_file_content(path).unwrap();
        HirError::UnknownOverloadableOperator(UnknownOverloadableOperatorError {
            operator: op.into(),
            span: *span,
            src: NamedSource::new(path, src),
        })
    }

    fn accessing_private_destructor_err(span: &Span, ty: &str) -> HirError {
        let path = span.path;
        let src = utils::get_file_content(path).unwrap();
        HirError::AccessingPrivateDestructor(AccessingPrivateDestructorError {
            span: *span,
            ty: ty.to_string(),
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

    fn union_field_cannot_be_automatically_deleted_warning(
        union_span: &Span,
        union_name: &str,
        variant_name: &str,
        variant_span: &Span,
        usage_loc_type: String,
        usage_loc_span: Span,
    ) {
        let path = union_span.path;
        let src = utils::get_file_content(path).unwrap();
        let warning: ErrReport = HirWarning::UnionFieldCannotBeAutomaticallyDeleted(
            UnionFieldCannotBeAutomaticallyDeletedWarning {
                union_span: *union_span,
                union_name: union_name.to_string(),
                variant_name: variant_name.to_string(),
                variant_span: *variant_span,
                usage_loc_type,
                usage_loc_span,
                src: NamedSource::new(path, src),
            },
        )
        .into();
        eprintln!("{:?}", warning);
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

    fn non_trivially_copyable_struct_holds_a_raw_pointer_with_no_custom_destructor_warning(
        class: &HirStruct<'hir>,
        pointer_span: Span,
    ) {
        let path = class.span.path;
        let src = utils::get_file_content(path).unwrap();
        let warning: ErrReport =
            HirWarning::UnsafeRawPointerStruct(UnsafeRawPointerStructWarning {
                src: NamedSource::new(path, src),
                struct_span: class.name_span,
                pointer_span,
                struct_name: class.name.to_string(),
            })
            .into();
        eprintln!("{:?}", warning);
    }

    /// Check if the expression is a pointer (`&expr`) to a local variable,
    /// or an identifier that holds a pointer to a local variable.
    /// Returns the name of the local variable if it is, None otherwise.
    ///
    /// This is used to detect when a function is trying to return a pointer
    /// to a local variable, which would be a dangling pointer.
    /// Get all local variables that the expression points to (directly or transitively).
    /// Returns a list of local variable names if any, empty vec otherwise.
    ///
    /// This is used to detect when a function is trying to return a pointer
    /// to a local variable, which would be a dangling pointer.
    fn get_local_ptr_targets(&self, expr: &HirExpr<'hir>) -> Vec<&'hir str> {
        match expr {
            HirExpr::Unary(u) => {
                if matches!(u.op, Some(HirUnaryOp::AsRef)) {
                    // Check what we're taking a pointer to
                    match u.expr.as_ref() {
                        // Check if this is a local variable (not a function parameter)
                        HirExpr::Ident(ident) if self.is_local_variable(ident.name) => {
                            return vec![ident.name];
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
                    self.get_local_ptr_targets(u.expr.as_ref())
                } else {
                    vec![]
                }
            }
            HirExpr::Ident(ident) => {
                // Check if this identifier holds pointers to local variables
                self.get_ptrs_to_locals(ident.name)
            }
            _ => vec![],
        }
    }

    /// Get the local variables that a variable points to (if any)
    fn get_ptrs_to_locals(&self, name: &str) -> Vec<&'hir str> {
        if let Some(context_map) = self.context_functions.last()
            && let Some(func_name) = self.current_func_name
            && let Some(context_func) = context_map.get(func_name)
            && let Some(var) = context_func.get_variable(name)
        {
            return var.ptrs_to_locals.clone();
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
            HirTy::Integer(_)
                | HirTy::LiteralInteger(_)
                | HirTy::UnsignedInteger(_)
                | HirTy::LiteralUnsignedInteger(_)
                | HirTy::Float(_)
                | HirTy::LiteralFloat(_)
                | HirTy::Char(_)
        )
    }

    /// == !=
    fn is_equality_comparable(&self, ty: &HirTy) -> bool {
        match ty {
            HirTy::Integer(_)
            | HirTy::LiteralInteger(_)
            | HirTy::UnsignedInteger(_)
            | HirTy::LiteralUnsignedInteger(_)
            | HirTy::Float(_)
            | HirTy::LiteralFloat(_)
            | HirTy::Char(_)
            | HirTy::Boolean(_)
            | HirTy::PtrTy(_)
            | HirTy::Unit(_) => true,
            HirTy::Named(n) => self.signature.enums.contains_key(n.name),
            _ => false,
        }
    }

    fn is_orderable_type(ty: &HirTy) -> bool {
        matches!(
            ty,
            HirTy::Integer(_)
                | HirTy::LiteralInteger(_)
                | HirTy::UnsignedInteger(_)
                | HirTy::LiteralUnsignedInteger(_)
                | HirTy::Float(_)
                | HirTy::LiteralFloat(_)
                | HirTy::Char(_)
        )
    }

    /// Can perform shift left/right operation
    fn is_shiftable_type(ty: &HirTy) -> bool {
        matches!(
            ty,
            // Potentially add `HirTy::Char(_)`
            HirTy::Integer(_)
                | HirTy::LiteralInteger(_)
                | HirTy::UnsignedInteger(_)
                | HirTy::LiteralUnsignedInteger(_)
        )
    }
    // Might be a good idea to collapse shiftable_type & this in one function
    // Potentially is_binary_operable_type (needs a better name)
    fn is_binary_comparable_type(ty: &HirTy) -> bool {
        matches!(
            ty,
            // Potentially add `HirTy::Char(_)`
            HirTy::Integer(_)
                | HirTy::LiteralInteger(_)
                | HirTy::UnsignedInteger(_)
                | HirTy::LiteralUnsignedInteger(_)
        )
    }
}
