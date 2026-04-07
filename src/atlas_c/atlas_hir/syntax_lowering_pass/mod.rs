pub mod case;

use heck::{ToPascalCase, ToSnakeCase};
use miette::{ErrReport, NamedSource};
use std::{collections::BTreeMap, vec};

use crate::atlas_c::{
    atlas_frontend::{
        parse,
        parser::{
            arena::AstArena,
            ast::{
                AstArg, AstBinaryOp, AstBlock, AstDestructor, AstEnum, AstExpr, AstExternFunction,
                AstFlag, AstFunction, AstGeneric, AstGenericConstraint, AstIdentifier, AstImport,
                AstItem, AstLiteral, AstMethod, AstMethodModifier, AstNamespace, AstProgram,
                AstStatement, AstStruct, AstType, AstUnaryOp, AstUnion,
            },
        },
    },
    atlas_hir::{
        HirImport, HirModule, HirModuleBody,
        arena::HirArena,
        error::{
            AssignmentCannotBeAnExpressionError, HirError, HirResult,
            IncorrectIntrinsicCallArgumentsError, NonConstantValueError,
            NullableTypeRequiresStdLibraryError, ReservedVariableNameError,
            StructNameCannotBeOneLetterError, UnknownFileImportError, UnknownTypeError,
            UnsupportedExpr, UnsupportedItemError, UselessError,
        },
        expr::{
            HirBinaryOpExpr, HirBinaryOperator, HirBooleanLiteralExpr, HirCastExpr,
            HirCharLiteralExpr, HirDeleteExpr, HirExpr, HirFieldAccessExpr, HirFieldInit,
            HirFloatLiteralExpr, HirFunctionCallExpr, HirFunctionKind, HirIdentExpr,
            HirIndexingExpr, HirIntegerLiteralExpr, HirIntrinsicCallExpr, HirListLiteralExpr,
            HirListLiteralWithSizeExpr, HirNullLiteralExpr, HirObjLiteralExpr, HirStaticAccessExpr,
            HirStringLiteralExpr, HirThisLiteral, HirUnaryOp, HirUnitLiteralExpr,
            HirUnsignedIntegerLiteralExpr, UnaryOpExpr,
        },
        item::{
            HirEnum, HirEnumVariant, HirFunction, HirStruct, HirStructDestructor, HirStructMethod,
            HirUnion,
        },
        monomorphization_pass::generic_pool::HirGenericPool,
        signature::{
            ConstantValue, HirFunctionParameterSignature, HirFunctionSignature,
            HirGenericConstraint, HirGenericConstraintKind, HirModuleSignature,
            HirStructConstantSignature, HirStructDestructorSignature, HirStructFieldSignature,
            HirStructMethodModifier, HirStructMethodSignature, HirStructSignature,
            HirTypeParameterItemSignature, HirUnionSignature, HirVisibility,
        },
        stmt::{
            HirAssignStmt, HirBlock, HirExprStmt, HirIfElseStmt, HirReturn, HirStatement,
            HirVariableStmt, HirWhileStmt,
        },
        syntax_lowering_pass::case::Case,
        ty::{HirGenericTy, HirNamedTy, HirTy},
        warning::{HirWarning, NameShouldBeInDifferentCaseWarning, ThisTypeIsStillUnstableWarning},
    },
    utils::{self, Span},
};

pub struct AstSyntaxLoweringPass<'ast, 'hir> {
    arena: &'hir HirArena<'hir>,
    ast: &'ast AstProgram<'ast>,
    ast_arena: &'ast AstArena<'ast>,
    pub generic_pool: HirGenericPool<'hir>,
    module_body: HirModuleBody<'hir>,
    module_signature: HirModuleSignature<'hir>,
    /// Collect warnings during lowering (Only nullable types for now)
    warnings: Vec<HirWarning>,
    /// Keep track of already imported modules to avoid duplicate imports
    pub already_imported: BTreeMap<&'hir str, ()>,
    pub using_std: bool,
    temp_counter: usize,
    namespace_stack: Vec<&'hir str>,
}

impl<'ast, 'hir> AstSyntaxLoweringPass<'ast, 'hir> {
    pub fn new(
        arena: &'hir HirArena<'hir>,
        ast: &'ast AstProgram,
        ast_arena: &'ast AstArena<'ast>,
        using_std: bool,
    ) -> Self {
        Self {
            arena,
            ast,
            ast_arena,
            generic_pool: HirGenericPool::new(arena),
            module_body: HirModuleBody::default(),
            module_signature: HirModuleSignature::default(),
            warnings: Vec::new(),
            already_imported: BTreeMap::new(),
            using_std,
            temp_counter: 0,
            namespace_stack: Vec::new(),
        }
    }
}

impl<'ast, 'hir> AstSyntaxLoweringPass<'ast, 'hir> {
    fn qualified_name(&self, name: &str) -> String {
        if self.namespace_stack.is_empty() || name.contains("::") {
            return name.to_string();
        }
        let mut parts = self.namespace_stack.join("::");
        parts.push_str("::");
        parts.push_str(name);
        parts
    }

    fn qualified_type_name(&self, name: &str) -> String {
        // Type references are always explicit.
        // If callers want namespaced types, they must write `ns::Type` directly.
        name.to_string()
    }

    fn enter_namespace(&mut self, ns: &'hir str) {
        self.namespace_stack.push(ns);
    }

    fn leave_namespace(&mut self) {
        let _ = self.namespace_stack.pop();
    }

    fn visit_namespace(&mut self, node: &'ast AstNamespace<'ast>) -> HirResult<()> {
        let ns_name = self.arena.names().get(node.name.name);
        self.enter_namespace(ns_name);
        for item in node.items {
            self.visit_item(item)?;
        }
        self.leave_namespace();
        Ok(())
    }

    fn method_returns_self(&self, method: &HirStructMethod<'hir>, struct_name: &'hir str) -> bool {
        match &method.signature.return_ty {
            HirTy::Named(n) => n.name == struct_name,
            HirTy::Generic(g) => g.name == struct_name,
            _ => false,
        }
    }

    pub fn lower(&mut self) -> HirResult<&'hir mut HirModule<'hir>> {
        for item in self.ast.items {
            self.visit_item(item)?;
        }

        for _ in 0..(self.warnings.len()) {
            let warning: HirWarning = self.warnings.remove(0);
            let report: ErrReport = warning.into();
            eprintln!("{:?}", report);
        }

        Ok(self.arena.intern(HirModule {
            body: self.module_body.clone(),
            signature: self.module_signature.clone(),
        }))
    }
    pub fn visit_item(&mut self, ast_item: &'ast AstItem<'ast>) -> HirResult<()> {
        match ast_item {
            AstItem::Namespace(ns) => {
                self.visit_namespace(ns)?;
            }
            AstItem::Constant(c) => {
                let path = ast_item.span().path;
                let src = utils::get_file_content(path).unwrap();
                if c.name.name.starts_with("__tmp") {
                    eprintln!(
                        "{:?}",
                        Into::<miette::ErrReport>::into(ReservedVariableNameError {
                            name: String::from("__tmp"),
                            src: NamedSource::new(path, src.clone()),
                            span: c.name.span
                        })
                    )
                }
                return Err(HirError::UnsupportedItem(UnsupportedItemError {
                    span: ast_item.span(),
                    item: "Global constants".to_string(),
                    src: NamedSource::new(path, src),
                }));
            }
            AstItem::Function(ast_function) => {
                let hir_func = self.visit_func(ast_function)?;
                let qualified = self.qualified_name(ast_function.name.name);
                let name = self.arena.names().get(&qualified);
                if !name.is_snake_case() && !hir_func.signature.is_external {
                    Self::name_should_be_in_different_case_warning(
                        &ast_function.name.span,
                        "snake_case",
                        "function",
                        name,
                        &name.to_snake_case(),
                    );
                }
                self.module_signature
                    .functions
                    .insert(name, hir_func.signature);
                self.module_body.functions.insert(name, hir_func);
            }
            AstItem::Struct(ast_struct) => {
                let class = self.visit_struct(ast_struct)?;
                self.module_signature
                    .structs
                    .insert(class.name, self.arena.intern(class.signature.clone()));
                self.module_body.structs.insert(class.name, class);
            }
            AstItem::ExternStruct(_ast_struct) => {
                let class = self.visit_struct(_ast_struct)?;
                self.module_signature
                    .structs
                    .insert(class.name, self.arena.intern(class.signature.clone()));
                self.module_body.structs.insert(class.name, class);
            }
            AstItem::Import(ast_import) => match self.visit_import(ast_import) {
                Ok((hir_module, mut generic_pool)) => {
                    let allocated_hir: &'hir HirModule<'hir> = self.arena.intern(hir_module);
                    for (name, signature) in allocated_hir.signature.functions.iter() {
                        self.module_signature.functions.insert(name, *signature);
                    }
                    for (name, signature) in allocated_hir.signature.structs.iter() {
                        self.module_signature.structs.insert(name, *signature);
                    }
                    for (name, hir_struct) in allocated_hir.body.structs.iter() {
                        self.module_body.structs.insert(name, hir_struct.clone());
                    }
                    for (name, hir_func) in allocated_hir.body.functions.iter() {
                        self.module_body.functions.insert(name, hir_func.clone());
                    }
                    for (name, hir_enum) in allocated_hir.body.enums.iter() {
                        self.module_body.enums.insert(name, hir_enum.clone());
                    }
                    for (name, signature) in allocated_hir.signature.enums.iter() {
                        self.module_signature.enums.insert(name, signature);
                    }
                    for (name, hir_union) in allocated_hir.body.unions.iter() {
                        self.module_body.unions.insert(name, hir_union.clone());
                    }
                    for (name, signature) in allocated_hir.signature.unions.iter() {
                        self.module_signature.unions.insert(name, signature);
                    }
                    self.generic_pool.structs.append(&mut generic_pool.structs);
                }
                Err(e) => match e {
                    HirError::UselessError(_) => {}
                    _ => return Err(e),
                },
            },
            AstItem::ExternFunction(ast_extern_func) => {
                self.visit_extern_func(ast_extern_func)?;
            }
            AstItem::Enum(e) => {
                let hir_enum = self.visit_enum(e)?;
                let qualified = self.qualified_name(e.name.name);
                let enum_name = self.arena.names().get(&qualified);
                self.module_body.enums.insert(enum_name, hir_enum.clone());
                self.module_signature
                    .enums
                    .insert(enum_name, self.arena.intern(hir_enum));
            }
            AstItem::Union(ast_union) => {
                let hir_union = self.visit_union(ast_union)?;
                let qualified = self.qualified_name(ast_union.name.name);
                let union_name = self.arena.names().get(&qualified);
                self.module_body
                    .unions
                    .insert(union_name, hir_union.clone());
                self.module_signature
                    .unions
                    .insert(union_name, self.arena.intern(hir_union.signature.clone()));
            }
        }
        Ok(())
    }

    fn visit_union(&mut self, ast_union: &'ast AstUnion<'ast>) -> HirResult<HirUnion<'hir>> {
        let qualified = self.qualified_name(ast_union.name.name);
        let name = self.arena.names().get(&qualified);
        if !name.is_pascal_case() {
            Self::name_should_be_in_different_case_warning(
                &ast_union.name.span,
                "PascalCase",
                "union",
                name,
                &name.to_pascal_case(),
            );
        }
        if name.len() == 1 {
            return Err(Self::name_single_character_error(&ast_union.name.span));
        }
        let mut variants = vec![];
        for v in ast_union.variants.iter() {
            //Currently only supporting discriminant values for unions
            variants.push(HirStructFieldSignature {
                span: v.span,
                vis: HirVisibility::from(v.vis),
                name: self.arena.names().get(v.name.name),
                name_span: v.name.span,
                ty: self.visit_ty(v.ty)?,
                ty_span: v.ty.span(),
                docstring: if let Some(docstring) = v.docstring {
                    Some(self.arena.names().get(docstring))
                } else {
                    None
                },
            });
        }
        let mut generics: Vec<&HirGenericConstraint<'_>> = Vec::new();
        if !ast_union.generics.is_empty() {
            for generic in ast_union.generics.iter() {
                generics.push(self.arena.intern(HirGenericConstraint {
                    span: generic.span,
                    generic_name: self.arena.names().get(generic.name.name),
                    kind: {
                        let mut constraints: Vec<&HirGenericConstraintKind<'_>> = vec![];
                        for constraint in generic.constraints.iter() {
                            constraints.push(self.arena.intern(self.visit_constraint(constraint)?));
                        }
                        constraints
                    },
                }));
            }
        }
        let signature = HirUnionSignature {
            declaration_span: ast_union.span,
            name_span: ast_union.name.span,
            vis: ast_union.vis.into(),
            is_instantiated: generics.is_empty(),
            generics,
            name,
            variants: {
                let mut map = BTreeMap::new();
                for variant in variants.iter() {
                    map.insert(variant.name, variant.clone());
                }
                map
            },
            // This is filled by the monomorphization pass if needed
            pre_mangled_ty: None,
            docstring: if let Some(docstring) = ast_union.docstring {
                Some(self.arena.names().get(docstring))
            } else {
                None
            },
        };
        let hir = HirUnion {
            span: ast_union.span,
            name,
            name_span: ast_union.name.span,
            vis: ast_union.vis.into(),
            variants,
            signature,
            pre_mangled_ty: None,
        };
        Ok(hir)
    }

    fn visit_enum(&mut self, ast_enum: &'ast AstEnum<'ast>) -> HirResult<HirEnum<'hir>> {
        let qualified = self.qualified_name(ast_enum.name.name);
        let name = self.arena.names().get(&qualified);
        if !name.is_pascal_case() {
            Self::name_should_be_in_different_case_warning(
                &ast_enum.name.span,
                "PascalCase",
                "enum",
                name,
                &name.to_pascal_case(),
            );
        }
        if name.len() == 1 {
            return Err(Self::name_single_character_error(&ast_enum.name.span));
        }
        let mut variants = Vec::new();
        for variant in ast_enum.variants.iter() {
            let variant = HirEnumVariant {
                span: variant.span,
                name: self.arena.names().get(variant.name.name),
                name_span: variant.name.span,
                value: variant.value,
            };
            variants.push(variant);
        }
        let hir = HirEnum {
            span: ast_enum.span,
            name,
            name_span: ast_enum.name.span,
            variants,
            vis: ast_enum.vis.into(),
            docstring: if let Some(docstring) = ast_enum.docstring {
                Some(self.arena.names().get(docstring))
            } else {
                None
            },
        };
        Ok(hir)
    }

    fn visit_extern_func(&mut self, ast_extern_func: &AstExternFunction<'ast>) -> HirResult<()> {
        let qualified = self.qualified_name(ast_extern_func.name.name);
        let name = self.arena.names().get(&qualified);
        let ty = self.visit_ty(ast_extern_func.ret_ty)?.clone();

        let mut params: Vec<HirFunctionParameterSignature<'hir>> = Vec::new();
        let mut type_params: Vec<&'hir HirTypeParameterItemSignature<'hir>> = Vec::new();

        let mut generics: Vec<&HirGenericConstraint<'_>> = Vec::new();
        if !ast_extern_func.generics.is_empty() {
            for generic in ast_extern_func.generics.iter() {
                generics.push(self.arena.intern(HirGenericConstraint {
                    span: generic.span,
                    generic_name: self.arena.names().get(generic.name.name),
                    kind: {
                        let mut constraints: Vec<&HirGenericConstraintKind<'_>> = vec![];
                        for constraint in generic.constraints.iter() {
                            constraints.push(self.arena.intern(self.visit_constraint(constraint)?));
                        }
                        constraints
                    },
                }));
            }
        }

        for (arg_name, arg_ty) in ast_extern_func
            .args_name
            .iter()
            .zip(ast_extern_func.args_ty.iter())
        {
            let hir_arg_ty = self.visit_ty(arg_ty)?;
            let hir_arg_name = self.arena.names().get(arg_name.name);

            params.push(HirFunctionParameterSignature {
                span: arg_name.span,
                name: hir_arg_name,
                name_span: arg_name.span,
                ty: hir_arg_ty,
                ty_span: arg_ty.span(),
            });

            type_params.push(self.arena.intern(HirTypeParameterItemSignature {
                span: arg_name.span,
                name: hir_arg_name,
                name_span: arg_name.span,
            }));
        }
        let hir = self.arena.intern(HirFunctionSignature {
            span: ast_extern_func.span,
            vis: ast_extern_func.vis.into(),
            params,
            is_instantiated: generics.is_empty(),
            generics,
            type_params,
            return_ty: ty,
            return_ty_span: Some(ast_extern_func.ret_ty.span()),
            is_external: true,
            pre_mangled_ty: None,
            docstring: if let Some(docstring) = ast_extern_func.docstring {
                Some(self.arena.names().get(docstring))
            } else {
                None
            },
            is_intrinsic: matches!(ast_extern_func.flag, AstFlag::Intrinsic(_)),
            c_name: ast_extern_func
                .c_name
                .map(|name| self.arena.names().get(name)),
        });
        self.module_signature.functions.insert(name, hir);
        Ok(())
    }

    fn visit_struct(&mut self, node: &'ast AstStruct<'ast>) -> HirResult<HirStruct<'hir>> {
        let qualified = self.qualified_name(node.name.name);
        let name = self.arena.names().get(&qualified);
        if !name.is_pascal_case() && !node.is_extern {
            Self::name_should_be_in_different_case_warning(
                &node.name.span,
                "PascalCase",
                "struct",
                name,
                &name.to_pascal_case(),
            );
        }
        if name.len() == 1 {
            return Err(Self::name_single_character_error(&node.name.span));
        }

        let mut methods = Vec::new();
        for method in node.methods.iter() {
            let hir_method = self.visit_method(method)?;
            methods.push(hir_method);
        }

        let mut generics: Vec<&HirGenericConstraint<'_>> = Vec::new();
        if !node.generics.is_empty() {
            for generic in node.generics.iter() {
                generics.push(self.arena.intern(HirGenericConstraint {
                    span: generic.span,
                    generic_name: self.arena.names().get(generic.name.name),
                    kind: {
                        let mut constraints: Vec<&HirGenericConstraintKind<'_>> = vec![];
                        for constraint in generic.constraints.iter() {
                            constraints.push(self.arena.intern(self.visit_constraint(constraint)?));
                        }
                        constraints
                    },
                }));
            }
        }

        let mut fields = Vec::new();
        for field in node.fields.iter() {
            let ty = self.visit_ty(field.ty)?;
            let name = self.arena.names().get(field.name.name);
            fields.push(HirStructFieldSignature {
                span: field.span,
                vis: HirVisibility::from(field.vis),
                name,
                name_span: field.name.span,
                ty,
                ty_span: field.ty.span(),
                docstring: if let Some(docstring) = field.docstring {
                    Some(self.arena.names().get(docstring))
                } else {
                    None
                },
            });
        }

        let mut operators = Vec::new();
        for operator in node.operators.iter() {
            operators.push(self.visit_bin_op(&operator.op)?);
        }

        let mut constants: BTreeMap<&'hir str, &'hir HirStructConstantSignature<'hir>> =
            BTreeMap::new();
        for constant in node.constants.iter() {
            let ty = self.visit_ty(constant.ty)?;
            let name = self.arena.names().get(constant.name.name);
            let const_expr = self.visit_expr(constant.value)?;
            let value = match ConstantValue::try_from(const_expr) {
                Ok(value) => value,
                Err(_) => {
                    let path = constant.value.span().path;
                    let src = utils::get_file_content(path).unwrap();
                    return Err(HirError::NonConstantValue(NonConstantValueError {
                        span: constant.value.span(),
                        src: NamedSource::new(path, src),
                    }));
                }
            };
            constants.insert(
                name,
                self.arena.intern(HirStructConstantSignature {
                    span: constant.span,
                    vis: node.vis.into(),
                    name,
                    name_span: constant.name.span,
                    ty,
                    ty_span: constant.ty.span(),
                    value: self.arena.intern(value),
                    docstring: if let Some(docstring) = constant.docstring {
                        Some(self.arena.names().get(docstring))
                    } else {
                        None
                    },
                }),
            );
        }

        let had_user_defined_destructor = node.destructor.is_some();
        let destructor = if let Some(destructor) = node.destructor {
            Some(self.visit_destructor(destructor)?)
        } else {
            None
        };

        let has_copy_method = methods.iter().any(|method| {
            method.name == "copy"
                && method.signature.modifier == HirStructMethodModifier::Const
                && method.signature.params.len() == 1
                && self.method_returns_self(method, name)
        });
        let has_default_method = methods.iter().any(|method| {
            method.name == "default"
                && method.signature.modifier == HirStructMethodModifier::Static
                && method.signature.params.is_empty()
                && self.method_returns_self(method, name)
        });
        let is_trivially_copyable = matches!(node.flag, AstFlag::TriviallyCopyable(_));

        let signature = HirStructSignature {
            declaration_span: node.span,
            name,
            name_span: node.name.span,
            // This is filled by the monomorphization pass if needed
            pre_mangled_ty: None,
            vis: node.vis.into(),
            flag: node.flag.into(),
            methods: {
                let mut map = BTreeMap::new();
                for method in methods.iter() {
                    map.insert(method.name, method.signature.clone());
                }
                map
            },
            fields: {
                let mut map = BTreeMap::new();
                for field in fields.iter() {
                    map.insert(field.name, field.clone());
                }
                map
            },
            operators,
            constants,
            is_instantiated: generics.is_empty(),
            generics,
            destructor: destructor.as_ref().map(|d| d.signature.clone()),
            had_user_defined_destructor,
            is_std_copyable: has_copy_method || is_trivially_copyable,
            is_std_default: has_default_method,
            is_trivially_copyable,
            docstring: if let Some(docstring) = node.docstring {
                Some(self.arena.names().get(docstring))
            } else {
                None
            },
            is_extern: node.is_extern,
            c_name: node.c_name.map(|name| self.arena.names().get(name)),
        };

        Ok(HirStruct {
            span: node.span,
            name,
            // This is filled by the monomorphization pass if needed
            pre_mangled_ty: None,
            name_span: node.name.span,
            signature,
            methods,
            fields,
            destructor,
            vis: node.vis.into(),
            flag: node.flag.into(),
        })
    }

    fn visit_constraint(
        &mut self,
        constraint: &'ast AstGenericConstraint<'ast>,
    ) -> HirResult<HirGenericConstraintKind<'hir>> {
        match constraint {
            AstGenericConstraint::Concept(concept_bound) => {
                let name = self.arena.names().get(concept_bound.name.name);
                Ok(HirGenericConstraintKind::Concept {
                    name,
                    span: concept_bound.span,
                })
            }
            AstGenericConstraint::Operator { op, span } => {
                let operator = self.visit_bin_op(op)?;
                Ok(HirGenericConstraintKind::Operator {
                    op: operator,
                    span: *span,
                })
            }
            AstGenericConstraint::Std(std) => Ok(HirGenericConstraintKind::Std {
                name: self.arena.names().get(std.name),
                span: std.span,
            }),
        }
    }

    fn visit_method(&mut self, node: &'ast AstMethod<'ast>) -> HirResult<HirStructMethod<'hir>> {
        let type_parameters = node
            .args
            .iter()
            .map(|arg| self.visit_type_param_item(arg))
            .collect::<HirResult<Vec<_>>>();
        let ret_type_span = node.ret.span();
        let ret_type = self.visit_ty(node.ret)?.clone();
        let parameters = node
            .args
            .iter()
            .map(|arg| self.visit_func_param(arg))
            .collect::<HirResult<Vec<_>>>();

        let body = self.visit_block(node.body)?;
        let (generics, where_clause) =
            self.merge_generic_constraints(node.generics, node.where_clause);

        let signature = self.arena.intern(HirStructMethodSignature {
            modifier: match node.modifier {
                AstMethodModifier::Const => HirStructMethodModifier::Const,
                AstMethodModifier::Static => HirStructMethodModifier::Static,
                AstMethodModifier::Mutable => HirStructMethodModifier::Mutable,
                AstMethodModifier::Consuming => HirStructMethodModifier::Consuming,
            },
            span: node.span,
            vis: node.vis.into(),
            params: parameters?,
            generics,
            type_params: type_parameters?,
            return_ty: ret_type,
            return_ty_span: Some(ret_type_span),
            where_clause,
            // Sets to true by default; monomorphization pass will update if needed
            is_constraint_satisfied: true,
            docstring: if let Some(docstring) = node.docstring {
                Some(self.arena.names().get(docstring))
            } else {
                None
            },
        });
        let method = HirStructMethod {
            span: node.span,
            name: self.arena.names().get(node.name.name),
            name_span: node.name.span,
            signature,
            body,
        };
        Ok(method)
    }

    /// Merges method-level generic constraints from the where clause into the method generics.
    /// Constraints for method-level generics are moved into the generic bounds, while struct-level constraints remain in the where clause.
    fn merge_generic_constraints(
        &mut self,
        method_generics: Option<&'ast [&'ast AstGeneric<'ast>]>,
        where_clause: Option<&'ast [&'ast AstGeneric<'ast>]>,
    ) -> (
        Option<Vec<&'hir HirGenericConstraint<'hir>>>,
        Option<Vec<&'hir HirGenericConstraint<'hir>>>,
    ) {
        // If no where clause, convert method generics to HIR and return
        let where_clause = match where_clause {
            Some(wc) => wc,
            None => {
                let method_hir = method_generics.map(|generics| {
                    generics
                        .iter()
                        .map(|generic| {
                            let constraints: Vec<&'hir HirGenericConstraintKind<'hir>> = generic
                                .constraints
                                .iter()
                                .map(|constraint| {
                                    self.arena
                                        .intern(self.visit_constraint(constraint).unwrap())
                                        as &'hir _
                                })
                                .collect();

                            self.arena.intern(HirGenericConstraint {
                                span: generic.span,
                                generic_name: self.arena.names().get(generic.name.name),
                                kind: constraints,
                            }) as &'hir _
                        })
                        .collect::<Vec<&'hir HirGenericConstraint<'hir>>>()
                });
                return (method_hir, None);
            }
        };

        // Build a set of method generic names for O(1) lookup
        let method_generic_names: std::collections::HashSet<&str> = method_generics
            .map(|generics| {
                generics
                    .iter()
                    .map(|g| self.arena.names().get(g.name.name))
                    .collect()
            })
            .unwrap_or_default();

        // Collect constraints from where clause, partitioned by generic name
        let mut method_level_constraints: std::collections::BTreeMap<
            &'hir str,
            Vec<&'ast AstGenericConstraint<'ast>>,
        > = std::collections::BTreeMap::new();
        let mut struct_level_generics: Vec<&'ast AstGeneric<'ast>> = Vec::new();

        for generic in where_clause {
            let generic_name = self.arena.names().get(generic.name.name);

            if method_generic_names.contains(generic_name) {
                // This constraint belongs to a method generic - collect for merging
                method_level_constraints
                    .entry(generic_name)
                    .or_default()
                    .extend(generic.constraints);
            } else {
                // This constraint belongs to a struct generic - keep in where clause
                struct_level_generics.push(generic);
            }
        }

        // Merge collected constraints into method generics
        let updated_method_generics = if let Some(generics) = method_generics {
            let merged_generics: Vec<&'hir HirGenericConstraint<'hir>> = generics
                .iter()
                .map(|generic| {
                    let generic_name = self.arena.names().get(generic.name.name);
                    let mut all_constraints: Vec<&'hir HirGenericConstraintKind<'hir>> = generic
                        .constraints
                        .iter()
                        .map(|constraint| {
                            self.arena
                                .intern(self.visit_constraint(constraint).unwrap())
                                as &'hir _
                        })
                        .collect();

                    // Add constraints from where clause for this generic
                    if let Some(extra_constraints) = method_level_constraints.get(generic_name) {
                        for constraint in extra_constraints {
                            all_constraints.push(
                                self.arena
                                    .intern(self.visit_constraint(constraint).unwrap()),
                            );
                        }
                    }

                    self.arena.intern(HirGenericConstraint {
                        span: generic.span,
                        generic_name,
                        kind: all_constraints,
                    }) as &'hir _
                })
                .collect();

            if merged_generics.is_empty() {
                None
            } else {
                Some(merged_generics)
            }
        } else {
            None
        };

        // Convert struct-level generics to HIR
        let updated_where_clause = if struct_level_generics.is_empty() {
            None
        } else {
            let where_hir: Vec<&'hir HirGenericConstraint<'hir>> = struct_level_generics
                .iter()
                .map(|generic| {
                    let constraints: Vec<&'hir HirGenericConstraintKind<'hir>> = generic
                        .constraints
                        .iter()
                        .map(|constraint| {
                            self.arena
                                .intern(self.visit_constraint(constraint).unwrap())
                                as &'hir _
                        })
                        .collect();

                    self.arena.intern(HirGenericConstraint {
                        span: generic.span,
                        generic_name: self.arena.names().get(generic.name.name),
                        kind: constraints,
                    }) as &'hir _
                })
                .collect();

            Some(where_hir)
        };

        (updated_method_generics, updated_where_clause)
    }

    fn visit_destructor(
        &mut self,
        destructor: &'ast AstDestructor<'ast>,
    ) -> HirResult<HirStructDestructor<'hir>> {
        let signature = HirStructDestructorSignature {
            span: destructor.span,
            vis: destructor.vis.into(),
            where_clause: None,
            docstring: if let Some(docstring) = destructor.docstring {
                Some(self.arena.names().get(docstring))
            } else {
                None
            },
        };
        let hir = HirStructDestructor {
            span: destructor.span,
            signature: self.arena.intern(signature),
            body: self.visit_block(destructor.body)?,
            vis: destructor.vis.into(),
        };
        Ok(hir)
    }

    fn visit_import(
        &mut self,
        node: &'ast AstImport<'ast>,
    ) -> HirResult<(&'hir HirModule<'hir>, HirGenericPool<'hir>)> {
        //TODO: Handle errors properly
        if !self.already_imported.contains_key(node.path) {
            self.already_imported
                .insert(self.arena.intern(node.path.to_owned()), ());
            let src = match crate::atlas_c::utils::get_file_content(node.path) {
                Ok(src) => src,
                Err(_) => {
                    let report: ErrReport = HirError::UnknownFileImport(UnknownFileImportError {
                        span: node.span,
                        src: NamedSource::new(
                            node.span.path,
                            utils::get_file_content(node.span.path).unwrap(),
                        ),
                        file_name: node.path.to_string(),
                    })
                    .into();
                    eprintln!("{:?}", report);
                    std::process::exit(1);
                }
            };
            let path = crate::atlas_c::utils::string_to_static_str(node.path.to_owned());
            let ast: AstProgram<'ast> = match parse(path, self.ast_arena, src) {
                Ok(ast) => ast,
                Err(e) => {
                    let report: ErrReport = (*e).into();
                    eprintln!("{:?}", report);
                    std::process::exit(1);
                }
            };
            let allocated_ast = self.ast_arena.alloc(ast);
            let mut ast_lowering_pass = AstSyntaxLoweringPass::<'ast, 'hir>::new(
                self.arena,
                allocated_ast,
                self.ast_arena,
                self.using_std,
            );
            ast_lowering_pass
                .already_imported
                .append(&mut self.already_imported);
            let hir = ast_lowering_pass.lower()?;
            self.already_imported
                .append(&mut ast_lowering_pass.already_imported);
            let path: &'hir str = self.arena.names().get(node.path);
            let hir_import: &'hir HirImport<'hir> = self.arena.intern(HirImport {
                span: node.span,
                path,
                path_span: node.span,
                alias: None,
                alias_span: None,
            });

            let new_hir = self.arena.intern(HirModule {
                body: {
                    let mut body = hir.body.clone();
                    body.imports.push(hir_import);
                    body
                },
                signature: hir.signature.clone(),
            });

            Ok((new_hir, ast_lowering_pass.generic_pool))
        } else {
            Err(HirError::UselessError(UselessError {
                span: node.span,
                src: NamedSource::new(
                    node.span.path,
                    utils::get_file_content(node.span.path).unwrap(),
                ),
            }))
        }
    }

    fn visit_block(&mut self, node: &'ast AstBlock<'ast>) -> HirResult<HirBlock<'hir>> {
        let mut statements = Vec::new();
        for stmt in node.stmts.iter() {
            let mut lowered = self.visit_stmt(stmt)?;
            statements.append(&mut lowered);
        }
        Ok(HirBlock {
            statements,
            span: node.span,
        })
    }

    fn visit_stmt(&mut self, node: &'ast AstStatement<'ast>) -> HirResult<Vec<HirStatement<'hir>>> {
        match node {
            AstStatement::While(ast_while) => {
                let condition = self.visit_expr(ast_while.condition)?;
                let body = self.visit_block(ast_while.body)?;
                let hir = HirStatement::While(HirWhileStmt {
                    span: node.span(),
                    condition,
                    body,
                });
                Ok(vec![hir])
            }
            AstStatement::Block(ast_block) => {
                let block = self.visit_block(ast_block)?;
                let hir = HirStatement::Block(block);
                Ok(vec![hir])
            }
            AstStatement::Const(ast_const) => {
                let name = self.arena.names().get(ast_const.name.name);
                if !name.is_snake_case() {
                    Self::name_should_be_in_different_case_warning(
                        &ast_const.span,
                        "snake_case",
                        "constant",
                        name,
                        &name.to_snake_case(),
                    );
                }
                if name.starts_with("__tmp") {
                    let path = ast_const.span.path;
                    let src = crate::atlas_c::utils::get_file_content(path).unwrap();
                    return Err(HirError::ReservedVariableName(ReservedVariableNameError {
                        name: String::from("__tmp"),
                        src: NamedSource::new(path, src.clone()),
                        span: ast_const.name.span,
                    }));
                }
                let ty = self.visit_ty(ast_const.ty)?;

                let value = self.visit_expr(ast_const.value)?;
                let (mut temps, value) = self.separate_temporaries(value, true);
                let hir = HirStatement::Const(HirVariableStmt {
                    span: node.span(),
                    name,
                    name_span: ast_const.name.span,
                    ty,
                    ty_span: Some(ast_const.ty.span()),
                    value,
                });
                temps.push(hir);
                Ok(temps)
            }
            AstStatement::Let(ast_let) => {
                let name = self.arena.names().get(ast_let.name.name);
                if name.starts_with("__tmp") {
                    let path = ast_let.span.path;
                    let src = crate::atlas_c::utils::get_file_content(path).unwrap();
                    return Err(HirError::ReservedVariableName(ReservedVariableNameError {
                        name: String::from("__tmp"),
                        src: NamedSource::new(path, src.clone()),
                        span: ast_let.name.span,
                    }));
                }
                if !name.is_snake_case() {
                    Self::name_should_be_in_different_case_warning(
                        &ast_let.span,
                        "snake_case",
                        "variable",
                        name,
                        &name.to_snake_case(),
                    );
                }
                let ty = ast_let.ty.map(|ty| self.visit_ty(ty)).transpose()?;

                let value = self.visit_expr(ast_let.value)?;
                let (mut temps, value) = self.separate_temporaries(value, true);
                let hir = HirStatement::Let(HirVariableStmt {
                    span: node.span(),
                    name,
                    name_span: ast_let.name.span,
                    // If no type is specified, we use an uninitialized type as a placeholder
                    ty: ty.unwrap_or(self.arena.types().get_uninitialized_ty()),
                    ty_span: ty.map(|_| ast_let.ty.unwrap().span()),
                    value,
                });
                temps.push(hir);
                Ok(temps)
            }
            AstStatement::Assign(assign) => {
                let target = self.visit_expr(assign.target)?;
                let value = self.visit_expr(assign.value)?;
                let (mut dst_temps, target) = self.separate_temporaries(target, true);
                let (mut val_temps, value) = self.separate_temporaries(value, true);
                let hir = HirStatement::Assign(HirAssignStmt {
                    span: node.span(),
                    dst: target,
                    val: value,
                    ty: self.arena.types().get_uninitialized_ty(),
                });
                dst_temps.append(&mut val_temps);
                dst_temps.push(hir);
                Ok(dst_temps)
            }
            AstStatement::IfElse(ast_if_else) => {
                let condition = self.visit_expr(ast_if_else.condition)?;
                let (mut cond_temps, condition) = self.separate_temporaries(condition, true);
                let then_branch = self.visit_block(ast_if_else.body)?;
                //If you don't type, the compiler will use it as an "Option<&mut HirBlock<'hir>>"
                //Which is dumb asf
                let else_branch: Option<HirBlock<'hir>> = match ast_if_else.else_body {
                    Some(else_body) => Some(self.visit_block(else_body)?),
                    None => None,
                };
                let hir = HirStatement::IfElse(HirIfElseStmt {
                    span: node.span(),
                    condition,
                    then_branch,
                    else_branch,
                });
                cond_temps.push(hir);
                Ok(cond_temps)
            }
            //The parser really need a bit of work
            AstStatement::Return(ast_return) => {
                if let Some(expr) = &ast_return.value {
                    let expr_hir = self.visit_expr(expr)?;
                    let (mut temps, expr) = self.separate_temporaries(expr_hir, true);
                    let hir = HirStatement::Return(HirReturn {
                        span: node.span(),
                        ty: expr.ty(),
                        value: Some(expr),
                    });
                    temps.push(hir);
                    Ok(temps)
                } else {
                    Ok(vec![HirStatement::Return(HirReturn {
                        span: node.span(),
                        value: None,
                        ty: self.arena.types().get_uninitialized_ty(),
                    })])
                }
            }
            AstStatement::Expr(ast_expr) => {
                let expr = self.visit_expr(ast_expr)?;
                let (mut temps, expr) = self.separate_temporaries(expr, true);
                let hir = HirStatement::Expr(HirExprStmt {
                    span: node.span(),
                    expr,
                });
                temps.push(hir);
                Ok(temps)
            } /*
              _ => {
                  let path = node.span().path;
                  let src = crate::atlas_c::utils::get_file_content(path).unwrap();
                  Err(HirError::UnsupportedStatement(UnsupportedStatement {
                      span: node.span(),
                      stmt: format!("{:?}", node),
                      src: NamedSource::new(path, src),
                  }))
              }
              */
        }
    }

    fn should_hoist_temporary_expr(&self, expr: &HirExpr<'hir>) -> bool {
        matches!(expr, HirExpr::Call(_))
    }

    fn find_function_signature_in_module(
        &self,
        module_sig: &HirModuleSignature<'hir>,
        name: &str,
    ) -> Option<&'hir HirFunctionSignature<'hir>> {
        if let Some(sig) = module_sig.functions.get(name) {
            return Some(*sig);
        }

        for imported in module_sig.imported_modules.values() {
            if let Some(sig) = self.find_function_signature_in_module(imported, name) {
                return Some(sig);
            }
        }

        None
    }

    fn call_returns_unit(&self, call: &HirFunctionCallExpr<'hir>) -> bool {
        if call.ty.is_unit() {
            return true;
        }

        match call.callee.as_ref() {
            HirExpr::Ident(ident) => self
                .find_function_signature_in_module(&self.module_signature, ident.name)
                .is_some_and(|sig| sig.return_ty.is_unit()),
            _ => false,
        }
    }

    fn fresh_temp_name(&mut self) -> &'hir str {
        let name = format!("__tmp{}", self.temp_counter);
        self.temp_counter += 1;
        self.arena.names().get(&name)
    }

    fn hoist_expr_into_temp(&mut self, expr: HirExpr<'hir>) -> (HirStatement<'hir>, HirExpr<'hir>) {
        let span = expr.span();
        let name = self.fresh_temp_name();
        let uninit = self.arena.types().get_uninitialized_ty();

        let stmt = HirStatement::Let(HirVariableStmt {
            span,
            name,
            name_span: span,
            ty: uninit,
            ty_span: None,
            value: expr,
        });

        let ident = HirExpr::Ident(HirIdentExpr {
            name,
            span,
            ty: uninit,
        });

        (stmt, ident)
    }

    fn separate_temporaries(
        &mut self,
        expr: HirExpr<'hir>,
        is_root: bool,
    ) -> (Vec<HirStatement<'hir>>, HirExpr<'hir>) {
        match expr {
            HirExpr::HirBinaryOperation(mut b) => {
                let (mut lhs_temps, lhs) = self.separate_temporaries(*b.lhs, false);
                let (mut rhs_temps, rhs) = self.separate_temporaries(*b.rhs, false);
                b.lhs = Box::new(lhs);
                b.rhs = Box::new(rhs);
                let rebuilt = HirExpr::HirBinaryOperation(b);
                lhs_temps.append(&mut rhs_temps);
                if !is_root && self.should_hoist_temporary_expr(&rebuilt) {
                    let (stmt, ident) = self.hoist_expr_into_temp(rebuilt);
                    lhs_temps.push(stmt);
                    (lhs_temps, ident)
                } else {
                    (lhs_temps, rebuilt)
                }
            }
            HirExpr::Call(mut c) => {
                let (mut callee_temps, callee) = self.separate_temporaries(*c.callee, false);
                c.callee = Box::new(callee);

                let mut new_args = Vec::with_capacity(c.args.len());
                for arg in c.args {
                    let (mut arg_temps, new_arg) = self.separate_temporaries(arg, false);
                    callee_temps.append(&mut arg_temps);
                    new_args.push(new_arg);
                }
                c.args = new_args;

                let rebuilt = HirExpr::Call(c);
                if !is_root
                    && self.should_hoist_temporary_expr(&rebuilt)
                    && !matches!(&rebuilt, HirExpr::Call(call) if self.call_returns_unit(call))
                {
                    let (stmt, ident) = self.hoist_expr_into_temp(rebuilt);
                    callee_temps.push(stmt);
                    (callee_temps, ident)
                } else {
                    (callee_temps, rebuilt)
                }
            }
            HirExpr::Unary(mut u) => {
                // Preserve root context for parser-introduced no-op unary wrappers.
                // Otherwise a root call statement like `panic(...)` can be treated as
                // non-root and incorrectly hoisted into `let __tmp: unit = ...`.
                let child_is_root = is_root && u.op.is_none();
                let (temps, inner) = self.separate_temporaries(*u.expr, child_is_root);
                u.expr = Box::new(inner);
                (temps, HirExpr::Unary(u))
            }
            HirExpr::Casting(mut c) => {
                let (temps, inner) = self.separate_temporaries(*c.expr, false);
                c.expr = Box::new(inner);
                (temps, HirExpr::Casting(c))
            }
            HirExpr::Indexing(mut i) => {
                let (mut target_temps, target) = self.separate_temporaries(*i.target, false);
                let (mut index_temps, index) = self.separate_temporaries(*i.index, false);
                i.target = Box::new(target);
                i.index = Box::new(index);
                target_temps.append(&mut index_temps);
                (target_temps, HirExpr::Indexing(i))
            }
            HirExpr::Delete(mut d) => {
                let (temps, inner) = self.separate_temporaries(*d.expr, false);
                d.expr = Box::new(inner);
                (temps, HirExpr::Delete(d))
            }
            HirExpr::FieldAccess(mut f) => {
                let (temps, target) = self.separate_temporaries(*f.target, false);
                f.target = Box::new(target);
                (temps, HirExpr::FieldAccess(f))
            }
            HirExpr::ObjLiteral(mut o) => {
                let mut temps = Vec::new();
                for field in o.fields.iter_mut() {
                    let (mut field_temps, value) =
                        self.separate_temporaries(*field.value.clone(), false);
                    *field.value = value;
                    temps.append(&mut field_temps);
                }
                let rebuilt = HirExpr::ObjLiteral(o);
                if !is_root && self.should_hoist_temporary_expr(&rebuilt) {
                    let (stmt, ident) = self.hoist_expr_into_temp(rebuilt);
                    temps.push(stmt);
                    (temps, ident)
                } else {
                    (temps, rebuilt)
                }
            }
            HirExpr::ListLiteral(mut l) => {
                let mut temps = Vec::new();
                let mut items = Vec::with_capacity(l.items.len());
                for item in l.items {
                    let (mut item_temps, lowered_item) = self.separate_temporaries(item, false);
                    temps.append(&mut item_temps);
                    items.push(lowered_item);
                }
                l.items = items;
                (temps, HirExpr::ListLiteral(l))
            }
            HirExpr::IntrinsicCall(mut i) => {
                let mut temps = Vec::new();
                let mut args = Vec::with_capacity(i.args.len());
                for arg in i.args {
                    let (mut arg_temps, lowered_arg) = self.separate_temporaries(arg, false);
                    temps.append(&mut arg_temps);
                    args.push(lowered_arg);
                }
                i.args = args;
                (temps, HirExpr::IntrinsicCall(i))
            }
            other => (Vec::new(), other),
        }
    }

    fn register_generic_type(
        &mut self,
        generic_type: &'hir HirGenericTy<'hir>,
    ) -> &'hir HirTy<'hir> {
        let mut found_generic_paramater = false;
        for ty in generic_type.inner.iter() {
            if let HirTy::Named(n) = ty {
                if n.name.len() == 1 {
                    found_generic_paramater = true;
                }
            } else if let HirTy::Generic(generic_ty) = ty {
                self.register_generic_type(generic_ty);
            }
        }
        if !found_generic_paramater {
            self.generic_pool
                .register_struct_instance(generic_type.clone(), &self.module_signature);
        }

        self.arena.intern(HirTy::Generic(generic_type.clone()))
    }

    fn visit_expr(&mut self, node: &'ast AstExpr<'ast>) -> HirResult<HirExpr<'hir>> {
        match node {
            AstExpr::Assign(_) => Err(HirError::AssignmentCannotBeAnExpression(
                AssignmentCannotBeAnExpressionError {
                    span: node.span(),
                    src: {
                        let path = node.span().path;
                        let src = crate::atlas_c::utils::get_file_content(path).unwrap();
                        NamedSource::new(path, src)
                    },
                },
            )),
            AstExpr::BinaryOp(b) => {
                let lhs = self.visit_expr(b.lhs)?;
                let rhs = self.visit_expr(b.rhs)?;
                let op = self.visit_bin_op(&b.op)?;
                let hir = HirExpr::HirBinaryOperation(HirBinaryOpExpr {
                    span: node.span(),
                    op,
                    op_span: Span {
                        start: lhs.span().end,
                        end: rhs.span().start,
                        path: b.span.path,
                    },
                    lhs: Box::new(lhs.clone()),
                    rhs: Box::new(rhs.clone()),
                    ty: self.arena.types().get_uninitialized_ty(),
                });
                Ok(hir)
            }
            AstExpr::UnaryOp(u) => {
                let expr = self.visit_expr(u.expr)?;
                let hir = HirExpr::Unary(UnaryOpExpr {
                    span: node.span(),
                    op: match u.op {
                        Some(AstUnaryOp::Neg) => Some(HirUnaryOp::Neg),
                        Some(AstUnaryOp::Not) => Some(HirUnaryOp::Not),
                        Some(AstUnaryOp::AsRef) => Some(HirUnaryOp::AsRef),
                        Some(AstUnaryOp::Deref) => Some(HirUnaryOp::Deref),
                        _ => None,
                    },
                    expr: Box::new(expr.clone()),
                    ty: expr.ty(),
                });
                Ok(hir)
            }
            AstExpr::Casting(c) => {
                let expr = self.visit_expr(c.value)?;
                let ty = self.visit_ty(c.ty)?;
                let hir = HirExpr::Casting(HirCastExpr {
                    span: node.span(),
                    expr: Box::new(expr.clone()),
                    target_ty: ty,
                });
                Ok(hir)
            }
            AstExpr::Call(c) => {
                let callee = self.visit_expr(c.callee)?;
                match &callee {
                    HirExpr::Ident(ident) => {
                        match ident.name {
                            "size_of" => {
                                if c.generics.len() != 1 {
                                    let path = node.span().path;
                                    let src =
                                        crate::atlas_c::utils::get_file_content(path).unwrap();
                                    return Err(HirError::IncorrectIntrinsicCallArguments(
                                        IncorrectIntrinsicCallArgumentsError {
                                            span: node.span(),
                                            name: "size_of".to_string(),
                                            expected: 1,
                                            found: c.generics.len(),
                                            src: NamedSource::new(path, src),
                                        },
                                    ));
                                }
                                let ty = self.visit_ty(c.generics[0])?;
                                let hir = HirExpr::IntrinsicCall(HirIntrinsicCallExpr {
                                    name: "size_of",
                                    args: vec![],
                                    args_ty: vec![ty],
                                    span: node.span(),
                                    ty,
                                });
                                return Ok(hir);
                            }
                            "align_of" => {
                                if c.generics.len() != 1 {
                                    let path = node.span().path;
                                    let src =
                                        crate::atlas_c::utils::get_file_content(path).unwrap();
                                    return Err(HirError::IncorrectIntrinsicCallArguments(
                                        IncorrectIntrinsicCallArgumentsError {
                                            span: node.span(),
                                            name: "align_of".to_string(),
                                            expected: 1,
                                            found: c.generics.len(),
                                            src: NamedSource::new(path, src),
                                        },
                                    ));
                                }
                                let ty = self.visit_ty(c.generics[0])?;
                                let hir = HirExpr::IntrinsicCall(HirIntrinsicCallExpr {
                                    name: "align_of",
                                    args: vec![],
                                    args_ty: vec![ty],
                                    span: node.span(),
                                    ty,
                                });
                                return Ok(hir);
                            }
                            "std::ptr::write" => {
                                // std::ptr::write<T>(*T, T);
                                if c.args.len() != 2 {
                                    let path = node.span().path;
                                    let src =
                                        crate::atlas_c::utils::get_file_content(path).unwrap();
                                    return Err(HirError::IncorrectIntrinsicCallArguments(
                                        IncorrectIntrinsicCallArgumentsError {
                                            span: node.span(),
                                            name: "std::ptr::write".to_string(),
                                            expected: 2,
                                            found: c.args.len(),
                                            src: NamedSource::new(path, src),
                                        },
                                    ));
                                }
                                let ptr_expr = self.visit_expr(c.args[0])?;
                                let val_expr = self.visit_expr(c.args[1])?;
                                let hir = HirExpr::IntrinsicCall(HirIntrinsicCallExpr {
                                    name: "std::ptr::write",
                                    ty: self.arena.types().get_unit_ty(),
                                    args: vec![ptr_expr, val_expr],
                                    // Don't prefill `args_ty` with `uninitialized` here —
                                    // leave it empty so the type checker infers parameter
                                    // types from the actual argument expressions.
                                    args_ty: vec![],
                                    span: node.span(),
                                });
                                return Ok(hir);
                            }
                            "std::ptr::read" => {
                                // std::ptr::read<T>(*T) -> T;
                                if c.args.len() != 1 {
                                    let path = node.span().path;
                                    let src =
                                        crate::atlas_c::utils::get_file_content(path).unwrap();
                                    return Err(HirError::IncorrectIntrinsicCallArguments(
                                        IncorrectIntrinsicCallArgumentsError {
                                            span: node.span(),
                                            name: "std::ptr::read".to_string(),
                                            expected: 1,
                                            found: c.args.len(),
                                            src: NamedSource::new(path, src),
                                        },
                                    ));
                                }
                                let ptr_expr = self.visit_expr(c.args[0])?;
                                let hir = HirExpr::IntrinsicCall(HirIntrinsicCallExpr {
                                    name: "std::ptr::read",
                                    ty: self.arena.types().get_uninitialized_ty(),
                                    args: vec![ptr_expr],
                                    // Don't prefill `args_ty` with `uninitialized` here —
                                    // leave it empty so the type checker infers parameter
                                    // types from the actual argument expressions.
                                    args_ty: vec![],
                                    span: node.span(),
                                });
                                return Ok(hir);
                            }
                            "std::move" => {
                                // std::move<T>(T) -> T
                                if c.args.len() != 1 {
                                    let path = node.span().path;
                                    let src =
                                        crate::atlas_c::utils::get_file_content(path).unwrap();
                                    return Err(HirError::IncorrectIntrinsicCallArguments(
                                        IncorrectIntrinsicCallArgumentsError {
                                            span: node.span(),
                                            name: "std::move".to_string(),
                                            expected: 1,
                                            found: c.args.len(),
                                            src: NamedSource::new(path, src),
                                        },
                                    ));
                                }
                                //let ty = self.visit_ty(c.generics[0])?;
                                let src_expr = self.visit_expr(c.args[0])?;
                                let hir = HirExpr::IntrinsicCall(HirIntrinsicCallExpr {
                                    name: "std::move",
                                    ty: src_expr.ty(),
                                    args: vec![src_expr],
                                    // Don't prefill `args_ty` with `uninitialized` here —
                                    // leave it empty so the type checker infers parameter
                                    // types from the actual argument expressions.
                                    args_ty: vec![],
                                    span: node.span(),
                                });
                                return Ok(hir);
                            }
                            _ => {}
                        }
                    }
                    _ => {}
                }
                let args = c
                    .args
                    .iter()
                    .map(|arg| self.visit_expr(arg))
                    .collect::<HirResult<Vec<_>>>()?;
                let mut generics = vec![];
                for generic in c.generics.iter() {
                    let generic_ty = match self.visit_ty(generic)? {
                        HirTy::Generic(ty) => self.register_generic_type(ty),
                        other => other,
                    };
                    generics.push(generic_ty);
                }
                let hir = HirExpr::Call(HirFunctionCallExpr {
                    span: node.span(),
                    callee: Box::new(callee.clone()),
                    callee_span: callee.span(),
                    args,
                    generics,
                    is_reference: c.is_reference,
                    args_ty: Vec::new(),
                    ty: self.arena.types().get_uninitialized_ty(),
                    kind: HirFunctionKind::Function,
                });
                Ok(hir)
            }
            AstExpr::Identifier(i) => {
                let hir = HirExpr::Ident(HirIdentExpr {
                    name: self.arena.names().get(i.name),
                    span: i.span,
                    ty: self.arena.types().get_uninitialized_ty(),
                });
                Ok(hir)
            }
            AstExpr::ObjLiteral(obj) => {
                // Let's get the actual type now:
                let mut ty = match obj.target {
                    AstExpr::Identifier(i) => {
                        let name = self.arena.names().get(i.name);
                        self.arena
                            .intern(HirTy::Named(HirNamedTy { span: i.span, name }))
                    }
                    AstExpr::StaticAccess(s) => self.visit_ty(s.target)?,
                    _ => {
                        let path = node.span().path;
                        let src = crate::atlas_c::utils::get_file_content(path).unwrap();
                        return Err(HirError::UnsupportedExpr(UnsupportedExpr {
                            span: node.span(),
                            expr: node.kind().to_string(),
                            src: NamedSource::new(path, src),
                        }));
                    }
                };
                if !obj.generics.is_empty() {
                    let mut generic_types = vec![];
                    for generic in obj.generics.iter() {
                        let generic_ty = match self.visit_ty(generic)? {
                            HirTy::Generic(ty) => self.register_generic_type(ty),
                            other => other,
                        };
                        generic_types.push(generic_ty.clone());
                    }
                    ty = self.register_generic_type(self.arena.intern(HirGenericTy {
                        span: node.span(),
                        inner: generic_types,
                        name: match &ty {
                            HirTy::Named(n) => n.name,
                            _ => {
                                let path = node.span().path;
                                let src = crate::atlas_c::utils::get_file_content(path).unwrap();
                                return Err(HirError::UnsupportedExpr(UnsupportedExpr {
                                    span: node.span(),
                                    expr: node.kind().to_string(),
                                    src: NamedSource::new(path, src),
                                }));
                            }
                        },
                    }));
                }
                let hir = HirExpr::ObjLiteral(HirObjLiteralExpr {
                    span: node.span(),
                    ty,
                    fields: obj
                        .fields
                        .iter()
                        .map(|field| {
                            Ok(HirFieldInit {
                                span: field.span,
                                name: self.arena.names().get(field.name.name),
                                name_span: field.name.span,
                                ty: self.arena.types().get_uninitialized_ty(),
                                value: Box::new(self.visit_expr(field.value)?),
                            })
                        })
                        .collect::<HirResult<Vec<_>>>()?,
                });
                Ok(hir)
            }
            AstExpr::Indexing(c) => {
                let target = self.visit_expr(c.target)?;
                let index = self.visit_expr(c.index)?;
                let hir = HirExpr::Indexing(HirIndexingExpr {
                    span: node.span(),
                    target: Box::new(target.clone()),
                    index: Box::new(index.clone()),
                    ty: self.arena.types().get_uninitialized_ty(),
                });
                Ok(hir)
            }
            AstExpr::Delete(d) => {
                let hir = HirExpr::Delete(HirDeleteExpr {
                    span: node.span(),
                    expr: Box::new(self.visit_expr(d.target)?),
                });
                Ok(hir)
            }
            AstExpr::Literal(l) => {
                let hir = match l {
                    AstLiteral::Integer(i) => HirExpr::IntegerLiteral(HirIntegerLiteralExpr {
                        span: l.span(),
                        value: i.value,
                        ty: self.arena.types().get_literal_int_ty(i.value, l.span()),
                    }),
                    AstLiteral::Boolean(b) => HirExpr::BooleanLiteral(HirBooleanLiteralExpr {
                        span: l.span(),
                        value: b.value,
                        ty: self.arena.types().get_boolean_ty(),
                    }),
                    AstLiteral::Float(f) => HirExpr::FloatLiteral(HirFloatLiteralExpr {
                        span: l.span(),
                        value: f.value,
                        ty: self.arena.types().get_literal_float_ty(f.value, l.span()),
                    }),
                    AstLiteral::UnsignedInteger(u) => {
                        HirExpr::UnsignedIntegerLiteral(HirUnsignedIntegerLiteralExpr {
                            span: l.span(),
                            value: u.value,
                            ty: self.arena.types().get_literal_uint_ty(u.value, l.span()),
                        })
                    }
                    AstLiteral::ThisLiteral(_) => HirExpr::ThisLiteral(HirThisLiteral {
                        span: l.span(),
                        ty: self.arena.types().get_uninitialized_ty(),
                    }),
                    AstLiteral::NullLiteral(n) => HirExpr::NullLiteral(HirNullLiteralExpr {
                        span: n.span,
                        ty: self.arena.types().get_uninitialized_ty(),
                    }),
                    AstLiteral::Char(ast_char) => HirExpr::CharLiteral(HirCharLiteralExpr {
                        span: l.span(),
                        value: ast_char.value,
                        ty: self.arena.types().get_char_ty(),
                    }),
                    AstLiteral::Unit(_) => HirExpr::UnitLiteral(HirUnitLiteralExpr {
                        span: l.span(),
                        ty: self.arena.types().get_unit_ty(),
                    }),
                    AstLiteral::String(ast_string) => {
                        HirExpr::StringLiteral(HirStringLiteralExpr {
                            span: l.span(),
                            value: self.arena.intern(ast_string.value.to_owned()),
                            ty: self.arena.types().get_ptr_ty(
                                self.arena.types().get_uint_ty(8),
                                false,
                                l.span(),
                            ),
                        })
                    }
                    AstLiteral::List(l) => {
                        let elements = l
                            .items
                            .iter()
                            .map(|e| self.visit_expr(e))
                            .collect::<HirResult<Vec<_>>>()?;
                        HirExpr::ListLiteral(HirListLiteralExpr {
                            span: l.span,
                            items: elements,
                            ty: self.arena.types().get_uninitialized_ty(),
                        })
                    }
                    AstLiteral::ListWithSize(l) => {
                        let item = self.visit_expr(l.item)?;
                        let size = self.visit_expr(l.size)?;
                        HirExpr::ListLiteralWithSize(HirListLiteralWithSizeExpr {
                            span: l.span,
                            item: Box::new(item),
                            size: Box::new(size),
                            ty: self.arena.types().get_uninitialized_ty(),
                        })
                    }
                };
                Ok(hir)
            }
            AstExpr::StaticAccess(ast_static_access) => {
                let hir = HirExpr::StaticAccess(HirStaticAccessExpr {
                    span: node.span(),
                    target: self.visit_ty(ast_static_access.target)?,
                    field: Box::new(HirIdentExpr {
                        name: self.arena.names().get(ast_static_access.field.name),
                        span: ast_static_access.field.span,
                        ty: self.arena.types().get_uninitialized_ty(),
                    }),
                    ty: self.arena.types().get_uninitialized_ty(),
                });
                Ok(hir)
            }
            AstExpr::FieldAccess(ast_field_access) => {
                let hir = HirExpr::FieldAccess(HirFieldAccessExpr {
                    span: node.span(),
                    target: Box::new(self.visit_expr(ast_field_access.target)?),
                    field: Box::new(HirIdentExpr {
                        name: self.arena.names().get(ast_field_access.field.name),
                        span: ast_field_access.field.span,
                        ty: self.arena.types().get_uninitialized_ty(),
                    }),
                    ty: self.arena.types().get_uninitialized_ty(),
                    is_arrow: ast_field_access.is_arrow,
                });
                Ok(hir)
            }
            _ => {
                //todo: if/else as an expression
                let path = node.span().path;
                let src = crate::atlas_c::utils::get_file_content(path).unwrap();
                Err(HirError::UnsupportedExpr(UnsupportedExpr {
                    span: node.span(),
                    expr: node.kind().to_string(),
                    src: NamedSource::new(path, src),
                }))
            }
        }
    }

    fn _visit_identifier(&self, node: &'ast AstIdentifier<'ast>) -> HirResult<HirIdentExpr<'hir>> {
        Ok(HirIdentExpr {
            name: self.arena.names().get(node.name),
            span: node.span,
            ty: self.arena.types().get_uninitialized_ty(),
        })
    }

    fn visit_bin_op(&self, bin_op: &'ast AstBinaryOp) -> HirResult<HirBinaryOperator> {
        let op = match bin_op {
            AstBinaryOp::Add => HirBinaryOperator::Add,
            AstBinaryOp::Sub => HirBinaryOperator::Sub,
            AstBinaryOp::Mul => HirBinaryOperator::Mul,
            AstBinaryOp::Div => HirBinaryOperator::Div,
            AstBinaryOp::Mod => HirBinaryOperator::Mod,
            AstBinaryOp::Eq => HirBinaryOperator::Eq,
            AstBinaryOp::NEq => HirBinaryOperator::Neq,
            AstBinaryOp::Lt => HirBinaryOperator::Lt,
            AstBinaryOp::Lte => HirBinaryOperator::Lte,
            AstBinaryOp::Gt => HirBinaryOperator::Gt,
            AstBinaryOp::Gte => HirBinaryOperator::Gte,
            AstBinaryOp::And => HirBinaryOperator::And,
            AstBinaryOp::Or => HirBinaryOperator::Or,
            //Other operators will soon come
        };
        Ok(op)
    }

    fn visit_func(&mut self, node: &'ast AstFunction<'ast>) -> HirResult<HirFunction<'hir>> {
        let type_parameters = node
            .args
            .iter()
            .map(|arg| self.visit_type_param_item(arg))
            .collect::<HirResult<Vec<_>>>();
        let ret_type_span = node.ret.span();
        let ret_type = self.visit_ty(node.ret)?.clone();
        let parameters = node
            .args
            .iter()
            .map(|arg| self.visit_func_param(arg))
            .collect::<HirResult<Vec<_>>>();

        let body = self.visit_block(node.body)?;

        let mut generics: Vec<&HirGenericConstraint<'_>> = Vec::new();
        if !node.generics.is_empty() {
            for generic in node.generics.iter() {
                generics.push(self.arena.intern(HirGenericConstraint {
                    span: generic.span,
                    generic_name: self.arena.names().get(generic.name.name),
                    kind: {
                        let mut constraints: Vec<&HirGenericConstraintKind<'_>> = vec![];
                        for constraint in generic.constraints.iter() {
                            constraints.push(self.arena.intern(self.visit_constraint(constraint)?));
                        }
                        constraints
                    },
                }));
            }
        }

        let signature = self.arena.intern(HirFunctionSignature {
            span: node.span,
            vis: node.vis.into(),
            params: parameters?,
            is_instantiated: generics.is_empty(),
            generics,
            type_params: type_parameters?,
            return_ty: ret_type,
            return_ty_span: Some(ret_type_span),
            is_external: false,
            is_intrinsic: false,
            pre_mangled_ty: None,
            docstring: if let Some(docstring) = node.docstring {
                Some(self.arena.names().get(docstring))
            } else {
                None
            },
            c_name: None,
        });
        let fun = HirFunction {
            span: node.span,
            name: {
                let qualified = self.qualified_name(node.name.name);
                self.arena.names().get(&qualified)
            },
            name_span: node.name.span,
            signature,
            body,
            pre_mangled_ty: None,
        };
        Ok(fun)
    }

    fn visit_func_param(
        &mut self,
        node: &'ast AstArg<'ast>,
    ) -> HirResult<HirFunctionParameterSignature<'hir>> {
        let name = self.arena.names().get(node.name.name);
        let ty = self.visit_ty(node.ty)?;

        let hir = HirFunctionParameterSignature {
            span: node.span,
            name,
            name_span: node.name.span,
            ty,
            ty_span: node.ty.span(),
        };
        Ok(hir)
    }

    fn visit_type_param_item(
        &self,
        node: &'ast AstArg<'ast>,
    ) -> HirResult<&'hir HirTypeParameterItemSignature<'hir>> {
        let name = self.arena.names().get(node.name.name);

        let hir = self.arena.intern(HirTypeParameterItemSignature {
            span: node.span,
            name,
            name_span: node.name.span,
        });
        Ok(hir)
    }

    fn visit_ty(&mut self, node: &'ast AstType<'ast>) -> HirResult<&'hir HirTy<'hir>> {
        let ty = match node {
            AstType::Boolean(_) => self.arena.types().get_boolean_ty(),
            AstType::Integer(i) => self.arena.types().get_int_ty(i.size_in_bits),
            AstType::Float(f) => self.arena.types().get_float_ty(f.size_in_bits),
            AstType::Char(_) => self.arena.types().get_char_ty(),
            AstType::UnsignedInteger(u) => self.arena.types().get_uint_ty(u.size_in_bits),
            AstType::Unit(_) => self.arena.types().get_unit_ty(),
            AstType::String(_) => self.arena.types().get_str_ty(),
            AstType::Named(n) => {
                let qualified = self.qualified_type_name(n.name.name);
                let name = self.arena.names().get(&qualified);
                self.arena.types().get_named_ty(name, n.span)
            }
            AstType::Slice(l) => {
                let ty = self.visit_ty(l.inner)?;
                self.arena.types().get_slice_ty(ty)
            }
            AstType::InlineArray(arr) => {
                let ty = self.visit_ty(arr.inner)?;
                self.arena.types().get_inline_arr_ty(ty, arr.size)
            }
            AstType::Nullable(n) => {
                if !self.using_std {
                    let path = node.span().path;
                    let src = crate::atlas_c::utils::get_file_content(path).unwrap();
                    return Err(HirError::NullableTypeRequiresStdLibrary(
                        NullableTypeRequiresStdLibraryError {
                            span: node.span(),
                            src: NamedSource::new(path, src),
                        },
                    ));
                }
                //They should not be unstable, but who knows
                Self::nullable_types_are_unstable_warning(&node.span());
                let ty = self.visit_ty(n.inner)?;
                self.arena
                    .types()
                    .get_generic_ty("Option", vec![ty], n.span)
            }
            AstType::Generic(g) => {
                let inner_types = g
                    .inner_types
                    .iter()
                    .map(|inner_ast_ty| self.visit_ty(inner_ast_ty))
                    .collect::<HirResult<Vec<_>>>()?;
                let qualified = self.qualified_type_name(g.name.name);
                let name = self.arena.names().get(&qualified);
                let ty = self
                    .arena
                    .types()
                    .get_generic_ty(name, inner_types.clone(), g.span);
                if let HirTy::Generic(g) = ty {
                    self.register_generic_type(g);
                }
                ty
            }
            //The "this" ty is replaced during the type checking phase
            AstType::ThisTy(_) => self.arena.types().get_uninitialized_ty(),
            AstType::PtrTy(ptr_ty) => {
                let inner_ty = self.visit_ty(ptr_ty.inner)?;
                self.arena
                    .types()
                    .get_ptr_ty(inner_ty, ptr_ty.is_const, ptr_ty.span)
            }
            AstType::Function(func_ty) => {
                let span = func_ty.span;
                let param_spans = func_ty.args.iter().map(|arg| arg.span()).collect();
                let parameters = func_ty
                    .args
                    .iter()
                    .map(|arg| self.visit_ty(arg))
                    .collect::<HirResult<Vec<_>>>()?;
                let return_ty = self.visit_ty(func_ty.ret)?;
                self.arena.types().get_function_ty_with_spans(
                    parameters,
                    param_spans,
                    return_ty,
                    func_ty.ret.span(),
                    span,
                )
            }
            AstType::Const(_) => {
                let path = node.span().path;
                let src = utils::get_file_content(path)
                    .unwrap_or_else(|_| panic!("Failed to open file {path}"));
                return Err(HirError::UnknownType(UnknownTypeError {
                    name: node.name(),
                    span: node.span(),
                    src: NamedSource::new(path, src),
                }));
            }
        };
        Ok(ty)
    }

    fn nullable_types_are_unstable_warning(span: &Span) {
        let path = span.path;
        let src = crate::atlas_c::utils::get_file_content(path).unwrap();
        let report: ErrReport =
            HirWarning::ThisTypeIsStillUnstable(ThisTypeIsStillUnstableWarning {
                src: NamedSource::new(path, src),
                span: *span,
                type_name: "The nullable type".to_string(),
                info: "Nullable types haven't been properly stabilized yet. Also they are just syntactic sugar for `optional<T>`".to_string(),
            })
            .into();
        eprintln!("{:?}", report);
    }

    fn name_should_be_in_different_case_warning(
        span: &Span,
        case_kind: &str,
        item_kind: &str,
        name: &str,
        expected_name: &str,
    ) {
        let path = span.path;
        //The standard library can do whatever it wants
        if !path.starts_with("std") {
            let src = crate::atlas_c::utils::get_file_content(path).unwrap();
            let report: ErrReport =
                HirWarning::NameShouldBeInDifferentCase(NameShouldBeInDifferentCaseWarning {
                    src: NamedSource::new(path, src),
                    span: *span,
                    case_kind: case_kind.to_string(),
                    item_kind: item_kind.to_string(),
                    name: name.to_string(),
                    expected_name: expected_name.to_string(),
                })
                .into();
            eprintln!("{:?}", report);
        }
    }

    fn name_single_character_error(span: &Span) -> HirError {
        let path = span.path;
        let src = crate::atlas_c::utils::get_file_content(path).unwrap();
        HirError::StructNameCannotBeOneLetter(StructNameCannotBeOneLetterError {
            src: NamedSource::new(path, src),
            span: *span,
        })
    }
}
