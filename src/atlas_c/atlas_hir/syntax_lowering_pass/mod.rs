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
                AstBinaryOp, AstBlock, AstConstructor, AstDestructor, AstEnum, AstExpr,
                AstExternFunction, AstFunction, AstGeneric, AstGenericConstraint, AstIdentifier,
                AstImport, AstItem, AstLiteral, AstMethod, AstMethodModifier, AstObjField,
                AstProgram, AstStatement, AstStruct, AstType, AstUnaryOp, AstUnion,
            },
        },
    },
    atlas_hir::{
        HirImport, HirModule, HirModuleBody,
        arena::HirArena,
        error::{
            ConstructorCannotHaveAWhereClauseError, HirError, HirResult, NonConstantValueError,
            NullableTypeRequiresStdLibraryError, StructNameCannotBeOneLetterError,
            TooManyReferenceLevelsError, UnsupportedExpr, UnsupportedItemError,
            UnsupportedStatement, UselessError,
        },
        expr::{
            HirAssignExpr, HirBinaryOpExpr, HirBinaryOperator, HirBooleanLiteralExpr, HirCastExpr,
            HirCharLiteralExpr, HirDeleteExpr, HirExpr, HirFieldAccessExpr, HirFieldInit,
            HirFloatLiteralExpr, HirFunctionCallExpr, HirIdentExpr, HirIndexingExpr,
            HirIntegerLiteralExpr, HirListLiteralExpr, HirNewArrayExpr, HirNewObjExpr,
            HirObjLiteralExpr, HirStaticAccessExpr, HirStringLiteralExpr, HirThisLiteral,
            HirUnaryOp, HirUnitLiteralExpr, HirUnsignedIntegerLiteralExpr, UnaryOpExpr,
        },
        item::{
            HirEnum, HirEnumVariant, HirFunction, HirStruct, HirStructConstructor, HirStructMethod,
            HirUnion,
        },
        monomorphization_pass::{MonomorphizationPass, generic_pool::HirGenericPool},
        signature::{
            ConstantValue, HirFunctionParameterSignature, HirFunctionSignature,
            HirGenericConstraint, HirGenericConstraintKind, HirModuleSignature,
            HirStructConstantSignature, HirStructConstructorSignature, HirStructFieldSignature,
            HirStructMethodModifier, HirStructMethodSignature, HirStructSignature,
            HirTypeParameterItemSignature, HirUnionSignature, HirVisibility,
        },
        stmt::{
            HirBlock, HirExprStmt, HirIfElseStmt, HirReturn, HirStatement, HirVariableStmt,
            HirWhileStmt,
        },
        syntax_lowering_pass::case::Case,
        ty::{HirGenericTy, HirNamedTy, HirTy},
        warning::{
            CannotGenerateACopyConstructorForThisTypeWarning, HirWarning,
            NameShouldBeInDifferentCaseWarning, ThisTypeIsStillUnstableWarning,
            UnionFieldCannotBeAutomaticallyDeletedWarning,
        },
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
        }
    }
}

impl<'ast, 'hir> AstSyntaxLoweringPass<'ast, 'hir> {
    pub fn lower(&mut self) -> HirResult<&'hir mut HirModule<'hir>> {
        for item in self.ast.items {
            self.visit_item(item)?;
        }

        for _ in 0..(self.warnings.len()) {
            let warning: HirWarning = self.warnings.remove(0);
            let report: ErrReport = warning.into();
            eprintln!("{:?}", report);
        }

        // Now we can generate all the copy constructors for the every structs
        // Collect struct names and info first to avoid borrow checker conflicts
        self.generate_all_copy_constructors()?;

        Ok(self.arena.intern(HirModule {
            body: self.module_body.clone(),
            signature: self.module_signature.clone(),
        }))
    }
    pub fn visit_item(&mut self, ast_item: &'ast AstItem<'ast>) -> HirResult<()> {
        match ast_item {
            AstItem::Constant(_) => {
                let path = ast_item.span().path;
                let src = utils::get_file_content(path).unwrap();
                return Err(HirError::UnsupportedItem(UnsupportedItemError {
                    span: ast_item.span(),
                    item: "Global constants".to_string(),
                    src: NamedSource::new(path, src),
                }));
            }
            AstItem::Function(ast_function) => {
                let hir_func = self.visit_func(ast_function)?;
                let name = self.arena.names().get(ast_function.name.name);
                if !name.is_snake_case() {
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
                self.module_body
                    .enums
                    .insert(self.arena.names().get(e.name.name), hir_enum.clone());
                self.module_signature.enums.insert(
                    self.arena.names().get(e.name.name),
                    self.arena.intern(hir_enum),
                );
            }
            AstItem::Union(ast_union) => {
                let hir_union = self.visit_union(ast_union)?;
                self.module_body.unions.insert(
                    self.arena.names().get(ast_union.name.name),
                    hir_union.clone(),
                );
                self.module_signature.unions.insert(
                    self.arena.names().get(ast_union.name.name),
                    self.arena.intern(hir_union.signature.clone()),
                );
            }
        }
        Ok(())
    }

    fn visit_union(&mut self, ast_union: &'ast AstUnion<'ast>) -> HirResult<HirUnion<'hir>> {
        let name = self.arena.names().get(ast_union.name.name);
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
            generics,
            name,
            variants: {
                let mut map = BTreeMap::new();
                for variant in variants.iter() {
                    map.insert(variant.name, variant.clone());
                }
                map
            },
        };
        let hir = HirUnion {
            span: ast_union.span,
            name,
            name_span: ast_union.name.span,
            vis: ast_union.vis.into(),
            variants,
            signature,
        };
        Ok(hir)
    }

    fn visit_enum(&mut self, ast_enum: &'ast AstEnum<'ast>) -> HirResult<HirEnum<'hir>> {
        let name = self.arena.names().get(ast_enum.name.name);
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
        };
        Ok(hir)
    }

    fn visit_extern_func(&mut self, ast_extern_func: &AstExternFunction<'ast>) -> HirResult<()> {
        let name = self.arena.names().get(ast_extern_func.name.name);
        if !name.is_snake_case() {
            Self::name_should_be_in_different_case_warning(
                &ast_extern_func.name.span,
                "snake_case",
                "extern function",
                name,
                &name.to_snake_case(),
            );
        }
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
            generics,
            type_params,
            return_ty: ty,
            return_ty_span: Some(ast_extern_func.ret_ty.span()),
            is_external: true,
        });
        self.module_signature.functions.insert(name, hir);
        Ok(())
    }

    fn visit_struct(&mut self, node: &'ast AstStruct<'ast>) -> HirResult<HirStruct<'hir>> {
        let name = self.arena.names().get(node.name.name);
        if !name.is_pascal_case() {
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
                }),
            );
        }

        let constructor =
            self.visit_constructor(node.name_span, node.constructor, &fields, false)?;
        let had_user_defined_constructor = node.constructor.is_some();
        let had_user_defined_destructor = node.destructor.is_some();
        let destructor = self.visit_destructor(name, node.name_span, node.destructor, &fields)?;
        let copy_constructor = if node.copy_constructor.is_some() {
            Some(self.visit_constructor(node.name_span, node.copy_constructor, &fields, true)?)
        } else {
            None
        };

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
            generics,
            constructor: constructor.signature.clone(),
            copy_constructor: copy_constructor.as_ref().map(|c| c.signature.clone()),
            destructor: destructor.signature.clone(),
            had_user_defined_constructor,
            had_user_defined_destructor,
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
            constructor,
            copy_constructor,
            destructor,
            had_user_defined_destructor,
            had_user_defined_constructor,
            vis: node.vis.into(),
            flag: node.flag.into(),
        })
    }

    fn make_default_constructor(
        &mut self,
        name_span: Span,
        fields: &[HirStructFieldSignature<'hir>],
    ) -> HirStructConstructor<'hir> {
        let mut params: Vec<HirFunctionParameterSignature<'hir>> = Vec::new();
        for field in fields.iter() {
            let ty = field.ty;
            let name = field.name;
            params.push(HirFunctionParameterSignature {
                span: field.span,
                name,
                name_span: field.name_span,
                ty,
                ty_span: field.ty_span,
            });
        }
        let mut type_params: Vec<HirTypeParameterItemSignature<'hir>> = Vec::new();
        for type_param in params.iter() {
            type_params.push(HirTypeParameterItemSignature {
                span: type_param.span,
                name: type_param.name,
                name_span: type_param.name_span,
            });
        }

        let constructor_signature = HirStructConstructorSignature {
            span: name_span,
            params: params.clone(),
            type_params: type_params.clone(),
            vis: HirVisibility::Public,
            where_clause: None,
            is_constraint_satisfied: true,
        };

        let mut statements = vec![];
        for field in fields.iter() {
            let init_expr = HirExpr::Assign(HirAssignExpr {
                span: field.span,
                lhs: Box::new(HirExpr::FieldAccess(HirFieldAccessExpr {
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
                })),
                rhs: Box::new(HirExpr::Ident(HirIdentExpr {
                    span: field.span,
                    name: field.name,
                    ty: field.ty,
                })),
                ty: field.ty,
            });
            statements.push(HirStatement::Expr(HirExprStmt {
                span: field.span,
                expr: init_expr,
            }));
        }

        HirStructConstructor {
            span: name_span,
            signature: self.arena.intern(constructor_signature),
            params,
            type_params,
            body: HirBlock {
                span: name_span,
                statements,
            },
            //Constructor is public by default
            vis: HirVisibility::Public,
        }
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

    fn visit_constructor(
        &mut self,
        name_span: Span,
        constructor: Option<&'ast AstConstructor<'ast>>,
        fields: &[HirStructFieldSignature<'hir>],
        is_copy_constructor: bool,
    ) -> HirResult<HirStructConstructor<'hir>> {
        if constructor.is_none() {
            let hir = self.make_default_constructor(name_span, fields);
            return Ok(hir);
        }
        let constructor = constructor.unwrap();
        if !is_copy_constructor && constructor.where_clause.is_some() {
            let path = constructor.span.path;
            let src = utils::get_file_content(path).unwrap();
            return Err(HirError::ConstructorCannotHaveAWhereClause(
                ConstructorCannotHaveAWhereClauseError {
                    span: constructor.span,
                    src: NamedSource::new(path, src),
                },
            ));
        }
        let mut params: Vec<HirFunctionParameterSignature<'hir>> = Vec::new();
        for param in constructor.args.iter() {
            let ty = self.visit_ty(param.ty)?;
            let name = self.arena.names().get(param.name.name);
            params.push(HirFunctionParameterSignature {
                span: param.span,
                name,
                name_span: param.name.span,
                ty,
                ty_span: param.ty.span(),
            });
        }

        let mut type_params: Vec<HirTypeParameterItemSignature<'hir>> = Vec::new();
        for type_param in params.iter() {
            type_params.push(HirTypeParameterItemSignature {
                span: type_param.span,
                name: type_param.name,
                name_span: type_param.name_span,
            });
        }

        let (_, where_clause) = self.merge_generic_constraints(None, constructor.where_clause);

        let constructor_signature = HirStructConstructorSignature {
            span: constructor.span,
            params: params.clone(),
            type_params: type_params.clone(),
            vis: constructor.vis.into(),
            where_clause,
            // Sets to true by default; monomorphization pass will update if needed
            is_constraint_satisfied: true,
        };

        let hir = HirStructConstructor {
            span: constructor.span,
            signature: self.arena.intern(constructor_signature),
            params,
            type_params,
            body: self.visit_block(constructor.body)?,
            vis: constructor.vis.into(),
        };
        Ok(hir)
    }

    fn visit_destructor(
        &mut self,
        struct_name: &str,
        name_span: Span,
        destructor: Option<&'ast AstDestructor<'ast>>,
        fields: &[HirStructFieldSignature<'hir>],
    ) -> HirResult<HirStructConstructor<'hir>> {
        if destructor.is_none() {
            let signature = HirStructConstructorSignature {
                span: name_span,
                params: Vec::new(),
                type_params: Vec::new(),
                vis: HirVisibility::Public,
                where_clause: None,
                is_constraint_satisfied: true,
            };
            let mut statements = vec![];
            for field in fields.iter() {
                if field.ty.is_primitive() {
                    // No need to delete primitive types
                    continue;
                }
                // TODO: Handle unions properly
                // It's very messy to use the AST for the check here, but for now it works
                if let Some(name) = self.get_union_name(field.ty)
                    && self.ast.items.iter().any(|item| {
                        if let AstItem::Union(ast_union) = item {
                            let union_name = self.arena.names().get(ast_union.name.name);
                            return union_name == name;
                        }
                        false
                    })
                {
                    // Deleting union causes Undefined Behavior, so we skip it
                    let path = field.span.path;
                    let src = utils::get_file_content(path).unwrap();
                    let warning: ErrReport = HirWarning::UnionFieldCannotBeAutomaticallyDeleted(
                        UnionFieldCannotBeAutomaticallyDeletedWarning {
                            span: field.span,
                            field_name: field.name.to_string(),
                            struct_name: struct_name.to_string(),
                            src: NamedSource::new(path, src),
                        },
                    )
                    .into();
                    eprintln!("{:?}", warning);
                    continue;
                }
                if self.ast.items.iter().any(|item| {
                    if let AstItem::Enum(ast_enum) = item {
                        let enum_name = self.arena.names().get(ast_enum.name.name);
                        return enum_name == field.ty.to_string();
                    }
                    false
                }) {
                    // No need to delete enums
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
                    })),
                });
                statements.push(HirStatement::Expr(HirExprStmt {
                    span: field.span,
                    expr: delete_expr,
                }));
            }
            let hir = HirStructConstructor {
                span: name_span,
                signature: self.arena.intern(signature),
                params: Vec::new(),
                type_params: Vec::new(),
                body: HirBlock {
                    span: name_span,
                    statements,
                },
                //Destructor is public by default
                vis: HirVisibility::Public,
            };
            return Ok(hir);
        }
        let destructor = destructor.unwrap();
        let mut params: Vec<HirFunctionParameterSignature<'hir>> = Vec::new();
        for param in destructor.args.iter() {
            let ty = self.visit_ty(param.ty)?;
            let name = self.arena.names().get(param.name.name);
            params.push(HirFunctionParameterSignature {
                span: param.span,
                name,
                name_span: param.name.span,
                ty,
                ty_span: param.ty.span(),
            });
        }

        let mut type_params: Vec<HirTypeParameterItemSignature<'hir>> = Vec::new();
        for type_param in params.iter() {
            type_params.push(HirTypeParameterItemSignature {
                span: type_param.span,
                name: type_param.name,
                name_span: type_param.name_span,
            });
        }
        let signature = HirStructConstructorSignature {
            span: destructor.span,
            params: params.clone(),
            type_params: type_params.clone(),
            vis: destructor.vis.into(),
            where_clause: None,
            is_constraint_satisfied: true,
        };
        let hir = HirStructConstructor {
            span: destructor.span,
            signature: self.arena.intern(signature),
            params,
            type_params,
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
            let src = crate::atlas_c::utils::get_file_content(node.path).unwrap();
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
            Err(HirError::UselessError(UselessError {}))
        }
    }

    fn visit_block(&mut self, node: &'ast AstBlock<'ast>) -> HirResult<HirBlock<'hir>> {
        let statements = node
            .stmts
            .iter()
            .map(|stmt| self.visit_stmt(stmt))
            .collect::<HirResult<Vec<_>>>()?;
        Ok(HirBlock {
            statements,
            span: node.span,
        })
    }

    fn visit_stmt(&mut self, node: &'ast AstStatement<'ast>) -> HirResult<HirStatement<'hir>> {
        match node {
            AstStatement::While(ast_while) => {
                let condition = self.visit_expr(ast_while.condition)?;
                let body = self.visit_block(ast_while.body)?;
                let hir = HirStatement::While(HirWhileStmt {
                    span: node.span(),
                    condition,
                    body,
                });
                Ok(hir)
            }
            AstStatement::Block(ast_block) => {
                let block = self.visit_block(ast_block)?;
                let hir = HirStatement::Block(block);
                Ok(hir)
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
                let ty = self.visit_ty(ast_const.ty)?;

                let value = self.visit_expr(ast_const.value)?;
                let hir = HirStatement::Const(HirVariableStmt {
                    span: node.span(),
                    name,
                    name_span: ast_const.name.span,
                    ty,
                    ty_span: Some(ast_const.ty.span()),
                    value,
                });
                Ok(hir)
            }
            AstStatement::Let(ast_let) => {
                let name = self.arena.names().get(ast_let.name.name);
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
                let hir = HirStatement::Let(HirVariableStmt {
                    span: node.span(),
                    name,
                    name_span: ast_let.name.span,
                    // If no type is specified, we use an uninitialized type as a placeholder
                    ty: ty.unwrap_or_else(|| self.arena.types().get_uninitialized_ty()),
                    ty_span: ty.map(|_| ast_let.ty.unwrap().span()),
                    value,
                });
                Ok(hir)
            }
            AstStatement::IfElse(ast_if_else) => {
                let condition = self.visit_expr(ast_if_else.condition)?;
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
                Ok(hir)
            }
            //The parser really need a bit of work
            AstStatement::Return(ast_return) => {
                let expr = self.visit_expr(ast_return.value)?;
                let hir = HirStatement::Return(HirReturn {
                    span: node.span(),
                    ty: expr.ty(),
                    value: expr,
                });
                Ok(hir)
            }
            AstStatement::Expr(ast_expr) => {
                let expr = self.visit_expr(ast_expr)?;
                let hir = HirStatement::Expr(HirExprStmt {
                    span: node.span(),
                    expr,
                });
                Ok(hir)
            }
            _ => {
                let path = node.span().path;
                let src = crate::atlas_c::utils::get_file_content(path).unwrap();
                Err(HirError::UnsupportedStatement(UnsupportedStatement {
                    span: node.span(),
                    stmt: format!("{:?}", node),
                    src: NamedSource::new(path, src),
                }))
            }
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
            AstExpr::Assign(a) => {
                let target = self.visit_expr(a.target)?;
                let value = self.visit_expr(a.value)?;
                let hir = HirExpr::Assign(HirAssignExpr {
                    span: node.span(),
                    lhs: Box::new(target.clone()),
                    rhs: Box::new(value.clone()),
                    ty: self.arena.types().get_uninitialized_ty(),
                });
                Ok(hir)
            }
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
                    ty,
                });
                Ok(hir)
            }
            AstExpr::Call(c) => {
                let callee = self.visit_expr(c.callee)?;
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
                    args_ty: Vec::new(),
                    ty: self.arena.types().get_uninitialized_ty(),
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
            AstExpr::NewObj(obj) => {
                let ty = match self.visit_ty(obj.ty)? {
                    HirTy::Generic(ty) => self.register_generic_type(ty),
                    other => other,
                };
                let hir = HirExpr::NewObj(HirNewObjExpr {
                    span: node.span(),
                    ty,
                    args: obj
                        .args
                        .iter()
                        .map(|arg| self.visit_expr(arg))
                        .collect::<HirResult<Vec<_>>>()?,
                    args_ty: Vec::new(),
                    // Filled in during type checking
                    is_copy_constructor_call: false,
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
            AstExpr::NewArray(a) => {
                let hir = HirExpr::NewArray(HirNewArrayExpr {
                    span: node.span(),
                    ty: self.visit_ty(a.ty)?,
                    size: Box::new(self.visit_expr(a.size)?),
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
                        ty: self.arena.types().get_integer64_ty(),
                    }),
                    AstLiteral::Boolean(b) => HirExpr::BooleanLiteral(HirBooleanLiteralExpr {
                        span: l.span(),
                        value: b.value,
                        ty: self.arena.types().get_boolean_ty(),
                    }),
                    AstLiteral::Float(f) => HirExpr::FloatLiteral(HirFloatLiteralExpr {
                        span: l.span(),
                        value: f.value,
                        ty: self.arena.types().get_float64_ty(),
                    }),
                    AstLiteral::UnsignedInteger(u) => {
                        HirExpr::UnsignedIntegerLiteral(HirUnsignedIntegerLiteralExpr {
                            span: l.span(),
                            value: u.value,
                            ty: self.arena.types().get_uint64_ty(),
                        })
                    }
                    AstLiteral::ThisLiteral(_) => HirExpr::ThisLiteral(HirThisLiteral {
                        span: l.span(),
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
                            ty: self.arena.types().get_str_ty(),
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
            //Generics aren't supported yet for normal functions
            generics,
            type_params: type_parameters?,
            return_ty: ret_type,
            return_ty_span: Some(ret_type_span),
            is_external: false,
        });
        let fun = HirFunction {
            span: node.span,
            name: self.arena.names().get(node.name.name),
            name_span: node.name.span,
            signature,
            body,
        };
        Ok(fun)
    }

    fn visit_func_param(
        &mut self,
        node: &'ast AstObjField<'ast>,
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
        node: &'ast AstObjField<'ast>,
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
            AstType::Integer(_) => self.arena.types().get_integer64_ty(),
            AstType::Float(_) => self.arena.types().get_float64_ty(),
            AstType::Char(_) => self.arena.types().get_char_ty(),
            AstType::UnsignedInteger(_) => self.arena.types().get_uint64_ty(),
            AstType::Unit(_) => self.arena.types().get_unit_ty(),
            AstType::String(_) => self.arena.types().get_str_ty(),
            AstType::Named(n) => {
                let name = self.arena.names().get(n.name.name);
                self.arena.types().get_named_ty(name, n.span)
            }
            AstType::List(l) => {
                let ty = self.visit_ty(l.inner)?;
                self.arena.types().get_list_ty(ty)
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
            AstType::ReadOnlyRef(const_ref) => {
                let inner_ty = self.visit_ty(const_ref.inner)?;
                self.arena.types().get_readonly_reference_ty(inner_ty)
            }
            AstType::Generic(g) => {
                let inner_types = g
                    .inner_types
                    .iter()
                    .map(|inner_ast_ty| self.visit_ty(inner_ast_ty))
                    .collect::<HirResult<Vec<_>>>()?;
                let name = self.arena.names().get(g.name.name);
                let ty = self
                    .arena
                    .types()
                    .get_generic_ty(name, inner_types.clone(), g.span);
                if let HirTy::Generic(g) = ty {
                    self.register_generic_type(g);
                }
                ty
            }
            AstType::MutableRef(ptr) => {
                let inner_ty = self.visit_ty(ptr.inner)?;
                self.arena.types().get_ref_ty(inner_ty)
            }
            //The "this" ty is replaced during the type checking phase
            AstType::ThisTy(_) => self.arena.types().get_uninitialized_ty(),
            AstType::ExternTy(extern_ty) => {
                let type_hint = if let Some(hint) = extern_ty.type_hint {
                    Some(self.visit_ty(hint)?)
                } else {
                    None
                };
                self.arena.types().get_extern_ty(type_hint)
            }
            AstType::Function(func_ty) => {
                let span = func_ty.span;
                let parameters = func_ty
                    .args
                    .iter()
                    .map(|arg| self.visit_ty(arg))
                    .collect::<HirResult<Vec<_>>>()?;
                let return_ty = self.visit_ty(func_ty.ret)?;
                self.arena
                    .types()
                    .get_function_ty(parameters, return_ty, span)
            }
        };
        Ok(ty)
    }

    fn generate_all_copy_constructors(&mut self) -> HirResult<()> {
        let structs_to_process: Vec<_> = self
            .module_body
            .structs
            .iter()
            .filter(|(_, s)| {
                s.copy_constructor.is_none()
                    && !s.flag.is_non_copyable()
                    // If the struct has a user-defined destructor AND no flag saying it's copyable,
                    //  we cannot auto-generate a copy constructor
                    && !(s.had_user_defined_destructor && s.flag.is_no_flag())
            })
            .map(|(name, s)| {
                (
                    ((*name).to_string(), s.name_span),
                    self.arena.types().get_named_ty(s.name, s.name_span),
                    s.signature
                        .fields
                        .values()
                        .cloned()
                        .collect::<Vec<HirStructFieldSignature>>(),
                    s.flag.clone(),
                )
            })
            .collect();

        // Now assign copy constructors using the collected data
        for ((struct_name, struct_span), ty, fields, flag) in structs_to_process {
            let copy_ctor = self.make_copy_constructor(ty, &fields);
            if copy_ctor.is_none() && flag.is_copyable() {
                let path = flag.span().unwrap().path;
                let src = utils::get_file_content(path).unwrap();
                let report: ErrReport = HirWarning::CannotGenerateACopyConstructorForThisType(
                    CannotGenerateACopyConstructorForThisTypeWarning {
                        type_name: struct_name.clone(),
                        flag_span: flag.span().unwrap(),
                        name_span: struct_span,
                        src: NamedSource::new(path, src),
                    },
                )
                .into();
                eprintln!("{:?}", report);
            }
            if let Some(strct) = self.module_body.structs.get_mut(struct_name.as_str()) {
                strct.signature.copy_constructor = copy_ctor.as_ref().map(|c| c.signature.clone());
                strct.copy_constructor = copy_ctor.clone();
                if let Some(current_struct_sig) =
                    self.module_signature.structs.get_mut(struct_name.as_str())
                {
                    *current_struct_sig = self.arena.intern(strct.signature.clone());
                }
            }
        }
        Ok(())
    }

    fn make_copy_constructor(
        &mut self,
        ty: &'hir HirTy<'hir>,
        fields: &[HirStructFieldSignature<'hir>],
    ) -> Option<HirStructConstructor<'hir>> {
        if self.can_be_copyable(ty) {
            let (name, span) = match ty {
                HirTy::Named(named) => (named.name, named.span),
                HirTy::Generic(generic) => (
                    MonomorphizationPass::generate_mangled_name(self.arena, generic, "struct"),
                    generic.span,
                ),
                // Shouldn't happen
                _ => ("hehehehehehe", Span::default()),
            };
            let params = vec![HirFunctionParameterSignature {
                span,
                name: self.arena.names().get("from"),
                name_span: span,
                ty: self.arena.types().get_readonly_reference_ty(ty),
                ty_span: span,
            }];

            let type_params = vec![HirTypeParameterItemSignature {
                span,
                name: self.arena.names().get("from"),
                name_span: span,
            }];

            let copy_ctor_signature = HirStructConstructorSignature {
                span,
                params: params.clone(),
                type_params: type_params.clone(),
                vis: HirVisibility::Public,
                where_clause: None,
                // Initially true; monomorphization pass will check constraints and update if needed
                is_constraint_satisfied: true,
            };
            // each statement is of the form: this.field = *from.field;
            let mut statements = vec![];
            for field in fields.iter() {
                let init_expr = HirExpr::Assign(HirAssignExpr {
                    span: field.span,
                    lhs: Box::new(HirExpr::FieldAccess(HirFieldAccessExpr {
                        span: field.span,
                        target: Box::new(HirExpr::ThisLiteral(HirThisLiteral {
                            span: field.span,
                            ty: self.arena.types().get_named_ty(name, span),
                        })),
                        field: Box::new(HirIdentExpr {
                            span: field.span,
                            name: field.name,
                            ty: field.ty,
                        }),
                        ty: field.ty,
                    })),
                    rhs: Box::new(HirExpr::Unary(UnaryOpExpr {
                        span: field.span,
                        op: Some(HirUnaryOp::Deref),
                        expr: Box::new(HirExpr::FieldAccess(HirFieldAccessExpr {
                            span: field.span,
                            target: Box::new(HirExpr::Ident(HirIdentExpr {
                                span,
                                name: self.arena.names().get("from"),
                                ty: self.arena.types().get_readonly_reference_ty(ty),
                            })),
                            field: Box::new(HirIdentExpr {
                                span: field.span,
                                name: field.name,
                                ty: field.ty,
                            }),
                            ty: field.ty,
                        })),
                        ty: field.ty,
                    })),
                    ty: field.ty,
                });
                statements.push(HirStatement::Expr(HirExprStmt {
                    span: field.span,
                    expr: init_expr,
                }));
            }
            let hir = HirStructConstructor {
                span,
                signature: self.arena.intern(copy_ctor_signature),
                params,
                type_params,
                body: HirBlock { span, statements },
                //Copy constructor is public by default
                vis: HirVisibility::Public,
            };
            return Some(hir);
        }
        None
    }

    /// A type can be copyable if:
    /// - It's a primitive type (int, float, bool, char, uint, string)
    /// - It's a reference type (&T, &const T) - references are pointers
    /// - If it has a copy constructor defined AND no pre define destructor (to avoid double free)
    /// - If all its fields are copyable
    fn can_be_copyable(&self, ty: &'hir HirTy<'hir>) -> bool {
        match ty {
            HirTy::Int64(_)
            | HirTy::Float64(_)
            | HirTy::Boolean(_)
            | HirTy::Char(_)
            | HirTy::UInt64(_)
            | HirTy::String(_)
            | HirTy::Unit(_)
            | HirTy::MutableReference(_)
            | HirTy::ReadOnlyReference(_)
            | HirTy::ExternTy(_)
            | HirTy::Function(_) => true,
            // TODO: Add support for list copy constructors
            // HirTy::List(list) => self.can_be_copyable(list.inner, module),
            HirTy::Named(named) => {
                let obj_name = named.name;
                if let Some(hir_struct) = self.module_signature.structs.get(obj_name) {
                    // TODO: Add a check for the presence of a destructor in the struct
                    if hir_struct.copy_constructor.is_some() {
                        return true;
                    }
                    //Check all fields
                    for field in hir_struct.fields.values() {
                        if !self.can_be_copyable(field.ty) {
                            return false;
                        }
                    }
                    return true;
                }
                if let Some(hir_enum) = self.module_signature.enums.get(obj_name) {
                    // Enums are just uint64 under the hood
                    return true;
                }
                false
            }
            HirTy::Generic(generic) => {
                let struct_name =
                    MonomorphizationPass::generate_mangled_name(self.arena, generic, "struct");
                // We only search in structs because unions can't be auto-copyable for safety reasons
                if let Some(hir_struct) = self.module_signature.structs.get(struct_name) {
                    // TODO: Add a check for the presence of a destructor in the struct
                    if hir_struct.copy_constructor.is_some() {
                        return true;
                    }
                    //Check all fields
                    for field in hir_struct.fields.values() {
                        if !self.can_be_copyable(field.ty) {
                            return false;
                        }
                    }
                    return true;
                }
                false
            }
            HirTy::Nullable(nullable) => self.can_be_copyable(nullable.inner),
            // We just assume other types are not copyable for now
            _ => false,
        }
    }

    fn nullable_types_are_unstable_warning(span: &Span) {
        let path = span.path;
        let src = crate::atlas_c::utils::get_file_content(path).unwrap();
        let report: ErrReport =
            HirWarning::ThisTypeIsStillUnstable(ThisTypeIsStillUnstableWarning {
                src: NamedSource::new(path, src),
                span: *span,
                type_name: "The nullable type".to_string(),
                info: "Nullable types haven't been properly stabilized yet. Also they are just syntactic sugar for `Option<T>`".to_string(),
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

    fn get_union_name(&self, ty: &'hir HirTy<'hir>) -> Option<&'hir str> {
        match ty {
            HirTy::Named(n) => Some(n.name),
            HirTy::Generic(g) => Some(g.name),
            _ => None,
        }
    }
}
