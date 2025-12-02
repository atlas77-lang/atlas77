pub mod case;

use heck::{ToPascalCase, ToSnakeCase};
use miette::{ErrReport, NamedSource, SourceOffset, SourceSpan};
use std::{collections::BTreeMap, path::PathBuf};

use crate::atlas_c::atlas_hir::error::InvalidReadOnlyTypeError;
use crate::atlas_c::atlas_hir::warning::NameShouldBeInDifferentCaseWarning;
use crate::atlas_c::{
    atlas_frontend::{
        parse,
        parser::{
            arena::AstArena,
            ast::{
                AstBinaryOp, AstBlock, AstConstructor, AstDestructor, AstExpr, AstFunction,
                AstIdentifier, AstImport, AstItem, AstLiteral, AstMethod, AstMethodModifier,
                AstNamedType, AstObjField, AstProgram, AstStatement, AstStruct, AstType,
                AstUnaryOp,
            },
        },
    },
    atlas_hir::{
        arena::HirArena, error::{
            HirError, HirResult, NonConstantValueError, UnsupportedExpr, UnsupportedStatement,
            UnsupportedTypeError, UselessError,
        }, expr::{
            HirAssignExpr, HirBinaryOp, HirBinaryOpExpr, HirBooleanLiteralExpr, HirCastExpr,
            HirCharLiteralExpr, HirDeleteExpr, HirExpr, HirFieldAccessExpr,
            HirFloatLiteralExpr, HirFunctionCallExpr, HirIdentExpr, HirIndexingExpr,
            HirIntegerLiteralExpr, HirListLiteralExpr, HirNewArrayExpr, HirNewObjExpr,
            HirNoneLiteral, HirStaticAccessExpr, HirStringLiteralExpr, HirThisLiteral, HirUnaryOp,
            HirUnitLiteralExpr, HirUnsignedIntegerLiteralExpr, UnaryOpExpr,
        },
        generic_pool::HirGenericPool,
        item::{HirFunction, HirStruct, HirStructConstructor, HirStructMethod},
        signature::{
            ConstantValue, HirFunctionParameterSignature, HirFunctionSignature, HirModuleSignature,
            HirStructConstantSignature, HirStructConstructorSignature, HirStructFieldSignature,
            HirStructMethodModifier, HirStructMethodSignature, HirStructSignature,
            HirTypeParameterItemSignature, HirVisibility,
        },
        stmt::{
            HirBlock, HirExprStmt, HirIfElseStmt, HirLetStmt, HirReturn, HirStatement, HirWhileStmt,
        },
        syntax_lowering_pass::case::Case,
        ty::{HirGenericTy, HirTy},
        warning::{HirWarning, NullableTypesAreUnstableWarning},
        HirImport,
        HirModule,
        HirModuleBody,
    },
    utils::Span,
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
}

impl<'ast, 'hir> AstSyntaxLoweringPass<'ast, 'hir> {
    pub fn new(
        arena: &'hir HirArena<'hir>,
        ast: &'ast AstProgram,
        ast_arena: &'ast AstArena<'ast>,
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

        Ok(self.arena.intern(HirModule {
            body: self.module_body.clone(),
            signature: self.module_signature.clone(),
        }))
    }
    pub fn visit_item(&mut self, ast_item: &'ast AstItem<'ast>) -> HirResult<()> {
        match ast_item {
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
                    self.generic_pool.structs.append(&mut generic_pool.structs);
                }
                Err(e) => match e {
                    HirError::UselessError(_) => {}
                    _ => return Err(e),
                },
            },
            AstItem::ExternFunction(ast_extern_func) => {
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

                let generics = if ast_extern_func.generics.is_some() {
                    Some(
                        ast_extern_func
                            .generics
                            .unwrap()
                            .iter()
                            .map(|g| self.visit_generic(g))
                            .collect::<HirResult<Vec<_>>>()?,
                    )
                } else {
                    None
                };

                for (arg_name, arg_ty) in ast_extern_func
                    .args_name
                    .iter()
                    .zip(ast_extern_func.args_ty.iter())
                {
                    let hir_arg_ty = self.visit_ty(arg_ty)?;
                    let hir_arg_name = self.arena.names().get(arg_name.name);

                    params.push(HirFunctionParameterSignature {
                        span: arg_name.span.clone(),
                        name: hir_arg_name,
                        name_span: arg_name.span.clone(),
                        ty: hir_arg_ty,
                        ty_span: arg_ty.span(),
                    });

                    type_params.push(self.arena.intern(HirTypeParameterItemSignature {
                        span: arg_name.span.clone(),
                        name: hir_arg_name,
                        name_span: arg_name.span.clone(),
                    }));
                }
                let hir = self.arena.intern(HirFunctionSignature {
                    span: ast_extern_func.span.clone(),
                    vis: ast_extern_func.vis.into(),
                    params,
                    generics,
                    type_params,
                    return_ty: ty,
                    return_ty_span: Some(ast_extern_func.ret_ty.span()),
                    is_external: true,
                });
                self.module_signature.functions.insert(name, hir);
            }
        }
        Ok(())
    }

    //todo: Add constraints to generics
    fn visit_generic(
        &self,
        generics: &'ast AstNamedType,
    ) -> HirResult<&'hir HirTypeParameterItemSignature<'hir>> {
        let name = self.arena.names().get(generics.name.name);
        let hir = self.arena.intern(HirTypeParameterItemSignature {
            span: generics.span.clone(),
            name,
            name_span: generics.name.span.clone(),
        });
        Ok(hir)
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

        let mut methods = Vec::new();
        for method in node.methods.iter() {
            let hir_method = self.visit_method(method)?;
            methods.push(hir_method);
        }

        let mut generics: Vec<&'hir str> = Vec::new();
        if !node.generics.is_empty() {
            for generic in node.generics.iter() {
                generics.push(self.arena.intern(generic.name.name.to_owned()));
            }
        }

        let mut fields = Vec::new();
        for field in node.fields.iter() {
            let ty = self.visit_ty(field.ty)?;
            let name = self.arena.names().get(field.name.name);
            fields.push(HirStructFieldSignature {
                span: field.span.clone(),
                vis: HirVisibility::from(field.vis),
                name,
                name_span: field.name.span.clone(),
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
                    let src = std::fs::read_to_string(PathBuf::from(&path))
                        .unwrap_or_else(|_| panic!("{} is not a valid path", path));
                    return Err(HirError::NonConstantValue(NonConstantValueError {
                        span: SourceSpan::new(
                            SourceOffset::from(constant.value.span().start),
                            constant.value.span().end - constant.value.span().start,
                        ),
                        src: NamedSource::new(path, src),
                    }));
                }
            };
            constants.insert(
                name,
                self.arena.intern(HirStructConstantSignature {
                    span: constant.span.clone(),
                    vis: node.vis.into(),
                    name,
                    name_span: constant.name.span.clone(),
                    ty,
                    ty_span: constant.ty.span(),
                    value: self.arena.intern(value),
                }),
            );
        }

        let constructor = self.visit_constructor(node.constructor, &fields)?;
        let destructor = self.visit_destructor(node.destructor)?;

        let constructor_signature = HirStructConstructorSignature {
            span: node.span.clone(),
            params: constructor.params.to_vec(),
            type_params: constructor.type_params.to_vec(),
            vis: constructor.vis,
        };
        let destructor_signature = HirStructConstructorSignature {
            span: node.span.clone(),
            params: destructor.params.to_vec(),
            type_params: destructor.type_params.to_vec(),
            vis: destructor.vis,
        };

        let signature = HirStructSignature {
            declaration_span: node.span.clone(),
            name_span: node.name.span.clone(),
            vis: node.vis.into(),
            name,
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
            constructor: constructor_signature,
            destructor: destructor_signature,
        };

        Ok(HirStruct {
            span: node.span.clone(),
            name,
            name_span: node.name.span.clone(),
            signature,
            methods,
            fields,
            constructor,
            destructor,
            vis: node.vis.into(),
        })
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
        let signature = self.arena.intern(HirStructMethodSignature {
            modifier: match node.modifier {
                AstMethodModifier::Const => HirStructMethodModifier::Const,
                AstMethodModifier::Static => HirStructMethodModifier::Static,
                AstMethodModifier::None => HirStructMethodModifier::None,
            },
            span: node.span.clone(),
            //TODO: PLACEHOLDER FOR NOW. NEED TO HANDLE VISIBILITY MODIFIERS IN METHODS
            vis: node.vis.into(),
            params: parameters?,
            //Generics aren't supported yet for normal functions
            generics: None,
            type_params: type_parameters?,
            return_ty: ret_type,
            return_ty_span: Some(ret_type_span),
        });
        let method = HirStructMethod {
            span: node.span.clone(),
            name: self.arena.names().get(node.name.name),
            name_span: node.name.span.clone(),
            signature,
            body,
        };
        Ok(method)
    }

    fn visit_constructor(
        &mut self,
        constructor: Option<&'ast AstConstructor<'ast>>,
        fields: &[HirStructFieldSignature<'hir>],
    ) -> HirResult<HirStructConstructor<'hir>> {
        if constructor.is_none() {
            let mut params: Vec<HirFunctionParameterSignature<'hir>> = Vec::new();
            for field in fields.iter() {
                let ty = field.ty;
                let name = field.name;
                params.push(HirFunctionParameterSignature {
                    span: field.span.clone(),
                    name,
                    name_span: field.name_span.clone(),
                    ty,
                    ty_span: field.ty_span.clone(),
                });
            }
            let mut type_params: Vec<HirTypeParameterItemSignature<'hir>> = Vec::new();
            for type_param in params.iter() {
                type_params.push(HirTypeParameterItemSignature {
                    span: type_param.span.clone(),
                    name: type_param.name,
                    name_span: type_param.name_span.clone(),
                });
            }
            let hir = HirStructConstructor {
                span: Span::default(),
                params,
                type_params,
                body: HirBlock {
                    span: Span::default(),
                    statements: Vec::new(),
                },
                //Constructor is public by default
                vis: HirVisibility::Public,
            };
            return Ok(hir);
        }
        let constructor = constructor.unwrap();
        let mut params: Vec<HirFunctionParameterSignature<'hir>> = Vec::new();
        for param in constructor.args.iter() {
            let ty = self.visit_ty(param.ty)?;
            let name = self.arena.names().get(param.name.name);
            params.push(HirFunctionParameterSignature {
                span: param.span.clone(),
                name,
                name_span: param.name.span.clone(),
                ty,
                ty_span: param.ty.span(),
            });
        }

        let mut type_params: Vec<HirTypeParameterItemSignature<'hir>> = Vec::new();
        for type_param in params.iter() {
            type_params.push(HirTypeParameterItemSignature {
                span: type_param.span.clone(),
                name: type_param.name,
                name_span: type_param.name_span.clone(),
            });
        }
        let hir = HirStructConstructor {
            span: constructor.span.clone(),
            params,
            type_params,
            body: self.visit_block(constructor.body)?,
            vis: constructor.vis.into(),
        };
        Ok(hir)
    }

    fn visit_destructor(
        &mut self,
        destructor: Option<&'ast AstDestructor<'ast>>,
    ) -> HirResult<HirStructConstructor<'hir>> {
        if destructor.is_none() {
            let hir = HirStructConstructor {
                span: Span::default(),
                params: Vec::new(),
                type_params: Vec::new(),
                body: HirBlock {
                    span: Span::default(),
                    statements: Vec::new(),
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
                span: param.span.clone(),
                name,
                name_span: param.name.span.clone(),
                ty,
                ty_span: param.ty.span(),
            });
        }

        let mut type_params: Vec<HirTypeParameterItemSignature<'hir>> = Vec::new();
        for type_param in params.iter() {
            type_params.push(HirTypeParameterItemSignature {
                span: type_param.span.clone(),
                name: type_param.name,
                name_span: type_param.name_span.clone(),
            });
        }
        let hir = HirStructConstructor {
            span: destructor.span.clone(),
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
            let ast: AstProgram<'ast> =
                parse(path, self.ast_arena, src).unwrap();
            let allocated_ast = self.ast_arena.alloc(ast);
            let mut ast_lowering_pass =
                AstSyntaxLoweringPass::<'ast, 'hir>::new(self.arena, allocated_ast, self.ast_arena);
            ast_lowering_pass
                .already_imported
                .append(&mut self.already_imported);
            let hir = ast_lowering_pass.lower()?;
            self.already_imported
                .append(&mut ast_lowering_pass.already_imported);
            let path: &'hir str = self.arena.names().get(node.path);
            let hir_import: &'hir HirImport<'hir> = self.arena.intern(HirImport {
                span: node.span.clone(),
                path,
                path_span: node.span.clone(),
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
            span: node.span.clone(),
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
                let hir = HirStatement::Const(HirLetStmt {
                    span: node.span(),
                    name,
                    name_span: ast_const.name.span.clone(),
                    ty: Some(ty),
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
                let hir = HirStatement::Let(HirLetStmt {
                    span: node.span(),
                    name,
                    name_span: ast_let.name.span.clone(),
                    ty,
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
                let src = crate::atlas_c::utils::get_file_content(&path).unwrap();
                Err(HirError::UnsupportedStatement(UnsupportedStatement {
                    span: SourceSpan::new(
                        SourceOffset::from(node.span().start),
                        node.span().end - node.span().start,
                    ),
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
                .register_struct_instance(generic_type.clone());
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
                let hir = HirExpr::HirBinaryOp(HirBinaryOpExpr {
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
                let hir = HirExpr::Call(HirFunctionCallExpr {
                    span: node.span(),
                    callee: Box::new(callee.clone()),
                    callee_span: callee.span(),
                    args,
                    args_ty: Vec::new(),
                    ty: self.arena.types().get_uninitialized_ty(),
                });
                Ok(hir)
            }
            AstExpr::Identifier(i) => {
                let hir = HirExpr::Ident(HirIdentExpr {
                    name: self.arena.names().get(i.name),
                    span: i.span.clone(),
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
                    AstLiteral::SelfLiteral(_) => HirExpr::ThisLiteral(HirThisLiteral {
                        span: l.span(),
                        ty: self.arena.types().get_uninitialized_ty(),
                    }),
                    AstLiteral::None(_) => {
                        Self::nullable_types_are_unstable_warning(&node.span());
                        HirExpr::NoneLiteral(HirNoneLiteral {
                            span: l.span(),
                            ty: self.arena.types().get_none_ty(),
                        })
                    }
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
                            span: l.span.clone(),
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
                    target: Box::new(self.visit_identifier(ast_static_access.target)?),
                    field: Box::new(HirIdentExpr {
                        name: self.arena.names().get(ast_static_access.field.name),
                        span: ast_static_access.field.span.clone(),
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
                        span: ast_field_access.field.span.clone(),
                        ty: self.arena.types().get_uninitialized_ty(),
                    }),
                    ty: self.arena.types().get_uninitialized_ty(),
                });
                Ok(hir)
            }
            _ => {
                //todo: if/else as an expression
                let path = node.span().path;
                let src = crate::atlas_c::utils::get_file_content(&path).unwrap();
                Err(HirError::UnsupportedExpr(UnsupportedExpr {
                    span: SourceSpan::new(
                        SourceOffset::from(node.span().start),
                        node.span().end - node.span().start,
                    ),
                    expr: format!("{:?}", node),
                    src: NamedSource::new(path, src),
                }))
            }
        }
    }

    fn visit_identifier(&self, node: &'ast AstIdentifier<'ast>) -> HirResult<HirIdentExpr<'hir>> {
        Ok(HirIdentExpr {
            name: self.arena.names().get(node.name),
            span: node.span.clone(),
            ty: self.arena.types().get_uninitialized_ty(),
        })
    }

    fn visit_bin_op(&self, bin_op: &'ast AstBinaryOp) -> HirResult<HirBinaryOp> {
        let op = match bin_op {
            AstBinaryOp::Add => HirBinaryOp::Add,
            AstBinaryOp::Sub => HirBinaryOp::Sub,
            AstBinaryOp::Mul => HirBinaryOp::Mul,
            AstBinaryOp::Div => HirBinaryOp::Div,
            AstBinaryOp::Mod => HirBinaryOp::Mod,
            AstBinaryOp::Eq => HirBinaryOp::Eq,
            AstBinaryOp::NEq => HirBinaryOp::Neq,
            AstBinaryOp::Lt => HirBinaryOp::Lt,
            AstBinaryOp::Lte => HirBinaryOp::Lte,
            AstBinaryOp::Gt => HirBinaryOp::Gt,
            AstBinaryOp::Gte => HirBinaryOp::Gte,
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
        let signature = self.arena.intern(HirFunctionSignature {
            span: node.span.clone(),
            vis: node.vis.into(),
            params: parameters?,
            //Generics aren't supported yet for normal functions
            generics: None,
            type_params: type_parameters?,
            return_ty: ret_type,
            return_ty_span: Some(ret_type_span),
            is_external: false,
        });
        let fun = HirFunction {
            span: node.span.clone(),
            name: self.arena.names().get(node.name.name),
            name_span: node.name.span.clone(),
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
            span: node.span.clone(),
            name,
            name_span: node.name.span.clone(),
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
            span: node.span.clone(),
            name,
            name_span: node.name.span.clone(),
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
            AstType::Null(_) => {
                Self::nullable_types_are_unstable_warning(&node.span());
                self.arena.types().get_none_ty()
            }
            AstType::Named(n) => {
                let name = self.arena.names().get(n.name.name);
                self.arena.types().get_named_ty(name, n.span.clone())
            }
            AstType::List(l) => {
                let ty = self.visit_ty(l.inner)?;
                self.arena.types().get_list_ty(ty)
            }
            AstType::Nullable(n) => {
                Self::nullable_types_are_unstable_warning(&node.span());
                let ty = self.visit_ty(n.inner)?;
                self.arena.types().get_nullable_ty(ty)
            }
            AstType::ReadOnly(r) => {
                let ty = self.visit_ty(r.inner)?;
                match ty {
                    HirTy::Reference(_) => {}
                    _ => {
                        let path = node.span().path;
                        let src = crate::atlas_c::utils::get_file_content(&path).unwrap();
                        return Err(HirError::InvalidReadOnlyType(InvalidReadOnlyTypeError {
                            span: SourceSpan::new(
                                SourceOffset::from(node.span().start),
                                node.span().end - node.span().start,
                            ),
                            ty: format!("{}", ty),
                            src: NamedSource::new(path, src),
                        }));
                    }
                };
                self.arena.types().get_readonly_ty(ty)
            }
            AstType::Generic(g) => {
                let inner_types = g
                    .inner_types
                    .iter()
                    .map(|inner_ast_ty| self.visit_ty(inner_ast_ty))
                    .collect::<HirResult<Vec<_>>>()?;
                let name = self.arena.names().get(g.name.name);
                let ty =
                    self.arena
                        .types()
                        .get_generic_ty(name, inner_types.clone(), g.span.clone());
                match ty {
                    HirTy::Generic(g) => {
                        self.register_generic_type(g);
                    }
                    _ => {}
                }
                ty
            }
            AstType::Reference(ptr) => {
                let inner_ty = self.visit_ty(ptr.inner)?;
                self.arena.types().get_reference_ty(inner_ty)
            }
            //The "this" ty is replaced during the type checking phase
            AstType::ThisTy(_) => self.arena.types().get_uninitialized_ty(),
            _ => {
                let path = node.span().path;
                let src = crate::atlas_c::utils::get_file_content(&path).unwrap();
                return Err(HirError::UnsupportedType(UnsupportedTypeError {
                    span: SourceSpan::new(
                        SourceOffset::from(node.span().start),
                        node.span().end - node.span().start,
                    ),
                    ty: format!("{:?}", node),
                    src: NamedSource::new(path, src),
                }));
            }
        };
        Ok(ty)
    }

    fn nullable_types_are_unstable_warning(span: &Span) {
        let path = span.path;
        let src = crate::atlas_c::utils::get_file_content(&path).unwrap();
        let report: ErrReport = HirWarning::NullableTypesAreUnstable(
            NullableTypesAreUnstableWarning {
                src: NamedSource::new(path, src),
                span: SourceSpan::new(SourceOffset::from(span.start), span.end - span.start),
            },
        ).into();
        eprintln!("{:?}", report);
    }

    fn name_should_be_in_different_case_warning(span: &Span, case_kind: &str, item_kind: &str, name: &str, expected_name: &str) {
        let path = span.path;
        let src = crate::atlas_c::utils::get_file_content(&path).unwrap();
        let report: ErrReport = HirWarning::NameShouldBeInDifferentCase(
            NameShouldBeInDifferentCaseWarning {
                src: NamedSource::new(path, src),
                span: SourceSpan::new(SourceOffset::from(span.start), span.end - span.start),
                case_kind: case_kind.to_string(),
                item_kind: item_kind.to_string(),
                name: name.to_string(),
                expected_name: expected_name.to_string(),
            },
        ).into();
        eprintln!("{:?}", report);
    }
}
