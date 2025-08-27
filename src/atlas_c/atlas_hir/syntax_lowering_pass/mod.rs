pub mod case;

use heck::{ToPascalCase, ToSnakeCase};
use miette::{SourceOffset, SourceSpan};
use std::collections::BTreeMap;

use crate::atlas_c::atlas_frontend::parser::ast::{
    AstClass, AstConstructor, AstDestructor, AstIdentifier, AstMethod, AstMethodModifier,
    AstNamedType,
};
use crate::atlas_c::atlas_frontend::{
    parse,
    parser::{
        arena::AstArena,
        ast::{
            AstBinaryOp, AstBlock, AstExpr, AstFunction, AstImport, AstItem, AstLiteral,
            AstObjField, AstProgram, AstStatement, AstType, AstUnaryOp,
        },
    },
};

const FILE_ATLAS: &str = include_str!("../../../atlas_lib/std/fs.atlas");
const IO_ATLAS: &str = include_str!("../../../atlas_lib/std/io.atlas");
const ARRAY_ATLAS: &str = include_str!("../../../atlas_lib/std/array.atlas");
const MATH_ATLAS: &str = include_str!("../../../atlas_lib/std/math.atlas");
const STRING_ATLAS: &str = include_str!("../../../atlas_lib/std/string.atlas");

use crate::atlas_c::atlas_hir::error::NonConstantValueError;
use crate::atlas_c::atlas_hir::expr::{
    HirCastExpr, HirCharLiteralExpr, HirDeleteExpr, HirFieldAccessExpr, HirIndexingExpr,
    HirListLiteralExpr, HirNewArrayExpr, HirNewObjExpr, HirNoneLiteral, HirSelfLiteral,
    HirStaticAccessExpr, HirStringLiteralExpr, HirUnitLiteralExpr,
};
use crate::atlas_c::atlas_hir::item::{HirClass, HirClassConstructor, HirClassMethod};
use crate::atlas_c::atlas_hir::signature::{
    ConstantValue, HirClassConstSignature, HirClassConstructorSignature, HirClassFieldSignature,
    HirClassMethodModifier, HirClassMethodSignature, HirClassSignature,
};
use crate::atlas_c::atlas_hir::syntax_lowering_pass::case::Case;
use crate::atlas_c::atlas_hir::{
    HirImport, HirModule, HirModuleBody,
    arena::HirArena,
    error::{HirError, HirResult, UnsupportedExpr, UnsupportedStatement},
    expr::{
        HirAssignExpr, HirBinaryOp, HirBinaryOpExpr, HirBooleanLiteralExpr, HirExpr,
        HirFloatLiteralExpr, HirFunctionCallExpr, HirIdentExpr, HirIntegerLiteralExpr, HirUnaryOp,
        HirUnsignedIntegerLiteralExpr, UnaryOpExpr,
    },
    item::HirFunction,
    signature::{
        HirFunctionParameterSignature, HirFunctionSignature, HirModuleSignature,
        HirTypeParameterItemSignature,
    },
    stmt::{
        HirBlock, HirExprStmt, HirIfElseStmt, HirLetStmt, HirReturn, HirStatement, HirWhileStmt,
    },
    ty::HirTy,
};

pub struct AstSyntaxLoweringPass<'ast, 'hir> {
    arena: &'hir HirArena<'hir>,
    ast: &'ast AstProgram<'ast>,
    ast_arena: &'ast AstArena<'ast>,
    //source code
    src: String,
}

impl<'ast, 'hir> AstSyntaxLoweringPass<'ast, 'hir> {
    pub fn new(
        arena: &'hir HirArena<'hir>,
        ast: &'ast AstProgram,
        ast_arena: &'ast AstArena<'ast>,
        src: String,
    ) -> Self {
        Self {
            arena,
            ast,
            ast_arena,
            src,
        }
    }
}

impl<'ast, 'hir> AstSyntaxLoweringPass<'ast, 'hir>
where
    'ast: 'hir,
{
    pub fn lower(&self) -> HirResult<HirModule> {
        let mut module_body = HirModuleBody::default();
        let mut module_signature = HirModuleSignature::default();

        let mut items = Vec::new();
        for item in self.ast.items {
            items.push(self.visit_item(&mut module_body, &mut module_signature, item)?);
        }
        Ok(HirModule {
            body: module_body,
            signature: module_signature,
        })
    }
    pub fn visit_item(
        &self,
        module_body: &mut HirModuleBody<'hir>,
        module_signature: &mut HirModuleSignature<'hir>,
        item: &'ast AstItem<'ast>,
    ) -> HirResult<()> {
        match item {
            AstItem::Func(f) => {
                let fun = self.visit_func(f)?;
                let name = self.arena.names().get(f.name.name);
                if !name.is_snake_case() {
                    eprintln!("Warning: {} is not snake case", name);
                    eprintln!(
                        "Try using snake_case for function names e.g. {}",
                        name.to_snake_case()
                    );
                }
                module_signature.functions.insert(name, fun.signature);
                module_body.functions.insert(name, fun);
            }
            //todo: add support for classes
            AstItem::Class(c) => {
                let class = self.visit_class(c)?;
                module_signature.classes.insert(class.name, class.signature);
                module_body.classes.insert(class.name, class);
            }
            AstItem::Import(i) => {
                let hir = self.visit_import(i)?;
                let allocated_hir: &'hir HirModule<'hir> = self.arena.intern(hir);
                for (name, signature) in allocated_hir.signature.functions.iter() {
                    module_signature.functions.insert(name, *signature);
                }
                allocated_hir.body.imports.iter().for_each(|i| {
                    module_body.imports.push(i);
                });
            }
            AstItem::ExternFunction(e) => {
                let name = self.arena.names().get(e.name.name);
                if !name.is_snake_case() {
                    eprintln!("Warning: {} is not snake case", name);
                    eprintln!(
                        "Try using snake_case for function names e.g. {}",
                        name.to_snake_case()
                    );
                }
                let ty = self.visit_ty(e.ret)?;

                let mut params: Vec<&HirFunctionParameterSignature<'hir>> = Vec::new();
                let mut type_params: Vec<&HirTypeParameterItemSignature<'_>> = Vec::new();

                let generics = if e.generics.is_some() {
                    Some(
                        e.generics
                            .unwrap()
                            .iter()
                            .map(|g| self.visit_generic(g))
                            .collect::<HirResult<Vec<_>>>()?,
                    )
                } else {
                    None
                };

                for (arg_name, arg_ty) in e.args_name.iter().zip(e.args_ty.iter()) {
                    let hir_arg_ty = self.visit_ty(arg_ty)?;
                    let hir_arg_name = self.arena.names().get(arg_name.name);

                    params.push(self.arena.intern(HirFunctionParameterSignature {
                        span: arg_name.span.clone(),
                        name: hir_arg_name,
                        name_span: arg_name.span.clone(),
                        ty: hir_arg_ty,
                        ty_span: arg_ty.span(),
                    }));

                    type_params.push(self.arena.intern(HirTypeParameterItemSignature {
                        span: arg_name.span.clone(),
                        name: hir_arg_name,
                        name_span: arg_name.span.clone(),
                    }));
                }
                let hir = self.arena.intern(HirFunctionSignature {
                    span: e.span.clone(),
                    vis: e.vis.into(),
                    params,
                    generics,
                    type_params,
                    return_ty: ty,
                    return_ty_span: Some(e.ret.span()),
                    is_external: true,
                });
                module_signature.functions.insert(name, hir);
            }
            _ => {}
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

    fn visit_class(&self, node: &'ast AstClass<'ast>) -> HirResult<HirClass<'hir>> {
        let name = self.arena.names().get(node.name.name);
        if !name.is_pascal_case() {
            eprintln!("Warning: {} is not pascal case", name);
            eprintln!(
                "Try using PascalCase for class names e.g. {}",
                name.to_pascal_case()
            );
        }

        let mut methods = Vec::new();
        for method in node.methods.iter() {
            let fun = self.visit_method(method)?;
            methods.push(fun);
        }

        let mut fields = Vec::new();
        for field in node.fields.iter() {
            let ty = self.visit_ty(field.ty)?;
            let name = self.arena.names().get(field.name.name);
            fields.push(HirClassFieldSignature {
                span: field.span.clone(),
                vis: field.vis.into(),
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

        let mut constants: BTreeMap<&'hir str, &'hir HirClassConstSignature<'hir>> =
            BTreeMap::new();
        for constant in node.constants.iter() {
            let ty = self.visit_ty(constant.ty)?;
            let name = self.arena.names().get(constant.name.name);
            let const_expr = self.visit_expr(constant.value)?;
            let value = match ConstantValue::try_from(const_expr) {
                Ok(value) => value,
                Err(_) => {
                    return Err(HirError::NonConstantValue(NonConstantValueError {
                        span: SourceSpan::new(
                            SourceOffset::from(constant.value.span().start),
                            constant.value.span().end - constant.value.span().start,
                        ),
                        src: self.src.clone(),
                    }));
                }
            };
            constants.insert(
                name,
                self.arena.intern(HirClassConstSignature {
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
        let constructor_signature = HirClassConstructorSignature {
            span: node.span.clone(),
            params: constructor.params.to_vec(),
            type_params: constructor.type_params.to_vec(),
        };
        let destructor_signature = HirClassConstructorSignature {
            span: node.span.clone(),
            params: destructor.params.to_vec(),
            type_params: destructor.type_params.to_vec(),
        };

        let signature = self.arena.intern(HirClassSignature {
            span: node.span.clone(),
            vis: node.vis.into(),
            name,
            methods: {
                let mut map = BTreeMap::new();
                for method in methods.iter() {
                    map.insert(method.name, method.signature);
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
            constructor: constructor_signature,
            destructor: destructor_signature,
        });

        Ok(HirClass {
            span: node.span.clone(),
            name,
            name_span: node.name.span.clone(),
            signature,
            methods,
            fields,
            constructor,
            destructor,
        })
    }

    fn visit_method(&self, node: &'ast AstMethod<'ast>) -> HirResult<HirClassMethod<'hir>> {
        let type_parameters = node
            .args
            .iter()
            .map(|arg| self.visit_type_param_item(arg))
            .collect::<HirResult<Vec<_>>>();
        let ret_type_span = node.ret.span();
        let ret_type = self.visit_ty(node.ret)?;
        let parameters = node
            .args
            .iter()
            .map(|arg| self.visit_func_param(arg))
            .collect::<HirResult<Vec<_>>>();

        let body = self.visit_block(node.body)?;
        let signature = self.arena.intern(HirClassMethodSignature {
            modifier: match node.modifier {
                AstMethodModifier::Const => HirClassMethodModifier::Const,
                AstMethodModifier::Static => HirClassMethodModifier::Static,
                AstMethodModifier::None => HirClassMethodModifier::None,
            },
            span: node.span.clone(),
            vis: node.vis.into(),
            params: parameters?,
            //Generics aren't supported yet for normal functions
            generics: None,
            type_params: type_parameters?,
            return_ty: ret_type,
            return_ty_span: Some(ret_type_span),
        });
        let method = HirClassMethod {
            span: node.span.clone(),
            name: self.arena.names().get(node.name.name),
            name_span: node.name.span.clone(),
            signature,
            body,
        };
        Ok(method)
    }

    fn visit_constructor(
        &self,
        constructor: Option<&'ast AstConstructor<'ast>>,
        fields: &[HirClassFieldSignature<'hir>],
    ) -> HirResult<HirClassConstructor<'hir>> {
        if constructor.is_none() {
            let mut params: Vec<&'hir HirFunctionParameterSignature<'hir>> = Vec::new();
            for field in fields.iter() {
                let ty = field.ty;
                let name = field.name;
                params.push(self.arena.intern(HirFunctionParameterSignature {
                    span: field.span.clone(),
                    name,
                    name_span: field.name_span.clone(),
                    ty,
                    ty_span: field.ty_span.clone(),
                }));
            }
            let mut type_params: Vec<&'hir HirTypeParameterItemSignature<'hir>> = Vec::new();
            for type_param in params.iter() {
                type_params.push(self.arena.intern(HirTypeParameterItemSignature {
                    span: type_param.span.clone(),
                    name: type_param.name,
                    name_span: type_param.name_span.clone(),
                }));
            }
            let hir = HirClassConstructor {
                span: logos::Span::default(),
                params,
                type_params,
                body: HirBlock {
                    span: logos::Span::default(),
                    statements: Vec::new(),
                },
            };
            return Ok(hir);
        }
        let constructor = constructor.unwrap();
        let mut params: Vec<&'hir HirFunctionParameterSignature<'hir>> = Vec::new();
        for param in constructor.args.iter() {
            let ty = self.visit_ty(param.ty)?;
            let name = self.arena.names().get(param.name.name);
            params.push(self.arena.intern(HirFunctionParameterSignature {
                span: param.span.clone(),
                name,
                name_span: param.name.span.clone(),
                ty,
                ty_span: param.ty.span(),
            }));
        }

        let mut type_params: Vec<&'hir HirTypeParameterItemSignature<'hir>> = Vec::new();
        for type_param in params.iter() {
            type_params.push(self.arena.intern(HirTypeParameterItemSignature {
                span: type_param.span.clone(),
                name: type_param.name,
                name_span: type_param.name_span.clone(),
            }));
        }
        let hir = HirClassConstructor {
            span: constructor.span.clone(),
            params,
            type_params,
            body: self.visit_block(constructor.body)?,
        };
        Ok(hir)
    }

    fn visit_destructor(
        &self,
        destructor: Option<&'ast AstDestructor<'ast>>,
    ) -> HirResult<HirClassConstructor<'hir>> {
        if destructor.is_none() {
            let hir = HirClassConstructor {
                span: logos::Span::default(),
                params: Vec::new(),
                type_params: Vec::new(),
                body: HirBlock {
                    span: logos::Span::default(),
                    statements: Vec::new(),
                },
            };
            return Ok(hir);
        }
        let destructor = destructor.unwrap();
        let mut params: Vec<&'hir HirFunctionParameterSignature<'hir>> = Vec::new();
        for param in destructor.args.iter() {
            let ty = self.visit_ty(param.ty)?;
            let name = self.arena.names().get(param.name.name);
            params.push(self.arena.intern(HirFunctionParameterSignature {
                span: param.span.clone(),
                name,
                name_span: param.name.span.clone(),
                ty,
                ty_span: param.ty.span(),
            }));
        }

        let mut type_params: Vec<&'hir HirTypeParameterItemSignature<'hir>> = Vec::new();
        for type_param in params.iter() {
            type_params.push(self.arena.intern(HirTypeParameterItemSignature {
                span: type_param.span.clone(),
                name: type_param.name,
                name_span: type_param.name_span.clone(),
            }));
        }
        let hir = HirClassConstructor {
            span: destructor.span.clone(),
            params,
            type_params,
            body: self.visit_block(destructor.body)?,
        };
        Ok(hir)
    }

    //This needs to be generalized
    fn visit_import(&self, node: &'ast AstImport<'ast>) -> HirResult<HirModule<'hir>> {
        match node.path.split("/").last().unwrap() {
            "io" => {
                let ast: AstProgram<'ast> = parse(
                    "atlas_stdlib/io.atlas",
                    self.ast_arena,
                    IO_ATLAS.to_string(),
                )
                .unwrap();
                let allocated_ast = self.ast_arena.alloc(ast);
                let hir = self.arena.intern(AstSyntaxLoweringPass::<'ast, 'hir>::new(
                    self.arena,
                    allocated_ast,
                    self.ast_arena,
                    IO_ATLAS.to_string(),
                ));
                let mut lower = hir.lower()?;

                let hir_import: &'hir HirImport<'_> = self.arena.intern(HirImport {
                    span: node.span.clone(),
                    path: node.path,
                    path_span: node.span.clone(),
                    alias: None,
                    alias_span: None,
                });

                lower.body.imports.push(hir_import);

                Ok(lower)
            }
            "math" => {
                let ast: AstProgram<'ast> = parse(
                    "atlas_stdlib/math.atlas",
                    self.ast_arena,
                    MATH_ATLAS.to_string(),
                )
                .unwrap();
                let allocated_ast = self.ast_arena.alloc(ast);
                let hir = self.arena.intern(AstSyntaxLoweringPass::<'ast, 'hir>::new(
                    self.arena,
                    allocated_ast,
                    self.ast_arena,
                    MATH_ATLAS.to_string(),
                ));
                let mut lower = hir.lower()?;
                let hir_import: &'hir HirImport<'_> = self.arena.intern(HirImport {
                    span: node.span.clone(),
                    path: node.path,
                    path_span: node.span.clone(),
                    alias: None,
                    alias_span: None,
                });

                lower.body.imports.push(hir_import);
                Ok(lower)
            }
            "file" => {
                let ast: AstProgram<'ast> = parse(
                    "atlas_stdlib/fs.atlas",
                    self.ast_arena,
                    FILE_ATLAS.to_string(),
                )
                .unwrap();
                let allocated_ast = self.ast_arena.alloc(ast);
                let hir = self.arena.intern(AstSyntaxLoweringPass::<'ast, 'hir>::new(
                    self.arena,
                    allocated_ast,
                    self.ast_arena,
                    FILE_ATLAS.to_string(),
                ));
                hir.lower()
            }
            "list" => {
                let ast: AstProgram<'ast> = parse(
                    "atlas_stdlib/list.atlas",
                    self.ast_arena,
                    ARRAY_ATLAS.to_string(),
                )
                .unwrap();
                let allocated_ast = self.ast_arena.alloc(ast);
                let hir = self.arena.intern(AstSyntaxLoweringPass::<'ast, 'hir>::new(
                    self.arena,
                    allocated_ast,
                    self.ast_arena,
                    ARRAY_ATLAS.to_string(),
                ));
                let mut lower = hir.lower()?;

                let hir_import: &'hir HirImport<'_> = self.arena.intern(HirImport {
                    span: node.span.clone(),
                    path: node.path,
                    path_span: node.span.clone(),
                    alias: None,
                    alias_span: None,
                });

                lower.body.imports.push(hir_import);
                Ok(lower)
            }
            "string" => {
                let ast: AstProgram<'ast> = parse(
                    "atlas_stdlib/string.atlas",
                    self.ast_arena,
                    STRING_ATLAS.to_string(),
                )
                .unwrap();
                let allocated_ast = self.ast_arena.alloc(ast);
                let hir = self.arena.intern(AstSyntaxLoweringPass::<'ast, 'hir>::new(
                    self.arena,
                    allocated_ast,
                    self.ast_arena,
                    STRING_ATLAS.to_string(),
                ));

                let mut lower = hir.lower()?;

                let hir_import: &'hir HirImport<'_> = self.arena.intern(HirImport {
                    span: node.span.clone(),
                    path: node.path,
                    path_span: node.span.clone(),
                    alias: None,
                    alias_span: None,
                });

                lower.body.imports.push(hir_import);

                Ok(lower)
            }
            "time" => {
                let ast: AstProgram<'ast> = parse(
                    "atlas_stdlib/time.atlas",
                    self.ast_arena,
                    STRING_ATLAS.to_string(),
                )
                .unwrap();
                let allocated_ast = self.ast_arena.alloc(ast);
                let hir = self.arena.intern(AstSyntaxLoweringPass::<'ast, 'hir>::new(
                    self.arena,
                    allocated_ast,
                    self.ast_arena,
                    IO_ATLAS.to_string(),
                ));
                hir.lower()
            }
            _ => Err(HirError::UnsupportedStatement(UnsupportedStatement {
                span: SourceSpan::new(
                    SourceOffset::from(node.span.start),
                    node.span.end - node.span.start,
                ),
                stmt: format!("{:?}", node),
                src: self.src.clone(),
            })),
        }
    }

    fn visit_block(&self, node: &'ast AstBlock<'ast>) -> HirResult<HirBlock<'hir>> {
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

    fn visit_stmt(&self, node: &'ast AstStatement<'ast>) -> HirResult<HirStatement<'hir>> {
        match node {
            AstStatement::While(w) => {
                let condition = self.visit_expr(w.condition)?;
                let body = self.visit_block(w.body)?;
                let hir = HirStatement::While(HirWhileStmt {
                    span: node.span(),
                    condition,
                    body,
                });
                Ok(hir)
            }
            AstStatement::Const(c) => {
                let name = self.arena.names().get(c.name.name);
                if !name.is_snake_case() {
                    eprintln!("Warning: {} is not snake case", name);
                    eprintln!(
                        "Try using snake_case for variable names e.g. {}",
                        name.to_snake_case()
                    );
                }
                let ty = self.visit_ty(c.ty)?;

                let value = self.visit_expr(c.value)?;
                let hir = HirStatement::Const(HirLetStmt {
                    span: node.span(),
                    name,
                    name_span: c.name.span.clone(),
                    ty: Some(ty),
                    ty_span: Some(c.ty.span()),
                    value,
                });
                Ok(hir)
            }
            AstStatement::Let(l) => {
                let name = self.arena.names().get(l.name.name);
                if !name.is_snake_case() {
                    eprintln!("Warning: {} is not snake case", name);
                    eprintln!(
                        "Try using snake_case for variable names e.g. {}",
                        name.to_snake_case()
                    );
                }
                let ty = l.ty.map(|ty| self.visit_ty(ty)).transpose()?;

                let value = self.visit_expr(l.value)?;
                let hir = HirStatement::Let(HirLetStmt {
                    span: node.span(),
                    name,
                    name_span: l.name.span.clone(),
                    ty,
                    ty_span: ty.map(|_| l.ty.unwrap().span()),
                    value,
                });
                Ok(hir)
            }
            AstStatement::IfElse(i) => {
                let condition = self.visit_expr(i.condition)?;
                let then_branch = self.visit_block(i.body)?;
                //If you don't type, the compiler will use it as an "Option<&mut HirBlock<'hir>>"
                //Which is dumb asf
                let else_branch: Option<HirBlock<'hir>> = match i.else_body {
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
            AstStatement::Break(b) => {
                let hir = HirStatement::Break(b.span.clone());
                Ok(hir)
            }
            AstStatement::Continue(c) => {
                let hir = HirStatement::Continue(c.span.clone());
                Ok(hir)
            }
            //The parser really need a bit of work
            AstStatement::Return(r) => {
                let expr = self.visit_expr(r.value)?;
                let hir = HirStatement::Return(HirReturn {
                    span: node.span(),
                    ty: expr.ty(),
                    value: expr,
                });
                Ok(hir)
            }
            AstStatement::Expr(e) => {
                let expr = self.visit_expr(e)?;
                let hir = HirStatement::Expr(HirExprStmt {
                    span: node.span(),
                    expr,
                });
                Ok(hir)
            }
            _ => Err(super::error::HirError::UnsupportedStatement(
                UnsupportedStatement {
                    span: SourceSpan::new(
                        SourceOffset::from(node.span().start),
                        node.span().end - node.span().start,
                    ),
                    stmt: format!("{:?}", node),
                    src: self.src.clone(),
                },
            )),
        }
    }

    fn visit_expr(&self, node: &'ast AstExpr<'ast>) -> HirResult<HirExpr<'hir>> {
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
                    op_span: 0..0,
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
            AstExpr::NewArray(a) => {
                let hir = HirExpr::NewArray(HirNewArrayExpr {
                    span: node.span(),
                    ty: self.visit_ty(a.ty)?,
                    size: Box::new(self.visit_expr(a.size)?),
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
            AstExpr::NewObj(obj) => {
                let hir = HirExpr::NewObj(HirNewObjExpr {
                    span: node.span(),
                    ty: self
                        .arena
                        .types()
                        .get_named_ty(obj.ty.name, obj.ty.span.clone()),
                    args: obj
                        .args
                        .iter()
                        .map(|arg| self.visit_expr(arg))
                        .collect::<HirResult<Vec<_>>>()?,
                    args_ty: Vec::new(),
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
                    AstLiteral::SelfLiteral(_) => HirExpr::SelfLiteral(HirSelfLiteral {
                        span: l.span(),
                        ty: self.arena.types().get_uninitialized_ty(),
                    }),
                    AstLiteral::None(_) => HirExpr::NoneLiteral(HirNoneLiteral {
                        span: l.span(),
                        ty: self.arena.types().get_none_ty(),
                    }),
                    AstLiteral::Char(c) => HirExpr::CharLiteral(HirCharLiteralExpr {
                        span: l.span(),
                        value: c.value,
                        ty: self.arena.types().get_char_ty(),
                    }),
                    AstLiteral::Unit(_) => HirExpr::UnitLiteral(HirUnitLiteralExpr {
                        span: l.span(),
                        ty: self.arena.types().get_unit_ty(),
                    }),
                    AstLiteral::String(s) => HirExpr::StringLiteral(HirStringLiteralExpr {
                        span: l.span(),
                        value: s.value,
                        ty: self.arena.types().get_str_ty(),
                    }),
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
            AstExpr::StaticAccess(s) => {
                let hir = HirExpr::StaticAccess(HirStaticAccessExpr {
                    span: node.span(),
                    target: Box::new(self.visit_identifier(s.target)?),
                    field: Box::new(HirIdentExpr {
                        name: self.arena.names().get(s.field.name),
                        span: s.field.span.clone(),
                        ty: self.arena.types().get_uninitialized_ty(),
                    }),
                    ty: self.arena.types().get_uninitialized_ty(),
                });
                Ok(hir)
            }
            AstExpr::FieldAccess(f) => {
                let hir = HirExpr::FieldAccess(HirFieldAccessExpr {
                    span: node.span(),
                    target: Box::new(self.visit_expr(f.target)?),
                    field: Box::new(HirIdentExpr {
                        name: self.arena.names().get(f.field.name),
                        span: f.field.span.clone(),
                        ty: self.arena.types().get_uninitialized_ty(),
                    }),
                    ty: self.arena.types().get_uninitialized_ty(),
                });
                Ok(hir)
            }

            _ => {
                //todo: if/else as an expression
                Err(HirError::UnsupportedExpr(UnsupportedExpr {
                    span: SourceSpan::new(
                        SourceOffset::from(node.span().start),
                        node.span().end - node.span().start,
                    ),
                    expr: format!("{:?}", node),
                    src: self.src.clone(),
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

    fn visit_func(&self, node: &'ast AstFunction<'ast>) -> HirResult<HirFunction<'hir>> {
        let type_parameters = node
            .args
            .iter()
            .map(|arg| self.visit_type_param_item(arg))
            .collect::<HirResult<Vec<_>>>();
        let ret_type_span = node.ret.span();
        let ret_type = self.visit_ty(node.ret)?;
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
        &self,
        node: &'ast AstObjField<'ast>,
    ) -> HirResult<&'hir HirFunctionParameterSignature<'hir>> {
        let name = self.arena.names().get(node.name.name);
        let ty = self.visit_ty(node.ty)?;

        let hir = self.arena.intern(HirFunctionParameterSignature {
            span: node.span.clone(),
            name,
            name_span: node.name.span.clone(),
            ty,
            ty_span: node.ty.span(),
        });
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

    fn visit_ty(&self, node: &'ast AstType<'ast>) -> HirResult<&'hir HirTy<'hir>> {
        let ty = match node {
            AstType::Boolean(_) => self.arena.types().get_boolean_ty(),
            AstType::Integer(_) => self.arena.types().get_integer64_ty(),
            AstType::Float(_) => self.arena.types().get_float64_ty(),
            AstType::Char(_) => self.arena.types().get_char_ty(),
            AstType::UnsignedInteger(_) => self.arena.types().get_uint64_ty(),
            AstType::Unit(_) => self.arena.types().get_unit_ty(),
            AstType::String(_) => self.arena.types().get_str_ty(),
            AstType::Null(_) => self.arena.types().get_none_ty(),
            AstType::Named(n) => {
                let name = self.arena.names().get(n.name.name);
                self.arena.types().get_named_ty(name, n.span.clone())
            }
            AstType::List(l) => {
                let ty = self.visit_ty(l.inner)?;
                self.arena.types().get_list_ty(ty)
            }
            AstType::Nullable(n) => {
                let ty = self.visit_ty(n.inner)?;
                self.arena.types().get_nullable_ty(ty)
            }
            AstType::ReadOnly(r) => {
                let ty = self.visit_ty(r.inner)?;
                self.arena.types().get_readonly_ty(ty)
            }
            //The self ty is replaced during the type checking phase
            AstType::SelfTy(_) => self.arena.types().get_uninitialized_ty(),
            _ => {
                return Err(HirError::UnsupportedExpr(UnsupportedExpr {
                    span: SourceSpan::new(
                        SourceOffset::from(node.span().start),
                        node.span().end - node.span().start,
                    ),
                    expr: format!("{:?}", node),
                    src: self.src.clone(),
                }));
            }
        };
        Ok(ty)
    }
}
