pub mod case;

use crate::atlas_c::atlas_frontend::parser::ast::{AstConstructor, AstDestructor, AstIdentifier, AstMethod, AstMethodModifier, AstNamedType, AstStruct};
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
use heck::{ToPascalCase, ToSnakeCase};
use logos::Span;
use miette::{SourceOffset, SourceSpan};
use std::cmp::PartialEq;
use std::collections::BTreeMap;

const FILE_ATLAS: &str = include_str!("../../../atlas_lib/std/fs.atlas");
const IO_ATLAS: &str = include_str!("../../../atlas_lib/std/io.atlas");
const ARRAY_ATLAS: &str = include_str!("../../../atlas_lib/std/array.atlas");
const MATH_ATLAS: &str = include_str!("../../../atlas_lib/std/math.atlas");
const STRING_ATLAS: &str = include_str!("../../../atlas_lib/std/string.atlas");

use crate::atlas_c::atlas_hir::error::HirError::UnknownType;
use crate::atlas_c::atlas_hir::error::{NonConstantValueError, NotEnoughGenericsError, UnknownFileImportError, UnknownTypeError, UnsupportedTypeError};
use crate::atlas_c::atlas_hir::expr::{HirCastExpr, HirCharLiteralExpr, HirConstructorExpr, HirDeleteExpr, HirFieldAccessExpr, HirFieldInit, HirIndexingExpr, HirListLiteralExpr, HirNewArrayExpr, HirNewObjExpr, HirNoneLiteral, HirStaticAccessExpr, HirStringLiteralExpr, HirThisLiteral, HirUnitLiteralExpr};
use crate::atlas_c::atlas_hir::item::{HirStruct, HirStructConstructor, HirStructMethod};
use crate::atlas_c::atlas_hir::signature::{ConstantValue, HirStructConstantSignature, HirStructConstructorSignature, HirStructFieldSignature, HirStructMethodModifier, HirStructMethodSignature, HirStructSignature, HirVisibility};
use crate::atlas_c::atlas_hir::syntax_lowering_pass::case::Case;
use crate::atlas_c::atlas_hir::ty::{HirGenericTy, HirListTy, HirNamedTy};
use crate::atlas_c::atlas_hir::{
    arena::HirArena, error::{HirError, HirResult, UnsupportedExpr, UnsupportedStatement}, expr::{
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
    HirImport,
    HirModule,
    HirModuleBody,
};

pub struct AstSyntaxLoweringPass<'ast, 'hir> {
    arena: &'hir HirArena<'hir>,
    ast: &'ast AstProgram<'ast>,
    ast_arena: &'ast AstArena<'ast>,
    //source code
    src: String,
    generic_structs_pool: BTreeMap<&'hir str, HirStruct<'hir>>,
    module_body: HirModuleBody<'hir>,
    module_signature: HirModuleSignature<'hir>,
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
            generic_structs_pool: BTreeMap::new(),
            module_body: HirModuleBody::default(),
            module_signature: HirModuleSignature::default(),
        }
    }
}

impl<'ast, 'hir> AstSyntaxLoweringPass<'ast, 'hir> {
    pub fn lower(&mut self) -> HirResult<&'hir mut HirModule<'hir>> {
        let mut items = Vec::new();
        for item in self.ast.items {
            items.push(self.visit_item(item)?);
        }
        Ok(self.arena.intern(HirModule {
            //TODO: Clone removal
            body: self.module_body.clone(),
            signature: self.module_signature.clone(),
        }))
    }
    pub fn visit_item(
        &mut self,
        item: &'ast AstItem<'ast>,
    ) -> HirResult<()> {
        match item {
            AstItem::Function(f) => {
                let fun = self.visit_func(f)?;
                let name = self.arena.names().get(f.name.name);
                if !name.is_snake_case() {
                    eprintln!("Warning: {} is not snake case", name);
                    eprintln!(
                        "Try using snake_case for function names e.g. {}",
                        name.to_snake_case()
                    );
                }
                self.module_signature.functions.insert(name, fun.signature);
                self.module_body.functions.insert(name, fun);
            }
            AstItem::Struct(c) => {
                let class = self.visit_struct(c)?;
                //Only insert non generics structs into the module signature
                if class.signature.generics.is_empty() {
                    self.module_signature.structs.insert(class.name, self.arena.intern(class.signature.clone()));
                    self.module_body.structs.insert(class.name, class);
                } else {
                    self.generic_structs_pool.insert(class.name, class);
                }
            }
            AstItem::Import(i) => {
                let hir = self.visit_import(i)?;
                let allocated_hir: &'hir HirModule<'hir> = self.arena.intern(hir);
                for (name, signature) in allocated_hir.signature.functions.iter() {
                    self.module_signature.functions.insert(name, *signature);
                }
                allocated_hir.body.imports.iter().for_each(|i| {
                    self.module_body.imports.push(i);
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
                let ty = self.visit_ty(e.ret)?.clone();

                let mut params: Vec<HirFunctionParameterSignature<'hir>> = Vec::new();
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
                    span: e.span.clone(),
                    vis: e.vis.into(),
                    params,
                    generics,
                    type_params,
                    return_ty: ty,
                    return_ty_span: Some(e.ret.span()),
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
        };
        let destructor_signature = HirStructConstructorSignature {
            span: node.span.clone(),
            params: destructor.params.to_vec(),
            type_params: destructor.type_params.to_vec(),
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

    fn visit_constructor(&mut self, constructor: Option<&'ast AstConstructor<'ast>>, fields: &[HirStructFieldSignature<'hir>]) -> HirResult<HirStructConstructor<'hir>> {
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
        };
        Ok(hir)
    }

    fn visit_destructor(&mut self, destructor: Option<&'ast AstDestructor<'ast>>) -> HirResult<HirStructConstructor<'hir>> {
        if destructor.is_none() {
            let hir = HirStructConstructor {
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
        };
        Ok(hir)
    }

    //This needs to be generalized
    fn visit_import(&mut self, node: &'ast AstImport<'ast>) -> HirResult<&'hir HirModule<'hir>> {
        let file_name = node.path.split("/").last().unwrap();
        match file_name {
            "io" => {
                let ast: AstProgram<'ast> = parse(
                    "atlas_lib/std/io.atlas",
                    self.ast_arena,
                    IO_ATLAS.to_string(),
                )
                    .unwrap();
                let allocated_ast: &'ast AstProgram = self.ast_arena.alloc(ast);
                let mut ast_lowering_pass = AstSyntaxLoweringPass::<'ast, 'hir>::new(
                    self.arena,
                    allocated_ast,
                    self.ast_arena,
                    IO_ATLAS.to_owned(),
                );
                let mut hir = ast_lowering_pass.lower()?;
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

                Ok(new_hir)
            }
            "math" => {
                let ast: AstProgram<'ast> = parse(
                    "atlas_lib/std/io/math.atlas",
                    self.ast_arena,
                    MATH_ATLAS.to_string(),
                )
                    .unwrap();
                let allocated_ast = self.ast_arena.alloc(ast);
                let mut hir = AstSyntaxLoweringPass::<'ast, 'hir>::new(
                    self.arena,
                    allocated_ast,
                    self.ast_arena,
                    MATH_ATLAS.to_string(),
                ).lower()?;
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

                Ok(new_hir)
            }
            "file" => {
                let ast: AstProgram<'ast> = parse(
                    "atlas_lib/std/io/fs.atlas",
                    self.ast_arena,
                    FILE_ATLAS.to_string(),
                )
                    .unwrap();
                let allocated_ast = self.ast_arena.alloc(ast);
                let mut hir = AstSyntaxLoweringPass::<'ast, 'hir>::new(
                    self.arena,
                    allocated_ast,
                    self.ast_arena,
                    FILE_ATLAS.to_string(),
                ).lower()?;

                let hir = self.arena.intern(hir);

                Ok(hir)
            }
            "list" => {
                let ast: AstProgram<'ast> = parse(
                    "atlas_lib/std/io/list.atlas",
                    self.ast_arena,
                    ARRAY_ATLAS.to_string(),
                )
                    .unwrap();
                let allocated_ast = self.ast_arena.alloc(ast);
                let mut hir = AstSyntaxLoweringPass::<'ast, 'hir>::new(
                    self.arena,
                    allocated_ast,
                    self.ast_arena,
                    ARRAY_ATLAS.to_string(),
                ).lower()?;
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

                Ok(new_hir)
            }
            "string" => {
                let ast: AstProgram<'ast> = parse(
                    "atlas_lib/std/io/string.atlas",
                    self.ast_arena,
                    STRING_ATLAS.to_string(),
                )
                    .unwrap();
                let allocated_ast = self.ast_arena.alloc(ast);
                let mut hir = AstSyntaxLoweringPass::<'ast, 'hir>::new(
                    self.arena,
                    allocated_ast,
                    self.ast_arena,
                    STRING_ATLAS.to_string(),
                ).lower()?;
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

                Ok(new_hir)
            }
            "time" => {
                let ast: AstProgram<'ast> = parse(
                    "atlas_lib/std/io/time.atlas",
                    self.ast_arena,
                    STRING_ATLAS.to_string(),
                )
                    .unwrap();
                let allocated_ast = self.ast_arena.alloc(ast);
                let hir = AstSyntaxLoweringPass::<'ast, 'hir>::new(
                    self.arena,
                    allocated_ast,
                    self.ast_arena,
                    IO_ATLAS.to_string(),
                ).lower()?;

                let hir = self.arena.intern(hir);

                Ok(hir)
            }
            _ => Err(HirError::UnknownFileImport(UnknownFileImportError {
                span: SourceSpan::new(
                    SourceOffset::from(node.span.start),
                    node.span.end - node.span.start,
                ),
                file_name: file_name.to_string(),
                src: self.src.clone(),
            })),
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
            _ => Err(HirError::UnsupportedStatement(
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
                let ty = self.visit_ty(obj.ty)?;
                let hir = HirExpr::NewObj(HirNewObjExpr {
                    span: node.span(),
                    ty: self.whatever_for_now(ty, obj.span.clone())?,
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
                    AstLiteral::None(_) => HirExpr::NoneLiteral(HirNoneLiteral {
                        span: l.span(),
                        ty: self.arena.types().get_none_ty(),
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
                    AstLiteral::String(ast_string) => HirExpr::StringLiteral(HirStringLiteralExpr {
                        span: l.span(),
                        value: self.arena.intern(ast_string.value.to_owned()),
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
            AstExpr::Constructor(ast_constructor) => {
                let fields = ast_constructor.fields.iter().map(|f| Ok(HirFieldInit {
                    ty: self.arena.types().get_uninitialized_ty(),
                    name: Box::new(HirIdentExpr {
                        ty: self.arena.types().get_uninitialized_ty(),
                        span: f.name.span.clone(),
                        name: self.arena.names().get(f.name.name),
                    }),
                    span: f.span.clone(),
                    value: Box::new(self.visit_expr(f.value)?),
                })).collect::<HirResult<Vec<_>>>()?;
                let constructor_name: &'hir mut str = self.arena.intern(ast_constructor.ty.name.to_owned());
                let hir = HirExpr::ConstructorExpr(HirConstructorExpr {
                    name: self.arena.names().get(ast_constructor.ty.name),
                    span: node.span(),
                    fields,
                    ty: self.arena.types().get_named_ty(constructor_name, ast_constructor.ty.span.clone()),
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
                    expr: format!("2{:?}", node),
                    src: self.src.clone(),
                }))
            }
        }
    }

    //TODO: Find a better name for this function. It's responsible for monomorphizing or not every `new Struct<T>()` expression.
    //TODO: e.g. if `T` is also a generic type, we can't monomorphize it yet.
    //What should I name it?
    fn whatever_for_now(&mut self, ty: &'hir HirTy<'hir>, span: Span) -> HirResult<&'hir HirTy<'hir>> {
        if let HirTy::Generic(g) = ty {
            let mut can_monomorphize = true;
            for inner_ty in g.inner.iter() {
                if let HirTy::Named(n) = inner_ty {
                    //If the inner type is a single letter, we assume it's a generic type parameter
                    can_monomorphize = n.name.len() != 1;
                } else if let HirTy::Generic(g) = inner_ty {
                    self.whatever_for_now(inner_ty, span.clone())?;
                }
                if !can_monomorphize {
                    break;
                }
            }
            if can_monomorphize {
                let monomorphized_ty = self.monomorphize_struct(g, &span)?;
                Ok(monomorphized_ty)
            } else {
                Ok(ty)
            }
        } else {
            Ok(ty)
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
            AstType::Generic(g) => {
                let inner_types = g
                    .inner_types
                    .iter()
                    .map(|inner_ast_ty| self.visit_ty(inner_ast_ty))
                    .collect::<HirResult<Vec<_>>>()?;
                let name = self.arena.names().get(g.name.name);
                let ty = self.arena.types().get_generic_ty(name, inner_types);
                self.whatever_for_now(ty, node.span())?
            }
            //The self ty is replaced during the type checking phase
            AstType::ThisTy(_) => self.arena.types().get_uninitialized_ty(),
            _ => {
                return Err(HirError::UnsupportedType(UnsupportedTypeError {
                    span: SourceSpan::new(
                        SourceOffset::from(node.span().start),
                        node.span().end - node.span().start,
                    ),
                    ty: format!("{:?}", node),
                    src: self.src.clone(),
                }));
            }
        };
        Ok(ty)
    }
}

impl<'ast, 'hir> AstSyntaxLoweringPass<'ast, 'hir> {
    /// Returns the mangled type name after monomorphization.
    pub fn monomorphize_struct(&mut self, actual_type: &'hir HirGenericTy<'hir>, span: &Span) -> HirResult<&'hir HirTy<'hir>> {
        let mangled_name = self.mangle_generic_struct_name(actual_type);
        if self.module_body.structs.contains_key(&mangled_name) {
            //Already monomorphized
            return Ok(self.arena.types().get_named_ty(mangled_name, span.clone()));
        }

        let base_name = actual_type.name;
        let template = match self.generic_structs_pool.contains_key(base_name) {
            true => self.generic_structs_pool.get(base_name).unwrap(),
            false => {
                return Err(UnknownType(UnknownTypeError {
                    name: base_name.to_string(),
                    span: SourceSpan::new(
                        SourceOffset::from(span.start),
                        span.end - span.start,
                    ),
                    src: self.src.clone(),
                }));
            }
        };

        let mut new_struct = template.clone();
        //Collect generic names
        let generic_names = template.signature.generics.clone();
        if generic_names.len() != actual_type.inner.len() {
            let declaration_span = template.name_span.clone();
            return Err(HirError::NotEnoughGenerics(NotEnoughGenericsError {
                ty_name: base_name.to_string(),
                expected: generic_names.len(),
                found: actual_type.inner.len(),
                declaration_span: SourceSpan::new(
                    SourceOffset::from(declaration_span.start),
                    declaration_span.end - declaration_span.start,
                ),
                error_span: SourceSpan::new(
                    SourceOffset::from(span.start),
                    span.end - span.start,
                ),
                src: self.src.clone(),
            }));
        }

        //TODO: Right now we can't have nested generics (i.e. Box<T> -> Vector<Box<int64>>)
        //TODO: Add a bunch of helper functions for that
        //TODO: NB: This would require calling the "monomorphize_struct" function recursively.
        //TODO: I'll do it once the basic monomorphization is working and stable enough.

        for (_, field_signature) in new_struct.signature.fields.iter_mut() {
            for (i, generic_name) in generic_names.iter().enumerate() {
                field_signature.ty = self.change_inner_type(field_signature.ty, generic_name, actual_type.inner[i].clone());
            }
        }

        for (i, arg) in new_struct.signature.constructor.params.clone().iter().enumerate() {
            for (j, generic_name) in generic_names.iter().enumerate() {
                let new_ty = self.change_inner_type(arg.ty, generic_name, actual_type.inner[j].clone());
                let new_ty = if let HirTy::Generic(g) = new_ty {
                    self.arena.intern(HirTy::Named(HirNamedTy {
                        name: self.mangle_generic_struct_name(self.arena.intern(g)),
                        span: arg.span.clone(),
                    }))
                } else {
                    new_ty
                };
                if new_ty != arg.ty {
                    new_struct.signature.constructor.params[i] = HirFunctionParameterSignature {
                        name: arg.name,
                        name_span: arg.name_span.clone(),
                        span: arg.span.clone(),
                        ty: new_ty,
                        ty_span: arg.ty_span.clone(),
                    };
                }
            }
        }

        for (_, func) in new_struct.signature.methods.iter_mut() {
            //args:
            for param in func.params.iter_mut() {
                for (i, generic_name) in generic_names.iter().enumerate() {
                    let type_to_change = self.arena.intern(param.ty.clone());
                    let new_ty = self.change_inner_type(type_to_change, generic_name, actual_type.inner[i].clone()).clone();
                    let new_ty = if let HirTy::Generic(g) = new_ty {
                        HirTy::Named(HirNamedTy {
                            name: self.mangle_generic_struct_name(self.arena.intern(g)),
                            span: param.span.clone(),
                        })
                    } else {
                        new_ty
                    };
                    param.ty = self.arena.intern(new_ty);
                }
            }

            //ret_type:
            for (i, generic_name) in generic_names.iter().enumerate() {
                let type_to_change = self.arena.intern(func.return_ty.clone());
                func.return_ty = self.change_inner_type(type_to_change, generic_name, actual_type.inner[i].clone()).clone();
            }
            func.generics = None;
        }

        //At this point the signature is fully monomorphized, but the actual struct itself isn't.
        //We still need to change the new_struct.fields/new_struct.methods/new_struct.constructor

        for (i, field) in new_struct.fields.clone().iter().enumerate() {
            new_struct.fields[i] = new_struct.signature.fields.get(field.name).unwrap().clone();
        }
        for (i, method) in new_struct.methods.clone().iter().enumerate() {
            new_struct.methods[i].signature = self.arena.intern(new_struct.signature.methods.get(method.name).unwrap().clone());
        }
        for (i, _) in new_struct.constructor.params.clone().iter().enumerate() {
            new_struct.constructor.params[i] = new_struct.signature.constructor.params[i].clone();
        }

        //And lastly, we need to update the statements inside the constructor and methods to reflect the new types.
        //It's mostly changing the name of every `new Struct<Generic>` to `new __atlas77__Struct__actual_types`

        let type_to_change: Vec<(&'hir str, &'hir HirTy<'hir>)> = generic_names
            .iter()
            .enumerate()
            .map(|(i, generic_name)| (*generic_name, self.arena.intern(actual_type.inner[i].clone()) as &'hir HirTy<'hir>))
            .collect::<Vec<(&'hir str, &'hir HirTy<'hir>)>>();

        for method in new_struct.methods.iter_mut() {
            for statement in method.body.statements.iter_mut() {
                self.monomorphize_statement(statement, type_to_change.clone())?;
            }
        }

        new_struct.signature.generics = vec![];
        new_struct.name = mangled_name;
        new_struct.signature.name = mangled_name;

        self.module_signature.structs.insert(mangled_name, self.arena.intern(new_struct.signature.clone()));
        self.module_body.structs.insert(mangled_name, new_struct);

        Ok(self.arena.types().get_named_ty(mangled_name, span.clone()))
    }

    fn monomorphize_statement(&mut self, statement: &mut HirStatement<'hir>, types_to_change: Vec<(&'hir str, &'hir HirTy<'hir>)>) -> HirResult<()> {
        match statement {
            HirStatement::Expr(expr_stmt) => {
                self.monomorphize_expression(&mut expr_stmt.expr, types_to_change)?;
            }
            HirStatement::Let(let_stmt) => {
                self.monomorphize_expression(&mut let_stmt.value, types_to_change)?;
            }
            HirStatement::While(while_stmt) => {
                for stmt in while_stmt.body.statements.iter_mut() {
                    self.monomorphize_statement(stmt, types_to_change.clone())?;
                }
                self.monomorphize_expression(&mut while_stmt.condition, types_to_change)?;
            }
            HirStatement::IfElse(if_else_stmt) => {
                for stmt in if_else_stmt.then_branch.statements.iter_mut() {
                    self.monomorphize_statement(stmt, types_to_change.clone())?;
                }
                if let Some(else_branch) = &mut if_else_stmt.else_branch {
                    for stmt in else_branch.statements.iter_mut() {
                        self.monomorphize_statement(stmt, types_to_change.clone())?;
                    }
                }
                self.monomorphize_expression(&mut if_else_stmt.condition, types_to_change)?;
            }
            HirStatement::Return(return_stmt) => {
                self.monomorphize_expression(&mut return_stmt.value, types_to_change)?;
            }
            _ => {}
        }
        Ok(())
    }

    fn monomorphize_expression(&mut self, expr: &mut HirExpr<'hir>, types_to_change: Vec<(&'hir str, &'hir HirTy<'hir>)>) -> HirResult<()> {
        match expr {
            HirExpr::NewObj(new_obj_expr) => {
                if let HirTy::Generic(_) = new_obj_expr.ty {
                    let monomorphized_ty = self.swap_generic_types_in_ty(new_obj_expr.ty, types_to_change.clone());
                    let monomorphized_ty = if let HirTy::Generic(g) = monomorphized_ty {
                        self.arena.intern(HirTy::Named(HirNamedTy {
                            name: self.mangle_generic_struct_name(g),
                            span: new_obj_expr.span.clone(),
                        }))
                    } else {
                        monomorphized_ty
                    };
                    new_obj_expr.ty = monomorphized_ty;
                }
            }
            HirExpr::Indexing(idx_expr) => {
                self.monomorphize_expression(&mut idx_expr.target, types_to_change.clone())?;
                self.monomorphize_expression(&mut idx_expr.index, types_to_change)?;
            }
            HirExpr::Assign(assign_expr) => {
                self.monomorphize_expression(&mut assign_expr.lhs, types_to_change.clone())?;
                self.monomorphize_expression(&mut assign_expr.rhs, types_to_change)?;
            }
            HirExpr::Unary(unary_expr) => {
                self.monomorphize_expression(&mut unary_expr.expr, types_to_change)?;
            }
            HirExpr::HirBinaryOp(binary_expr) => {
                self.monomorphize_expression(&mut binary_expr.lhs, types_to_change.clone())?;
                self.monomorphize_expression(&mut binary_expr.rhs, types_to_change)?;
            }
            HirExpr::Call(call_expr) => {
                for arg in call_expr.args.iter_mut() {
                    self.monomorphize_expression(arg, types_to_change.clone())?;
                }
                self.monomorphize_expression(&mut call_expr.callee, types_to_change)?;
            }
            HirExpr::Casting(casting_expr) => {
                self.monomorphize_expression(&mut casting_expr.expr, types_to_change)?;
            }
            HirExpr::Delete(delete_expr) => {
                self.monomorphize_expression(&mut delete_expr.expr, types_to_change)?;
            }
            HirExpr::ListLiteral(list_expr) => {
                for item in list_expr.items.iter_mut() {
                    self.monomorphize_expression(item, types_to_change.clone())?;
                }
            }
            HirExpr::NewArray(new_array_expr) => {
                if let HirTy::Generic(g) = new_array_expr.ty {
                    match self.monomorphize_struct(g, &new_array_expr.span) {
                        Ok(monomorphized_ty) => {
                            new_array_expr.ty = monomorphized_ty;
                        }
                        Err(e) => {
                            eprintln!("Error during monomorphization: {:?}", e);
                        }
                    }
                }
                self.monomorphize_expression(&mut new_array_expr.size, types_to_change)?;
            }
            _ => {}
        }

        Ok(())
    }

    //This function swaps generic types in a given type according to the provided mapping.
    //It does not mangle the name, it just replaces the generic types with the actual types.
    fn swap_generic_types_in_ty(&self, ty: &'hir HirTy<'hir>, types_to_change: Vec<(&'hir str, &'hir HirTy<'hir>)>) -> &'hir HirTy<'hir> {
        println!("Swapping in type: {}. types_to_change: {:?}", ty, types_to_change);
        match ty {
            HirTy::Named(n) => {
                for (generic_name, actual_ty) in types_to_change.iter() {
                    if n.name == *generic_name {
                        return actual_ty;
                    }
                }
                ty
            }
            HirTy::List(l) => {
                let new_inner = self.swap_generic_types_in_ty(l.inner, types_to_change);
                self.arena.intern(HirTy::List(HirListTy {
                    inner: new_inner,
                }))
            }
            HirTy::Generic(g) => {
                let new_inner_types: Vec<HirTy<'hir>> = g
                    .inner
                    .iter()
                    .map(|inner_ty| self.swap_generic_types_in_ty(inner_ty, types_to_change.clone()).clone())
                    .collect();
                self.arena.intern(HirTy::Generic(HirGenericTy {
                    name: g.name,
                    inner: new_inner_types,
                }))
            }
            _ => ty,
        }
    }

    /// Produce a stable mangled name for a generic instantiation.
    ///
    /// Format: __atlas77__<base_name>__<type1>_<type2>_..._<typeN>
    #[inline]
    fn mangle_generic_struct_name(&self, generic: &'hir HirGenericTy<'hir>) -> &'hir str {
        let parts: Vec<String> = generic
            .inner
            .iter()
            .map(|t| {
                match t {
                    HirTy::Generic(g) => self.mangle_generic_struct_name(g).to_string(),
                    _ => format!("{}", t),
                }
            })
            .collect();
        let name = format!("__atlas77__struct__{}__{}", generic.name, parts.join("_"));
        self.arena.intern(name)
    }

    /// Compute a stable mangled name for a monomorphized function given its base name
    /// and the actual type arguments.
    #[inline]
    fn mangle_function_name(&self, base_name: &str, actual_tys: &[&'hir HirTy<'hir>]) -> String {
        let parts: Vec<String> = actual_tys
            .iter()
            .map(|t| format!("{}", t))
            .collect();
        format!("__atlas77__fun__{}__{}", base_name, parts.join("_"))
    }

    /// Add a new struct signature to the module signature.
    ///
    /// The name of the new struct will be `__atlas77__StructType_actual_type_names`, so it's actually mangled.

    /// Helper function to change the inner type of generic type recursively if matches.
    fn change_inner_type(&self, type_to_change: &'hir HirTy<'hir>, generic_name: &'hir str, new_type: HirTy<'hir>) -> &'hir HirTy<'hir> {
        match type_to_change {
            HirTy::Named(n) => {
                if n.name == generic_name {
                    self.arena.intern(new_type)
                } else {
                    type_to_change
                }
            }
            HirTy::List(l) => {
                self.arena.intern(HirTy::List(HirListTy {
                    inner: self.change_inner_type(l.inner, generic_name, new_type),
                }))
            }
            HirTy::Generic(g) => {
                self.arena.intern(HirTy::Named(HirNamedTy {
                    name: self.arena.intern(self.mangle_generic_struct_name(g)),
                    span: Span::default(),
                }))
            }
            _ => {
                type_to_change
            }
        }
    }
}

