/// Contains the definition of the CodeGenArena
pub mod arena;
mod program;
mod table;

use crate::atlas_c::atlas_codegen::table::Table;
use crate::atlas_c::atlas_hir;
use crate::atlas_c::atlas_hir::error::HirError;
use crate::atlas_c::atlas_hir::expr::HirUnaryOp;
use crate::atlas_c::atlas_hir::item::HirStruct;
use crate::atlas_c::atlas_hir::signature::{ConstantValue, HirStructMethodModifier};
use crate::atlas_c::atlas_hir::{
    error::{HirResult, UnsupportedExpr, UnsupportedStatement},
    expr::HirExpr,
    signature::HirFunctionParameterSignature,
    stmt::{HirBlock, HirStatement},
    ty::HirTy,
    HirModule,
};
use crate::atlas_vm::instruction::{
    ImportedLibrary, Instruction, Label, ProgramDescriptor, StructDescriptor, Type,
};
use arena::CodeGenArena;
use miette::{NamedSource, SourceOffset, SourceSpan};
use std::collections::{BTreeMap, HashMap};
use std::path::PathBuf;

/// Result of codegen
pub type CodegenResult<T> = Result<T, HirError>;

/// Unit of codegen
pub struct CodeGenUnit<'hir, 'codegen>
where
    'codegen: 'hir,
{
    hir: &'hir HirModule<'hir>,
    program: ProgramDescriptor<'codegen>,
    arena: CodeGenArena<'codegen>,
    //simulate a var_map so the codegen can translate it into stack operations
    local_variables: Table<&'hir str>,
    //store the function position
    current_pos: usize,
    string_pool: Vec<&'codegen str>,
    struct_pool: Vec<StructDescriptor<'codegen>>,
    //todo: Replace this with the path of the current module to be codegen
    src: String,
}

impl<'hir, 'codegen> CodeGenUnit<'hir, 'codegen> {
    /// Create a new CodeGenUnit
    pub fn new(hir: &'hir HirModule<'hir>, arena: CodeGenArena<'codegen>, src: String) -> Self {
        Self {
            hir,
            program: ProgramDescriptor::new(),
            arena,
            local_variables: Table::new(),
            current_pos: 0,
            string_pool: Vec::new(),
            struct_pool: Vec::new(),
            src,
        }
    }

    /// - TODO: Refactor the whole codegen thingy to output and atlas_asm::program::Program
    ///
    /// - TODO: Add LoadConst instruction & remove all the Push_XXX instructions
    #[deprecated]
    pub fn compile(&mut self) -> CodegenResult<ProgramDescriptor> {
        let mut labels: Vec<Label> = Vec::new();
        for (struct_name, hir_struct) in self.hir.body.structs.clone() {
            self.generate_struct_descriptor(struct_name, &hir_struct);
        }

        let mut functions: HashMap<&'codegen str, usize> = HashMap::new();
        for (func_name, function) in self.hir.body.functions.clone() {
            let mut bytecode = Vec::new();

            let params = function.signature.params.clone();
            self.generate_bytecode_args(params, &mut bytecode, 0)?;

            for arg in function.signature.params.iter() {
                self.local_variables.insert(arg.name);
            }

            self.generate_bytecode_block(&function.body, &mut bytecode, self.src.clone())?;
            bytecode.insert(
                0,
                Instruction::LocalSpace {
                    nb_vars: self.local_variables.len() as u8,
                },
            );

            if func_name == "main" {
                bytecode.push(Instruction::Halt);
            }

            let len = bytecode.len();
            let func_name = self.arena.alloc(func_name.to_string());

            functions.insert(func_name, self.current_pos);

            labels.push(Label {
                name: self.arena.alloc(func_name.to_string()),
                position: self.current_pos,
                body: self.arena.alloc(bytecode),
            });

            self.current_pos += len;
            self.local_variables.clear();
        }
        for (struct_name, hir_struct) in self.hir.body.structs.clone() {
            self.generate_bytecode_struct(
                struct_name,
                &hir_struct,
                &mut labels,
                self.src.clone(),
                &mut functions,
            )?;
        }

        self.program.entry_point = String::from("main");
        self.program.labels = labels;
        self.program.global.string_pool = self.arena.alloc(self.string_pool.clone());
        self.program.structs = self.arena.alloc(self.struct_pool.clone());
        self.program.functions = functions;
        let libraries = self
            .hir
            .body
            .imports
            .iter()
            .map(|l| ImportedLibrary {
                name: l.path.to_string(),
                is_std: true,
            })
            .collect::<Vec<_>>();
        self.program.libraries = libraries;
        Ok(self.program.clone())
    }

    fn generate_bytecode_struct(
        &mut self,
        struct_name: &str,
        hir_struct: &HirStruct<'hir>,
        labels: &mut Vec<Label<'codegen>>,
        src: String,
        functions: &mut HashMap<&'codegen str, usize>,
    ) -> HirResult<()> {
        for method in hir_struct.methods.iter() {
            let mut bytecode = Vec::new();
            let params = method.signature.params.clone();
            if method.signature.modifier != HirStructMethodModifier::Static {
                self.local_variables.insert("self");
                bytecode.push(Instruction::LoadArg { index: 0 });
                self.generate_bytecode_args(params, &mut bytecode, 1)?;
            } else {
                self.generate_bytecode_args(params, &mut bytecode, 0)?;
            }
            self.generate_bytecode_block(&method.body, &mut bytecode, src.clone())?;

            bytecode.insert(
                0,
                Instruction::LocalSpace {
                    nb_vars: self.local_variables.len() as u8,
                },
            );

            let len = bytecode.len();
            let method_name = self.arena.alloc(
                if method.signature.modifier == HirStructMethodModifier::Static {
                    format!("{}::{}", struct_name, method.name)
                } else {
                    format!("{}.{}", struct_name, method.name)
                },
            );
            functions.insert(method_name, self.current_pos);
            labels.push(Label {
                name: method_name,
                position: self.current_pos,
                body: self.arena.alloc(bytecode),
            });
            self.current_pos += len;
            self.local_variables.clear();
        }
        Ok(())
    }
    fn generate_struct_descriptor(&mut self, struct_name: &str, hir_struct: &HirStruct<'hir>) {
        println!("Generating hir_struct descriptor for {}", struct_name);
        let mut fields: Vec<&'codegen str> = Vec::new();
        let mut constants: BTreeMap<&'codegen str, ConstantValue> = BTreeMap::new();
        for field in hir_struct.fields.iter() {
            fields.push(self.arena.alloc(field.name.to_string()));
        }
        for (constant_name, constant) in hir_struct.signature.constants.iter() {
            constants.insert(
                self.arena.alloc(constant_name.to_string()),
                constant.value.clone(),
            );
        }
        let struct_constant = StructDescriptor {
            name: self.arena.alloc(struct_name.to_string()),
            fields,
            constants,
        };
        self.struct_pool.push(struct_constant);
    }
    fn generate_bytecode_block(
        &mut self,
        block: &HirBlock<'hir>,
        bytecode: &mut Vec<Instruction>,
        src: String,
    ) -> HirResult<()> {
        for stmt in &block.statements {
            self.generate_bytecode_stmt(stmt, bytecode, src.clone())?;
        }
        Ok(())
    }

    fn generate_bytecode_stmt(
        &mut self,
        stmt: &HirStatement<'hir>,
        bytecode: &mut Vec<Instruction>,
        src: String,
    ) -> HirResult<()> {
        match stmt {
            HirStatement::Return(e) => {
                self.generate_bytecode_expr(&e.value, bytecode, src)?;
                bytecode.push(Instruction::Return);
            }
            HirStatement::IfElse(i) => {
                self.generate_bytecode_expr(&i.condition, bytecode, src.clone())?;
                let mut then_body = Vec::new();
                self.generate_bytecode_block(&i.then_branch, &mut then_body, src.clone())?;

                bytecode.push(Instruction::JmpZ {
                    pos: (then_body.len() + if i.else_branch.is_some() { 1 } else { 0 }) as isize,
                });
                bytecode.append(&mut then_body);
                if let Some(e) = &i.else_branch {
                    let mut else_body = Vec::new();
                    self.generate_bytecode_block(e, &mut else_body, src)?;

                    bytecode.push(Instruction::Jmp {
                        pos: (else_body.len() + 1) as isize,
                    });
                    bytecode.append(&mut else_body);
                }
            }
            HirStatement::While(w) => {
                let start = bytecode.len() as isize;
                self.generate_bytecode_expr(&w.condition, bytecode, src.clone())?;
                let mut body = Vec::new();

                self.generate_bytecode_block(&w.body, &mut body, src)?;
                //If the condition is false jump to the end of the loop
                bytecode.push(Instruction::JmpZ {
                    pos: (body.len() + 1) as isize,
                });
                bytecode.append(&mut body);
                //Jump back to the start of the loop
                bytecode.push(Instruction::Jmp {
                    pos: start - bytecode.len() as isize,
                });
            }
            HirStatement::Const(const_stmt) => {
                let mut value = Vec::new();
                self.generate_bytecode_expr(&const_stmt.value, &mut value, src)?;
                value.push(Instruction::StoreVar(
                    self.local_variables.insert(const_stmt.name),
                ));
                bytecode.append(&mut value);
            }
            HirStatement::Let(let_stmt) => {
                let mut value = Vec::new();
                self.generate_bytecode_expr(&let_stmt.value, &mut value, src)?;
                value.push(Instruction::StoreVar(
                    self.local_variables.insert(let_stmt.name),
                ));
                bytecode.append(&mut value);
            }
            HirStatement::Expr(e) => {
                self.generate_bytecode_expr(&e.expr, bytecode, src)?;
                bytecode.push(Instruction::Pop);
            }
            _ => {
                let path = stmt.span().path.clone();
                let src = std::fs::read_to_string(PathBuf::from(&path))
                    .unwrap_or_else(|_| panic!("{} is not a valid path", path));
                return Err(HirError::UnsupportedStatement(UnsupportedStatement {
                    span: SourceSpan::new(
                        SourceOffset::from(stmt.span().start),
                        stmt.span().end - stmt.span().start,
                    ),
                    stmt: format!("{:?}", stmt),
                    src: NamedSource::new(path, src),
                }));
            }
        }
        Ok(())
    }

    fn generate_bytecode_expr(
        &mut self,
        expr: &HirExpr<'hir>,
        bytecode: &mut Vec<Instruction>,
        src: String,
    ) -> HirResult<()> {
        match expr {
            HirExpr::Assign(a) => {
                let lhs = a.lhs.as_ref();
                match lhs {
                    HirExpr::Ident(i) => {
                        self.generate_bytecode_expr(&a.rhs, bytecode, src.clone())?;
                        bytecode.push(Instruction::StoreVar(
                            self.local_variables.get_index(i.name).unwrap(),
                        ));
                    }
                    HirExpr::FieldAccess(field_access) => {
                        //Get the Struct pointer
                        self.generate_bytecode_expr(&field_access.target, bytecode, src.clone())?;
                        //Get the value
                        self.generate_bytecode_expr(&a.rhs, bytecode, src.clone())?;
                        let struct_name = match field_access.target.ty() {
                            HirTy::Named(struct_name) => struct_name,
                            _ => {
                                let path = expr.span().path.clone();
                                let src = std::fs::read_to_string(PathBuf::from(&path))
                                    .unwrap_or_else(|_| panic!("{} is not a valid path", path));
                                return Err(HirError::UnsupportedExpr(UnsupportedExpr {
                                    span: SourceSpan::new(
                                        SourceOffset::from(expr.span().start),
                                        expr.span().end - expr.span().start,
                                    ),
                                    expr: format!(
                                        "No field access for {:?}",
                                        field_access.target.ty()
                                    ),
                                    src: NamedSource::new(path, src),
                                }));
                            }
                        };
                        let struct_descriptor = self
                            .struct_pool
                            .iter()
                            .find(|c| c.name == struct_name.name)
                            .unwrap_or_else(|| {
                                //should never happen
                                panic!("Struct {} not found", struct_name.name)
                            });
                        //get the position of the field
                        let field = struct_descriptor
                            .fields
                            .iter()
                            .position(|f| *f == field_access.field.name)
                            .unwrap();
                        //Store the value in the field
                        bytecode.push(Instruction::SetField { field })
                    }
                    _ => {
                        let path = expr.span().path.clone();
                        let src = std::fs::read_to_string(PathBuf::from(&path))
                            .unwrap_or_else(|_| panic!("{} is not a valid path", path));
                        return Err(HirError::UnsupportedExpr(UnsupportedExpr {
                            span: SourceSpan::new(
                                SourceOffset::from(expr.span().start),
                                expr.span().end - expr.span().start,
                            ),
                            expr: format!("{:?}", expr),
                            src: NamedSource::new(path, src),
                        }));
                    }
                }
                bytecode.push(Instruction::PushUnit);
            }
            HirExpr::HirBinaryOp(b) => {
                self.generate_bytecode_expr(&b.lhs, bytecode, src.clone())?;
                self.generate_bytecode_expr(&b.rhs, bytecode, src)?;
                match b.op {
                    atlas_hir::expr::HirBinaryOp::Add => bytecode.push(Instruction::Add),
                    atlas_hir::expr::HirBinaryOp::Sub => bytecode.push(Instruction::Sub),
                    atlas_hir::expr::HirBinaryOp::Mul => bytecode.push(Instruction::Mul),
                    atlas_hir::expr::HirBinaryOp::Div => bytecode.push(Instruction::Div),
                    atlas_hir::expr::HirBinaryOp::Mod => bytecode.push(Instruction::Mod),
                    atlas_hir::expr::HirBinaryOp::Eq => bytecode.push(Instruction::Eq),
                    atlas_hir::expr::HirBinaryOp::Neq => bytecode.push(Instruction::Neq),
                    atlas_hir::expr::HirBinaryOp::Gt => bytecode.push(Instruction::Gt),
                    atlas_hir::expr::HirBinaryOp::Gte => bytecode.push(Instruction::Gte),
                    atlas_hir::expr::HirBinaryOp::Lt => bytecode.push(Instruction::Lt),
                    atlas_hir::expr::HirBinaryOp::Lte => bytecode.push(Instruction::Lte),
                    _ => unimplemented!("Unsupported binary operator for now"),
                }
            }
            HirExpr::Unary(u) => {
                //There is no unary instruction, so -x is the same as 0 - x
                //And !x is the same as x == 0
                self.generate_bytecode_expr(&u.expr, bytecode, src.clone())?;
                if let Some(op) = &u.op {
                    match op {
                        HirUnaryOp::Neg => {
                            match u.expr.ty() {
                                HirTy::Int64(_) => {
                                    bytecode.push(Instruction::PushInt(0));
                                }
                                HirTy::Float64(_) => {
                                    bytecode.push(Instruction::PushFloat(0.0));
                                }
                                // This won't really work, because you're subtracting a 32-bit char from a 64-bit integer
                                HirTy::Char(_) => {
                                    bytecode.push(Instruction::PushInt(0));
                                }
                                _ => {
                                    let path = u.span.path.clone();
                                    let src = std::fs::read_to_string(PathBuf::from(&path))
                                        .unwrap_or_else(|_| panic!("{} is not a valid path", path));
                                    return Err(atlas_hir::error::HirError::UnsupportedExpr(
                                        UnsupportedExpr {
                                            span: SourceSpan::new(
                                                SourceOffset::from(expr.span().start),
                                                expr.span().end - expr.span().start,
                                            ),
                                            expr: format!("Can't negate: {:?}", expr),
                                            src: NamedSource::new(path, src),
                                        },
                                    ));
                                }
                            }
                            bytecode.push(Instruction::Swap);
                            bytecode.push(Instruction::Sub);
                        }
                        HirUnaryOp::Not => {
                            if let HirTy::Boolean(_) = u.expr.ty() {
                                bytecode.push(Instruction::PushBool(false));
                                bytecode.push(Instruction::Eq);
                            } else {
                                let path = u.span.path.clone();
                                let src = std::fs::read_to_string(PathBuf::from(&path))
                                    .unwrap_or_else(|_| panic!("{} is not a valid path", path));
                                return Err(HirError::UnsupportedExpr(UnsupportedExpr {
                                    span: SourceSpan::new(
                                        SourceOffset::from(expr.span().start),
                                        expr.span().end - expr.span().start,
                                    ),
                                    expr: format!("Can't negate: {:?}", expr),
                                    src: NamedSource::new(path, src),
                                }));
                            }
                        }
                    }
                }
            }
            HirExpr::Casting(c) => {
                self.generate_bytecode_expr(&c.expr, bytecode, src.clone())?;
                match c.ty {
                    HirTy::Int64(_) => {
                        bytecode.push(Instruction::CastTo(Type::Integer));
                    }
                    HirTy::Float64(_) => {
                        bytecode.push(Instruction::CastTo(Type::Float));
                    }
                    HirTy::UInt64(_) => {
                        bytecode.push(Instruction::CastTo(Type::UnsignedInteger));
                    }
                    HirTy::Boolean(_) => {
                        bytecode.push(Instruction::CastTo(Type::Boolean));
                    }
                    HirTy::String(_) => {
                        bytecode.push(Instruction::CastTo(Type::String));
                    }
                    HirTy::Char(_) => {
                        bytecode.push(Instruction::CastTo(Type::Char));
                    }
                    _ => {
                        let path = c.span.path.clone();
                        let src = std::fs::read_to_string(PathBuf::from(&path))
                            .unwrap_or_else(|_| panic!("{} is not a valid path", path));
                        return Err(HirError::UnsupportedExpr(
                            UnsupportedExpr {
                                span: SourceSpan::new(
                                    SourceOffset::from(expr.span().start),
                                    expr.span().end - expr.span().start,
                                ),
                                expr: format!("Can't cast: {:?}", expr),
                                src: NamedSource::new(path, src),
                            },
                        ));
                    }
                }
            }
            HirExpr::Call(f) => {
                let callee = f.callee.as_ref();
                match callee {
                    HirExpr::Ident(i) => {
                        for arg in &f.args {
                            self.generate_bytecode_expr(arg, bytecode, src.clone())?;
                        }
                        let func = self.hir.signature.functions.get(i.name).unwrap();
                        if func.is_external {
                            bytecode.push(Instruction::ExternCall {
                                func_id: i.name.to_string(),
                                nb_args: f.args.len() as u8,
                            });
                        } else {
                            bytecode.push(Instruction::Call {
                                func_id: i.name.to_string(),
                                nb_args: f.args.len() as u8,
                            });
                        }
                    }
                    HirExpr::FieldAccess(field_access) => {
                        //Get the Class pointer:
                        self.generate_bytecode_expr(&field_access.target, bytecode, src.clone())?;
                        for arg in &f.args {
                            self.generate_bytecode_expr(arg, bytecode, src.clone())?;
                        }
                        let struct_name = match field_access.target.ty() {
                            HirTy::Named(struct_name) => struct_name,
                            _ => {
                                let path = field_access.span.path.clone();
                                let src = std::fs::read_to_string(PathBuf::from(&path))
                                    .unwrap_or_else(|_| panic!("{} is not a valid path", path));
                                return Err(HirError::UnsupportedExpr(UnsupportedExpr {
                                    span: SourceSpan::new(
                                        SourceOffset::from(expr.span().start),
                                        expr.span().end - expr.span().start,
                                    ),
                                    expr: format!("Can't call from: {:?}", expr),
                                    src: NamedSource::new(path, src),
                                }));
                            }
                        };
                        bytecode.push(Instruction::Call {
                            func_id: format!("{}.{}", struct_name.name, field_access.field.name),
                            nb_args: f.args.len() as u8 + 1,
                        })
                    }
                    HirExpr::StaticAccess(static_access) => {
                        for arg in &f.args {
                            self.generate_bytecode_expr(arg, bytecode, src.clone())?;
                        }
                        bytecode.push(Instruction::Call {
                            func_id: format!(
                                "{}::{}",
                                static_access.target.name, static_access.field.name
                            ),
                            nb_args: f.args.len() as u8,
                        })
                    }
                    _ => {
                        let path = expr.span().path.clone();
                        let src = std::fs::read_to_string(PathBuf::from(&path))
                            .unwrap_or_else(|_| panic!("{} is not a valid path", path));
                        return Err(HirError::UnsupportedExpr(UnsupportedExpr {
                            span: SourceSpan::new(
                                SourceOffset::from(expr.span().start),
                                expr.span().end - expr.span().start,
                            ),
                            expr: format!("Can't call from: {:?}", expr),
                            src: NamedSource::new(path, src),
                        }));
                    }
                }
            }
            HirExpr::IntegerLiteral(i) => bytecode.push(Instruction::PushInt(i.value)),
            HirExpr::FloatLiteral(f) => bytecode.push(Instruction::PushFloat(f.value)),
            HirExpr::BooleanLiteral(b) => bytecode.push(Instruction::PushBool(b.value)),
            HirExpr::CharLiteral(c) => bytecode.push(Instruction::PushInt(c.value as i64)),
            HirExpr::UnitLiteral(_) => bytecode.push(Instruction::PushUnit),
            HirExpr::UnsignedIntegerLiteral(u) => {
                bytecode.push(Instruction::PushInt(u.value as i64)); //TODO: Change to PushUnsignedInt when supported
            }
            HirExpr::StringLiteral(s) => {
                self.string_pool.push(self.arena.alloc(s.value.to_string()));
                let index = self.string_pool.len() - 1;
                bytecode.push(Instruction::PushStr(index));
            }
            HirExpr::Ident(i) => bytecode.push(Instruction::LoadVar(
                self.local_variables.get_index(i.name).unwrap(),
            )),
            HirExpr::ThisLiteral(_) => bytecode.push(Instruction::LoadVar(
                self.local_variables.get_index("self").unwrap(),
            )),
            HirExpr::FieldAccess(field_access) => {
                self.generate_bytecode_expr(field_access.target.as_ref(), bytecode, src.clone())?;
                let struct_name = match field_access.target.ty() {
                    HirTy::Named(struct_name) => struct_name,
                    _ => {
                        let path = field_access.span.path.clone();
                        let src = std::fs::read_to_string(PathBuf::from(&path))
                            .unwrap_or_else(|_| panic!("{} is not a valid path", path));
                        return Err(HirError::UnsupportedExpr(UnsupportedExpr {
                            span: SourceSpan::new(
                                SourceOffset::from(expr.span().start),
                                expr.span().end - expr.span().start,
                            ),
                            expr: format!("No field access for {:?}", field_access.target.ty()),
                            src: NamedSource::new(path, src),
                        }));
                    }
                };
                let struct_descriptor = self
                    .struct_pool
                    .iter()
                    .find(|c| c.name == struct_name.name)
                    .unwrap_or_else(|| {
                        //should never happen
                        panic!("Struct {} not found", struct_name.name)
                    });
                //get the position of the field
                let field = struct_descriptor
                    .fields
                    .iter()
                    .position(|f| *f == field_access.field.name)
                    .unwrap();
                bytecode.push(Instruction::GetField { field })
            }
            HirExpr::StaticAccess(static_access) => match static_access.field.ty {
                HirTy::String(_) => {
                    let target_name = static_access.target.name;
                    let struct_signature = self.hir.signature.structs.get(target_name).unwrap();
                    let value = match struct_signature
                        .constants
                        .get(static_access.field.name)
                        .unwrap()
                        .value
                    {
                        ConstantValue::String(s) => String::from(s),
                        _ => {
                            let path = static_access.span.path.clone();
                            let src = std::fs::read_to_string(PathBuf::from(&path))
                                .unwrap_or_else(|_| panic!("{} is not a valid path", path));
                            return Err(HirError::UnsupportedExpr(UnsupportedExpr {
                                span: SourceSpan::new(
                                    SourceOffset::from(expr.span().start),
                                    expr.span().end - expr.span().start,
                                ),
                                expr: format!(
                                    "No string constant for {}",
                                    static_access.field.name
                                ),
                                src: NamedSource::new(path, src),
                            }));
                        }
                    };
                    self.string_pool.push(self.arena.alloc(value));
                    let index = self.string_pool.len() - 1;
                    bytecode.push(Instruction::PushStr(index));
                }
                HirTy::Float64(_) => {
                    let struct_signature = self
                        .hir
                        .signature
                        .structs
                        .get(static_access.target.name)
                        .unwrap();
                    let value = match struct_signature
                        .constants
                        .get(static_access.field.name)
                        .unwrap()
                        .value
                    {
                        ConstantValue::Float(f) => *f,
                        _ => {
                            let path = expr.span().path.clone();
                            let src = std::fs::read_to_string(PathBuf::from(&path))
                                .unwrap_or_else(|_| panic!("{} is not a valid path", path));
                            return Err(HirError::UnsupportedExpr(UnsupportedExpr {
                                span: SourceSpan::new(
                                    SourceOffset::from(expr.span().start),
                                    expr.span().end - expr.span().start,
                                ),
                                expr: format!("No float constant for {}", static_access.field.name),
                                src: NamedSource::new(path, src),
                            }));
                        }
                    };
                    bytecode.push(Instruction::PushFloat(value));
                }
                HirTy::Int64(_) => {
                    let struct_signature = self
                        .hir
                        .signature
                        .structs
                        .get(static_access.target.name)
                        .unwrap();
                    let value = match struct_signature
                        .constants
                        .get(static_access.field.name)
                        .unwrap()
                        .value
                    {
                        ConstantValue::Int(i) => *i,
                        _ => {
                            let path = expr.span().path.clone();
                            let src = std::fs::read_to_string(PathBuf::from(&path))
                                .unwrap_or_else(|_| panic!("{} is not a valid path", path));
                            return Err(HirError::UnsupportedExpr(UnsupportedExpr {
                                span: SourceSpan::new(
                                    SourceOffset::from(expr.span().start),
                                    expr.span().end - expr.span().start,
                                ),
                                expr: format!("No int constant for {}", static_access.field.name),
                                src: NamedSource::new(path, src),
                            }));
                        }
                    };
                    bytecode.push(Instruction::PushInt(value));
                }
                HirTy::Char(_) => {
                    let struct_signature = self
                        .hir
                        .signature
                        .structs
                        .get(static_access.target.name)
                        .unwrap();
                    let value = match struct_signature
                        .constants
                        .get(static_access.field.name)
                        .unwrap()
                        .value
                    {
                        ConstantValue::Char(c) => *c,
                        _ => {
                            let path = expr.span().path.clone();
                            let src = std::fs::read_to_string(PathBuf::from(&path))
                                .unwrap_or_else(|_| panic!("{} is not a valid path", path));
                            return Err(HirError::UnsupportedExpr(UnsupportedExpr {
                                span: SourceSpan::new(
                                    SourceOffset::from(expr.span().start),
                                    expr.span().end - expr.span().start,
                                ),
                                expr: format!("No char constant for {}", static_access.field.name),
                                src: NamedSource::new(path, src),
                            }));
                        }
                    };
                    bytecode.push(Instruction::PushInt(value as i64));
                }
                HirTy::UInt64(_) => {
                    let struct_signature = self
                        .hir
                        .signature
                        .structs
                        .get(static_access.target.name)
                        .unwrap();
                    let value = match struct_signature
                        .constants
                        .get(static_access.field.name)
                        .unwrap()
                        .value
                    {
                        ConstantValue::UInt(u) => *u,
                        _ => {
                            let path = expr.span().path.clone();
                            let src = std::fs::read_to_string(PathBuf::from(&path))
                                .unwrap_or_else(|_| panic!("{} is not a valid path", path));
                            return Err(HirError::UnsupportedExpr(UnsupportedExpr {
                                span: SourceSpan::new(
                                    SourceOffset::from(expr.span().start),
                                    expr.span().end - expr.span().start,
                                ),
                                expr: format!("No uint constant for {}", static_access.field.name),
                                src: NamedSource::new(path, src),
                            }));
                        }
                    };
                    bytecode.push(Instruction::PushInt(value as i64));
                }
                HirTy::List(_) => {
                    let path = expr.span().path.clone();
                    let src = std::fs::read_to_string(PathBuf::from(&path))
                        .unwrap_or_else(|_| panic!("{} is not a valid path", path));
                    return Err(HirError::UnsupportedExpr(UnsupportedExpr {
                        span: SourceSpan::new(
                            SourceOffset::from(expr.span().start),
                            expr.span().end - expr.span().start,
                        ),
                        expr: format!(
                            "Lists aren't supported as constants for now {}",
                            static_access.field.name
                        ),
                        src: NamedSource::new(path, src),
                    }));
                }
                _ => {
                    let path = expr.span().path.clone();
                    let src = std::fs::read_to_string(PathBuf::from(&path))
                        .unwrap_or_else(|_| panic!("{} is not a valid path", path));
                    return Err(HirError::UnsupportedExpr(UnsupportedExpr {
                        span: SourceSpan::new(
                            SourceOffset::from(expr.span().start),
                            expr.span().end - expr.span().start,
                        ),
                        expr: format!("Unsupported type for now {}", static_access.field.name),
                        src: NamedSource::new(path, src),
                    }));
                }
            },
            HirExpr::NoneLiteral(_) => {
                bytecode.push(Instruction::PushUnit);
            }
            HirExpr::ConstructorExpr(constructor) => {
                bytecode.push(Instruction::NewObj {
                    obj_descriptor: self
                        .struct_pool
                        .iter()
                        .position(|s| s.name == constructor.name)
                        .unwrap(), //TODO: handle error
                });
                //Now we need to set the fields
                for field_init in &constructor.fields {
                    //Duplicate the object reference
                    bytecode.push(Instruction::Dup);
                    //Generate the value
                    self.generate_bytecode_expr(&field_init.value, bytecode, src.clone())?;
                    //Get the field index
                    let struct_descriptor = self
                        .struct_pool
                        .iter()
                        .find(|c| c.name == constructor.name)
                        .unwrap_or_else(|| {
                            //should never happen
                            panic!("Struct {} not found", constructor.name)
                        });
                    let field_index = struct_descriptor
                        .fields
                        .iter()
                        .position(|f| *f == field_init.name.name)
                        .unwrap();
                    //Set the field
                    bytecode.push(Instruction::SetField { field: field_index });
                }
            }
            _ => unimplemented!("Unsupported expression for now: {:?}", expr),
        }
        Ok(())
    }

    fn generate_bytecode_args(
        &mut self,
        args: Vec<HirFunctionParameterSignature<'hir>>,
        bytecode: &mut Vec<Instruction>,
        //The index of the first arguments
        base_index: u8,
    ) -> HirResult<()> {
        for (i, arg) in args.iter().enumerate() {
            bytecode.push(Instruction::LoadArg {
                index: (i as u8) + base_index,
            });
            self.local_variables.insert(arg.name);
        }
        Ok(())
    }
}
