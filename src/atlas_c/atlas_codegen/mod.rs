/// Contains the definition of the CodeGenArena
pub mod arena;
mod table;

use crate::atlas_c::atlas_hir::{
    error::{HirResult, UnsupportedExpr, UnsupportedStatement},
    expr::HirExpr,
    signature::HirFunctionParameterSignature,
    stmt::{HirBlock, HirStatement},
    ty::HirTy,
    HirModule,
};
use crate::atlas_vm::runtime::instruction::{ClassDescriptor, ImportedLibrary, Instruction, Label, ProgramDescriptor, Type};
use std::collections::{BTreeMap, HashMap};

use crate::atlas_c::atlas_codegen::table::Table;
use crate::atlas_c::atlas_hir;
use crate::atlas_c::atlas_hir::error::HirError;
use crate::atlas_c::atlas_hir::expr::HirUnaryOp;
use crate::atlas_c::atlas_hir::item::{HirClass, HirClassConstructor};
use crate::atlas_c::atlas_hir::signature::{ConstantValue, HirClassMethodModifier};
use arena::CodeGenArena;
use miette::{SourceOffset, SourceSpan};

/// Result of codegen
pub type CodegenResult<T> = Result<T, HirError>;

/// Unit of codegen
pub struct CodeGenUnit<'hir, 'codegen>
where
    'codegen: 'hir,
{
    hir: HirModule<'hir>,
    program: ProgramDescriptor<'codegen>,
    arena: CodeGenArena<'codegen>,
    //simulate a var_map so the codegen can translate it into stack operations
    local_variables: Table<&'hir str>,
    //store the function position
    current_pos: usize,
    string_pool: Vec<&'codegen str>,
    class_pool: Vec<ClassDescriptor<'codegen>>,
    //todo: Replace this with the path of the current module to be codegen
    src: String,
}

impl<'hir, 'codegen> CodeGenUnit<'hir, 'codegen> {
    /// Create a new CodeGenUnit
    pub fn new(hir: HirModule<'hir>, arena: CodeGenArena<'codegen>, src: String) -> Self {
        Self {
            hir,
            program: ProgramDescriptor::new(),
            arena,
            local_variables: Table::new(),
            current_pos: 0,
            string_pool: Vec::new(),
            class_pool: Vec::new(),
            src,
        }
    }

    pub fn compile(&mut self) -> CodegenResult<ProgramDescriptor> {
        let mut labels: Vec<Label> = Vec::new();
        for (class_name, class) in self.hir.body.classes.clone() {
            self.generate_class_descriptor(class_name, &class);
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
            bytecode.insert(0, Instruction::LocalSpace {
                nb_vars: self.local_variables.len() as u8
            });

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
        for (class_name, class) in self.hir.body.classes.clone() {
            self.generate_bytecode_class(class_name, &class, &mut labels, self.src.clone(), &mut functions)?;
            let destructor_pos = self.generate_bytecode_constructor(class_name, &class.destructor, &mut labels, self.src.clone(), false)?;
            self.local_variables.clear();
            functions.insert(self.arena.alloc(format!("{}.delete", class_name)), destructor_pos);
            let constructor_pos = self.generate_bytecode_constructor(class_name, &class.constructor, &mut labels, self.src.clone(), true)?;
            self.local_variables.clear();
            functions.insert(self.arena.alloc(format!("{}.new", class_name)), constructor_pos);
            let class_pos = self.class_pool.iter().position(|constant_class| {
                constant_class.name == class_name
            }).unwrap();
            self.class_pool[class_pos].destructor_pos = destructor_pos;
            self.class_pool[class_pos].constructor_pos = constructor_pos;
        }

        self.program.entry_point = String::from("main");
        self.program.labels = labels;
        self.program.global.string_pool = self.arena.alloc(self.string_pool.clone());
        self.program.classes = self.arena.alloc(self.class_pool.clone());
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

    fn generate_bytecode_class(
        &mut self,
        class_name: &str,
        class: &HirClass<'hir>,
        labels: &mut Vec<Label<'codegen>>,
        src: String,
        functions: &mut HashMap<&'codegen str, usize>,
    ) -> HirResult<()> {
        for method in class.methods.iter() {
            let mut bytecode = Vec::new();
            let params = method.signature.params.clone();
            if method.signature.modifier != HirClassMethodModifier::Static {
                self.local_variables.insert("self");
                bytecode.push(Instruction::LoadArg {
                    index: 0,
                });
                self.generate_bytecode_args(params, &mut bytecode, 1)?;
            } else {
                self.generate_bytecode_args(params, &mut bytecode, 0)?;
            }
            self.generate_bytecode_block(&method.body, &mut bytecode, src.clone())?;

            bytecode.insert(0, Instruction::LocalSpace {
                nb_vars: self.local_variables.len() as u8
            });

            let len = bytecode.len();
            let method_name = self.arena.alloc(
                if method.signature.modifier == HirClassMethodModifier::Static {
                    format!("{}::{}", class_name, method.name)
                } else {
                    format!("{}.{}", class_name, method.name)
                }
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
    fn generate_class_descriptor(
        &mut self,
        class_name: &str,
        class: &HirClass<'hir>,
    ) {
        println!("Generating class descriptor for {}", class_name);
        let mut fields: Vec<&'codegen str> = Vec::new();
        let mut constants: BTreeMap<&'codegen str, ConstantValue> = BTreeMap::new();
        for field in class.fields.iter() {
            fields.push(self.arena.alloc(field.name.to_string()));
        }
        for (constant_name, constant) in class.signature.constants.iter() {
            constants.insert(self.arena.alloc(constant_name.to_string()), constant.value.clone());
        }
        let class_constant = ClassDescriptor {
            name: self.arena.alloc(class_name.to_string()),
            fields,
            //takes self as param
            constructor_nb_args: class.constructor.params.len() + 1, //+1 for self
            constructor_pos: usize::default(),
            //takes self as param
            destructor_nb_args: class.destructor.params.len() + 1, //+1 for self
            destructor_pos: usize::default(),
            constants,
        };
        self.class_pool.push(class_constant);
    }
    fn generate_bytecode_constructor(
        &mut self,
        class_name: &str,
        constructor: &HirClassConstructor<'hir>,
        labels: &mut Vec<Label<'codegen>>,
        src: String,
        // If the HirClassConstructor is a constructor or a destructor
        is_constructor: bool,
    ) -> CodegenResult<usize> {
        println!("Doing {} of {}", if is_constructor { "constructor" } else { "destructor" }, class_name);
        let mut bytecode = Vec::new();
        let params = constructor.params.clone();

        self.local_variables.insert("self");
        bytecode.push(Instruction::LoadArg {
            index: 0,
        });
        self.generate_bytecode_args(params, &mut bytecode, 1)?;

        self.generate_bytecode_block(&constructor.body, &mut bytecode, src.clone())?;

        //Return the self reference
        bytecode.push(Instruction::LoadVar(self.local_variables.get_index("self").unwrap()));
        bytecode.push(Instruction::Return);
        println!("nbParams: {}", constructor.params.len());
        bytecode.insert(0, Instruction::LocalSpace {
            nb_vars: self.local_variables.len() as u8
        });
        let len = bytecode.len();
        labels.push(Label {
            name: self.arena.alloc(if is_constructor { format!("{}.new", class_name) } else { format!("{}.delete", class_name) }),
            position: self.current_pos,
            body: self.arena.alloc(bytecode),
        });
        self.current_pos += len;

        Ok(self.current_pos - len)
    }

    fn generate_bytecode_block(
        &mut self,
        block: &HirBlock<'hir>,
        bytecode: &mut Vec<Instruction<'codegen>>,
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
        bytecode: &mut Vec<Instruction<'codegen>>,
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
                value.push(Instruction::StoreVar(self.local_variables.insert(const_stmt.name)));
                bytecode.append(&mut value);
            }
            HirStatement::Let(let_stmt) => {
                let mut value = Vec::new();
                self.generate_bytecode_expr(&let_stmt.value, &mut value, src)?;
                value.push(Instruction::StoreVar(self.local_variables.insert(let_stmt.name)));
                bytecode.append(&mut value);
            }
            HirStatement::Expr(e) => {
                self.generate_bytecode_expr(&e.expr, bytecode, src)?;
                bytecode.push(Instruction::Pop);
            }
            _ => {
                return Err(HirError::UnsupportedStatement(
                    UnsupportedStatement {
                        span: SourceSpan::new(
                            SourceOffset::from(stmt.span().start),
                            stmt.span().end - stmt.span().start,
                        ),
                        stmt: format!("{:?}", stmt),
                        src: src.clone(),
                    },
                ))
            }
        }
        Ok(())
    }

    fn generate_bytecode_expr(
        &mut self,
        expr: &HirExpr<'hir>,
        bytecode: &mut Vec<Instruction<'codegen>>,
        src: String,
    ) -> HirResult<()> {
        match expr {
            HirExpr::Assign(a) => {
                let lhs = a.lhs.as_ref();
                match lhs {
                    HirExpr::Ident(i) => {
                        self.generate_bytecode_expr(&a.rhs, bytecode, src.clone())?;
                        bytecode.push(Instruction::StoreVar(self.local_variables.get_index(i.name).unwrap()));
                    }
                    HirExpr::Indexing(i) => {
                        match i.target.ty() {
                            HirTy::List(_) => {
                                //Get the Index
                                self.generate_bytecode_expr(&i.index, bytecode, src.clone())?;
                                //Get the list pointer
                                self.generate_bytecode_expr(&i.target, bytecode, src.clone())?;
                                //Get the value
                                self.generate_bytecode_expr(&a.rhs, bytecode, src)?;
                                //Store the value in the list
                                bytecode.push(Instruction::ListStore);
                            }
                            HirTy::String(_) => {
                                eprintln!("String store: {:?}", a);
                                //Get the Index
                                self.generate_bytecode_expr(&i.index, bytecode, src.clone())?;
                                //Get the string pointer
                                self.generate_bytecode_expr(&i.target, bytecode, src.clone())?;
                                //Get the value
                                self.generate_bytecode_expr(&a.rhs, bytecode, src)?;
                                //Store the value in the string
                                bytecode.push(Instruction::StringStore);
                            }
                            _ => {
                                return Err(HirError::UnsupportedExpr(
                                    UnsupportedExpr {
                                        span: SourceSpan::new(
                                            SourceOffset::from(expr.span().start),
                                            expr.span().end - expr.span().start,
                                        ),
                                        expr: format!("{:?}", expr),
                                        src: src.clone(),
                                    },
                                ));
                            }
                        }
                    }
                    HirExpr::FieldAccess(field_access) => {
                        //Get the Class pointer
                        self.generate_bytecode_expr(&field_access.target, bytecode, src.clone())?;
                        //Get the value
                        self.generate_bytecode_expr(&a.rhs, bytecode, src.clone())?;
                        let class_name = match field_access.target.ty() {
                            HirTy::Named(class_name) => class_name,
                            _ => {
                                return Err(HirError::UnsupportedExpr(UnsupportedExpr {
                                    span: SourceSpan::new(
                                        SourceOffset::from(expr.span().start),
                                        expr.span().end - expr.span().start,
                                    ),
                                    expr: format!("No field access for {:?}", field_access.target.ty()),
                                    src,
                                }))
                            }
                        };
                        let class = self.class_pool.iter().find(|c| {
                            c.name == class_name.name
                        }).unwrap_or_else(|| {
                            //should never happen
                            panic!("Class {} not found", class_name.name)
                        });
                        //get the position of the field
                        let field = class.fields.iter().position(|f| *f == field_access.field.name).unwrap();
                        //Store the value in the field
                        bytecode.push(Instruction::SetField {
                            field
                        })
                    }
                    _ => {
                        return Err(HirError::UnsupportedExpr(
                            UnsupportedExpr {
                                span: SourceSpan::new(
                                    SourceOffset::from(expr.span().start),
                                    expr.span().end - expr.span().start,
                                ),
                                expr: format!("{:?}", expr),
                                src,
                            },
                        ));
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
                                    return Err(atlas_hir::error::HirError::UnsupportedExpr(
                                        UnsupportedExpr {
                                            span: SourceSpan::new(
                                                SourceOffset::from(expr.span().start),
                                                expr.span().end - expr.span().start,
                                            ),
                                            expr: format!("Can't negate: {:?}", expr),
                                            src,
                                        },
                                    ))
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
                                return Err(HirError::UnsupportedExpr(
                                    UnsupportedExpr {
                                        span: SourceSpan::new(
                                            SourceOffset::from(expr.span().start),
                                            expr.span().end - expr.span().start,
                                        ),
                                        expr: format!("Can't negate: {:?}", expr),
                                        src,
                                    },
                                ));
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
                        return Err(atlas_hir::error::HirError::UnsupportedExpr(
                            UnsupportedExpr {
                                span: SourceSpan::new(
                                    SourceOffset::from(expr.span().start),
                                    expr.span().end - expr.span().start,
                                ),
                                expr: format!("Can't cast: {:?}", expr),
                                src,
                            },
                        ))
                    }
                }
            }
            HirExpr::Indexing(i) => {
                match i.target.ty() {
                    HirTy::List(_) => {
                        self.generate_bytecode_expr(&i.target, bytecode, src.clone())?;
                        self.generate_bytecode_expr(&i.index, bytecode, src)?;
                        bytecode.push(Instruction::ListLoad);
                    }
                    HirTy::String(_) => {
                        self.generate_bytecode_expr(&i.target, bytecode, src.clone())?;
                        self.generate_bytecode_expr(&i.index, bytecode, src)?;
                        bytecode.push(Instruction::StringLoad);
                    }
                    _ => {
                        return Err(HirError::UnsupportedExpr(
                            UnsupportedExpr {
                                span: SourceSpan::new(
                                    SourceOffset::from(expr.span().start),
                                    expr.span().end - expr.span().start,
                                ),
                                expr: format!("Can't index: {:?}", expr),
                                src: src.clone(),
                            },
                        ))
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
                                function_name: self.arena.alloc(i.name.to_string()),
                                nb_args: f.args.len() as u8,
                            });
                        } else {
                            bytecode.push(Instruction::FunctionCall {
                                function_name: self.arena.alloc(i.name.to_string()),
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
                        let class_name = match field_access.target.ty() {
                            HirTy::Named(class_name) => class_name,
                            _ => {
                                return Err(HirError::UnsupportedExpr(
                                    UnsupportedExpr {
                                        span: SourceSpan::new(
                                            SourceOffset::from(expr.span().start),
                                            expr.span().end - expr.span().start,
                                        ),
                                        expr: format!("Can't call from: {:?}", expr),
                                        src,
                                    },
                                ))
                            }
                        };
                        bytecode.push(Instruction::FunctionCall {
                            function_name: self.arena.alloc(format!("{}.{}", class_name.name, field_access.field.name)),
                            nb_args: f.args.len() as u8 + 1,
                        })
                    }
                    HirExpr::StaticAccess(static_access) => {
                        for arg in &f.args {
                            self.generate_bytecode_expr(arg, bytecode, src.clone())?;
                        }
                        bytecode.push(Instruction::FunctionCall {
                            function_name: self.arena.alloc(format!("{}::{}", static_access.target.name, static_access.field.name)),
                            nb_args: f.args.len() as u8,
                        })
                    }
                    _ => {
                        return Err(HirError::UnsupportedExpr(
                            UnsupportedExpr {
                                span: SourceSpan::new(
                                    SourceOffset::from(expr.span().start),
                                    expr.span().end - expr.span().start,
                                ),
                                expr: format!("Can't call from: {:?}", expr),
                                src: src.clone(),
                            },
                        ));
                    }
                }
            }
            HirExpr::IntegerLiteral(i) => bytecode.push(Instruction::PushInt(i.value)),
            HirExpr::FloatLiteral(f) => bytecode.push(Instruction::PushFloat(f.value)),
            HirExpr::BooleanLiteral(b) => bytecode.push(Instruction::PushBool(b.value)),
            HirExpr::CharLiteral(c) => bytecode.push(Instruction::PushChar(c.value)),
            HirExpr::UnitLiteral(_) => bytecode.push(Instruction::PushUnit),
            HirExpr::UnsignedIntegerLiteral(u) => {
                bytecode.push(Instruction::PushUnsignedInt(u.value))
            }
            HirExpr::StringLiteral(s) => {
                self.string_pool.push(self.arena.alloc(s.value.to_string()));
                let index = self.string_pool.len() - 1;
                bytecode.push(Instruction::PushStr(index));
            }
            HirExpr::ListLiteral(l) => {
                bytecode.push(Instruction::PushUnsignedInt(l.items.len() as u64));
                bytecode.push(Instruction::NewList);
                l.items.iter().enumerate().for_each(|(u, i)| {
                    //Duplicate the list reference
                    bytecode.push(Instruction::Dup);
                    //Push the index
                    bytecode.push(Instruction::PushUnsignedInt(u as u64));
                    //Swap the index and the list reference
                    bytecode.push(Instruction::Swap);
                    //Generate the expression
                    self.generate_bytecode_expr(i, bytecode, src.clone()).unwrap();
                    //Store the value in the list
                    bytecode.push(Instruction::ListStore);
                });
            }
            HirExpr::Delete(d) => {
                println!("Doing delete for {:?}", d);
                self.generate_bytecode_expr(&d.expr, bytecode, src)?;
                bytecode.push(Instruction::DeleteObj);
            }
            HirExpr::Ident(i) => bytecode.push(Instruction::LoadVar(self.local_variables.get_index(i.name).unwrap())),
            HirExpr::SelfLiteral(_) => bytecode.push(Instruction::LoadVar(self.local_variables.get_index("self").unwrap())),
            HirExpr::FieldAccess(field_access) => {
                self.generate_bytecode_expr(field_access.target.as_ref(), bytecode, src.clone())?;
                let class_name = match field_access.target.ty() {
                    HirTy::Named(class_name) => class_name,
                    _ => {
                        return Err(HirError::UnsupportedExpr(UnsupportedExpr {
                            span: SourceSpan::new(
                                SourceOffset::from(expr.span().start),
                                expr.span().end - expr.span().start,
                            ),
                            expr: format!("No field access for {:?}", field_access.target.ty()),
                            src: src.clone(),
                        }))
                    }
                };
                let class = self.class_pool.iter().find(|c| {
                    c.name == class_name.name
                }).unwrap_or_else(|| {
                    //should never happen
                    panic!("Class {} not found", class_name.name)
                });
                //get the position of the field
                let field = class.fields.iter().position(|f| *f == field_access.field.name).unwrap();
                bytecode.push(Instruction::GetField {
                    field
                })
            }
            HirExpr::StaticAccess(static_access) => {
                match static_access.field.ty {
                    HirTy::String(_) => {
                        let target_name = static_access.target.name;
                        let class_signature = self.hir.signature.classes.get(target_name).unwrap();
                        let value = match class_signature.constants.get(static_access.field.name).unwrap().value {
                            ConstantValue::String(s) => String::from(s),
                            _ => {
                                return Err(HirError::UnsupportedExpr(UnsupportedExpr {
                                    span: SourceSpan::new(
                                        SourceOffset::from(expr.span().start),
                                        expr.span().end - expr.span().start,
                                    ),
                                    expr: format!("No string constant for {}", static_access.field.name),
                                    src: src.clone(),
                                }))
                            }
                        };
                        self.string_pool.push(self.arena.alloc(value));
                        let index = self.string_pool.len() - 1;
                        bytecode.push(Instruction::PushStr(index));
                    }
                    HirTy::Float64(_) => {
                        let class_signature = self.hir.signature.classes.get(static_access.target.name).unwrap();
                        let value = match class_signature.constants.get(static_access.field.name).unwrap().value {
                            ConstantValue::Float(f) => *f,
                            _ => {
                                return Err(HirError::UnsupportedExpr(UnsupportedExpr {
                                    span: SourceSpan::new(
                                        SourceOffset::from(expr.span().start),
                                        expr.span().end - expr.span().start,
                                    ),
                                    expr: format!("No float constant for {}", static_access.field.name),
                                    src: src.clone(),
                                }))
                            }
                        };
                        bytecode.push(Instruction::PushFloat(value));
                    }
                    HirTy::Int64(_) => {
                        let class_signature = self.hir.signature.classes.get(static_access.target.name).unwrap();
                        let value = match class_signature.constants.get(static_access.field.name).unwrap().value {
                            ConstantValue::Int(i) => *i,
                            _ => {
                                return Err(HirError::UnsupportedExpr(UnsupportedExpr {
                                    span: SourceSpan::new(
                                        SourceOffset::from(expr.span().start),
                                        expr.span().end - expr.span().start,
                                    ),
                                    expr: format!("No int constant for {}", static_access.field.name),
                                    src: src.clone(),
                                }))
                            }
                        };
                        bytecode.push(Instruction::PushInt(value));
                    }
                    HirTy::Char(_) => {
                        let class_signature = self.hir.signature.classes.get(static_access.target.name).unwrap();
                        let value = match class_signature.constants.get(static_access.field.name).unwrap().value {
                            ConstantValue::Char(c) => *c,
                            _ => {
                                return Err(HirError::UnsupportedExpr(UnsupportedExpr {
                                    span: SourceSpan::new(
                                        SourceOffset::from(expr.span().start),
                                        expr.span().end - expr.span().start,
                                    ),
                                    expr: format!("No char constant for {}", static_access.field.name),
                                    src: src.clone(),
                                }))
                            }
                        };
                        bytecode.push(Instruction::PushChar(value));
                    }
                    HirTy::UInt64(_) => {
                        let class_signature = self.hir.signature.classes.get(static_access.target.name).unwrap();
                        let value = match class_signature.constants.get(static_access.field.name).unwrap().value {
                            ConstantValue::UInt(u) => *u,
                            _ => {
                                return Err(HirError::UnsupportedExpr(UnsupportedExpr {
                                    span: SourceSpan::new(
                                        SourceOffset::from(expr.span().start),
                                        expr.span().end - expr.span().start,
                                    ),
                                    expr: format!("No uint constant for {}", static_access.field.name),
                                    src: src.clone(),
                                }))
                            }
                        };
                        bytecode.push(Instruction::PushUnsignedInt(value));
                    }
                    HirTy::List(_) => {
                        return Err(HirError::UnsupportedExpr(UnsupportedExpr {
                            span: SourceSpan::new(
                                SourceOffset::from(expr.span().start),
                                expr.span().end - expr.span().start,
                            ),
                            expr: format!("Lists aren't supported as constants for now {}", static_access.field.name),
                            src: src.clone(),
                        }))
                    }
                    _ => {
                        return Err(HirError::UnsupportedExpr(UnsupportedExpr {
                            span: SourceSpan::new(
                                SourceOffset::from(expr.span().start),
                                expr.span().end - expr.span().start,
                            ),
                            expr: format!("Unsupported type for now {}", static_access.field.name),
                            src: src.clone(),
                        }))
                    }
                }
            }
            HirExpr::NewArray(a) => {
                self.generate_bytecode_expr(&a.size, bytecode, src.clone())?;
                bytecode.push(Instruction::NewList);
            }
            HirExpr::NewObj(new_obj) => {
                let class_pos = match new_obj.ty {
                    HirTy::Named(class_name) => {
                        self.class_pool.iter().position(|class| class.name == class_name.name).unwrap()
                    }
                    _ => return Err(HirError::UnsupportedExpr(UnsupportedExpr {
                        span: SourceSpan::new(
                            SourceOffset::from(new_obj.span.start),
                            new_obj.span.end - new_obj.span.start,
                        ),
                        expr: format!("No constructor for {}", new_obj.ty),
                        src: src.clone(),
                    }))
                };
                //Need to create a NewObj & call its constructor (constructor name = ClassName.ClassName)
                bytecode.push(Instruction::NewObj {
                    class_descriptor: class_pos
                });

                for arg in new_obj.args.iter() {
                    self.generate_bytecode_expr(arg, bytecode, src.clone())?;
                }
                bytecode.push(Instruction::FunctionCall {
                    function_name: self.arena.alloc(format!("{}.new", self.class_pool[class_pos].name)),
                    nb_args: new_obj.args.len() as u8 + 1,
                });
            }
            HirExpr::NoneLiteral(_) => {
                bytecode.push(Instruction::PushNull);
            }
        }
        Ok(())
    }

    fn generate_bytecode_args(
        &mut self,
        args: Vec<&HirFunctionParameterSignature<'hir>>,
        bytecode: &mut Vec<Instruction<'codegen>>,
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
