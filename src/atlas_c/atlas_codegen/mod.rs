/// Contains the definition of the CodeGenArena
pub mod arena;
pub mod instruction;
mod table;

use crate::atlas_c::atlas_codegen::instruction::{
    ImportedLibrary, Instruction, Label, ProgramDescriptor, StructDescriptor, Type,
};
use crate::atlas_c::atlas_codegen::table::Table;
use crate::atlas_c::atlas_hir::arena::HirArena;
use crate::atlas_c::atlas_hir::error::{HirError, NoReturnInFunctionError};
use crate::atlas_c::atlas_hir::expr::{HirBinaryOperator, HirUnaryOp};
use crate::atlas_c::atlas_hir::item::{HirStruct, HirStructConstructor};
use crate::atlas_c::atlas_hir::monomorphization_pass::MonomorphizationPass;
use crate::atlas_c::atlas_hir::signature::{ConstantValue, HirStructMethodModifier};
use crate::atlas_c::atlas_hir::{
    HirModule,
    error::{HirResult, UnsupportedExpr, UnsupportedStatement},
    expr::HirExpr,
    stmt::{HirBlock, HirStatement},
    ty::HirTy,
};
use crate::atlas_c::utils;
use arena::CodeGenArena;
use miette::NamedSource;
use std::collections::{BTreeMap, HashMap};

/// Result of codegen
pub type CodegenResult<T> = Result<T, HirError>;

pub const THIS_NAME: &str = "this";

/// Unit of codegen
pub struct CodeGenUnit<'hir, 'codegen>
where
    'codegen: 'hir,
{
    hir: &'hir HirModule<'hir>,
    program: ProgramDescriptor<'codegen>,
    codegen_arena: CodeGenArena<'codegen>,
    hir_arena: &'hir HirArena<'hir>,
    //simulate a var_map so the codegen can translate it into stack operations
    local_variables: Table<&'hir str>,
    //store the function position
    current_pos: usize,
    struct_pool: Vec<StructDescriptor<'codegen>>,
    //todo: Replace this with the path of the current module to be codegen
}

impl<'hir, 'codegen> CodeGenUnit<'hir, 'codegen> {
    /// Create a new CodeGenUnit
    pub fn new(
        hir: &'hir HirModule<'hir>,
        arena: CodeGenArena<'codegen>,
        hir_arena: &'hir HirArena<'hir>,
    ) -> Self {
        Self {
            hir,
            program: ProgramDescriptor::new(),
            codegen_arena: arena,
            hir_arena,
            local_variables: Table::new(),
            current_pos: 0,
            struct_pool: Vec::new(),
        }
    }

    /// - TODO: Refactor the whole codegen thingy to output and atlas_asm::program::Program
    ///
    /// - TODO: Add LoadConst instruction & remove all the Push_XXX instructions
    pub fn compile(&mut self) -> CodegenResult<ProgramDescriptor<'codegen>> {
        let mut labels: Vec<Label> = Vec::new();
        for (struct_name, hir_struct) in self.hir.body.structs.clone() {
            self.generate_struct_descriptor(struct_name, &hir_struct);
        }

        let mut functions: HashMap<&'codegen str, usize> = HashMap::new();
        for (func_name, function) in self.hir.body.functions.clone() {
            let mut bytecode = Vec::new();

            for arg in function.signature.params.iter() {
                self.local_variables.insert(arg.name);
            }

            self.generate_bytecode_block(&function.body, &mut bytecode)?;

            //There is no need to reserve space for local variables if there is none
            if self.local_variables.len() > 0 {
                bytecode.insert(
                    0,
                    Instruction::LocalSpace {
                        //Parameters are already counted in the function call stack frame
                        nb_vars: self.local_variables.len() as u8
                            - function.signature.params.len() as u8,
                    },
                );
            }

            if func_name == "main" {
                bytecode.push(Instruction::Halt);
            } else if let HirTy::Unit(_) = function.signature.return_ty {
                let last_instruction = &bytecode[bytecode.len() - 1];
                match last_instruction {
                    Instruction::Return => {}
                    _ => bytecode.push(Instruction::Return),
                }
            } else {
                let last_instruction = &bytecode[bytecode.len() - 1];
                let path = function.span.path;
                let src = utils::get_file_content(path).unwrap();
                match last_instruction {
                    Instruction::Return => {}
                    _ => {
                        return Err(HirError::NoReturnInFunction(NoReturnInFunctionError {
                            span: function.span,
                            func_name: func_name.to_string(),
                            src: NamedSource::new(path, src),
                        }));
                    }
                }
            }

            let len = bytecode.len();
            let func_name = self.codegen_arena.alloc(func_name.to_string());

            functions.insert(func_name, self.current_pos);

            labels.push(Label {
                name: self.codegen_arena.alloc(func_name.to_string()),
                position: self.current_pos,
                body: self.codegen_arena.alloc(bytecode),
            });

            self.current_pos += len;
            self.local_variables.clear();
        }
        for (struct_name, hir_struct) in self.hir.body.structs.clone() {
            self.generate_bytecode_struct(struct_name, &hir_struct, &mut labels, &mut functions)?;
        }

        self.program.entry_point = String::from("main");
        self.program.labels = labels;
        self.program.structs = self.codegen_arena.alloc(self.struct_pool.clone());
        self.program.functions.extend(functions);
        let libraries = self
            .hir
            .body
            .imports
            .iter()
            .map(|l| ImportedLibrary {
                name: l.path.to_string(),
                is_std: Self::is_std(l.path),
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
        functions: &mut HashMap<&'codegen str, usize>,
    ) -> HirResult<()> {
        //generate constructor
        self.generate_bytecode_constructor(struct_name, &hir_struct.constructor, labels)?;
        self.local_variables.clear();
        //generate destructor
        self.generate_bytecode_destructor(struct_name, &hir_struct.destructor, labels)?;
        self.local_variables.clear();

        for method in hir_struct.methods.iter() {
            let mut bytecode = Vec::new();
            //If the method is not static, reserve space for `this`
            if method.signature.modifier != HirStructMethodModifier::Static {
                self.local_variables.insert(THIS_NAME);
            }
            for arg in method.signature.params.iter() {
                self.local_variables.insert(arg.name);
            }

            self.generate_bytecode_block(&method.body, &mut bytecode)?;

            //There is no need to reserve space for local variables if there is none
            if self.local_variables.len() > 0 {
                bytecode.insert(
                    0,
                    Instruction::LocalSpace {
                        nb_vars: self.local_variables.len() as u8,
                    },
                );
            }

            if let HirTy::Unit(_) = method.signature.return_ty {
                let last_instruction = &bytecode[bytecode.len() - 1];
                match last_instruction {
                    Instruction::Return => {}
                    _ => bytecode.push(Instruction::Return),
                }
            } else {
                let last_instruction = &bytecode[bytecode.len() - 1];
                let path = method.span.path;
                let src = utils::get_file_content(path).unwrap();
                match last_instruction {
                    Instruction::Return => {}
                    _ => {
                        return Err(HirError::NoReturnInFunction(NoReturnInFunctionError {
                            span: method.span,
                            func_name: method.name.to_string(),
                            src: NamedSource::new(path, src),
                        }));
                    }
                }
            }

            let len = bytecode.len();
            let method_name = self.codegen_arena.alloc(
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
                body: self.codegen_arena.alloc(bytecode),
            });
            self.current_pos += len;
            self.local_variables.clear();
        }
        Ok(())
    }

    fn generate_bytecode_constructor(
        &mut self,
        struct_name: &str,
        constructor: &HirStructConstructor<'hir>,
        labels: &mut Vec<Label<'codegen>>,
    ) -> HirResult<()> {
        let mut bytecode = Vec::new();
        let params = constructor.params.clone();

        self.local_variables.insert(THIS_NAME);
        for arg in params.iter() {
            self.local_variables.insert(arg.name);
        }
        //self reference of the object
        let this_idx = self.local_variables.get_index(THIS_NAME).unwrap();

        self.generate_bytecode_block(&constructor.body, &mut bytecode)?;

        let local_space = self.local_variables.len() - params.len();
        if local_space > 0 {
            bytecode.insert(
                0,
                Instruction::LocalSpace {
                    // Reserve space for local variables excluding parameters
                    nb_vars: local_space as u8,
                },
            );
        }

        //Return the self reference
        bytecode.push(Instruction::LoadVar(this_idx));
        bytecode.push(Instruction::Return);

        let len = bytecode.len();
        labels.push(Label {
            name: self
                .codegen_arena
                .alloc(format!("{}.{}", struct_name, "new")),
            position: self.current_pos,
            body: self.codegen_arena.alloc(bytecode),
        });
        self.program.functions.insert(
            self.codegen_arena
                .alloc(format!("{}.{}", struct_name, "new")),
            self.current_pos,
        );
        self.current_pos += len;

        Ok(())
    }

    fn generate_bytecode_destructor(
        &mut self,
        struct_name: &str,
        destructor: &HirStructConstructor<'hir>,
        labels: &mut Vec<Label<'codegen>>,
    ) -> HirResult<()> {
        let mut bytecode = Vec::new();
        let params = destructor.params.clone();

        self.local_variables.insert(THIS_NAME);
        for arg in params.iter() {
            self.local_variables.insert(arg.name);
        }

        self.generate_bytecode_block(&destructor.body, &mut bytecode)?;

        let local_space = self.local_variables.len() - params.len();
        if local_space > 0 {
            bytecode.insert(
                0,
                Instruction::LocalSpace {
                    // Reserve space for local variables excluding parameters
                    nb_vars: local_space as u8,
                },
            );
        }

        //Return Unit
        bytecode.push(Instruction::LoadConst(ConstantValue::Unit));
        bytecode.push(Instruction::Return);

        let len = bytecode.len();
        labels.push(Label {
            name: self
                .codegen_arena
                .alloc(format!("{}.{}", struct_name, "destroy")),
            position: self.current_pos,
            body: self.codegen_arena.alloc(bytecode),
        });
        self.program.functions.insert(
            self.codegen_arena
                .alloc(format!("{}.{}", struct_name, "destroy")),
            self.current_pos,
        );
        self.current_pos += len;

        Ok(())
    }

    fn generate_struct_descriptor(&mut self, struct_name: &str, hir_struct: &HirStruct<'hir>) {
        let mut fields: Vec<&'codegen str> = Vec::new();
        let mut constants: BTreeMap<&'codegen str, ConstantValue> = BTreeMap::new();
        for field in hir_struct.fields.iter() {
            fields.push(self.codegen_arena.alloc(field.name.to_string()));
        }
        for (constant_name, constant) in hir_struct.signature.constants.iter() {
            constants.insert(
                self.codegen_arena.alloc(constant_name.to_string()),
                constant.value.clone(),
            );
        }
        let struct_constant = StructDescriptor {
            name: self.codegen_arena.alloc(struct_name.to_string()),
            fields,
            constants,
        };
        self.struct_pool.push(struct_constant);
    }
    fn generate_bytecode_block(
        &mut self,
        block: &HirBlock<'hir>,
        bytecode: &mut Vec<Instruction>,
    ) -> HirResult<()> {
        for stmt in &block.statements {
            self.generate_bytecode_stmt(stmt, bytecode)?;
        }
        Ok(())
    }

    fn generate_bytecode_stmt(
        &mut self,
        stmt: &HirStatement<'hir>,
        bytecode: &mut Vec<Instruction>,
    ) -> HirResult<()> {
        match stmt {
            HirStatement::Return(e) => {
                self.generate_bytecode_expr(&e.value, bytecode)?;
                bytecode.push(Instruction::Return);
            }
            HirStatement::IfElse(i) => {
                self.generate_bytecode_expr(&i.condition, bytecode)?;
                let mut then_body = Vec::new();
                self.generate_bytecode_block(&i.then_branch, &mut then_body)?;

                bytecode.push(Instruction::JmpZ {
                    //Jump over the `JMP else_body` instruction if there is an else branch
                    pos: (then_body.len() + if i.else_branch.is_some() { 1 } else { 0 }) as isize,
                });
                bytecode.append(&mut then_body);
                if let Some(e) = &i.else_branch {
                    let mut else_body = Vec::new();
                    self.generate_bytecode_block(e, &mut else_body)?;
                    //TODO: If the then body ends with Return or Halt, no need to jump to the else body
                    //TODO: But that would mean the `JmpZ` instruction would need to be slightly tweaked to
                    //NB: This is not semantically incorrect, but it's a waste of system memory
                    bytecode.push(Instruction::Jmp {
                        //No need to add 1 here because the runtime does self.pc += 1 after executing an instruction
                        pos: (else_body.len()) as isize,
                    });
                    bytecode.append(&mut else_body);
                }
            }
            HirStatement::While(w) => {
                let start = bytecode.len() as isize;
                self.generate_bytecode_expr(&w.condition, bytecode)?;
                let mut body = Vec::new();

                self.generate_bytecode_block(&w.body, &mut body)?;
                //If the condition is false jump to the end of the loop
                bytecode.push(Instruction::JmpZ {
                    pos: (body.len() + 1) as isize,
                });
                bytecode.append(&mut body);
                //Jump back to the start of the loop
                bytecode.push(Instruction::Jmp {
                    pos: start - bytecode.len() as isize - 1,
                });
            }
            HirStatement::Const(const_stmt) => {
                let mut value = Vec::new();
                self.generate_bytecode_expr(&const_stmt.value, &mut value)?;
                value.push(Instruction::StoreVar(
                    self.local_variables.insert(const_stmt.name),
                ));
                bytecode.append(&mut value);
            }
            HirStatement::Let(let_stmt) => {
                let mut value = Vec::new();
                self.generate_bytecode_expr(&let_stmt.value, &mut value)?;
                value.push(Instruction::StoreVar(
                    self.local_variables.insert(let_stmt.name),
                ));
                bytecode.append(&mut value);
            }
            HirStatement::Expr(e) => {
                self.generate_bytecode_expr(&e.expr, bytecode)?;
                if let Some(Instruction::LoadConst(ConstantValue::Unit)) = bytecode.last() {
                    bytecode.pop();
                    return Ok(());
                }
                //TODO: Remove this Pop for instructions that leave nothing on the stack
                //(e.g. function calls that return Unit)
                //NB: This is not semantically incorrect
                bytecode.push(Instruction::Pop);
            }
            _ => {
                let path = stmt.span().path;
                let src = utils::get_file_content(path).unwrap();
                return Err(HirError::UnsupportedStatement(UnsupportedStatement {
                    span: stmt.span(),
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
    ) -> HirResult<()> {
        match expr {
            HirExpr::Assign(a) => {
                let lhs = a.lhs.as_ref();
                match lhs {
                    HirExpr::Indexing(idx_expr) => {
                        match idx_expr.target.ty() {
                            HirTy::List(_) => {
                                //Get the index
                                self.generate_bytecode_expr(&idx_expr.index, bytecode)?;
                                //Get the list pointer
                                self.generate_bytecode_expr(&idx_expr.target, bytecode)?;
                                //Get the value to store
                                self.generate_bytecode_expr(&a.rhs, bytecode)?;
                                //Store the value in the list at the given index
                                bytecode.push(Instruction::IndexStore);
                            }
                            HirTy::String(_) => {
                                //Get the index
                                self.generate_bytecode_expr(&idx_expr.index, bytecode)?;
                                //Get the string pointer
                                self.generate_bytecode_expr(&idx_expr.target, bytecode)?;
                                //Get the value to store
                                self.generate_bytecode_expr(&a.rhs, bytecode)?;
                                //Store the value in the string at the given index
                                bytecode.push(Instruction::StringStore);
                            }
                            _ => {
                                return Err(Self::unsupported_expr_err(
                                    expr,
                                    format!("Indexing assignment for non-list type: {}", expr.ty()),
                                ));
                            }
                        }
                    }
                    HirExpr::Ident(ident) => {
                        self.generate_bytecode_expr(&a.rhs, bytecode)?;
                        bytecode.push(Instruction::StoreVar(
                            self.local_variables.get_index(ident.name).unwrap(),
                        ));
                    }
                    HirExpr::FieldAccess(field_access) => {
                        //Get the Struct pointer
                        self.generate_bytecode_expr(&field_access.target, bytecode)?;
                        //Get the value
                        self.generate_bytecode_expr(&a.rhs, bytecode)?;
                        let struct_name = match self.get_class_name_of_type(field_access.target.ty()) {
                            Some(n) => n,
                            None => {
                                return Err(Self::unsupported_expr_err(
                                    expr,
                                    format!("[CodeGen] No field access for: {}", field_access.target.ty()),
                                ));
                            }
                        };
                        let struct_descriptor = self
                            .struct_pool
                            .iter()
                            .find(|c| c.name == struct_name)
                            .unwrap_or_else(|| {
                                //should never happen
                                panic!("Struct {} not found", struct_name)
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
                        return Err(Self::unsupported_expr_err(
                            expr,
                            format!("Unsupported type: {}", expr.ty()),
                        ));
                    }
                }
                bytecode.push(Instruction::LoadConst(ConstantValue::Unit));
            }
            HirExpr::HirBinaryOperation(b) => {
                self.generate_bytecode_expr(&b.lhs, bytecode)?;
                self.generate_bytecode_expr(&b.rhs, bytecode)?;
                match b.op {
                    HirBinaryOperator::Add => bytecode.push(Instruction::Add(b.ty.into())),
                    HirBinaryOperator::Sub => bytecode.push(Instruction::Sub(b.ty.into())),
                    HirBinaryOperator::Mul => bytecode.push(Instruction::Mul(b.ty.into())),
                    HirBinaryOperator::Div => bytecode.push(Instruction::Div(b.ty.into())),
                    HirBinaryOperator::Mod => bytecode.push(Instruction::Mod(b.ty.into())),
                    HirBinaryOperator::Eq => bytecode.push(Instruction::Eq(b.ty.into())),
                    HirBinaryOperator::Neq => bytecode.push(Instruction::Neq(b.ty.into())),
                    HirBinaryOperator::Gt => bytecode.push(Instruction::Gt(b.ty.into())),
                    HirBinaryOperator::Gte => bytecode.push(Instruction::Gte(b.ty.into())),
                    HirBinaryOperator::Lt => bytecode.push(Instruction::Lt(b.ty.into())),
                    HirBinaryOperator::Lte => bytecode.push(Instruction::Lte(b.ty.into())),
                    HirBinaryOperator::And => {
                        bytecode.push(Instruction::And);
                    }
                    HirBinaryOperator::Or => {
                        bytecode.push(Instruction::Or);
                    }
                }
            }
            HirExpr::Unary(u) => {
                //There is no unary instruction, so -x is the same as 0 - x
                //And !x is the same as x == 0
                self.generate_bytecode_expr(&u.expr, bytecode)?;
                if let Some(op) = &u.op {
                    match op {
                        HirUnaryOp::Neg => {
                            match u.expr.ty() {
                                HirTy::Int64(_) => {
                                    bytecode.push(Instruction::LoadConst(ConstantValue::Int(0)));
                                }
                                HirTy::Float64(_) => {
                                    bytecode
                                        .push(Instruction::LoadConst(ConstantValue::Float(0.0)));
                                }
                                // This won't really work, because you're subtracting a 32-bit char from a 64-bit integer
                                HirTy::Char(_) => {
                                    bytecode.push(Instruction::LoadConst(ConstantValue::Char(
                                        0 as char,
                                    )));
                                }
                                _ => {
                                    return Err(Self::unsupported_expr_err(
                                        expr,
                                        format!("Can't negate: {}", expr.ty()),
                                    ));
                                }
                            }
                            bytecode.push(Instruction::Swap);
                            bytecode.push(Instruction::Sub(u.expr.ty().into()));
                        }
                        HirUnaryOp::Not => {
                            if let HirTy::Boolean(_) = u.expr.ty() {
                                bytecode.push(Instruction::LoadConst(ConstantValue::Bool(false)));
                                bytecode.push(Instruction::Eq(Type::Boolean));
                            } else {
                                return Err(Self::unsupported_expr_err(
                                    expr,
                                    format!("Can't negate: {}", expr.ty()),
                                ));
                            }
                        }
                        HirUnaryOp::AsReadOnlyRef | HirUnaryOp::AsMutableRef => {
                            //No instruction needed, just a type change &const T and T are represented the same in memory
                            //It's the type system that prevents misuse
                        }
                    }
                }
            }
            HirExpr::Casting(c) => {
                self.generate_bytecode_expr(&c.expr, bytecode)?;
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
                        return Err(Self::unsupported_expr_err(
                            expr,
                            format!("Can't cast from {} to: {}", c.expr.ty(), c.ty),
                        ));
                    }
                }
            }
            HirExpr::Indexing(idx) => {
                match idx.target.ty() {
                    HirTy::List(_) => {
                        //Get the index
                        self.generate_bytecode_expr(&idx.index, bytecode)?;
                        //Get the list pointer
                        self.generate_bytecode_expr(&idx.target, bytecode)?;
                        //Load the value at the given index
                        bytecode.push(Instruction::IndexLoad);
                    }
                    HirTy::String(_) => {
                        //Get the index
                        self.generate_bytecode_expr(&idx.index, bytecode)?;
                        //Get the string pointer
                        self.generate_bytecode_expr(&idx.target, bytecode)?;
                        //Load the char at the given index
                        bytecode.push(Instruction::StringLoad);
                    }
                    _ => {
                        return Err(Self::unsupported_expr_err(
                            expr,
                            format!("Indexing for non list type: {}", expr.ty()),
                        ));
                    }
                }
            }
            HirExpr::Call(f) => {
                let callee = f.callee.as_ref();
                match callee {
                    HirExpr::Ident(i) => {
                        for arg in &f.args {
                            self.generate_bytecode_expr(arg, bytecode)?;
                        }
                        let func = self.hir.signature.functions.get(i.name).unwrap();
                        if func.is_external {
                            bytecode.push(Instruction::ExternCall {
                                func_name: i.name.to_string(),
                            });
                        } else {
                            bytecode.push(Instruction::Call {
                                func_name: i.name.to_string(),
                                nb_args: f.args.len() as u8,
                            });
                        }
                    }
                    HirExpr::FieldAccess(field_access) => {
                        //Get the Class pointer:
                        self.generate_bytecode_expr(&field_access.target, bytecode)?;
                        for arg in &f.args {
                            self.generate_bytecode_expr(arg, bytecode)?;
                        }
                        let struct_name = match self.get_class_name_of_type(field_access.target.ty()) {
                            Some(n) => n,
                            _ => {
                                return Err(Self::unsupported_expr_err(
                                    expr,
                                    format!("Can't call from: {}", field_access.target.ty()),
                                ));
                            }
                        };
                        bytecode.push(Instruction::Call {
                            func_name: format!("{}.{}", struct_name, field_access.field.name),
                            nb_args: f.args.len() as u8 + 1,
                        })
                    }
                    HirExpr::StaticAccess(static_access) => {
                        let name = match static_access.target.as_ref() {
                            HirTy::Named(n) => n.name,
                            HirTy::Generic(g) => {
                                MonomorphizationPass::mangle_generic_struct_name(self.hir_arena, g)
                            }
                            _ => {
                                return Err(Self::unsupported_expr_err(
                                    expr,
                                    format!("Can't call from: {}", expr.ty()),
                                ));
                            }
                        };
                        for arg in &f.args {
                            self.generate_bytecode_expr(arg, bytecode)?;
                        }
                        bytecode.push(Instruction::Call {
                            func_name: format!("{}::{}", name, static_access.field.name),
                            nb_args: f.args.len() as u8,
                        })
                    }
                    _ => {
                        return Err(Self::unsupported_expr_err(
                            expr,
                            format!("Can't call from: {}", expr.ty()),
                        ));
                    }
                }
            }
            HirExpr::IntegerLiteral(i) => {
                bytecode.push(Instruction::LoadConst(ConstantValue::Int(i.value)))
            }
            HirExpr::FloatLiteral(f) => {
                bytecode.push(Instruction::LoadConst(ConstantValue::Float(f.value)))
            }
            HirExpr::BooleanLiteral(b) => {
                bytecode.push(Instruction::LoadConst(ConstantValue::Bool(b.value)))
            }
            HirExpr::CharLiteral(c) => {
                bytecode.push(Instruction::LoadConst(ConstantValue::Char(c.value)))
            }
            HirExpr::UnitLiteral(_) => bytecode.push(Instruction::LoadConst(ConstantValue::Unit)),
            HirExpr::UnsignedIntegerLiteral(u) => {
                bytecode.push(Instruction::LoadConst(ConstantValue::UInt(u.value))); //TODO: Change to PushUnsignedInt when supported
            }
            HirExpr::StringLiteral(s) => bytecode.push(Instruction::LoadConst(
                ConstantValue::String(s.value.to_string()),
            )),
            HirExpr::Ident(i) => {
                let var_index = match self.local_variables.get_index(i.name) {
                    Some(idx) => idx,
                    None => {
                        return Err(Self::unsupported_expr_err(
                            expr,
                            format!("Variable {} not found", i.name),
                        ));
                    }
                };
                bytecode.push(Instruction::LoadVar(var_index))
            }
            HirExpr::ThisLiteral(this) => {
                let var_index = match self.local_variables.get_index(THIS_NAME) {
                    Some(idx) => idx,
                    None => {
                        return Err(Self::unsupported_expr_err(
                            expr,
                            format!("`this` isn't used in a struct context: {}", this.ty),
                        ));
                    }
                };
                bytecode.push(Instruction::LoadVar(var_index))
            }
            HirExpr::FieldAccess(field_access) => {
                self.generate_bytecode_expr(field_access.target.as_ref(), bytecode)?;
                let struct_name = match self.get_class_name_of_type(field_access.target.ty()) {
                    Some(n) => n,
                    None => {
                        return Err(Self::unsupported_expr_err(
                            expr,
                            format!("Field access for: {}", field_access.target.ty()),
                        ));
                    }
                };
                let struct_descriptor = self
                    .struct_pool
                    .iter()
                    .find(|c| c.name == struct_name)
                    .unwrap_or_else(|| {
                        //should never happen
                        panic!("Struct {} not found", struct_name)
                    });
                //get the position of the field
                let field = struct_descriptor
                    .fields
                    .iter()
                    .position(|f| *f == field_access.field.name)
                    .unwrap();
                bytecode.push(Instruction::GetField { field })
            }
            HirExpr::StaticAccess(static_access) => {
                let struct_name = match static_access.target.as_ref() {
                    HirTy::Named(n) => n.name,
                    HirTy::Generic(g) => {
                        MonomorphizationPass::mangle_generic_struct_name(self.hir_arena, g)
                    }
                    _ => {
                        return Err(Self::unsupported_expr_err(
                            expr,
                            format!("Can't call from: {}", expr.ty()),
                        ));
                    }
                };
                match static_access.field.ty {
                    HirTy::String(_) => {
                        let struct_signature = self.hir.signature.structs.get(struct_name).unwrap();
                        let value = match struct_signature
                            .constants
                            .get(static_access.field.name)
                            .unwrap()
                            .value
                        {
                            ConstantValue::String(s) => String::from(s),
                            _ => {
                                return Err(Self::unsupported_expr_err(
                                    expr,
                                    format!("No string constant for: {}", expr.ty()),
                                ));
                            }
                        };
                        bytecode.push(Instruction::LoadConst(ConstantValue::String(value)));
                    }
                    HirTy::Float64(_) => {
                        let struct_signature = self.hir.signature.structs.get(struct_name).unwrap();
                        let value = match struct_signature
                            .constants
                            .get(static_access.field.name)
                            .unwrap()
                            .value
                        {
                            ConstantValue::Float(f) => *f,
                            _ => {
                                return Err(Self::unsupported_expr_err(
                                    expr,
                                    format!("No float constant for: {}", expr.ty()),
                                ));
                            }
                        };
                        bytecode.push(Instruction::LoadConst(ConstantValue::Float(value)));
                    }
                    HirTy::Int64(_) => {
                        let struct_signature = self.hir.signature.structs.get(struct_name).unwrap();
                        let value = match struct_signature
                            .constants
                            .get(static_access.field.name)
                            .unwrap()
                            .value
                        {
                            ConstantValue::Int(i) => *i,
                            _ => {
                                return Err(Self::unsupported_expr_err(
                                    expr,
                                    format!("No int constant for: {}", expr.ty()),
                                ));
                            }
                        };
                        bytecode.push(Instruction::LoadConst(ConstantValue::Int(value)));
                    }
                    HirTy::Char(_) => {
                        let struct_signature = self.hir.signature.structs.get(struct_name).unwrap();
                        let value = match struct_signature
                            .constants
                            .get(static_access.field.name)
                            .unwrap()
                            .value
                        {
                            ConstantValue::Char(c) => *c,
                            _ => {
                                return Err(Self::unsupported_expr_err(
                                    expr,
                                    format!("No char constant for: {}", expr.ty()),
                                ));
                            }
                        };
                        bytecode.push(Instruction::LoadConst(ConstantValue::Char(value)));
                    }
                    HirTy::UInt64(_) => {
                        let struct_signature = self.hir.signature.structs.get(struct_name).unwrap();
                        let value = match struct_signature
                            .constants
                            .get(static_access.field.name)
                            .unwrap()
                            .value
                        {
                            ConstantValue::UInt(u) => *u,
                            _ => {
                                return Err(Self::unsupported_expr_err(
                                    expr,
                                    format!("No uint constant for: {}", expr.ty()),
                                ));
                            }
                        };
                        bytecode.push(Instruction::LoadConst(ConstantValue::UInt(value)));
                    }
                    HirTy::List(_) => {
                        return Err(Self::unsupported_expr_err(
                            expr,
                            format!("Lists aren't supported as constants now: {}", expr.ty()),
                        ));
                    }
                    _ => {
                        return Err(Self::unsupported_expr_err(
                            expr,
                            format!("Unsupported type for now: {}", expr.ty()),
                        ));
                    }
                }
            }
            HirExpr::NewObj(new_obj) => {
                let name = match &new_obj.ty {
                    HirTy::Named(n) => n.name,
                    HirTy::Generic(g) => {
                        MonomorphizationPass::mangle_generic_struct_name(self.hir_arena, g)
                    }
                    _ => {
                        return Err(Self::unsupported_expr_err(
                            expr,
                            format!("Can't instantiate objects of type: {}", expr.ty()),
                        ));
                    }
                };
                let obj_descriptor = self
                    .struct_pool
                    .iter()
                    .position(|s| s.name == name)
                    .unwrap();
                bytecode.push(Instruction::NewObj { obj_descriptor });
                for arg in &new_obj.args {
                    self.generate_bytecode_expr(arg, bytecode)?;
                }
                bytecode.push(Instruction::Call {
                    func_name: format!("{}.new", name),
                    nb_args: (new_obj.args.len() + 1) as u8, // +1 for the this pointer
                });
            }
            HirExpr::NewArray(new_array) => {
                self.generate_bytecode_expr(&new_array.size, bytecode)?;
                bytecode.push(Instruction::NewArray)
            }
            HirExpr::Delete(delete) => {
                let name = match &delete.expr.ty() {
                    HirTy::Named(_) | HirTy::ReadOnlyReference(_) | HirTy::MutableReference(_) | HirTy::Generic(_) => {
                        self.get_class_name_of_type(delete.expr.ty()).unwrap()
                    },
                    HirTy::String(_) | HirTy::List(_) => {
                        //Strings and Lists have their own delete instruction
                        self.generate_bytecode_expr(&delete.expr, bytecode)?;
                        bytecode.push(Instruction::DeleteObj);
                        return Ok(());
                    }
                    _ => {
                        return Err(Self::unsupported_expr_err(
                            expr,
                            format!("Unsupported expression: {}", expr.ty()),
                        ));
                    }
                };

                let _ = self
                    .struct_pool
                    .iter()
                    .find(|c| c.name == name)
                    .unwrap_or_else(|| {
                        //should never happen
                        panic!("Struct {} not found", name)
                    });
                //Call the destructor
                self.generate_bytecode_expr(&delete.expr, bytecode)?;
                bytecode.push(Instruction::Call {
                    func_name: format!("{}.destroy", name),
                    nb_args: 1,
                });
                //Free the object memory
                bytecode.push(Instruction::DeleteObj);
            }
            _ => {
                return Err(Self::unsupported_expr_err(
                    expr,
                    format!("Unsupported expression: {}", expr.ty()),
                ));
            }
        }
        Ok(())
    }

    fn unsupported_expr_err(expr: &HirExpr, message: String) -> HirError {
        let path = expr.span().path;
        let src = utils::get_file_content(path).unwrap();
        HirError::UnsupportedExpr(UnsupportedExpr {
            span: expr.span(),
            expr: message,
            src: NamedSource::new(path, src),
        })
    }

    fn is_std(path: &str) -> bool {
        path.starts_with("std")
    }

    fn get_class_name_of_type(&self, ty: &HirTy<'hir>) -> Option<&'hir str> {
        match ty {
            HirTy::Named(n) => Some(n.name),
            HirTy::Generic(g) => Some(MonomorphizationPass::mangle_generic_struct_name(
                self.hir_arena,
                g,
            )),
            HirTy::ReadOnlyReference(read_only) => self.get_class_name_of_type(read_only.inner),
            HirTy::MutableReference(mutable) => self.get_class_name_of_type(mutable.inner),
            _ => None,
        }
    }

}
