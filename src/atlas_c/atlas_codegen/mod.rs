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
use crate::atlas_c::atlas_hir::ty::HirGenericTy;
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
        // Don't process unions - they're not real objects, just type-level constructs
        // Union field access should be transparent (no-op)

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

        //generate copy constructor
        if let Some(copy_ctor) = &hir_struct.copy_constructor {
            self.generate_bytecode_copy_constructor(struct_name, copy_ctor, labels)?;
            self.local_variables.clear();
        }

        for method in hir_struct.methods.iter() {
            let mut bytecode = Vec::new();
            //If the method is not static, reserve space for `this`
            let has_this = method.signature.modifier != HirStructMethodModifier::Static;
            if has_this {
                self.local_variables.insert(THIS_NAME);
            }
            for arg in method.signature.params.iter() {
                self.local_variables.insert(arg.name);
            }

            self.generate_bytecode_block(&method.body, &mut bytecode)?;

            //There is no need to reserve space for local variables if there is none
            // Methods: LocalSpace should only reserve space for locals, not for parameters
            // Parameters (including 'this' for non-static methods) are already placed by Call instruction
            let num_params = method.signature.params.len() + if has_this { 1 } else { 0 };
            let local_space = self.local_variables.len() - num_params;
            if local_space > 0 {
                bytecode.insert(
                    0,
                    Instruction::LocalSpace {
                        nb_vars: local_space as u8,
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

    fn generate_bytecode_copy_constructor(
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
                .alloc(format!("{}_copy_ctor", struct_name)),
            position: self.current_pos,
            body: self.codegen_arena.alloc(bytecode),
        });
        self.program.functions.insert(
            self.codegen_arena
                .alloc(format!("{}_copy_ctor", struct_name)),
            self.current_pos,
        );
        self.current_pos += len;

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
            name: self.codegen_arena.alloc(format!("{}_ctor", struct_name)),
            position: self.current_pos,
            body: self.codegen_arena.alloc(bytecode),
        });
        self.program.functions.insert(
            self.codegen_arena.alloc(format!("{}_ctor", struct_name)),
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

        // Destructor has implicit 'this' parameter passed via Call (nb_args=1),
        // but 'this' is in local_variables (not in params list).
        // LocalSpace should only reserve space for locals, not for 'this' which is already placed by Call.
        let local_space = self.local_variables.len() - params.len() - 1; // -1 for implicit 'this'
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
            name: self.codegen_arena.alloc(format!("{}_dtor", struct_name)),
            position: self.current_pos,
            body: self.codegen_arena.alloc(bytecode),
        });
        self.program.functions.insert(
            self.codegen_arena.alloc(format!("{}_dtor", struct_name)),
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
            is_union: false,
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
                let before_len = bytecode.len();
                self.generate_bytecode_expr(&e.expr, bytecode)?;
                let after_len = bytecode.len();

                // If no bytecode was generated (e.g., delete of primitives), don't add Pop
                if before_len == after_len {
                    return Ok(());
                }

                // Skip Pop for instructions that don't leave anything on the stack
                match bytecode.last() {
                    Some(Instruction::LoadConst(ConstantValue::Unit)) => {
                        bytecode.pop();
                        return Ok(());
                    }
                    // DeleteObj for strings/lists doesn't push anything to the stack
                    Some(Instruction::DeleteObj) => {
                        return Ok(());
                    }
                    _ => {}
                }
                //TODO: Remove this Pop for instructions that leave nothing on the stack
                //(e.g. function calls that return Unit)
                //NB: This is not semantically incorrect
                bytecode.push(Instruction::Pop);
            }
            HirStatement::Block(block) => {
                for stmt in &block.statements {
                    self.generate_bytecode_stmt(stmt, bytecode)?;
                }
            }
            HirStatement::Assign(assign) => {
                let dst = &assign.dst;
                match dst {
                    HirExpr::Indexing(idx_expr) => {
                        match idx_expr.target.ty() {
                            HirTy::List(_) => {
                                //Get the index
                                self.generate_bytecode_expr(&idx_expr.index, bytecode)?;
                                //Get the list pointer
                                self.generate_bytecode_expr(&idx_expr.target, bytecode)?;
                                //Get the value to store
                                self.generate_bytecode_expr(&assign.val, bytecode)?;
                                //Store the value in the list at the given index
                                bytecode.push(Instruction::IndexStore);
                            }
                            HirTy::String(_) => {
                                //Get the index
                                self.generate_bytecode_expr(&idx_expr.index, bytecode)?;
                                //Get the string pointer
                                self.generate_bytecode_expr(&idx_expr.target, bytecode)?;
                                //Get the value to store
                                self.generate_bytecode_expr(&assign.val, bytecode)?;
                                //Store the value in the string at the given index
                                bytecode.push(Instruction::StringStore);
                            }
                            _ => {
                                return Err(Self::unsupported_expr_err(
                                    dst,
                                    format!("Indexing assignment for non-list type: {}", dst.ty()),
                                ));
                            }
                        }
                    }
                    HirExpr::Ident(ident) => {
                        self.generate_bytecode_expr(&assign.val, bytecode)?;
                        bytecode.push(Instruction::StoreVar(
                            self.local_variables.get_index(ident.name).unwrap(),
                        ));
                    }
                    HirExpr::FieldAccess(field_access) => {
                        //Get the Struct pointer
                        self.generate_bytecode_expr(&field_access.target, bytecode)?;
                        //Get the value
                        self.generate_bytecode_expr(&assign.val, bytecode)?;
                        let struct_name =
                            match self.get_struct_name_of_type(field_access.target.ty()) {
                                Some(n) => n,
                                None => {
                                    return Err(Self::unsupported_expr_err(
                                        dst,
                                        format!(
                                            "[CodeGen] No field access for: {}",
                                            field_access.target.ty()
                                        ),
                                    ));
                                }
                            };
                        let struct_descriptor = match self
                            .struct_pool
                            .iter()
                            .find(|c| c.name == struct_name)
                        {
                            Some(s) => s,
                            None => {
                                let union_name = match self
                                    .get_union_name_of_type(field_access.target.ty())
                                {
                                    Some(n) => n,
                                    None => {
                                        return Err(Self::unsupported_expr_err(
                                            dst,
                                            format!(
                                                "[CodeGen] No struct or union descriptor for: {}",
                                                struct_name
                                            ),
                                        ));
                                    }
                                };
                                if self.hir.signature.unions.contains_key(union_name) {
                                    // No operation needed for union field access
                                    return Ok(());
                                }
                                return Err(Self::unsupported_expr_err(
                                    dst,
                                    format!("[CodeGen] No struct descriptor for: {}", struct_name),
                                ));
                            }
                        };
                        //get the position of the field
                        let field = struct_descriptor
                            .fields
                            .iter()
                            .position(|f| *f == field_access.field.name)
                            .unwrap();
                        //Store the value in the field
                        bytecode.push(Instruction::SetField { field });
                    }
                    // Handle assignment through dereference: *ref = value
                    HirExpr::Unary(u) if u.op == Some(HirUnaryOp::Deref) => {
                        // First, get the reference (the address to store to)
                        self.generate_bytecode_expr(&u.expr, bytecode)?;
                        // Then, get the value to store
                        self.generate_bytecode_expr(&assign.val, bytecode)?;
                        // Store through the reference
                        bytecode.push(Instruction::StoreIndirect);
                    }
                    _ => {
                        return Err(Self::unsupported_expr_err(
                            dst,
                            format!("Unsupported type: {}", dst.ty()),
                        ));
                    }
                }
                bytecode.push(Instruction::LoadConst(ConstantValue::Unit));
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
            HirExpr::HirBinaryOperation(b) => {
                let mut lhs_bytecode: Vec<Instruction> = vec![];
                let mut rhs_bytecode: Vec<Instruction> = vec![];
                self.generate_bytecode_expr(&b.lhs, &mut lhs_bytecode)?;
                self.generate_bytecode_expr(&b.rhs, &mut rhs_bytecode)?;
                let instruction = match b.op {
                    HirBinaryOperator::Add => Instruction::Add(b.lhs.ty().into()),
                    HirBinaryOperator::Sub => Instruction::Sub(b.lhs.ty().into()),
                    HirBinaryOperator::Mul => Instruction::Mul(b.lhs.ty().into()),
                    HirBinaryOperator::Div => Instruction::Div(b.lhs.ty().into()),
                    HirBinaryOperator::Mod => Instruction::Mod(b.lhs.ty().into()),
                    HirBinaryOperator::Eq => {
                        // If the typechecking pass did a good job, both sides should have the same type
                        if let Some(ty) = b.lhs.ty().get_inner_ref_ty() {
                            lhs_bytecode.push(Instruction::LoadIndirect);
                            rhs_bytecode.push(Instruction::LoadIndirect);
                            Instruction::Eq(ty.into())
                        } else {
                            Instruction::Eq(b.lhs.ty().into())
                        }
                    }
                    HirBinaryOperator::Neq => {
                        // If the typechecking pass did a good job, both sides should have the same type
                        if let Some(ty) = b.lhs.ty().get_inner_ref_ty() {
                            lhs_bytecode.push(Instruction::LoadIndirect);
                            rhs_bytecode.push(Instruction::LoadIndirect);
                            Instruction::Neq(ty.into())
                        } else {
                            Instruction::Neq(b.lhs.ty().into())
                        }
                    }
                    HirBinaryOperator::Gt => Instruction::Gt(b.lhs.ty().into()),
                    HirBinaryOperator::Gte => Instruction::Gte(b.lhs.ty().into()),
                    HirBinaryOperator::Lt => Instruction::Lt(b.lhs.ty().into()),
                    HirBinaryOperator::Lte => Instruction::Lte(b.lhs.ty().into()),
                    HirBinaryOperator::And => Instruction::And,
                    HirBinaryOperator::Or => Instruction::Or,
                };
                bytecode.extend(lhs_bytecode);
                bytecode.extend(rhs_bytecode);
                bytecode.push(instruction);
            }
            HirExpr::Unary(u) => {
                // Handle AsRef specially - don't generate the inner expression's value first
                if let Some(HirUnaryOp::AsRef) = &u.op {
                    self.generate_bytecode_ref_expr(&u.expr, bytecode)?;
                    return Ok(());
                }

                // For other unary ops, first generate the inner expression
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
                        HirUnaryOp::AsRef => {
                            // This case should never be reached because we handle AsRef at the start of HirExpr::Unary
                            unreachable!(
                                "AsRef should be handled before generating inner expression bytecode"
                            );
                        }
                        HirUnaryOp::Deref => {
                            // The expression is a reference, we need to load the value at that address
                            // The reference value is already on the stack from generate_bytecode_expr
                            bytecode.push(Instruction::LoadIndirect);
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
            HirExpr::Call(func_expr) => {
                let callee = func_expr.callee.as_ref();
                match callee {
                    HirExpr::Ident(i) => {
                        for arg in &func_expr.args {
                            self.generate_bytecode_expr(arg, bytecode)?;
                        }
                        let base_func = self.hir.signature.functions.get(i.name);
                        let is_external = base_func.map(|f| f.is_external).unwrap_or(false);
                        let name = if func_expr.generics.is_empty() || is_external {
                            i.name
                        } else {
                            MonomorphizationPass::generate_mangled_name(
                                self.hir_arena,
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
                        if is_external {
                            bytecode.push(Instruction::ExternCall {
                                func_name: name.to_string(),
                            });
                        } else {
                            bytecode.push(Instruction::Call {
                                func_name: name.to_string(),
                                nb_args: func_expr.args.len() as u8,
                            });
                        }
                    }
                    HirExpr::FieldAccess(field_access) => {
                        // Get the struct name first to look up method signature
                        let struct_name =
                            match self.get_struct_name_of_type(field_access.target.ty()) {
                                Some(n) => n,
                                _ => {
                                    return Err(Self::unsupported_expr_err(
                                        expr,
                                        format!("Can't call from: {}", field_access.target.ty()),
                                    ));
                                }
                            };

                        // Check if the method takes a reference to `this` (only Const modifier)
                        // Note: &this (Mutable) methods do NOT take refs, they pass `this` directly
                        // but just don't consume ownership. Only &const this (Const) takes a reference.
                        let method_takes_ref = self
                            .hir
                            .signature
                            .structs
                            .get(struct_name)
                            .and_then(|s| s.methods.get(field_access.field.name))
                            //&this (mutable) methods do not take refs, only &const this methods do
                            //&this methods are similar to passing by value, but they just don't take ownership
                            .map(|m| matches!(m.modifier, HirStructMethodModifier::Const))
                            .unwrap_or(false);

                        // Generate receiver: address if method takes ref, value otherwise
                        if method_takes_ref {
                            // Need to pass the address of the receiver
                            self.generate_receiver_addr(&field_access.target, bytecode)?;
                        } else {
                            // Pass the value (ownership transfer)
                            self.generate_bytecode_expr(&field_access.target, bytecode)?;
                        }

                        for arg in &func_expr.args {
                            self.generate_bytecode_expr(arg, bytecode)?;
                        }
                        bytecode.push(Instruction::Call {
                            func_name: format!("{}.{}", struct_name, field_access.field.name),
                            nb_args: func_expr.args.len() as u8 + 1,
                        })
                    }
                    HirExpr::StaticAccess(static_access) => {
                        let name = match static_access.target {
                            HirTy::Named(n) => n.name,
                            HirTy::Generic(g) => MonomorphizationPass::generate_mangled_name(
                                self.hir_arena,
                                g,
                                "struct",
                            ),
                            _ => {
                                return Err(Self::unsupported_expr_err(
                                    expr,
                                    format!("Can't call from: {}", expr.ty()),
                                ));
                            }
                        };
                        for arg in &func_expr.args {
                            self.generate_bytecode_expr(arg, bytecode)?;
                        }
                        bytecode.push(Instruction::Call {
                            func_name: format!("{}::{}", name, static_access.field.name),
                            nb_args: func_expr.args.len() as u8,
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
            HirExpr::ListLiteral(list) => {
                bytecode.push(Instruction::LoadConst(ConstantValue::UInt(
                    list.items.len() as u64,
                )));
                bytecode.push(Instruction::NewArray);
                for (i, item) in list.items.iter().enumerate() {
                    bytecode.push(Instruction::Dup);
                    bytecode.push(Instruction::LoadConst(ConstantValue::UInt(i as u64)));
                    bytecode.push(Instruction::Swap);
                    self.generate_bytecode_expr(item, bytecode)?;
                    bytecode.push(Instruction::IndexStore);
                }
            }
            HirExpr::Ident(i) => {
                let var_index = match self.local_variables.get_index(i.name) {
                    Some(idx) => idx,
                    None => {
                        return Err(Self::unsupported_expr_err(
                            expr,
                            format!("STUFF: Variable {} not found", i.name),
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
                // Check if target is a reference type
                let is_ref = matches!(
                    field_access.target.ty(),
                    HirTy::MutableReference(_) | HirTy::ReadOnlyReference(_)
                );
                // If target is a reference, dereference it first to get the object pointer
                if is_ref {
                    bytecode.push(Instruction::LoadIndirect);
                }
                let obj_name = match self.get_struct_name_of_type(field_access.target.ty()) {
                    Some(n) => n,
                    None => {
                        return Err(Self::unsupported_expr_err(
                            expr,
                            format!("Field access for: {}", field_access.target.ty()),
                        ));
                    }
                };
                if let Some(struct_descriptor) =
                    self.struct_pool.iter().find(|s| s.name == obj_name)
                {
                    // Get the position of the field
                    let field = struct_descriptor
                        .fields
                        .iter()
                        .position(|f| *f == field_access.field.name)
                        .unwrap();
                    // For unions, all fields are at index 0 (same memory location)
                    // We still generate the instruction but always use field 0
                    let actual_field = if struct_descriptor.is_union { 0 } else { field };
                    // If accessing through a reference, return field address (reference to field)
                    // Otherwise return field value
                    if is_ref {
                        bytecode.push(Instruction::GetFieldAddr {
                            field: actual_field,
                        });
                    } else {
                        bytecode.push(Instruction::GetField {
                            field: actual_field,
                        });
                    }
                } else {
                    // This is a union field access - unions aren't in struct_pool
                    // Union field access is a no-op at runtime (type-checking only)
                    // The union value is already on the stack, do nothing
                }
            }
            HirExpr::StaticAccess(static_access) => {
                let struct_name = match static_access.target {
                    HirTy::Named(n) => n.name,
                    HirTy::Generic(g) => {
                        MonomorphizationPass::generate_mangled_name(self.hir_arena, g, "struct")
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
            // Only used for union literals for now.
            HirExpr::ObjLiteral(obj_lit) => {
                // Unions are not objects - just push the underlying value
                self.generate_bytecode_expr(&obj_lit.fields[0].value, bytecode)?;
            }
            HirExpr::NewObj(new_obj) => {
                let name = match &new_obj.ty {
                    HirTy::Named(n) => n.name,
                    HirTy::Generic(g) => {
                        MonomorphizationPass::generate_mangled_name(self.hir_arena, g, "struct")
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
                if new_obj.is_copy_constructor_call {
                    bytecode.push(Instruction::Call {
                        func_name: format!("{}_copy_ctor", name),
                        nb_args: 2, // &this + from reference
                    });
                } else {
                    bytecode.push(Instruction::Call {
                        func_name: format!("{}_ctor", name),
                        nb_args: (new_obj.args.len() + 1) as u8, // +1 for the this pointer
                    });
                }
            }
            HirExpr::NewArray(new_array) => {
                self.generate_bytecode_expr(&new_array.size, bytecode)?;
                bytecode.push(Instruction::NewArray)
            }
            HirExpr::Delete(delete) => {
                let name = match &delete.expr.ty() {
                    HirTy::Named(_) | HirTy::Generic(_) => {
                        self.get_struct_name_of_type(delete.expr.ty()).unwrap()
                    }
                    HirTy::String(_) | HirTy::List(_) => {
                        //Strings and Lists have their own delete instruction
                        self.generate_bytecode_expr(&delete.expr, bytecode)?;
                        bytecode.push(Instruction::DeleteObj);
                        return Ok(());
                    }
                    _ => {
                        //Just ignore delete for primitive types & references
                        return Ok(());
                    }
                };

                let _ = self
                    .struct_pool
                    .iter()
                    .find(|c| c.name == name)
                    .unwrap_or_else(|| {
                        //should never happen
                        let err = Self::unsupported_expr_err(
                            expr,
                            format!("Can't delete object of type: {}", delete.expr.ty()),
                        );
                        eprintln!("{:?}", Into::<miette::Report>::into(err));
                        std::process::exit(1);
                    });
                // Generate the object pointer, duplicate it so we can both call destroy and free
                self.generate_bytecode_expr(&delete.expr, bytecode)?;
                bytecode.push(Instruction::Dup); // Stack: [obj_ptr, obj_ptr]
                // Call the destructor (consumes one copy)
                bytecode.push(Instruction::Call {
                    func_name: format!("{}_dtor", name),
                    nb_args: 1,
                }); // Stack: [obj_ptr, unit]
                // Pop the Unit return value from destroy
                bytecode.push(Instruction::Pop); // Stack: [obj_ptr]
                // Free the object memory
                bytecode.push(Instruction::DeleteObj); // Stack: []
            }
            // Move expressions: ownership transfer, just generate the inner expression
            // The ownership semantics are handled by the ownership pass
            HirExpr::Move(move_expr) => {
                self.generate_bytecode_expr(&move_expr.expr, bytecode)?;
            }
            // Copy expressions: for primitives, just generate the inner expression (bitwise copy)
            // For objects with a copy constructor, call it.
            // For strings, deep copy using CloneString instruction
            HirExpr::Copy(copy_expr) => {
                // Check if this is an object type that needs copy constructor call
                match copy_expr.ty {
                    HirTy::Named(named) => {
                        // A copy constructor signature is: `Name(this, from: &const Name) -> Name`
                        if let Some(obj_descriptor) =
                            self.struct_pool.iter().position(|s| s.name == named.name)
                        {
                            bytecode.push(Instruction::NewObj { obj_descriptor });
                            // The copy constructor takes &const this, so we need to pass a reference to the object
                            self.generate_receiver_addr(&copy_expr.expr, bytecode)?;
                            bytecode.push(Instruction::Call {
                                func_name: format!("{}_copy_ctor", named.name),
                                nb_args: 2, //This pointer + from reference
                            });
                        } else {
                            // Might be an enum
                            self.generate_bytecode_expr(&copy_expr.expr, bytecode)?;
                        }
                    }
                    HirTy::Generic(g) => {
                        let mangled_name = MonomorphizationPass::generate_mangled_name(
                            self.hir_arena,
                            g,
                            "struct",
                        );
                        let obj_descriptor = self
                            .struct_pool
                            .iter()
                            .position(|s| s.name == mangled_name)
                            .unwrap();
                        bytecode.push(Instruction::NewObj { obj_descriptor });
                        // _copy takes &const this, so we need to pass a reference to the object
                        self.generate_receiver_addr(&copy_expr.expr, bytecode)?;
                        bytecode.push(Instruction::Call {
                            func_name: format!("{}_copy_ctor", mangled_name),
                            nb_args: 2, //This pointer + from reference
                        });
                    }
                    // For strings, deep copy the string data
                    HirTy::String(_) => {
                        self.generate_bytecode_expr(&copy_expr.expr, bytecode)?;
                        bytecode.push(Instruction::CloneString);
                    }
                    // For primitives and other types, just evaluate the expression
                    // (bitwise copy is implicit)
                    _ => {
                        self.generate_bytecode_expr(&copy_expr.expr, bytecode)?;
                    }
                }
            }
        }
        Ok(())
    }

    fn generate_bytecode_ref_expr(
        &mut self,
        expr: &HirExpr<'hir>,
        bytecode: &mut Vec<Instruction>,
    ) -> CodegenResult<()> {
        match expr {
            HirExpr::Ident(ident) => {
                let var_index = match self.local_variables.get_index(ident.name) {
                    Some(idx) => idx,
                    None => {
                        return Err(Self::unsupported_expr_err(
                            expr,
                            format!("Variable {} not found", ident.name),
                        ));
                    }
                };
                bytecode.push(Instruction::LoadVarAddr(var_index));
            }
            HirExpr::ThisLiteral(_) => {
                let var_index = self.local_variables.get_index(THIS_NAME).unwrap();
                bytecode.push(Instruction::LoadVarAddr(var_index));
            }
            HirExpr::FieldAccess(field_access) => {
                // Check if this is a union field access by looking up the type
                let obj_name_for_check = self
                    .get_struct_name_of_type(field_access.target.ty())
                    .unwrap_or_default();
                let is_union_access = !self
                    .struct_pool
                    .iter()
                    .any(|s| s.name == obj_name_for_check)
                    && !obj_name_for_check.is_empty();

                // For union field access, get the address of the union itself (all fields same location)
                // For struct field access, get the struct object value first
                if is_union_access {
                    self.generate_bytecode_ref_expr(field_access.target.as_ref(), bytecode)?;
                    // Union field access is no-op - address of union = address of any field
                    return Ok(());
                }

                self.generate_bytecode_expr(field_access.target.as_ref(), bytecode)?;
                // If target is a reference, dereference it first to get the object pointer
                if matches!(
                    field_access.target.ty(),
                    HirTy::MutableReference(_) | HirTy::ReadOnlyReference(_)
                ) {
                    bytecode.push(Instruction::LoadIndirect);
                }
                let obj_name = match self.get_struct_name_of_type(field_access.target.ty()) {
                    Some(n) => n,
                    None => {
                        return Err(Self::unsupported_expr_err(
                            expr,
                            format!("Field access for: {}", field_access.target.ty()),
                        ));
                    }
                };
                if let Some(struct_descriptor) =
                    self.struct_pool.iter().find(|s| s.name == obj_name)
                {
                    let field = struct_descriptor
                        .fields
                        .iter()
                        .position(|f| *f == field_access.field.name)
                        .unwrap();
                    bytecode.push(Instruction::GetFieldAddr { field });
                }
                // If not found, it's a union - field access is no-op
            }
            HirExpr::Indexing(idx_expr) => {
                // Generate code to get the address of the indexed element
                self.generate_bytecode_expr(&idx_expr.index, bytecode)?;
                self.generate_bytecode_expr(&idx_expr.target, bytecode)?;
                bytecode.push(Instruction::IndexGetAddr);
            }
            // For some reason, we can get Unary(Unary(Unary(expr))) here
            // So we need to recursively handle the case where op is None, if it's None, we just treat it as a normal expression
            HirExpr::Unary(u) => {
                if u.op.is_none() {
                    self.generate_bytecode_ref_expr(&u.expr, bytecode)?;
                    //bytecode.push(Instruction::LoadIndirect);
                } else {
                    self.generate_bytecode_expr(expr, bytecode)?;
                }
            }
            HirExpr::Copy(copy_expr) => {
                // For copy expressions that produce a temporary value (call/newobj/complex expr),
                // evaluate the expression, store it in a temporary local slot, and then push
                // the address of that slot. This mirrors `generate_receiver_addr`'s behavior and
                // guarantees the address refers to a stable location the callee can read.
                self.generate_bytecode_expr(&copy_expr.expr, bytecode)?;
                let temp_idx = self.local_variables.insert_anonymous();
                bytecode.push(Instruction::StoreVar(temp_idx));
                bytecode.push(Instruction::LoadVarAddr(temp_idx));
            }
            HirExpr::Move(move_expr) => {
                // For move expressions, materialize into a temp slot then take its address
                // (similar reasons as for Copy)
                self.generate_bytecode_expr(&move_expr.expr, bytecode)?;
                let temp_idx = self.local_variables.insert_anonymous();
                bytecode.push(Instruction::StoreVar(temp_idx));
                bytecode.push(Instruction::LoadVarAddr(temp_idx));
            }
            _ => {
                return Err(Self::unsupported_expr_err(
                    expr,
                    format!(
                        "Cannot take reference of this expression in this context as it would be unsafe\nExpression Type: {}",
                        expr.ty()
                    ),
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

    /// Generates the address of a receiver expression for method calls that take &const this or &this.
    /// This is used when the method takes a reference to `this` rather than ownership.
    fn generate_receiver_addr(
        &mut self,
        target: &HirExpr<'hir>,
        bytecode: &mut Vec<Instruction>,
    ) -> CodegenResult<()> {
        match target {
            HirExpr::Ident(ident) => {
                let var_index = match self.local_variables.get_index(ident.name) {
                    Some(idx) => idx,
                    None => {
                        return Err(Self::unsupported_expr_err(
                            target,
                            format!("Variable {} not found", ident.name),
                        ));
                    }
                };
                // If the identifier's type is already a reference, load the value (which IS the address)
                // Otherwise, load the address of the variable
                if matches!(
                    ident.ty,
                    HirTy::MutableReference(_) | HirTy::ReadOnlyReference(_)
                ) {
                    bytecode.push(Instruction::LoadVar(var_index));
                } else {
                    bytecode.push(Instruction::LoadVarAddr(var_index));
                }
            }
            HirExpr::ThisLiteral(_) => {
                let var_index = self.local_variables.get_index(THIS_NAME).unwrap();
                bytecode.push(Instruction::LoadVarAddr(var_index));
            }
            HirExpr::FieldAccess(field_access) => {
                let obj_name = match self.get_struct_name_of_type(field_access.target.ty()) {
                    Some(n) => n,
                    None => {
                        return Err(Self::unsupported_expr_err(
                            target,
                            format!("Field access for: {}", field_access.target.ty()),
                        ));
                    }
                };

                // Check if this is a union field access
                let is_union = !self.struct_pool.iter().any(|s| s.name == obj_name);

                if is_union {
                    // For union field access, all fields are at the same address
                    // So just get the address of the target (the union itself)
                    self.generate_receiver_addr(field_access.target.as_ref(), bytecode)?;
                } else {
                    // For struct field access, generate the target then get field address
                    self.generate_bytecode_expr(field_access.target.as_ref(), bytecode)?;
                    // If target is a reference, dereference it first to get the object pointer
                    if matches!(
                        field_access.target.ty(),
                        HirTy::MutableReference(_) | HirTy::ReadOnlyReference(_)
                    ) {
                        bytecode.push(Instruction::LoadIndirect);
                    }

                    let struct_descriptor = self
                        .struct_pool
                        .iter()
                        .find(|s| s.name == obj_name)
                        .unwrap();
                    let field = struct_descriptor
                        .fields
                        .iter()
                        .position(|f| *f == field_access.field.name)
                        .unwrap();
                    bytecode.push(Instruction::GetFieldAddr { field });
                }
            }
            HirExpr::Unary(u) => {
                // If the target is already a reference (like *ptr or &x), handle that
                if let Some(HirUnaryOp::AsRef) = &u.op {
                    // Already taking a reference, just generate that
                    self.generate_bytecode_expr(target, bytecode)?;
                } else if let Some(HirUnaryOp::Deref) = &u.op {
                    // Dereferencing a pointer - the inner expression IS the address
                    self.generate_bytecode_expr(&u.expr, bytecode)?;
                } else {
                    return Err(Self::unsupported_expr_err(
                        target,
                        format!("Cannot take address of unary expression: {:?}", u.op),
                    ));
                }
            }
            // For function calls that return a struct/object, we need to store the result
            // in a temporary variable first, then take the address of that variable.
            // This is because &const this methods expect a reference (slot address), not a raw object pointer.
            HirExpr::Call(_) | HirExpr::NewObj(_) | HirExpr::ObjLiteral(_) | HirExpr::Copy(_) => {
                // Generate the expression (puts object pointer on stack)
                self.generate_bytecode_expr(target, bytecode)?;
                // Store in a temporary slot - we use a unique index to avoid collisions
                let temp_idx = self.local_variables.insert_anonymous();
                bytecode.push(Instruction::StoreVar(temp_idx));
                // Push the address of that slot
                bytecode.push(Instruction::LoadVarAddr(temp_idx));
            }
            // If the target is already a reference type, we can just generate its value
            // (the value IS the address)
            _ if matches!(
                target.ty(),
                HirTy::MutableReference(_) | HirTy::ReadOnlyReference(_)
            ) =>
            {
                self.generate_bytecode_expr(target, bytecode)?;
            }
            // For struct types (Named, Generic), the value on stack is a heap pointer
            _ if matches!(target.ty(), HirTy::Named(_) | HirTy::Generic(_)) => {
                self.generate_bytecode_expr(target, bytecode)?;
            }
            _ => {
                return Err(Self::unsupported_expr_err(
                    target,
                    format!(
                        "Cannot take address of expression for method receiver: {}",
                        target.ty()
                    ),
                ));
            }
        }
        Ok(())
    }

    fn is_std(path: &str) -> bool {
        path.starts_with("std")
    }

    fn get_struct_name_of_type(&self, ty: &HirTy<'hir>) -> Option<&'hir str> {
        match ty {
            HirTy::Named(n) => Some(n.name),
            HirTy::Generic(g) => Some(MonomorphizationPass::generate_mangled_name(
                self.hir_arena,
                g,
                "struct",
            )),
            HirTy::ReadOnlyReference(read_only) => self.get_struct_name_of_type(read_only.inner),
            HirTy::MutableReference(mutable) => self.get_struct_name_of_type(mutable.inner),
            _ => None,
        }
    }

    fn get_union_name_of_type(&self, ty: &HirTy<'hir>) -> Option<&'hir str> {
        match ty {
            HirTy::Named(n) => Some(n.name),
            HirTy::Generic(g) => Some(MonomorphizationPass::generate_mangled_name(
                self.hir_arena,
                g,
                "union",
            )),
            HirTy::ReadOnlyReference(read_only) => self.get_union_name_of_type(read_only.inner),
            HirTy::MutableReference(mutable) => self.get_union_name_of_type(mutable.inner),
            _ => None,
        }
    }
}
