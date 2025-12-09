use crate::atlas_c::atlas_asm::error::{ASMError, ASMResult, UnsupportedInstructionError};
use crate::atlas_c::atlas_codegen::instruction::ProgramDescriptor;
use crate::atlas_c::atlas_codegen::instruction::{Instruction, Type};
use crate::atlas_c::atlas_hir::signature::ConstantValue;
use crate::atlas_c::utils::Span;
use crate::atlas_vm::instruction::{Arg, Instr, OpCode, StructDescriptor};
use crate::atlas_vm::vm_data::VMTag;
use std::collections::BTreeMap;
use std::fmt::{Display, Formatter};

pub mod error;

pub struct AsmConstantMap {
    //The constant pool should be reworked in the near future
    pub constant_pool: Vec<ConstantValue>,
}

pub struct WipConstantValue {
    pub tag: WipConstantValueTag,
    pub data: WipConstantValueData,
}

pub union WipConstantValueData {
    pub int_value: i64,
    pub uint_value: u64,
    pub float_value: f64,
    pub bool_value: bool,
    pub char_value: char,
    /// Pointer to a string in the constant pool.
    ///
    /// Yes the string has been leaked, but it doesn't matter
    /// as the constant pool lives for as long as the program
    pub string_value: &'static str,
}

#[repr(u8)]
pub enum WipConstantValueTag {
    Int,
    UInt,
    Float,
    Bool,
    Char,
    String,
}

impl Default for AsmConstantMap {
    fn default() -> Self {
        Self::new()
    }
}

impl AsmConstantMap {
    pub fn new() -> Self {
        Self {
            constant_pool: vec![],
        }
    }
    pub fn get(&mut self, constant: ConstantValue) -> usize {
        for (i, c) in self.constant_pool.iter().enumerate() {
            if *c == constant {
                return i;
            }
        }
        self.constant_pool.push(constant);
        self.constant_pool.len() - 1
    }
}

pub struct Assembler {
    asm_constant_map: AsmConstantMap,
    /// TODO: the main function should also be stored here
    asm_function_map: BTreeMap<usize, AsmFunction>,
}

pub struct AsmProgram {
    pub bytecode: Vec<Instr>,
    pub constant_pool: Vec<ConstantValue>,
    /// Mapping of function_id to its data
    pub function_map: BTreeMap<usize, AsmFunction>,
    pub entry_point: Option<usize>,
    pub using_std: bool,
    //A Vec for fast access
    pub struct_descriptors: Vec<StructDescriptor>,
}

#[derive(Clone)]
pub struct AsmFunction {
    pub name: String,
    pub entry_point: usize,
    pub nb_args: usize,
    /// Useful for debugging and error reporting
    pub declaration_span: Span,
}

impl Default for Assembler {
    fn default() -> Self {
        Self::new()
    }
}

impl Assembler {
    pub fn new() -> Assembler {
        Self {
            asm_constant_map: AsmConstantMap::new(),
            asm_function_map: BTreeMap::new(),
        }
    }
    pub fn asm_from_instruction(
        &mut self,
        using_std: bool,
        source: ProgramDescriptor,
    ) -> ASMResult<AsmProgram> {
        let mut bytecode: Vec<Instr> = vec![];
        let mut i = 0;
        while i < source.len() {
            let instr = &source[i];
            match instr {
                Instruction::LoadConst(const_value) => match const_value {
                    ConstantValue::String(s) => {
                        let const_index =
                            self.asm_constant_map.get(ConstantValue::String(s.clone()));
                        let instr = Instr {
                            opcode: OpCode::LOAD_CONST,
                            arg: Arg::from_u24(const_index as u32),
                        };
                        bytecode.push(instr);
                    }
                    ConstantValue::Int(_) => {
                        let const_index = self.asm_constant_map.get(const_value.clone());
                        let instr = Instr {
                            opcode: OpCode::LOAD_CONST,
                            arg: Arg::from_u24(const_index as u32),
                        };
                        bytecode.push(instr);
                    }
                    ConstantValue::UInt(_) => {
                        let const_index = self.asm_constant_map.get(const_value.clone());
                        let instr = Instr {
                            opcode: OpCode::LOAD_CONST,
                            arg: Arg::from_u24(const_index as u32),
                        };
                        bytecode.push(instr);
                    }
                    ConstantValue::Float(_) => {
                        let const_index = self.asm_constant_map.get(const_value.clone());
                        let instr = Instr {
                            opcode: OpCode::LOAD_CONST,
                            arg: Arg::from_u24(const_index as u32),
                        };
                        bytecode.push(instr);
                    }
                    ConstantValue::Bool(_) => {
                        let const_index = self.asm_constant_map.get(const_value.clone());
                        let instr = Instr {
                            opcode: OpCode::LOAD_CONST,
                            arg: Arg::from_u24(const_index as u32),
                        };
                        bytecode.push(instr);
                    }
                    ConstantValue::Char(c) => {
                        let const_index = self.asm_constant_map.get(ConstantValue::Char(*c));
                        let instr = Instr {
                            opcode: OpCode::LOAD_CONST,
                            arg: Arg::from_u24(const_index as u32),
                        };
                        bytecode.push(instr);
                    }
                    ConstantValue::Unit => {
                        let const_index = self.asm_constant_map.get(ConstantValue::Unit);
                        let instr = Instr {
                            opcode: OpCode::LOAD_CONST,
                            arg: Arg::from_u24(const_index as u32),
                        };
                        bytecode.push(instr);
                    }
                    _ => unimplemented!("Loading constant {}", const_value),
                },
                Instruction::Pop => bytecode.push(Instr {
                    opcode: OpCode::POP,
                    arg: Arg::default(),
                }),
                Instruction::Dup => bytecode.push(Instr {
                    opcode: OpCode::DUP,
                    arg: Arg::default(),
                }),
                Instruction::Swap => bytecode.push(Instr {
                    opcode: OpCode::SWAP,
                    arg: Arg::default(),
                }),
                Instruction::StoreVar(index) => {
                    let instr = Instr {
                        opcode: OpCode::STORE_VAR,
                        arg: Arg::from_u24(*index as u32),
                    };
                    bytecode.push(instr);
                }
                Instruction::LoadVar(index) => {
                    let instr = Instr {
                        opcode: OpCode::LOAD_VAR,
                        arg: Arg::from_u24(*index as u32),
                    };
                    bytecode.push(instr);
                }
                Instruction::IndexLoad => {
                    let instr = Instr {
                        opcode: OpCode::INDEX_LOAD,
                        arg: Arg::default(),
                    };
                    bytecode.push(instr);
                }
                Instruction::IndexStore => {
                    let instr = Instr {
                        arg: Arg::default(),
                        opcode: OpCode::INDEX_STORE,
                    };
                    bytecode.push(instr);
                }
                Instruction::StringLoad => {
                    let instr = Instr {
                        opcode: OpCode::STRING_LOAD,
                        arg: Arg::default(),
                    };
                    bytecode.push(instr);
                }
                Instruction::StringStore => {
                    let instr = Instr {
                        opcode: OpCode::STRING_STORE,
                        arg: Arg::default(),
                    };
                    bytecode.push(instr);
                }
                Instruction::NewArray => {
                    let instr = Instr {
                        opcode: OpCode::NEW_ARRAY,
                        arg: Arg::default(),
                    };
                    bytecode.push(instr);
                }
                Instruction::Add(ty) => {
                    match ty {
                        Type::Object | Type::String | Type::List | Type::Unit | Type::Boolean => {
                            return Err(ASMError::UnsupportedInstruction(
                                UnsupportedInstructionError {
                                    details: format!(
                                        "Add operation not supported for this type: {:?}",
                                        ty
                                    ),
                                },
                            ));
                        }
                        //Integer and Char use the same addition instruction
                        Type::Integer | Type::Char => {
                            let instr = Instr {
                                opcode: OpCode::INT_ADD,
                                arg: Arg::default(),
                            };
                            bytecode.push(instr);
                        }
                        Type::Float => {
                            let instr = Instr {
                                opcode: OpCode::FLOAT_ADD,
                                arg: Arg::default(),
                            };
                            bytecode.push(instr);
                        }
                        Type::UnsignedInteger => {
                            let instr = Instr {
                                opcode: OpCode::UINT_ADD,
                                arg: Arg::default(),
                            };
                            bytecode.push(instr);
                        }
                    }
                }
                Instruction::Sub(ty) => {
                    match ty {
                        Type::Object | Type::String | Type::List | Type::Unit | Type::Boolean => {
                            return Err(ASMError::UnsupportedInstruction(
                                UnsupportedInstructionError {
                                    details: format!(
                                        "Subtraction operation not supported for this type: {:?}",
                                        ty
                                    ),
                                },
                            ));
                        }
                        //Integer and Char use the same subtraction instruction
                        Type::Integer | Type::Char => {
                            let instr = Instr {
                                opcode: OpCode::INT_SUB,
                                arg: Arg::default(),
                            };
                            bytecode.push(instr);
                        }
                        Type::Float => {
                            let instr = Instr {
                                opcode: OpCode::FLOAT_SUB,
                                arg: Arg::default(),
                            };
                            bytecode.push(instr);
                        }
                        Type::UnsignedInteger => {
                            let instr = Instr {
                                opcode: OpCode::UINT_SUB,
                                arg: Arg::default(),
                            };
                            bytecode.push(instr);
                        }
                    }
                }
                Instruction::Mul(ty) => {
                    match ty {
                        Type::Object | Type::String | Type::List | Type::Unit | Type::Boolean => {
                            return Err(ASMError::UnsupportedInstruction(
                                UnsupportedInstructionError {
                                    details: format!(
                                        "Multiplication operation not supported for this type: {:?}",
                                        ty
                                    ),
                                },
                            ));
                        }
                        //Integer and Char use the same multiplication instruction
                        Type::Integer | Type::Char => {
                            let instr = Instr {
                                opcode: OpCode::INT_MUL,
                                arg: Arg::default(),
                            };
                            bytecode.push(instr);
                        }
                        Type::Float => {
                            let instr = Instr {
                                opcode: OpCode::FLOAT_MUL,
                                arg: Arg::default(),
                            };
                            bytecode.push(instr);
                        }
                        Type::UnsignedInteger => {
                            let instr = Instr {
                                opcode: OpCode::UINT_MUL,
                                arg: Arg::default(),
                            };
                            bytecode.push(instr);
                        }
                    }
                }
                Instruction::Div(ty) => {
                    match ty {
                        Type::Object | Type::String | Type::List | Type::Unit | Type::Boolean => {
                            return Err(ASMError::UnsupportedInstruction(
                                UnsupportedInstructionError {
                                    details: format!(
                                        "Division operation not supported for this type: {:?}",
                                        ty
                                    ),
                                },
                            ));
                        }
                        //Integer and Char use the same division instruction
                        Type::Integer | Type::Char => {
                            let instr = Instr {
                                opcode: OpCode::INT_DIV,
                                arg: Arg::default(),
                            };
                            bytecode.push(instr);
                        }
                        Type::Float => {
                            let instr = Instr {
                                opcode: OpCode::FLOAT_DIV,
                                arg: Arg::default(),
                            };
                            bytecode.push(instr);
                        }
                        Type::UnsignedInteger => {
                            let instr = Instr {
                                opcode: OpCode::UINT_DIV,
                                arg: Arg::default(),
                            };
                            bytecode.push(instr);
                        }
                    }
                }
                Instruction::Mod(ty) => {
                    match ty {
                        Type::Object | Type::String | Type::List | Type::Unit | Type::Boolean => {
                            return Err(ASMError::UnsupportedInstruction(
                                UnsupportedInstructionError {
                                    details: format!(
                                        "Modulo operation not supported for this type: {:?}",
                                        ty
                                    ),
                                },
                            ));
                        }
                        //Integer and Char use the same modulo instruction
                        Type::Integer | Type::Char => {
                            let instr = Instr {
                                opcode: OpCode::INT_MOD,
                                arg: Arg::default(),
                            };
                            bytecode.push(instr);
                        }
                        Type::Float => {
                            let instr = Instr {
                                opcode: OpCode::FLOAT_MOD,
                                arg: Arg::default(),
                            };
                            bytecode.push(instr);
                        }
                        Type::UnsignedInteger => {
                            let instr = Instr {
                                opcode: OpCode::UINT_MOD,
                                arg: Arg::default(),
                            };
                            bytecode.push(instr);
                        }
                    }
                }
                Instruction::Eq(ty) => {
                    match ty {
                        Type::Object | Type::String | Type::List | Type::Unit => {
                            return Err(ASMError::UnsupportedInstruction(
                                UnsupportedInstructionError {
                                    details: format!(
                                        "Equality operation not supported for this type: {:?}",
                                        ty
                                    ),
                                },
                            ));
                        }
                        //Integer and Char use the same equality instruction
                        Type::Integer | Type::Char => {
                            let instr = Instr {
                                opcode: OpCode::INT_EQUAL,
                                arg: Arg::default(),
                            };
                            bytecode.push(instr);
                        }
                        Type::Float => {
                            let instr = Instr {
                                opcode: OpCode::FLOAT_EQUAL,
                                arg: Arg::default(),
                            };
                            bytecode.push(instr);
                        }
                        Type::UnsignedInteger => {
                            let instr = Instr {
                                opcode: OpCode::UINT_EQUAL,
                                arg: Arg::default(),
                            };
                            bytecode.push(instr);
                        }
                        Type::Boolean => {
                            let instr = Instr {
                                opcode: OpCode::BOOL_EQUAL,
                                arg: Arg::default(),
                            };
                            bytecode.push(instr);
                        }
                    }
                }
                Instruction::Neq(ty) => {
                    match ty {
                        Type::Object | Type::String | Type::List | Type::Unit => {
                            return Err(ASMError::UnsupportedInstruction(
                                UnsupportedInstructionError {
                                    details: format!(
                                        "Inequality operation not supported for this type: {:?}",
                                        ty
                                    ),
                                },
                            ));
                        }
                        //Integer and Char use the same inequality instruction
                        Type::Integer | Type::Char => {
                            let instr = Instr {
                                opcode: OpCode::INT_NOT_EQUAL,
                                arg: Arg::default(),
                            };
                            bytecode.push(instr);
                        }
                        Type::Float => {
                            let instr = Instr {
                                opcode: OpCode::FLOAT_NOT_EQUAL,
                                arg: Arg::default(),
                            };
                            bytecode.push(instr);
                        }
                        Type::UnsignedInteger => {
                            let instr = Instr {
                                opcode: OpCode::UINT_NOT_EQUAL,
                                arg: Arg::default(),
                            };
                            bytecode.push(instr);
                        }
                        Type::Boolean => {
                            let instr = Instr {
                                opcode: OpCode::BOOL_NOT_EQUAL,
                                arg: Arg::default(),
                            };
                            bytecode.push(instr);
                        }
                    }
                }
                Instruction::Gt(ty) => {
                    match ty {
                        Type::Object | Type::String | Type::List | Type::Unit | Type::Boolean => {
                            return Err(ASMError::UnsupportedInstruction(
                                UnsupportedInstructionError {
                                    details: format!(
                                        "Greater than operation not supported for this type: {:?}",
                                        ty
                                    ),
                                },
                            ));
                        }
                        //Integer and Char use the same greater than instruction
                        Type::Integer | Type::Char => {
                            let instr = Instr {
                                opcode: OpCode::INT_GREATER_THAN,
                                arg: Arg::default(),
                            };
                            bytecode.push(instr);
                        }
                        Type::Float => {
                            let instr = Instr {
                                opcode: OpCode::FLOAT_GREATER_THAN,
                                arg: Arg::default(),
                            };
                            bytecode.push(instr);
                        }
                        Type::UnsignedInteger => {
                            let instr = Instr {
                                opcode: OpCode::UINT_GREATER_THAN,
                                arg: Arg::default(),
                            };
                            bytecode.push(instr);
                        }
                    }
                }
                Instruction::Gte(ty) => {
                    match ty {
                        Type::Object | Type::String | Type::List | Type::Unit | Type::Boolean => {
                            return Err(ASMError::UnsupportedInstruction(
                                UnsupportedInstructionError {
                                    details: format!(
                                        "Greater than or equal operation not supported for this type: {:?}",
                                        ty
                                    ),
                                },
                            ));
                        }
                        //Integer and Char use the same greater than or equal instruction
                        Type::Integer | Type::Char => {
                            let instr = Instr {
                                opcode: OpCode::INT_GREATER_THAN_EQUAL,
                                arg: Arg::default(),
                            };
                            bytecode.push(instr);
                        }
                        Type::Float => {
                            let instr = Instr {
                                opcode: OpCode::FLOAT_GREATER_THAN_EQUAL,
                                arg: Arg::default(),
                            };
                            bytecode.push(instr);
                        }
                        Type::UnsignedInteger => {
                            let instr = Instr {
                                opcode: OpCode::UINT_GREATER_THAN_EQUAL,
                                arg: Arg::default(),
                            };
                            bytecode.push(instr);
                        }
                    }
                }
                Instruction::Lt(ty) => {
                    match ty {
                        Type::Object | Type::String | Type::List | Type::Unit | Type::Boolean => {
                            return Err(ASMError::UnsupportedInstruction(
                                UnsupportedInstructionError {
                                    details: format!(
                                        "Less than operation not supported for this type: {:?}",
                                        ty
                                    ),
                                },
                            ));
                        }
                        //Integer and Char use the same less than instruction
                        Type::Integer | Type::Char => {
                            let instr = Instr {
                                opcode: OpCode::INT_LESS_THAN,
                                arg: Arg::default(),
                            };
                            bytecode.push(instr);
                        }
                        Type::Float => {
                            let instr = Instr {
                                opcode: OpCode::FLOAT_LESS_THAN,
                                arg: Arg::default(),
                            };
                            bytecode.push(instr);
                        }
                        Type::UnsignedInteger => {
                            let instr = Instr {
                                opcode: OpCode::UINT_LESS_THAN,
                                arg: Arg::default(),
                            };
                            bytecode.push(instr);
                        }
                    }
                }
                Instruction::Lte(ty) => {
                    match ty {
                        Type::Object | Type::String | Type::List | Type::Unit | Type::Boolean => {
                            return Err(ASMError::UnsupportedInstruction(
                                UnsupportedInstructionError {
                                    details: format!(
                                        "Less than or equal operation not supported for this type: {:?}",
                                        ty
                                    ),
                                },
                            ));
                        }
                        //Integer and Char use the same less than or equal instruction
                        Type::Integer | Type::Char => {
                            let instr = Instr {
                                opcode: OpCode::INT_LESS_THAN_EQUAL,
                                arg: Arg::default(),
                            };
                            bytecode.push(instr);
                        }
                        Type::Float => {
                            let instr = Instr {
                                opcode: OpCode::FLOAT_LESS_THAN_EQUAL,
                                arg: Arg::default(),
                            };
                            bytecode.push(instr);
                        }
                        Type::UnsignedInteger => {
                            let instr = Instr {
                                opcode: OpCode::UINT_LESS_THAN_EQUAL,
                                arg: Arg::default(),
                            };
                            bytecode.push(instr);
                        }
                    }
                }
                Instruction::And => {
                    let instr = Instr {
                        opcode: OpCode::BOOL_AND,
                        arg: Arg::default(),
                    };
                    bytecode.push(instr);
                }
                Instruction::Or => {
                    let instr = Instr {
                        opcode: OpCode::BOOL_OR,
                        arg: Arg::default(),
                    };
                    bytecode.push(instr);
                }
                Instruction::Jmp { pos } => {
                    let instr = Instr {
                        opcode: OpCode::JMP,
                        arg: Arg::from_i24(*pos as i32),
                    };
                    bytecode.push(instr);
                }
                Instruction::JmpZ { pos } => {
                    let instr = Instr {
                        opcode: OpCode::JMP_Z,
                        arg: Arg::from_i24(*pos as i32),
                    };
                    bytecode.push(instr);
                }
                Instruction::LocalSpace { nb_vars } => {
                    let instr = Instr {
                        opcode: OpCode::LOCAL_SPACE,
                        arg: Arg::from_u24(*nb_vars as u32),
                    };
                    bytecode.push(instr);
                }
                Instruction::Call { func_name, nb_args } => {
                    let func_ptr = *source.functions.get(func_name.as_str()).unwrap_or_else(|| {
                        panic!("Function {} not found in function map", func_name)
                    });
                    //func_ptr will be used as the func_id for now
                    self.asm_function_map
                        .entry(func_ptr)
                        .or_insert(AsmFunction {
                            name: func_name.clone(),
                            entry_point: func_ptr, //Will be filled later
                            nb_args: *nb_args as usize,
                            declaration_span: Span::default(),
                        });
                    let instr = Instr {
                        opcode: OpCode::CALL,
                        arg: Arg::from_u24(func_ptr as u32),
                    };
                    bytecode.push(instr);
                }
                Instruction::ExternCall { func_name } => {
                    let func_ptr = self
                        .asm_constant_map
                        .get(ConstantValue::String(func_name.clone()));
                    let instr = Instr {
                        opcode: OpCode::EXTERN_CALL,
                        arg: Arg::from_u24(func_ptr as u32),
                    };
                    bytecode.push(instr);
                }
                Instruction::Return => {
                    let instr = Instr {
                        opcode: OpCode::RETURN,
                        arg: Arg::default(),
                    };
                    bytecode.push(instr);
                }
                Instruction::NewObj { obj_descriptor } => {
                    let instr = Instr {
                        opcode: OpCode::NEW_OBJ,
                        arg: Arg::from_u24(*obj_descriptor as u32),
                    };
                    bytecode.push(instr);
                }
                Instruction::GetField { field } => {
                    let instr = Instr {
                        opcode: OpCode::GET_FIELD,
                        arg: Arg::from_u24(*field as u32),
                    };
                    bytecode.push(instr);
                }
                Instruction::SetField { field } => {
                    let instr = Instr {
                        opcode: OpCode::SET_FIELD,
                        arg: Arg::from_u24(*field as u32),
                    };
                    bytecode.push(instr);
                }
                Instruction::DeleteObj => {
                    let instr = Instr {
                        opcode: OpCode::DELETE_OBJ,
                        arg: Arg::default(),
                    };
                    bytecode.push(instr);
                }
                Instruction::CastTo(ty) => {
                    let ty: VMTag = (*ty).into();
                    let instr = Instr {
                        opcode: OpCode::CAST_TO,
                        arg: Arg::from_u24(ty as u32),
                    };
                    bytecode.push(instr);
                }
                Instruction::Halt => {
                    let instr = Instr {
                        opcode: OpCode::Halt,
                        arg: Arg::default(),
                    };
                    bytecode.push(instr);
                }
            }
            i += 1;
        }

        let entry_point = source.functions.get("main").copied();
        let mut struct_descriptors = Vec::new();
        for struct_descriptor in source.structs.iter() {
            struct_descriptors.push(StructDescriptor {
                name: struct_descriptor.name.to_owned(),
                nb_fields: struct_descriptor.fields.len(),
                fields: struct_descriptor
                    .fields
                    .clone()
                    .into_iter()
                    .map(|s| s.to_owned())
                    .collect(),
            });
        }
        Ok(AsmProgram {
            bytecode,
            constant_pool: self.asm_constant_map.constant_pool.clone(),
            entry_point,
            function_map: self.asm_function_map.clone(),
            struct_descriptors,
            using_std,
        })
    }
}

impl Display for AsmProgram {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "section .config")?;
        writeln!(f, "\tUSE_STANDARD_LIB: {}", self.using_std)?;

        writeln!(f, "\nsection .data")?;
        for (i, constant) in self.constant_pool.iter().enumerate() {
            writeln!(f, "\t{:04} = {}", i, constant)?;
        }

        writeln!(f, "\nsection .structs")?;
        for (i, struct_descriptor) in self.struct_descriptors.iter().enumerate() {
            writeln!(f, "\t{} @{:04}", struct_descriptor.name, i)?;
        }

        writeln!(f, "\nsection .functions")?;
        for (func_id, asm_function) in self.function_map.iter() {
            writeln!(f, "\t{} @{:04}", asm_function.name, func_id)?;
        }

        writeln!(f, "\nsection .text")?;
        for instruction in &self.bytecode {
            let instr = match instruction.opcode {
                // === Literals & constants ===
                OpCode::LOAD_CONST => {
                    format!("LOAD_CONST &{:04}", instruction.arg.as_u24())
                }

                // === Stack manipulation ===
                OpCode::POP => "POP".to_string(),
                OpCode::DUP => "DUP".to_string(),
                OpCode::SWAP => "SWAP".to_string(),

                // === Variables ===
                OpCode::STORE_VAR => {
                    format!("STORE_VAR @{}", instruction.arg.as_u24())
                }
                OpCode::LOAD_VAR => {
                    format!("LOAD_VAR @{}", instruction.arg.as_u24())
                }

                // === Collections & indexing ===
                OpCode::INDEX_LOAD => "INDEX_LOAD".to_string(),
                OpCode::INDEX_STORE => "INDEX_STORE".to_string(),
                OpCode::STRING_LOAD => "STRING_LOAD".to_string(),
                OpCode::STRING_STORE => "STRING_STORE".to_string(),
                OpCode::NEW_ARRAY => "NEW_ARRAY".to_string(),

                // === Arithmetic ===
                OpCode::INT_ADD => "INT_ADD".to_string(),
                OpCode::FLOAT_ADD => "FLOAT_ADD".to_string(),
                OpCode::UINT_ADD => "UINT_ADD".to_string(),

                OpCode::INT_SUB => "INT_SUB".to_string(),
                OpCode::FLOAT_SUB => "FLOAT_SUB".to_string(),
                OpCode::UINT_SUB => "UINT_SUB".to_string(),

                OpCode::INT_MUL => "INT_MUL".to_string(),
                OpCode::FLOAT_MUL => "FLOAT_MUL".to_string(),
                OpCode::UINT_MUL => "UINT_MUL".to_string(),

                OpCode::INT_DIV => "INT_DIV".to_string(),
                OpCode::FLOAT_DIV => "FLOAT_DIV".to_string(),
                OpCode::UINT_DIV => "UINT_DIV".to_string(),

                OpCode::INT_MOD => "INT_MOD".to_string(),
                OpCode::UINT_MOD => "UINT_MOD".to_string(),
                OpCode::FLOAT_MOD => "FLOAT_MOD".to_string(),

                // === Comparisons ===
                OpCode::INT_EQUAL => "INT_EQUAL".to_string(),
                OpCode::UINT_EQUAL => "UINT_EQUAL".to_string(),
                OpCode::FLOAT_EQUAL => "FLOAT_EQUAL".to_string(),
                OpCode::BOOL_EQUAL => "BOOL_EQUAL".to_string(),

                OpCode::INT_NOT_EQUAL => "INT_NOT_EQUAL".to_string(),
                OpCode::UINT_NOT_EQUAL => "UINT_NOT_EQUAL".to_string(),
                OpCode::FLOAT_NOT_EQUAL => "FLOAT_NOT_EQUAL".to_string(),
                OpCode::BOOL_NOT_EQUAL => "BOOL_NOT_EQUAL".to_string(),

                OpCode::INT_GREATER_THAN => "INT_GREATER_THAN".to_string(),
                OpCode::UINT_GREATER_THAN => "UINT_GREATER_THAN".to_string(),
                OpCode::FLOAT_GREATER_THAN => "FLOAT_GREATER_THAN".to_string(),

                OpCode::INT_GREATER_THAN_EQUAL => "INT_GREATER_THAN_EQUAL".to_string(),
                OpCode::UINT_GREATER_THAN_EQUAL => "UINT_GREATER_THAN_EQUAL".to_string(),
                OpCode::FLOAT_GREATER_THAN_EQUAL => "FLOAT_GREATER_THAN_EQUAL".to_string(),

                OpCode::INT_LESS_THAN => "INT_LESS_THAN".to_string(),
                OpCode::UINT_LESS_THAN => "UINT_LESS_THAN".to_string(),
                OpCode::FLOAT_LESS_THAN => "FLOAT_LESS_THAN".to_string(),

                OpCode::INT_LESS_THAN_EQUAL => "INT_LESS_THAN_EQUAL".to_string(),
                OpCode::UINT_LESS_THAN_EQUAL => "UINT_LESS_THAN_EQUAL".to_string(),
                OpCode::FLOAT_LESS_THAN_EQUAL => "FLOAT_LESS_THAN_EQUAL".to_string(),

                // === Logical ===
                OpCode::BOOL_AND => "BOOL_AND".to_string(),
                OpCode::BOOL_OR => "BOOL_OR".to_string(),

                // === Control flow ===
                OpCode::JMP => {
                    format!("JMP {}", instruction.arg.as_i24())
                }
                OpCode::JMP_Z => {
                    format!("JMP_Z {}", instruction.arg.as_i24())
                }

                // === Functions ===
                OpCode::LOCAL_SPACE => {
                    format!("LOCAL_SPACE #{}", instruction.arg.as_u24())
                }
                OpCode::CALL => {
                    format!("CALL @{:04}", instruction.arg.as_u24())
                }
                OpCode::EXTERN_CALL => {
                    format!("EXTERN_CALL &{:04}", instruction.arg.as_u24())
                }
                OpCode::RETURN => "RETURN".to_string(),

                // === Objects ===
                OpCode::NEW_OBJ => {
                    format!("NEW_OBJ #{}", instruction.arg.as_u24())
                }
                OpCode::GET_FIELD => {
                    format!("GET_FIELD #{}", instruction.arg.as_u24())
                }
                OpCode::SET_FIELD => {
                    format!("SET_FIELD #{}", instruction.arg.as_u24())
                }
                OpCode::DELETE_OBJ => "DELETE_OBJ".to_string(),

                // === Type ops ===
                OpCode::CAST_TO => {
                    format!("CAST_TO #{}", instruction.arg.as_u24())
                }

                // === Misc ===
                OpCode::NoOp => "NOOP".to_string(),
                OpCode::Halt => "HALT".to_string(),
            };
            writeln!(f, "\t{}", instr)?;
        }
        Ok(())
    }
}
