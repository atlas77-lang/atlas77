use crate::atlas_c::atlas_asm::error::{ASMError, ASMResult, UnsupportedInstructionError};
use crate::atlas_c::atlas_hir::signature::ConstantValue;
use crate::atlas_c::atlas_codegen::instruction::Instruction;
use crate::atlas_c::atlas_codegen::instruction::ProgramDescriptor;
use crate::atlas_c::utils::Span;
use std::collections::BTreeMap;
use std::fmt::{Display, Formatter};
use crate::atlas_vm::instruction::{Arg, Instr, OpCode};

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
    pub has_standard_lib: bool,
}

#[derive(Clone)]
pub struct AsmFunction {
    pub name: String,
    pub entry_point: usize,
    pub nb_args: usize,
    /// Useful for debugging and error reporting
    pub declaration_span: Span,
}

impl Assembler {
    pub fn new() -> Assembler {
        Self {
            asm_constant_map: AsmConstantMap::new(),
            asm_function_map: BTreeMap::new(),
        }
    }
    pub fn asm_from_instruction(&mut self, has_standard_lib: bool, source: ProgramDescriptor) -> ASMResult<AsmProgram> {
        let mut bytecode: Vec<Instr> = vec![];
        let mut i = 0;
        while i < source.len() {
            let instr = &source[i];
            match instr {
                Instruction::LoadConst(const_value) => {
                    match const_value {
                        ConstantValue::String(s) => {
                            let const_index = self.asm_constant_map.get(ConstantValue::String(s.clone()));
                            let instr = Instr {
                                opcode: OpCode::LoadConst,
                                arg: Arg::from_u24(const_index as u32)
                            };
                            bytecode.push(instr);
                        }
                        ConstantValue::Int(_) => {
                            let const_index = self.asm_constant_map.get(const_value.clone());
                            let instr = Instr {
                                opcode: OpCode::LoadConst,
                                arg: Arg::from_u24(const_index as u32)
                            };
                            bytecode.push(instr);
                        }
                        ConstantValue::UInt(_) => {
                            let const_index = self.asm_constant_map.get(const_value.clone());
                            let instr = Instr {
                                opcode: OpCode::LoadConst,
                                arg: Arg::from_u24(const_index as u32)
                            };
                            bytecode.push(instr);
                        }
                        ConstantValue::Float(_) => {
                            let const_index = self.asm_constant_map.get(const_value.clone());
                            let instr = Instr {
                                opcode: OpCode::LoadConst,
                                arg: Arg::from_u24(const_index as u32)
                            };
                            bytecode.push(instr);
                        }
                        ConstantValue::Bool(_) => {
                            let const_index = self.asm_constant_map.get(const_value.clone());
                            let instr = Instr {
                                opcode: OpCode::LoadConst,
                                arg: Arg::from_u24(const_index as u32)
                            };
                            bytecode.push(instr);
                        }
                        ConstantValue::Char(c) => {
                            let const_index = self.asm_constant_map.get(ConstantValue::Char(*c));
                            let instr = Instr {
                                opcode: OpCode::LoadConst,
                                arg: Arg::from_u24(const_index as u32)
                            };
                            bytecode.push(instr);
                        }
                        _ => unimplemented!("Loading constant {}", const_value),
                    }
                }
                Instruction::Pop => {
                    bytecode.push(Instr {
                        opcode: OpCode::Pop,
                        arg: Arg::default()
                    })
                }
                Instruction::ExternCall { func_name } => {
                    let func_ptr = self.asm_constant_map.get(ConstantValue::String(func_name.clone()));
                    let instr = Instr {
                        opcode: OpCode::ExternCall,
                        arg: Arg::from_u24(func_ptr as u32)
                    };
                    bytecode.push(instr);
                }
                Instruction::Halt => {
                    let instr = Instr {
                        opcode: OpCode::Halt,
                        arg: Arg::default()
                    };
                    bytecode.push(instr);
                }
                Instruction::LocalSpace { nb_vars } => {
                    let instr = Instr {
                        opcode: OpCode::LocalSpace,
                        arg: Arg::from_u24(*nb_vars as u32)
                    };
                    bytecode.push(instr);
                }
                Instruction::LoadArg { index } => {
                    let instr = Instr {
                        opcode: OpCode::LoadArg,
                        arg: Arg::from_u24(*index as u32)
                    };
                    bytecode.push(instr);
                }
                Instruction::LoadVar(index) => {
                    let instr = Instr {
                        opcode: OpCode::LoadVar,
                        arg: Arg::from_u24(*index as u32)
                    };
                    bytecode.push(instr);
                }
                Instruction::Lte => {
                    let instr = Instr {
                        opcode: OpCode::Lte,
                        arg: Arg::default()
                    };
                    bytecode.push(instr);
                }
                Instruction::JmpZ { pos } => {
                    let instr = Instr {
                        opcode: OpCode::JmpZ,
                        arg: Arg::from_i24(*pos as i32)
                    };
                    bytecode.push(instr);
                }
                Instruction::Return => {
                    let instr = Instr {
                        opcode: OpCode::Return,
                        arg: Arg::default()
                    };
                    bytecode.push(instr);
                }
                Instruction::Jmp { pos } => {
                    let instr = Instr {
                        opcode: OpCode::Jmp,
                        arg: Arg::from_i24(*pos as i32)
                    };
                    bytecode.push(instr);
                }
                //TODO: Add the types of the operands in the instruction
                Instruction::Sub => {
                    let instr = Instr {
                        opcode: OpCode::Sub,
                        arg: Arg::default()
                    };
                    bytecode.push(instr);
                }
                Instruction::Add => {
                    let instr = Instr {
                        opcode: OpCode::Add,
                        arg: Arg::default()
                    };
                    bytecode.push(instr);
                }
                Instruction::Call { func_name, nb_args } => {
                    let func_ptr = *source.functions.get(func_name.as_str()).unwrap_or_else(|| {
                        panic!("Function {} not found in function map", func_name)
                    });
                    eprintln!("Function call to {} with ptr {}", func_name, func_ptr);
                    //func_ptr will be used as the func_id for now
                    self.asm_function_map.entry(
                        func_ptr,
                    ).or_insert(
                        AsmFunction {
                            name: func_name.clone(),
                            entry_point: 0, //Will be filled later
                            nb_args: *nb_args as usize,
                            declaration_span: Span::default(),
                        }
                    );
                    let instr = Instr {
                        opcode: OpCode::Call,
                        arg: Arg::from_u24(func_ptr as u32)
                    };
                    bytecode.push(instr);
                }
                _ => {
                    return Err(ASMError::UnsupportedInstruction(
                        UnsupportedInstructionError {
                            details: instr.to_string(),
                        },
                    ));
                }
            }
            i += 1;
        }
        
        let entry_point = source.functions.get("main").map(|main_func| *main_func);
        Ok(AsmProgram {
            bytecode,
            constant_pool: self.asm_constant_map.constant_pool.clone(),
            entry_point,
            function_map: self.asm_function_map.clone(),
            has_standard_lib,
        })
    }
}

impl Display for AsmProgram {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "section .config")?;
        writeln!(f, "\tUSE_STANDARD_LIB: {}", self.has_standard_lib)?;

        writeln!(f, "\nsection .data")?;
        for (i, constant) in self.constant_pool.iter().enumerate() {
            writeln!(f, "\t{:04} = {}", i, constant)?;
        }

        writeln!(f, "\nsection .functions")?;
        for (func_id, asm_function) in self.function_map.iter() {
            writeln!(f, "\t{} @{:04}", asm_function.name, func_id)?;
        }

        writeln!(f, "\nsection .text")?;
        for instruction in &self.bytecode {
            let instr = match instruction.opcode {
                OpCode::LoadConst => {
                    format!("LOAD_CONST &{:04}", instruction.arg.get_all())
                }
                OpCode::Pop => {
                    "POP".to_string()
                }
                OpCode::ExternCall => {
                    let func_ptr = instruction.arg.get_all();
                    format!("CALL_EXTERNAL_FUNCTION &{:04}", func_ptr)
                }
                OpCode::Halt => {
                    "HALT".to_string()
                }
                OpCode::Add => {
                    "ADD".to_string()
                }
                OpCode::Sub => {
                    "SUB".to_string()
                }
                OpCode::Lte => {
                    "LTE".to_string()
                }
                OpCode::LoadVar => {
                    format!("LOAD_VAR @{}", instruction.arg.get_all())
                }
                OpCode::LoadArg => {
                    format!("LOAD_ARG @{}", instruction.arg.get_all())
                }
                OpCode::LocalSpace => {
                    format!("LOCAL_SPACE #{}", instruction.arg.get_all())
                }
                OpCode::Return => {
                    "RETURN".to_string()
                }
                OpCode::JmpZ => {
                    format!("JMP_Z {}", instruction.arg.get_all() as i32)
                }
                OpCode::Jmp => {
                    format!("JMP {}", instruction.arg.get_all() as i32)
                }
                OpCode::Call => {
                    format!("CALL @{:04}", instruction.arg.get_all())
                }
                _ => {
                    format!("UNKNOWN_INSTRUCTION {:?}", instruction)
                }
            };
            writeln!(f, "\t{}", instr)?;
        }
        Ok(())
    }
}
