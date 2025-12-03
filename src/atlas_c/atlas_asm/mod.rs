use crate::atlas_c::atlas_asm::error::{ASMError, ASMResult, UnsupportedInstructionError};
use crate::atlas_c::atlas_hir::signature::ConstantValue;
use crate::atlas_c::atlas_codegen::instruction::Instruction;
use crate::atlas_c::atlas_codegen::instruction::ProgramDescriptor;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use crate::atlas_vm::instruction::{Arg, Instr, OpCode};

pub mod asm;
pub mod error;
pub mod program;

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
}

pub struct AsmProgram {
    pub bytecode: Vec<Instr>,
    pub constant_pool: Vec<ConstantValue>,
    /// Mapping of function names to their entry point in the bytecode
    /// Useful for debugging
    pub function_map: HashMap<String, usize>,
    pub entry_point: Option<usize>,
    pub has_standard_lib: bool,
}

impl Assembler {
    pub fn new() -> Assembler {
        Self {
            asm_constant_map: AsmConstantMap::new(),
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
        let mut function_map = HashMap::new();
        for (func_name, &pos) in source.functions.iter() {
            function_map.insert(func_name.to_string(), pos);
        }
        let entry_point = source.functions.get("main").map(|main_func| *main_func);
        Ok(AsmProgram {
            bytecode,
            constant_pool: self.asm_constant_map.constant_pool.clone(),
            entry_point,
            function_map,
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
            writeln!(f, "\t{:04}: {}", i, constant)?;
        }

        writeln!(f, "\nsection .functions")?;
        for (func_name, &pos) in self.function_map.iter() {
            writeln!(f, "\t{}: {:04}", func_name, pos)?;
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
                _ => {
                    format!("UNKNOWN_INSTRUCTION {:?}", instruction)
                }
            };
            writeln!(f, "\t{}", instr)?;
        }
        Ok(())
    }
}
