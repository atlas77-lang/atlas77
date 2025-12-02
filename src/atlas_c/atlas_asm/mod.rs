use crate::atlas_c::atlas_asm::error::{ASMError, ASMResult, WeirdStuffError};
use crate::atlas_vm::instruction::Instruction;
use crate::atlas_vm::instruction::ProgramDescriptor;
pub mod asm;
mod error;
mod program;

pub struct Assembler {}

pub enum AssemblerError {
    WeirdStuff,
}

impl Assembler {
    pub fn new() -> Self {
        Self {}
    }
    fn make_instruction(&self, byte: u8) -> u32 {
        (byte as u32) << 24
    }
    pub fn asm_from_instruction(&self, source: ProgramDescriptor) -> ASMResult<Vec<u32>> {
        let mut bytecode: Vec<u32> = vec![];
        let mut i = 0;
        while i < source.len() {
            let instr = &source[i];
            match instr {
                Instruction::LoadConst(idx) => {
                    //This assumes idx fits in 24 bits
                    let instruction = self.make_instruction(asm::LOAD_CONST);
                    instruction | (*idx);
                    bytecode.push(instruction);
                }
                Instruction::Pop => {
                    bytecode.push(self.make_instruction(asm::POP));
                }
                Instruction::Dup => {
                    bytecode.push(self.make_instruction(asm::DUP));
                }
                Instruction::Swap => {
                    bytecode.push(self.make_instruction(asm::SWAP));
                }
                Instruction::StoreVar(idx) => {
                    //This assumes idx fits in 24 bits
                    let instruction = self.make_instruction(asm::STORE_VAR);
                    instruction | (*idx as u32);
                    bytecode.push(instruction);
                }
                Instruction::LoadVar(idx) => {
                    let instruction = self.make_instruction(asm::LOAD_VAR);
                    instruction | (*idx as u32);
                    bytecode.push(instruction);
                }
                Instruction::IndexLoad => {
                    bytecode.push(self.make_instruction(asm::INDEX_LOAD));
                }
                Instruction::IndexStore => {
                    bytecode.push(self.make_instruction(asm::INDEX_STORE));
                }
                Instruction::NewList => {
                    bytecode.push(self.make_instruction(asm::NEW_LIST));
                }
                Instruction::Add => {
                    bytecode.push(self.make_instruction(asm::ADD));
                }
                Instruction::Sub => {
                    bytecode.push(self.make_instruction(asm::SUB));
                }
                Instruction::Mul => {
                    bytecode.push(self.make_instruction(asm::MUL));
                }
                Instruction::Div => {
                    bytecode.push(self.make_instruction(asm::DIV));
                }
                Instruction::Mod => {
                    bytecode.push(self.make_instruction(asm::MOD));
                }
                Instruction::Eq => {
                    bytecode.push(self.make_instruction(asm::EQUAL));
                }
                Instruction::Neq => {
                    bytecode.push(self.make_instruction(asm::NOT_EQUAL));
                }
                Instruction::Gt => {
                    bytecode.push(self.make_instruction(asm::GREATER_THAN));
                }
                Instruction::Gte => {
                    bytecode.push(self.make_instruction(asm::GREATER_THAN_OR_EQUAL));
                }
                Instruction::Lt => {
                    bytecode.push(self.make_instruction(asm::LESS_THAN));
                }
                Instruction::Lte => {
                    bytecode.push(self.make_instruction(asm::LESS_THAN_OR_EQUAL));
                }
                Instruction::Jmp { pos } => {
                    //WARNING: This assumes pos fits in 16 bits, so the program can't be longer than 65k instructions
                    //This should be fine for now
                    let mut instruction = self.make_instruction(asm::JUMP);
                    //Relative jump, so we need to keep the sign
                    let pos = *pos as i16 as u16 as u32;
                    instruction |= pos;
                    bytecode.push(instruction);
                }
                /*
                Instruction::JmpZ { pos } => {
                    bytecode.push(asm::JUMP_IF_FALSE);
                    bytecode.extend(&pos.to_le_bytes());
                }
                Instruction::LocalSpace {
                    nb_vars,
                } => {
                    bytecode.push(asm::RESERVE_LOCAL_SPACE);
                    bytecode.push(*nb_vars);
                }
                Instruction::Call { func_id, nb_args } => {
                    let fn_id = source.functions.get(&func_id as &str).ok_or(ASMError::WeirdStuff(WeirdStuffError { details: "That function doesn't seem to exist".to_string() }))?;
                    bytecode.push(asm::CALL_FUNCTION);
                    bytecode.extend(&fn_id.to_le_bytes());
                    bytecode.push(*nb_args);
                }
                Instruction::ExternCall { func_id, nb_args } => {
                    //WARNING: This is temporary
                    bytecode.push(asm::CALL_EXTERNAL_FUNCTION);
                    //Null terminated string
                    for byte in func_id.as_bytes() {
                        bytecode.push(*byte);
                    }
                    bytecode.push(0);
                    bytecode.push(*nb_args);
                }
                Instruction::LoadArg { index } => {
                    bytecode.push(asm::LOAD_ARGUMENT);
                    bytecode.push(*index);
                }
                Instruction::Return => {
                    bytecode.push(asm::RETURN);
                }
                Instruction::NewObj { obj_descriptor } => {
                    bytecode.push(asm::NEW_OBJECT);
                    bytecode.extend(&obj_descriptor.to_le_bytes());
                }
                Instruction::GetField { field } => {
                    bytecode.push(asm::LOAD_FIELD);
                    bytecode.extend(&field.to_le_bytes());
                }
                Instruction::SetField { field } => {
                    bytecode.push(asm::STORE_FIELD);
                    bytecode.extend(&field.to_le_bytes());
                }
                Instruction::CastTo(ty) => {
                    bytecode.push(asm::CAST_TO);
                    let ty_byte = match ty {
                        //TODO: We need to add this in runtime::asm so it's not magic numbers
                        crate::atlas_vm::instruction::Type::Integer => 0x01,
                        crate::atlas_vm::instruction::Type::Float => 0x02,
                        crate::atlas_vm::instruction::Type::UnsignedInteger => 0x03,
                        crate::atlas_vm::instruction::Type::Boolean => 0x04,
                        crate::atlas_vm::instruction::Type::String => 0x05,
                        crate::atlas_vm::instruction::Type::Char => 0x06,
                    };
                    bytecode.push(ty_byte);
                }
                Instruction::Halt => {
                    bytecode.push(asm::HALT);
                }*/
                _ => {
                    return Err(ASMError::WeirdStuff(WeirdStuffError {
                        details: "Unimplemented instruction in assembler".to_string(),
                    }));
                }
            }
            i += 1;
        }
        Ok(bytecode)
    }
    pub fn asm_from_text(&self, source: String) -> Result<&[u8], AssemblerError> {
        // Implementation goes here
        Ok([0 as u8].as_ref())
    }
    /*
    pub fn display_asm(&self, asm: &[u8]) -> String {
        let mut text = String::new();
        let mut i = 0;
        while i < asm.len() {
            match asm[i] {
                asm::PUSH_UNIT => text.push_str("PUSH_UNIT\n"),
                asm::PUSH_INT => {
                    //Collect the next 8 bytes as i64
                    let int_bytes = &asm[i + 1..i + 9];
                    let int_value = i64::from_le_bytes(int_bytes.try_into().unwrap());
                    text.push_str(&format!("PUSH_INT {}\n", int_value));
                    i += 8;
                }
                asm::PUSH_FLOAT => {
                    let float_bytes = &asm[i + 1..i + 9];
                    let float_value = f64::from_le_bytes(float_bytes.try_into().unwrap());
                    text.push_str(&format!("PUSH_FLOAT {}\n", float_value));
                    i += 8;
                }
                asm::PUSH_BOOL => {
                    let bool_byte = asm[i + 1];
                    let bool_value = match bool_byte {
                        0 => false,
                        _ => true,
                    };
                    text.push_str(&format!("PUSH_BOOL {}\n", bool_value));
                    i += 1;
                }
                asm::PUSH_STR => {
                    let str_bytes = &asm[i + 1..i + 9];
                    let str_index = usize::from_le_bytes(str_bytes.try_into().unwrap());
                    text.push_str(&format!("PUSH_STR {}\n", str_index));
                    i += size_of::<usize>();
                }
                asm::PUSH_LIST => {
                    let list_bytes = &asm[i + 1..i + 9];
                    let list_index = usize::from_le_bytes(list_bytes.try_into().unwrap());
                    text.push_str(&format!("PUSH_LIST {}\n", list_index));
                    i += size_of::<usize>();
                }
                asm::POP => text.push_str("POP\n"),
                asm::DUP => text.push_str("DUP\n"),
                asm::SWAP => text.push_str("SWAP\n"),
                asm::STORE_VAR => {
                    let var_bytes = &asm[i + 1..i + 9];
                    let var_index = usize::from_le_bytes(var_bytes.try_into().unwrap());
                    text.push_str(&format!("STORE_VAR {}\n", var_index));
                    i += size_of::<usize>();
                }
                asm::LOAD_VAR => {
                    let var_bytes = &asm[i + 1..i + 9];
                    let var_index = usize::from_le_bytes(var_bytes.try_into().unwrap());
                    text.push_str(&format!("LOAD_VAR {}\n", var_index));
                    i += size_of::<usize>();
                }
                asm::INDEX_LOAD => text.push_str("INDEX_LOAD\n"),
                asm::INDEX_STORE => text.push_str("INDEX_STORE\n"),
                asm::NEW_LIST => text.push_str("NEW_LIST\n"),
                asm::ADD => text.push_str("ADD\n"),
                asm::SUB => text.push_str("SUB\n"),
                asm::MUL => text.push_str("MUL\n"),
                asm::DIV => text.push_str("DIV\n"),
                asm::MOD => text.push_str("MOD\n"),
                asm::EQUAL => text.push_str("EQUAL\n"),
                asm::NOT_EQUAL => text.push_str("NOT_EQUAL\n"),
                asm::GREATER_THAN => text.push_str("GREATER_THAN\n"),
                asm::GREATER_THAN_OR_EQUAL => text.push_str("GREATER_THAN_OR_EQUAL\n"),
                asm::LESS_THAN => text.push_str("LESS_THAN\n"),
                asm::LESS_THAN_OR_EQUAL => text.push_str("LESS_THAN_OR_EQUAL\n"),
                asm::JUMP => {
                    let pos_bytes = &asm[i + 1..i + 9];
                    let pos = isize::from_le_bytes(pos_bytes.try_into().unwrap());
                    text.push_str(&format!("JUMP {}\n", pos));
                    i += size_of::<isize>();
                }
                asm::JUMP_IF_FALSE => {
                    let pos_bytes = &asm[i + 1..i + 9];
                    let pos = isize::from_le_bytes(pos_bytes.try_into().unwrap());
                    text.push_str(&format!("JUMP_IF_FALSE {}\n", pos));
                    i += size_of::<isize>();
                }
                asm::RESERVE_LOCAL_SPACE => {
                    let nb_vars = asm[i + 1];
                    text.push_str(&format!("RESERVE_LOCAL_SPACE {}\n", nb_vars));
                    i += 1;
                }
                asm::CALL_FUNCTION => {
                    let fn_id_bytes = &asm[i + 1..i + 9];
                    let fn_id = usize::from_le_bytes(fn_id_bytes.try_into().unwrap());
                    let nb_args = asm[i + 5];
                    text.push_str(&format!("CALL_FUNCTION {} {}\n", fn_id, nb_args));
                    i += size_of::<usize>() + 1;
                }
                asm::CALL_EXTERNAL_FUNCTION => {
                    //Read until null terminator
                    let mut j = i + 1;
                    let mut func_name_bytes = vec![];
                    while asm[j] != 0 {
                        func_name_bytes.push(asm[j]);
                        j += 1;
                    }
                    let func_name = String::from_utf8(func_name_bytes).unwrap();
                    let nb_args = asm[j + 1];
                    text.push_str(&format!("CALL_EXTERNAL_FUNCTION {} {}\n", func_name, nb_args));
                    i = j + 1;
                }
                asm::LOAD_ARGUMENT => {
                    let index = asm[i + 1];
                    text.push_str(&format!("LOAD_ARGUMENT {}\n", index));
                    i += 1;
                }
                asm::RETURN => text.push_str("RETURN\n"),
                asm::NEW_OBJECT => {
                    let obj_bytes = &asm[i + 1..i + 9];
                    let obj_descriptor = usize::from_le_bytes(obj_bytes.try_into().unwrap());
                    text.push_str(&format!("NEW_OBJECT {}\n", obj_descriptor));
                    i += size_of::<usize>();
                }
                asm::LOAD_FIELD => {
                    let field_bytes = &asm[i + 1..i + 9];
                    let field = usize::from_le_bytes(field_bytes.try_into().unwrap());
                    text.push_str(&format!("LOAD_FIELD {}\n", field));
                    i += size_of::<usize>();
                }
                asm::STORE_FIELD => {
                    let field_bytes = &asm[i + 1..i + 9];
                    let field = usize::from_le_bytes(field_bytes.try_into().unwrap());
                    text.push_str(&format!("STORE_FIELD {}\n", field));
                    i += size_of::<usize>();
                }
                asm::CAST_TO => {
                    let ty_byte = asm[i + 1];
                    let ty_str = match ty_byte {
                        0x01 => "Integer",
                        0x02 => "Float",
                        0x03 => "UnsignedInteger",
                        0x04 => "Boolean",
                        0x05 => "String",
                        0x06 => "Char",
                        _ => unreachable!("Invalid type byte: {}", ty_byte),
                    };
                    text.push_str(&format!("CAST_TO {}\n", ty_str));
                    i += 1;
                }
                asm::HALT => text.push_str("HALT\n"),
                _ => unreachable!("Unknown bytecode: {}", asm[i]),
            }
            i += 1;
        }
        text
    }*/
}
