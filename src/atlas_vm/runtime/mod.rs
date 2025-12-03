pub mod vm_state;

use crate::atlas_vm::runtime::vm_state::VMState;
use crate::atlas_vm::vm_data::VMData;

use crate::atlas_c::atlas_asm::{asm::{CALL_EXTERNAL_FUNCTION, HALT, LOAD_CONST, POP}, AsmProgram};
use crate::atlas_c::atlas_hir::signature::ConstantValue;
use crate::atlas_vm;
use crate::atlas_vm::error::{RuntimeError, RuntimeResult};
use crate::atlas_vm::heap::{Heap, HEAP_DEFAULT_SIZE};
use crate::atlas_vm::object::ObjectKind;
use crate::atlas_vm::stack::Stack;
use std::collections::BTreeMap;

pub type CallBack = fn(VMState) -> RuntimeResult<VMData>;

pub struct AtlasRuntime<'run> {
    pub stack: Stack,
    pub heap: Heap,
    pub extern_fn: BTreeMap<&'run str, CallBack>,
    /// Program Counter
    pub pc: usize,
    pub asm_program: AsmProgram,
}

impl<'run> AtlasRuntime<'run> {
    pub fn new(asm_program: AsmProgram, extern_fn: BTreeMap<&'run str, CallBack>) -> Self {
        let mut extern_fn = extern_fn;
        if asm_program.has_standard_lib {
            extern_fn.insert("println", atlas_vm::libraries::io::println as CallBack);
        }
        Self {
            stack: Stack::new(),
            heap: Heap::new(HEAP_DEFAULT_SIZE),
            extern_fn,
            pc: 0,
            asm_program,
        }
    }
    pub fn run(&mut self) -> RuntimeResult<()> {
        loop {
            let instr = match self.asm_program.bytecode.get(self.pc) {
                Some(i) => *i,
                None => return Err(RuntimeError::OutOfBoundProgram(self.pc)),
            };
            self.pc += 1;
            match self.execute_instruction(instr) {
                Ok(_) => {}
                Err(RuntimeError::HaltEncountered) => break,
                Err(e) => return Err(e),
            }
        }
        Ok(())
    }
    #[inline(always)]
    fn decode_instruction(&self, bytecode: u32) -> u8 {
        (bytecode >> 24) as u8
    }
    #[inline(always)]
    /// This assumes the given bytecode as the format: [opcode:8][operand:24]
    ///
    /// Returns the operand part of the instruction
    fn get_24bits_operand(&self, bytecode: u32) -> u32 {
        bytecode & 0x00FFFFFF
    }
    #[inline(always)]
    /// This assumes the given bytecode as the format: [opcode:8][operand1:8][operand2:16]
    ///
    /// Returns (operand1, operand2)
    fn get_8bits_and_16bits_operands(&self, bytecode: u32) -> (u8, u16) {
        let operand1 = ((bytecode >> 16) & 0x000000FF) as u8;
        let operand2 = (bytecode & 0x0000FFFF) as u16;
        (operand1, operand2)
    }
    fn execute_instruction(&mut self, instr: u32) -> RuntimeResult<()> {
        let opcode = self.decode_instruction(instr);
        match opcode {
            LOAD_CONST => {
                let const_ptr = self.get_24bits_operand(instr) as usize;
                let val = &self.asm_program.constant_pool[const_ptr];
                match val {
                    ConstantValue::String(s) => {
                        let obj = ObjectKind::String(s.to_string());
                        let obj_idx = self.heap.put(obj)?;
                        self.stack.push(VMData::new_string(obj_idx))?;
                    }
                    ConstantValue::Int(i) => {
                        self.stack.push(VMData::new_i64(*i))?;
                    }
                    ConstantValue::UInt(u) => {
                        self.stack.push(VMData::new_u64(*u))?;
                    }
                    ConstantValue::Float(f) => {
                        self.stack.push(VMData::new_f64(*f))?;
                    }
                    ConstantValue::Bool(b) => {
                        self.stack.push(VMData::new_bool(*b))?;
                    }
                    ConstantValue::Char(c) => {
                        self.stack.push(VMData::new_char(*c))?;
                    }
                    ConstantValue::Unit => {
                        self.stack.push(VMData::new_unit())?;
                    }
                    ConstantValue::List(l) => {
                        unimplemented!("Loading constant lists is not implemented yet");
                    }
                }
                Ok(())
            }
            CALL_EXTERNAL_FUNCTION => {
                let func_ptr = self.get_24bits_operand(instr);
                let func_name = match &self.asm_program.constant_pool[func_ptr as usize] {
                    ConstantValue::String(s) => s.as_str(),
                    _ => return Err(RuntimeError::InvalidConstantPoolPointer(func_ptr as usize)),
                };
                let extern_fn = match self.extern_fn.get(func_name) {
                    Some(f) => f,
                    None => return Err(RuntimeError::ExternFunctionNotFound(func_name.to_string())),
                };
                // Prepare a new VMState for the external function call
                let vm_state = VMState::new(&mut self.stack, &mut self.heap);
                // Call the external function
                let result = extern_fn(vm_state)?;
                // Push the result onto the stack
                self.stack.push(result)?;
                Ok(())
            }
            POP => {
                self.stack.pop()?;
                Ok(())
            }
            HALT => {
                Err(RuntimeError::HaltEncountered)
            }
            _ => unimplemented!("{:?}", instr),
        }
    }
}
