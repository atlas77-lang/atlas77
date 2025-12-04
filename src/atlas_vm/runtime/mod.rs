pub mod vm_state;

use crate::atlas_vm::runtime::vm_state::VMState;
use crate::atlas_vm::vm_data::VMData;

use crate::atlas_c::atlas_asm::AsmProgram;
use crate::atlas_c::atlas_hir::signature::ConstantValue;
use crate::atlas_vm;
use crate::atlas_vm::error::{RuntimeError, RuntimeResult};
use crate::atlas_vm::heap::{Heap, HEAP_DEFAULT_SIZE};
use crate::atlas_vm::object::ObjectKind;
use crate::atlas_vm::stack::Stack;
use std::collections::BTreeMap;
use crate::atlas_vm::instruction::{Instr, OpCode};

pub type CallBack = fn(VMState) -> RuntimeResult<VMData>;

pub struct AtlasRuntime<'run> {
    pub stack: Stack,
    pub heap: Heap,
    pub extern_fn: BTreeMap<&'run str, CallBack>,
    /// Program Counter
    pub pc: usize,
    /// Base pointer for the stack frame
    pub base_ptr: usize,
    pub asm_program: AsmProgram,
    /// Arguments for the current function call
    pub args: [VMData; 16],
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
            base_ptr: 0,
            args: [VMData::new_unit(); 16],
            asm_program,
        }
    }
    pub fn run(&mut self) -> RuntimeResult<()> {
        let entry_point = self.asm_program.entry_point.expect("There should be a main function");
        self.pc = entry_point;
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
    fn execute_instruction(&mut self, instr: Instr) -> RuntimeResult<()> {
        println!("Executing instruction at pc {}: {:?}", self.pc, instr);
        match instr.opcode {
            OpCode::LocalSpace => {
                let size = instr.arg.get_all() as usize;
                self.stack.top += size;
                Ok(())
            }
            OpCode::Jmp => {
                let where_to = instr.arg.get_all() as isize;
                self.pc += where_to as usize;
                Ok(())
            }
            OpCode::LoadConst => {
                let const_ptr = instr.arg.get_all() as usize;
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
                    ConstantValue::List(_) => {
                        unimplemented!("Loading constant lists is not implemented yet");
                    }
                }
                Ok(())
            }
            OpCode::ExternCall => {
                let func_ptr = instr.arg.get_all();
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
            OpCode::Pop => {
                self.stack.pop()?;
                Ok(())
            }
            OpCode::LoadArg => {
                let arg_idx = instr.arg.get_all() as usize;
                let data = self.args[arg_idx];
                eprintln!("Loading argument {}: {}", arg_idx, data);
                self.stack[self.base_ptr + arg_idx + 2] = data; // +2 for the base pointer and the return address
                Ok(())
            }
            OpCode::LoadVar => {
                let local_slot_idx = instr.arg.get_all() as usize;
                let data = self.stack[self.base_ptr + local_slot_idx + 2]; // +2 for the base pointer and the return address
                eprintln!("Loading local variable {}: {}", local_slot_idx, data);
                self.stack.push(data)
            }
            //Let's assume the type is `int64` for now
            OpCode::Lte => {
                let a = self.stack.pop()?.as_i64();
                let b = self.stack.pop()?.as_i64();
                let res = VMData::new_bool(b <= a);
                self.stack.push(res)
            }
            OpCode::JmpZ => {
                let where_to = instr.arg.get_all() as isize;
                let condition = self.stack.pop()?.as_bool();
                if condition {
                    self.pc += where_to as usize;
                }
                Ok(())
            }
            OpCode::Return => {
                let program_counter = self.stack[self.base_ptr].as_u64() as usize;
                let stack_ptr = self.stack[self.base_ptr + 1].as_u64() as usize;
                let return_value = self.stack.pop()?;
                // Restore the base pointer and program counter
                self.base_ptr = stack_ptr;
                self.pc = program_counter;
                // Push the return value onto the stack
                self.stack.push(return_value)
            }
            //Is everything actually there? Let me list all the steps I think are necessary:
            //1. Get the function id from the instruction argument
            //2. Look up the function in the function map
            //3. Set up the arguments by popping them from the stack
            //4. Save the current pc and base pointer onto the stack
            //5. Update the pc to the function's entry point
            //6. Update the base pointer to the current top of the stack
            OpCode::Call => {
                let stack_ptr = self.base_ptr;
                let func_id = instr.arg.get_all() as usize;
                let func_data = self.asm_program.function_map.get(&func_id).ok_or(
                    RuntimeError::FunctionNotFound(func_id),
                )?;
                let nb_args = func_data.nb_args;
                // Set up arguments
                for i in 0..nb_args {
                    self.args[i as usize] = self.stack.pop()?;
                }
                
                self.pc = func_data.entry_point;
                self.stack.top = stack_ptr;
                self.base_ptr = self.stack.top;
                
                let program_counter = self.pc;
                self.stack[self.base_ptr] = VMData::new_u64(program_counter as u64);
                self.stack[self.base_ptr + 1] = VMData::new_u64(stack_ptr as u64);
                Ok(())
            }
            OpCode::Add => {
                let b = self.stack.pop()?;
                let a = self.stack.pop()?;
                let res = VMData::new_i64(b.as_i64() + a.as_i64());
                self.stack.push(res)
            }
            OpCode::Sub => {
                let b = self.stack.pop()?;
                let a = self.stack.pop()?;
                let res = VMData::new_i64(b.as_i64() - a.as_i64());
                self.stack.push(res)
            }
            OpCode::Halt => {
                Err(RuntimeError::HaltEncountered)
            }
            _ => unimplemented!("{:?}", instr),
        }
    }
}
