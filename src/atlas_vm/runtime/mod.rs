pub mod vm_state;

use crate::atlas_vm::runtime::vm_state::VMState;
use crate::atlas_vm::vm_data::{VMData, VMTag};

use crate::atlas_c::atlas_asm::AsmProgram;
use crate::atlas_c::atlas_hir::signature::ConstantValue;
use crate::atlas_vm;
use crate::atlas_vm::error::{RuntimeError, RuntimeResult};
use crate::atlas_vm::heap::{HEAP_DEFAULT_SIZE, Heap};
use crate::atlas_vm::instruction::{Instr, OpCode};
use crate::atlas_vm::object::{ObjectKind, RawStructure, Structure};
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
    /// Arguments for the current function call
    pub args: [VMData; 16],
}

impl<'run> AtlasRuntime<'run> {
    pub fn new(asm_program: AsmProgram, extern_fn: BTreeMap<&'run str, CallBack>) -> Self {
        let mut extern_fn = extern_fn;
        if asm_program.using_std {
            //std/io
            for (name, func) in atlas_vm::libraries::io::IO_FUNCTIONS.iter() {
                extern_fn.insert(name, *func as CallBack);
            }
            //std/fs
            for (name, func) in atlas_vm::libraries::fs::FILE_FUNCTIONS.iter() {
                extern_fn.insert(name, *func as CallBack);
            }
            //std/time
            for (name, func) in atlas_vm::libraries::time::TIME_FUNCTIONS.iter() {
                extern_fn.insert(name, *func as CallBack);
            }
            //std/string
            for (name, func) in atlas_vm::libraries::string::STRING_FUNCTIONS.iter() {
                extern_fn.insert(name, *func as CallBack);
            }
            //std/math
            for (name, func) in atlas_vm::libraries::math::MATH_FUNCTIONS.iter() {
                extern_fn.insert(name, *func as CallBack);
            }
            //std/list
            for (name, func) in atlas_vm::libraries::vector::VECTOR_FUNCTIONS.iter() {
                extern_fn.insert(name, *func as CallBack);
            }
        }
        Self {
            stack: Stack::new(),
            heap: Heap::new(HEAP_DEFAULT_SIZE),
            extern_fn,
            pc: 0,
            args: [VMData::new_unit(); 16],
            asm_program,
        }
    }
    pub fn run(&mut self) -> RuntimeResult<()> {
        let entry_point = self
            .asm_program
            .entry_point
            .expect("There should be a main function");
        self.pc = entry_point;
        let mut instr_count: i64 = 0;
        let start = std::time::Instant::now();
        self.stack.new_stack_frame(self.pc, 0);
        loop {
            let instr = match self.asm_program.bytecode.get(self.pc) {
                Some(i) => *i,
                None => return Err(RuntimeError::OutOfBoundProgram(self.pc)),
            };
            //eprintln!("{}", self.stack); //--- IGNORE ---
            //eprintln!("Instr @ {}: {:?}", self.pc, instr.opcode); //--- IGNORE ---
            self.pc += 1;
            match self.execute_instruction(instr) {
                Ok(_) => {}
                Err(RuntimeError::HaltEncountered) => break,
                Err(e) => return Err(e),
            }
            instr_count += 1;
        }
        let duration = start.elapsed();
        eprintln!("Execution finished in: {:?}", duration);
        eprintln!("Execution finished after {} instructions.", instr_count);
        eprintln!(
            "Amount of mips (Million Instructions Per Second): {}",
            (instr_count as f64) / (duration.as_secs_f64() * 1_000_000.0)
        );
        Ok(())
    }

    //TODO: Add more error handling
    #[inline(always)] //This function should be inlined for performance, as it's called very often
    fn execute_instruction(&mut self, instr: Instr) -> RuntimeResult<()> {
        match instr.opcode {
            OpCode::LOAD_CONST => {
                let const_ptr = instr.arg.as_u24() as usize;
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
                        self.stack.push(VMData::new_boolean(*b))?;
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
            OpCode::POP => {
                self.stack.pop()?;
                Ok(())
            }
            OpCode::DUP => {
                let val = self.stack.get_last()?;
                self.stack.push(*val)?;
                Ok(())
            }
            OpCode::SWAP => {
                let a = self.stack.pop()?;
                let b = self.stack.pop()?;
                self.stack.push(a)?;
                self.stack.push(b)?;
                Ok(())
            }
            OpCode::STORE_VAR => {
                let local_slot_idx = instr.arg.as_u24() as usize;
                let data = self.stack.pop()?;
                self.stack.set_var(local_slot_idx, data);
                Ok(())
            }
            OpCode::LOAD_VAR => {
                let local_slot_idx = instr.arg.as_u24() as usize;
                let data = self.stack.get_var(local_slot_idx);
                self.stack.push(data)
            }
            OpCode::INDEX_LOAD => {
                let ptr = self.stack.pop()?.as_object();
                let index = self.stack.pop()?.as_u64() as usize;
                let raw_list = self.heap.get(ptr)?;
                let list = raw_list.list();
                let val = list[index];
                self.stack.push(val)?;
                Ok(())
            }
            OpCode::INDEX_STORE => {
                let val = self.stack.pop()?;
                let ptr = self.stack.pop()?.as_object();
                let index = self.stack.pop()?.as_u64() as usize;
                let raw_list = self.heap.get_mut(ptr)?;
                let list = raw_list.list_mut();
                list[index] = val;
                Ok(())
            }
            OpCode::STRING_LOAD => {
                let ptr = self.stack.pop()?.as_object();
                let index = self.stack.pop()?.as_u64() as usize;
                let raw_string = self.heap.get(ptr)?;
                let string = raw_string.string();
                let ch = string.chars().nth(index).unwrap();
                self.stack.push(VMData::new_char(ch))?;
                Ok(())
            }
            OpCode::STRING_STORE => {
                let ch = self.stack.pop()?.as_char();
                let ptr = self.stack.pop()?.as_object();
                let index = self.stack.pop()?.as_u64() as usize;
                let raw_string = self.heap.get_mut(ptr)?;
                let string = raw_string.string_mut();
                let mut chars: Vec<char> = string.chars().collect();
                chars[index] = ch;
                *string = chars.into_iter().collect();
                Ok(())
            }
            OpCode::NEW_ARRAY => {
                let size = self.stack.pop()?.as_u64() as usize;
                let list = vec![VMData::new_unit(); size];
                let obj_idx = self.heap.put(ObjectKind::List(list))?;
                self.stack.push(VMData::new_list(obj_idx))?;
                Ok(())
            }
            OpCode::INT_ADD => {
                let b = self.stack.pop()?;
                let a = self.stack.pop()?;
                let res = VMData::new_i64(a.as_i64() + b.as_i64());
                self.stack.push(res)
            }
            OpCode::FLOAT_ADD => {
                let b = self.stack.pop()?;
                let a = self.stack.pop()?;
                let res = VMData::new_f64(a.as_f64() + b.as_f64());
                self.stack.push(res)
            }
            OpCode::UINT_ADD => {
                let b = self.stack.pop()?;
                let a = self.stack.pop()?;
                let res = VMData::new_u64(a.as_u64() + b.as_u64());
                self.stack.push(res)
            }
            OpCode::INT_SUB => {
                let b = self.stack.pop()?;
                let a = self.stack.pop()?;
                let res = VMData::new_i64(a.as_i64() - b.as_i64());
                self.stack.push(res)
            }
            OpCode::FLOAT_SUB => {
                let b = self.stack.pop()?;
                let a = self.stack.pop()?;
                let res = VMData::new_f64(a.as_f64() - b.as_f64());
                self.stack.push(res)
            }
            OpCode::UINT_SUB => {
                let b = self.stack.pop()?;
                let a = self.stack.pop()?;
                let res = VMData::new_u64(a.as_u64() - b.as_u64());
                self.stack.push(res)
            }
            OpCode::INT_MUL => {
                let b = self.stack.pop()?;
                let a = self.stack.pop()?;
                let res = VMData::new_i64(a.as_i64() * b.as_i64());
                self.stack.push(res)
            }
            OpCode::FLOAT_MUL => {
                let b = self.stack.pop()?;
                let a = self.stack.pop()?;
                let res = VMData::new_f64(a.as_f64() * b.as_f64());
                self.stack.push(res)
            }
            OpCode::UINT_MUL => {
                let b = self.stack.pop()?;
                let a = self.stack.pop()?;
                let res = VMData::new_u64(a.as_u64() * b.as_u64());
                self.stack.push(res)
            }
            OpCode::INT_DIV => {
                let b = self.stack.pop()?;
                let a = self.stack.pop()?;
                if b.as_i64() == 0 {
                    return Err(RuntimeError::DivisionByZero);
                }
                let res = VMData::new_i64(a.as_i64() / b.as_i64());
                self.stack.push(res)
            }
            OpCode::FLOAT_DIV => {
                let b = self.stack.pop()?;
                let a = self.stack.pop()?;
                if b.as_f64() == 0.0 {
                    return Err(RuntimeError::DivisionByZero);
                }
                let res = VMData::new_f64(a.as_f64() / b.as_f64());
                self.stack.push(res)
            }
            OpCode::UINT_DIV => {
                let b = self.stack.pop()?;
                let a = self.stack.pop()?;
                if b.as_u64() == 0 {
                    return Err(RuntimeError::DivisionByZero);
                }
                let res = VMData::new_u64(a.as_u64() / b.as_u64());
                self.stack.push(res)
            }
            OpCode::INT_MOD => {
                let b = self.stack.pop()?;
                let a = self.stack.pop()?;
                let res = VMData::new_i64(a.as_i64() % b.as_i64());
                self.stack.push(res)
            }
            OpCode::FLOAT_MOD => {
                let b = self.stack.pop()?;
                let a = self.stack.pop()?;
                let res = VMData::new_f64(a.as_f64() % b.as_f64());
                self.stack.push(res)
            }
            OpCode::UINT_MOD => {
                let b = self.stack.pop()?;
                let a = self.stack.pop()?;
                let res = VMData::new_u64(a.as_u64() % b.as_u64());
                self.stack.push(res)
            }
            OpCode::INT_EQUAL => {
                let b = self.stack.pop()?;
                let a = self.stack.pop()?;
                let res = VMData::new_boolean(a.as_i64() == b.as_i64());
                self.stack.push(res)
            }
            OpCode::UINT_EQUAL => {
                let b = self.stack.pop()?;
                let a = self.stack.pop()?;
                let res = VMData::new_boolean(a.as_u64() == b.as_u64());
                self.stack.push(res)
            }
            OpCode::FLOAT_EQUAL => {
                let b = self.stack.pop()?;
                let a = self.stack.pop()?;
                let res = VMData::new_boolean(a.as_f64() == b.as_f64());
                self.stack.push(res)
            }
            OpCode::BOOL_EQUAL => {
                let b = self.stack.pop()?;
                let a = self.stack.pop()?;
                let res = VMData::new_boolean(a.as_boolean() == b.as_boolean());
                self.stack.push(res)
            }
            OpCode::CHAR_EQUAL => {
                let b = self.stack.pop()?;
                let a = self.stack.pop()?;
                let res = VMData::new_boolean(a.as_char() == b.as_char());
                self.stack.push(res)
            }
            OpCode::INT_NOT_EQUAL => {
                let b = self.stack.pop()?;
                let a = self.stack.pop()?;
                let res = VMData::new_boolean(a.as_i64() != b.as_i64());
                self.stack.push(res)
            }
            OpCode::UINT_NOT_EQUAL => {
                let b = self.stack.pop()?;
                let a = self.stack.pop()?;
                let res = VMData::new_boolean(a.as_u64() != b.as_u64());
                self.stack.push(res)
            }
            OpCode::FLOAT_NOT_EQUAL => {
                let b = self.stack.pop()?;
                let a = self.stack.pop()?;
                let res = VMData::new_boolean(a.as_f64() != b.as_f64());
                self.stack.push(res)
            }
            OpCode::BOOL_NOT_EQUAL => {
                let b = self.stack.pop()?;
                let a = self.stack.pop()?;
                let res = VMData::new_boolean(a.as_boolean() != b.as_boolean());
                self.stack.push(res)
            }
            OpCode::CHAR_NOT_EQUAL => {
                let b = self.stack.pop()?;
                let a = self.stack.pop()?;
                let res = VMData::new_boolean(a.as_char() != b.as_char());
                self.stack.push(res)
            }
            OpCode::INT_GREATER_THAN => {
                let b = self.stack.pop()?;
                let a = self.stack.pop()?;
                let res = VMData::new_boolean(a.as_i64() > b.as_i64());
                self.stack.push(res)
            }
            OpCode::UINT_GREATER_THAN => {
                let b = self.stack.pop()?;
                let a = self.stack.pop()?;
                let res = VMData::new_boolean(a.as_u64() > b.as_u64());
                self.stack.push(res)
            }
            OpCode::FLOAT_GREATER_THAN => {
                let b = self.stack.pop()?;
                let a = self.stack.pop()?;
                let res = VMData::new_boolean(a.as_f64() > b.as_f64());
                self.stack.push(res)
            }
            OpCode::INT_GREATER_THAN_EQUAL => {
                let b = self.stack.pop()?;
                let a = self.stack.pop()?;
                let res = VMData::new_boolean(a.as_i64() >= b.as_i64());
                self.stack.push(res)
            }
            OpCode::UINT_GREATER_THAN_EQUAL => {
                let b = self.stack.pop()?;
                let a = self.stack.pop()?;
                let res = VMData::new_boolean(a.as_u64() >= b.as_u64());
                self.stack.push(res)
            }
            OpCode::FLOAT_GREATER_THAN_EQUAL => {
                let b = self.stack.pop()?;
                let a = self.stack.pop()?;
                let res = VMData::new_boolean(a.as_f64() >= b.as_f64());
                self.stack.push(res)
            }
            OpCode::INT_LESS_THAN => {
                let b = self.stack.pop()?;
                let a = self.stack.pop()?;
                let res = VMData::new_boolean(a.as_i64() < b.as_i64());
                self.stack.push(res)
            }
            OpCode::UINT_LESS_THAN => {
                let b = self.stack.pop()?;
                let a = self.stack.pop()?;
                let res = VMData::new_boolean(a.as_u64() < b.as_u64());
                self.stack.push(res)
            }
            OpCode::FLOAT_LESS_THAN => {
                let b = self.stack.pop()?;
                let a = self.stack.pop()?;
                let res = VMData::new_boolean(a.as_f64() < b.as_f64());
                self.stack.push(res)
            }
            OpCode::INT_LESS_THAN_EQUAL => {
                let b = self.stack.pop()?;
                let a = self.stack.pop()?;
                let res = VMData::new_boolean(a.as_i64() <= b.as_i64());
                self.stack.push(res)
            }
            OpCode::UINT_LESS_THAN_EQUAL => {
                let b = self.stack.pop()?;
                let a = self.stack.pop()?;
                let res = VMData::new_boolean(a.as_u64() <= b.as_u64());
                self.stack.push(res)
            }
            OpCode::FLOAT_LESS_THAN_EQUAL => {
                let b = self.stack.pop()?;
                let a = self.stack.pop()?;
                let res = VMData::new_boolean(a.as_f64() <= b.as_f64());
                self.stack.push(res)
            }
            OpCode::BOOL_AND => {
                let b = self.stack.pop()?.as_boolean();
                let a = self.stack.pop()?.as_boolean();
                let res = VMData::new_boolean(a && b);
                self.stack.push(res)
            }
            OpCode::BOOL_OR => {
                let b = self.stack.pop()?.as_boolean();
                let a = self.stack.pop()?.as_boolean();
                let res = VMData::new_boolean(a || b);
                self.stack.push(res)
            }
            OpCode::JMP => {
                let where_to = instr.arg.as_i24();
                if where_to.is_negative() {
                    self.pc -= where_to.unsigned_abs() as usize;
                } else {
                    self.pc += where_to as usize;
                }
                Ok(())
            }
            OpCode::JMP_Z => {
                let where_to = instr.arg.as_i24();
                let condition = self.stack.pop()?.as_boolean();
                if !condition {
                    if where_to.is_negative() {
                        self.pc -= where_to.unsigned_abs() as usize;
                    } else {
                        self.pc += where_to as usize
                    }
                }
                Ok(())
            }
            OpCode::LOCAL_SPACE => {
                let size = instr.arg.as_u24() as usize;
                self.stack.reserve_space(size)
            }
            OpCode::CALL => {
                let func_id = instr.arg.as_u24() as usize;
                let func_data = self
                    .asm_program
                    .function_map
                    .get(&func_id)
                    .ok_or(RuntimeError::FunctionNotFound(func_id))?;
                let nb_args = func_data.nb_args;
                // Pop arguments from the stack into self.args (preserving original order)
                // caller pushed args left-to-right, so we pop right-to-left into self.args[0..]
                for i in 0..nb_args {
                    self.args[i] = self.stack.pop()?;
                }
                //Only display nb_args args for clarity
                //eprintln!("Calling function {} with [{:?}] args", func_data.name, &self.args[0..nb_args]); //--- IGNORE ---

                // Create new stack frame (save old pc & bp)
                self.stack.new_stack_frame(self.pc, self.stack.base_ptr);

                // Place the arguments into the new frame's variable slots.
                // We popped args into self.args such that self.args[0] is last pushed arg.
                // We must place them so arg 0 (first function param) is at var slot 0.
                for i in 0..nb_args {
                    let src_idx = nb_args - 1 - i; // reverse the popped order
                    let val = self.args[src_idx];
                    self.stack.set_var(i, val);
                }

                self.stack.top += nb_args;

                // Jump to callee entry point
                self.pc = func_data.entry_point;
                Ok(())
            }
            OpCode::EXTERN_CALL => {
                let func_ptr = instr.arg.as_u24();
                let func_name = match &self.asm_program.constant_pool[func_ptr as usize] {
                    ConstantValue::String(s) => s.as_str(),
                    _ => return Err(RuntimeError::InvalidConstantPoolPointer(func_ptr as usize)),
                };
                let extern_fn = match self.extern_fn.get(func_name) {
                    Some(f) => f,
                    None => {
                        return Err(RuntimeError::ExternFunctionNotFound(func_name.to_string()));
                    }
                };
                // Prepare a new VMState for the external function call
                let vm_state = VMState::new(
                    &mut self.stack,
                    &mut self.heap,
                    &self.asm_program.constant_pool,
                    &self.asm_program.struct_descriptors,
                );
                // Call the external function
                let result = extern_fn(vm_state)?;
                // Push the result onto the stack
                self.stack.push(result)?;
                Ok(())
            }
            OpCode::RETURN => {
                let (pc, ret_val) = self.stack.return_from_stack_frame()?;
                // Restore the base pointer and program counter
                self.pc = pc;
                // Push the return value onto the stack
                self.stack.push(ret_val)
            }
            OpCode::NEW_OBJ => {
                let obj_descriptor_ptr = instr.arg.as_u24() as usize;
                let struct_descriptor = &self.asm_program.struct_descriptors[obj_descriptor_ptr];
                let nb_fields = struct_descriptor.nb_fields;
                let fields = vec![VMData::new_unit(); nb_fields];
                let obj = ObjectKind::Structure(Structure {
                    fields: RawStructure {
                        ptr: fields,
                        len: nb_fields,
                    },
                    struct_descriptor: obj_descriptor_ptr,
                });
                let obj_idx = self.heap.put(obj)?;
                self.stack.push(VMData::new_object(obj_idx))
            }
            OpCode::GET_FIELD => {
                let field_idx = instr.arg.as_u24() as usize;
                let stack_top = self.stack.pop()?;
                let obj_ptr = match stack_top {
                    VMData {
                        tag: VMTag::Object | VMTag::List | VMTag::String,
                        ..
                    } => {
                        stack_top.as_object()
                    }
                    VMData {
                        tag: VMTag::Ref,
                        ..
                    } => {
                        let ref_ptr = stack_top.as_ref();
                        // Safety: The pointer should be valid as long as the referenced variable
                        // is still in scope. The type system should ensure this.
                        let deref_data = unsafe { *ref_ptr };
                        if !deref_data.is_object() {
                            return Err(RuntimeError::InvalidObjectAccess(
                                deref_data.tag,
                            ));
                        }
                        deref_data.as_object()
                    }
                    _ => {
                        return Err(RuntimeError::InvalidObjectAccess(
                            stack_top.tag,
                        ));
                    }
                };
                let raw_obj = self.heap.get(obj_ptr)?;
                let structure = raw_obj.structure();
                let field_value = structure.fields.ptr[field_idx];
                self.stack.push(field_value)
            }
            OpCode::SET_FIELD => {
                let field_idx = instr.arg.as_u24() as usize;
                let value = self.stack.pop()?;
                let obj_ptr = self.stack.pop()?.as_object();
                let raw_obj = self.heap.get_mut(obj_ptr)?;
                let structure = raw_obj.structure_mut();
                structure.fields.ptr[field_idx] = value;
                Ok(())
            }
            OpCode::DELETE_OBJ => {
                let stack_data = self.stack.pop()?;
                let obj_ptr = if stack_data.is_object() {
                    stack_data.as_object()
                } else {
                    return Ok(());
                };
                self.heap.free(obj_ptr)
            }
            // === Reference operations ===
            OpCode::LOAD_VAR_ADDR => {
                // Get the address of a local variable and push it as a reference
                let local_slot_idx = instr.arg.as_u24() as usize;
                let var_ptr = self.stack.get_var_ptr(local_slot_idx);
                self.stack.push(VMData::new_ref(var_ptr))
            }
            OpCode::LOAD_INDIRECT => {
                // Dereference: load the value at the address on top of the stack
                let ref_data = self.stack.pop()?;
                let ptr = ref_data.as_ref();
                // Safety: The pointer should be valid as long as the referenced variable
                // is still in scope. The type system should ensure this.
                let value = unsafe { *ptr };
                self.stack.push(value)
            }
            OpCode::STORE_INDIRECT => {
                // Store a value to the address (dereference and assign)
                let value = self.stack.pop()?;
                let ref_data = self.stack.pop()?;
                let ptr = ref_data.as_ref();
                // Safety: The pointer should be valid as long as the referenced variable
                // is still in scope. The type system should ensure this.
                unsafe { *ptr = value };
                Ok(())
            }
            OpCode::GET_FIELD_ADDR => {
                // Get the address of a field in an object
                let field_idx = instr.arg.as_u24() as usize;
                let obj_ptr = self.stack.pop()?.as_object();
                let raw_obj = self.heap.get_mut(obj_ptr)?;
                let structure = raw_obj.structure_mut();
                let field_ptr = &mut structure.fields.ptr[field_idx] as *mut VMData;
                self.stack.push(VMData::new_ref(field_ptr))
            }
            //CAST_TO should really be reworked, it's shitty right now
            OpCode::CAST_TO => {
                let target_type = VMTag::from(instr.arg.as_u24() as u8);
                let value = self.stack.pop()?;
                let casted_value = match target_type as VMTag {
                    VMTag::Int64 => match value.tag {
                        VMTag::String => {
                            let str_ptr = self.heap.get(value.as_object())?;
                            let string = str_ptr.string();
                            let parsed = string
                                .parse::<i64>()
                                .map_err(|_| RuntimeError::InvalidCast(value.tag, target_type))?;
                            VMData::new_i64(parsed)
                        }
                        VMTag::Int64 => value,
                        VMTag::UInt64 => VMData::new_i64(value.as_u64() as i64),
                        VMTag::Float64 => VMData::new_i64(value.as_f64() as i64),
                        VMTag::Boolean => VMData::new_i64(if value.as_boolean() { 1 } else { 0 }),
                        VMTag::Char => VMData::new_i64(value.as_char() as i64),
                        _ => return Err(RuntimeError::InvalidCast(value.tag, target_type)),
                    },
                    VMTag::UInt64 => match value.tag {
                        VMTag::String => {
                            let str_ptr = self.heap.get(value.as_object())?;
                            let string = str_ptr.string();
                            let parsed = string
                                .parse::<u64>()
                                .map_err(|_| RuntimeError::InvalidCast(value.tag, target_type))?;
                            VMData::new_u64(parsed)
                        }
                        VMTag::Int64 => VMData::new_u64(value.as_i64() as u64),
                        VMTag::UInt64 => value,
                        VMTag::Float64 => VMData::new_u64(value.as_f64() as u64),
                        VMTag::Boolean => VMData::new_u64(if value.as_boolean() { 1 } else { 0 }),
                        VMTag::Char => VMData::new_u64(value.as_char() as u64),
                        _ => return Err(RuntimeError::InvalidCast(value.tag, target_type)),
                    },
                    VMTag::Float64 => match value.tag {
                        VMTag::String => {
                            let str_ptr = self.heap.get(value.as_object())?;
                            let string = str_ptr.string();
                            let parsed = string
                                .parse::<f64>()
                                .map_err(|_| RuntimeError::InvalidCast(value.tag, target_type))?;
                            VMData::new_f64(parsed)
                        }
                        VMTag::Int64 => VMData::new_f64(value.as_i64() as f64),
                        VMTag::UInt64 => VMData::new_f64(value.as_u64() as f64),
                        VMTag::Float64 => value,
                        VMTag::Boolean => {
                            VMData::new_f64(if value.as_boolean() { 1.0 } else { 0.0 })
                        }
                        VMTag::Char => VMData::new_f64(value.as_char() as u8 as f64),
                        _ => return Err(RuntimeError::InvalidCast(value.tag, target_type)),
                    },
                    VMTag::Boolean => match value.tag {
                        VMTag::String => {
                            let str_ptr = self.heap.get(value.as_object())?;
                            let string = str_ptr.string().to_lowercase();
                            let parsed = match string.as_str() {
                                "true" => true,
                                "false" => false,
                                _ => return Err(RuntimeError::InvalidCast(value.tag, target_type)),
                            };
                            VMData::new_boolean(parsed)
                        }
                        VMTag::Int64 => VMData::new_boolean(value.as_i64() != 0),
                        VMTag::UInt64 => VMData::new_boolean(value.as_u64() != 0),
                        VMTag::Float64 => VMData::new_boolean(value.as_f64() != 0.0),
                        VMTag::Boolean => value,
                        VMTag::Char => VMData::new_boolean(value.as_char() != '\0'),
                        _ => return Err(RuntimeError::InvalidCast(value.tag, target_type)),
                    },
                    VMTag::Char => match value.tag {
                        VMTag::String => {
                            let str_ptr = self.heap.get(value.as_object())?;
                            let string = str_ptr.string();
                            let mut chars = string.chars();
                            let ch = chars
                                .next()
                                .ok_or(RuntimeError::InvalidCast(value.tag, target_type))?;
                            if chars.next().is_some() {
                                return Err(RuntimeError::InvalidCast(value.tag, target_type));
                            }
                            VMData::new_char(ch)
                        }
                        VMTag::Int64 => {
                            let i = value.as_i64();
                            if i < 0 || i > u8::MAX as i64 {
                                return Err(RuntimeError::InvalidCast(value.tag, target_type));
                            }
                            VMData::new_char(i as u8 as char)
                        }
                        VMTag::UInt64 => {
                            let u = value.as_u64();
                            if u > u8::MAX as u64 {
                                return Err(RuntimeError::InvalidCast(value.tag, target_type));
                            }
                            VMData::new_char(u as u8 as char)
                        }
                        _ => return Err(RuntimeError::InvalidCast(value.tag, target_type)),
                    },
                    VMTag::String => {
                        let str_ptr = self.heap.get(value.as_object())?;
                        let string = str_ptr.string();
                        let ptr = match self.heap.put(ObjectKind::String(string.to_string())) {
                            Ok(idx) => idx,
                            Err(_) => return Err(RuntimeError::OutOfMemory),
                        };
                        VMData::new_string(ptr)
                    }
                    _ => return Err(RuntimeError::InvalidCast(value.tag, target_type)),
                };
                self.stack.push(casted_value)
            }
            OpCode::Halt => Err(RuntimeError::HaltEncountered),
            _ => unimplemented!("{:?}", instr),
        }
    }
}
