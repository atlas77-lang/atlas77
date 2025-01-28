pub mod errors;
pub mod memory;
pub mod runtime;
pub mod libraries;

use errors::RuntimeError;
use libraries::{
    fs::FILE_FUNCTIONS, io::IO_FUNCTIONS, list::LIST_FUNCTIONS, math::MATH_FUNCTIONS,
    string::STRING_FUNCTIONS, time::TIME_FUNCTIONS,
};
use runtime::instruction::{Instruction, Program, Type};
use std::collections::HashMap;

use crate::memory::object_map::ObjectKind;
use crate::memory::varmap::VarMap;
use crate::memory::{object_map::Memory, stack::Stack, vm_data::VMData};

pub type RuntimeResult<T> = Result<T, RuntimeError>;
pub type CallBack = fn(runtime::vm_state::VMState) -> RuntimeResult<VMData>;

pub struct Atlas77VM<'run> {
    pub program: Program,
    pub stack: Stack,
    stack_frame: Vec<(usize, usize)>, //previous pc and previous stack top
    pub object_map: Memory,
    pub var_map: VarMap,
    pub extern_fn: HashMap<&'run str, CallBack>,
    pub pc: usize,
}

impl Atlas77VM<'_> {
    pub fn new(program: Program) -> Self {
        let mut extern_fn: HashMap<&str, CallBack> = HashMap::new();
        program.libraries.iter().for_each(|lib| {
            if lib.is_std {
                let lib_name = lib.name.split('/').last().unwrap();
                match lib_name {
                    "file" => {
                        FILE_FUNCTIONS.iter().for_each(|(name, func)| {
                            extern_fn.insert(name, *func);
                        });
                    }
                    "io" => {
                        IO_FUNCTIONS.iter().for_each(|(name, func)| {
                            extern_fn.insert(name, *func);
                        });
                    }
                    "list" => {
                        LIST_FUNCTIONS.iter().for_each(|(name, func)| {
                            extern_fn.insert(name, *func);
                        });
                    }
                    "math" => {
                        MATH_FUNCTIONS.iter().for_each(|(name, func)| {
                            extern_fn.insert(name, *func);
                        });
                    }
                    "string" => {
                        STRING_FUNCTIONS.iter().for_each(|(name, func)| {
                            extern_fn.insert(name, *func);
                        });
                    }
                    "time" => {
                        TIME_FUNCTIONS.iter().for_each(|(name, func)| {
                            extern_fn.insert(name, *func);
                        });
                    }
                    _ => panic!("Unknown standard libraries"),
                }
            }
        });
        Self {
            program,
            stack: Stack::new(),
            stack_frame: Vec::new(),
            object_map: Memory::new(1024),
            var_map: VarMap::new(),
            extern_fn,
            pc: 0,
        }
    }
    pub fn run(&mut self) -> RuntimeResult<VMData> {
        let label = self
            .program
            .labels
            .iter()
            .find(|label| label.name == self.program.entry_point);

        self.stack.extends(
            &self
                .program
                .global
                .function_pool
                .iter()
                .map(|t| VMData::new_fn_ptr(*t))
                .collect::<Vec<_>>(),
        )?;
        println!("fn_ptr in the stack: {:?} & all fn_name: {:?}",
                 self.stack.iter().map(|x| x.as_fn_ptr()).collect::<Vec<_>>(),
                 self.program.labels.iter().map(|x| x.name.clone()).collect::<Vec<_>>()
        );
        if let Some(label) = label {
            self.pc = label.position;
        } else {
            return Err(RuntimeError::EntryPointNotFound(
                self.program.entry_point.to_string(),
            ));
        }
        while self.pc < self.program.len() {
            let instr = self.program[self.pc].clone();
            self.execute_instruction(instr)?;
            println!("Stack: {}", self.stack);
        }
        self.stack.pop_with_rc(&mut self.object_map)
    }
    /// TODO: Add check for unsigned int
    pub fn execute_instruction(&mut self, instr: Instruction) -> RuntimeResult<()> {
        match instr {
            Instruction::PushInt(i) => {
                let val = VMData::new_i64(i);
                self.stack.push_with_rc(val, &mut self.object_map)?;
                self.pc += 1;
            }
            Instruction::PushFloat(f) => {
                let val = VMData::new_f64(f);
                self.stack.push_with_rc(val, &mut self.object_map)?;
                self.pc += 1;
            }
            Instruction::PushUnsignedInt(u) => {
                let val = VMData::new_u64(u);
                self.stack.push_with_rc(val, &mut self.object_map)?;
                self.pc += 1;
            }
            Instruction::PushBool(b) => {
                let val = VMData::new_bool(b);
                self.stack.push_with_rc(val, &mut self.object_map)?;
                self.pc += 1;
            }
            Instruction::PushStr(u) => {
                let string = self.program.global.string_pool[u].clone();
                let ptr = match self.object_map.put(ObjectKind::String(string)) {
                    Ok(ptr) => ptr,
                    Err(_) => return Err(RuntimeError::OutOfMemory),
                };
                self.stack.push(VMData::new_string(ptr))?;
                self.pc += 1;
            }
            Instruction::Lt => {
                let a = self.stack.pop_with_rc(&mut self.object_map)?;
                let b = self.stack.pop_with_rc(&mut self.object_map)?;
                let res = VMData::new_bool(b.as_i64() < a.as_i64());
                self.stack.push_with_rc(res, &mut self.object_map)?;
                self.pc += 1;
            }
            Instruction::Lte => {
                let a = self.stack.pop_with_rc(&mut self.object_map)?;
                let b = self.stack.pop_with_rc(&mut self.object_map)?;
                let res = VMData::new_bool(b.as_i64() <= a.as_i64());
                self.stack.push_with_rc(res, &mut self.object_map)?;
                self.pc += 1;
            }
            Instruction::Gt => {
                let a = self.stack.pop_with_rc(&mut self.object_map)?;
                let b = self.stack.pop_with_rc(&mut self.object_map)?;
                let res = VMData::new_bool(b.as_i64() > a.as_i64());
                self.stack.push_with_rc(res, &mut self.object_map)?;
                self.pc += 1;
            }
            Instruction::Gte => {
                let a = self.stack.pop_with_rc(&mut self.object_map)?;
                let b = self.stack.pop_with_rc(&mut self.object_map)?;
                let res = VMData::new_bool(b.as_i64() >= a.as_i64());
                self.stack.push_with_rc(res, &mut self.object_map)?;
                self.pc += 1;
            }
            Instruction::Eq => {
                let a = self.stack.pop_with_rc(&mut self.object_map)?;
                let b = self.stack.pop_with_rc(&mut self.object_map)?;
                let res = VMData::new_bool(b.as_i64() == a.as_i64());
                self.stack.push_with_rc(res, &mut self.object_map)?;
                self.pc += 1;
            }
            Instruction::Neq => {
                let a = self.stack.pop_with_rc(&mut self.object_map)?;
                let b = self.stack.pop_with_rc(&mut self.object_map)?;
                let res = VMData::new_bool(b.as_i64() != a.as_i64());
                self.stack.push_with_rc(res, &mut self.object_map)?;
                self.pc += 1;
            }
            Instruction::JmpZ { pos } => {
                let cond = self.stack.pop_with_rc(&mut self.object_map)?;
                if !cond.as_bool() {
                    self.pc += (pos + 1) as usize;
                } else {
                    self.pc += 1;
                }
            }
            Instruction::CastTo(t) => {
                let val = self.stack.pop_with_rc(&mut self.object_map)?;
                let res = match t {
                    Type::String => {
                        let string = val.to_string();
                        let ptr = match self.object_map.put(ObjectKind::String(string)) {
                            Ok(ptr) => {
                                ptr
                            }
                            Err(_) => return Err(RuntimeError::OutOfMemory),
                        };
                        VMData::new_string(ptr)
                    }
                    Type::Boolean => {
                        match val.tag {
                            VMData::TAG_STR => {
                                let raw_string = self.object_map.get(val.as_object());
                                let string = raw_string.string();
                                VMData::new_bool(string.parse::<bool>().unwrap())
                            }
                            _ => VMData::new_bool(val.as_bool()),
                        }
                    }
                    Type::Float => {
                        match val.tag {
                            VMData::TAG_STR => {
                                let raw_string = self.object_map.get(val.as_object());
                                let string = raw_string.string();
                                VMData::new_f64(string.parse::<f64>().unwrap())
                            }
                            VMData::TAG_U64 => VMData::new_f64(val.as_u64() as f64),
                            VMData::TAG_I64 => VMData::new_f64(val.as_i64() as f64),
                            VMData::TAG_BOOL => VMData::new_f64(val.as_bool() as i64 as f64),
                            _ => unreachable!("Invalid cast to float"),
                        }
                    }
                    Type::Integer => {
                        match val.tag {
                            VMData::TAG_STR => {
                                let raw_string = self.object_map.get(val.as_object());
                                let string = raw_string.string();
                                VMData::new_i64(string.parse::<i64>().unwrap())
                            }
                            VMData::TAG_U64 => VMData::new_i64(val.as_u64() as i64),
                            VMData::TAG_FLOAT => VMData::new_i64(val.as_f64() as i64),
                            VMData::TAG_BOOL => VMData::new_i64(val.as_bool() as i64),
                            _ => unreachable!("Invalid cast to integer"),
                        }
                    }
                    Type::UnsignedInteger => {
                        match val.tag {
                            VMData::TAG_STR => {
                                let raw_string = self.object_map.get(val.as_object());
                                let string = raw_string.string();
                                VMData::new_u64(string.parse::<u64>().unwrap())
                            }
                            VMData::TAG_I64 => VMData::new_u64(val.as_i64() as u64),
                            VMData::TAG_FLOAT => VMData::new_u64(val.as_f64() as u64),
                            VMData::TAG_BOOL => VMData::new_u64(val.as_bool() as u64),
                            _ => unreachable!("Invalid cast to unsigned integer"),
                        }
                    }
                };
                self.stack.push(res)?;
                self.pc += 1;
            }
            Instruction::Jmp { pos } => {
                self.pc = (self.pc as isize + pos) as usize;
            }
            Instruction::Store { var_name } => {
                let val = self.stack.pop()?;
                if let Some(get_old_val) = self.var_map.get(&var_name)  {
                    match get_old_val.tag {
                        VMData::TAG_OBJECT | VMData::TAG_LIST | VMData::TAG_STR => {
                            self.object_map.rc_dec(get_old_val.as_object());
                        }
                        _ => {}
                    }
                }
                self.var_map.insert(var_name, val, &mut self.object_map);
                self.pc += 1;
            }
            Instruction::Load { var_name } => {
                let val = self.var_map.get(&var_name).unwrap();
                self.stack.push_with_rc(*val, &mut self.object_map)?;
                self.pc += 1;
            }
            Instruction::Swap => {
                let val1 = self.stack.pop()?;
                let val2 = self.stack.pop()?;
                self.stack.push(val1)?;
                self.stack.push(val2)?;
                self.pc += 1;
            }
            Instruction::Dup => {
                let val = *self.stack.last()?;
                self.stack.push_with_rc(val, &mut self.object_map)?;
                self.pc += 1;
            }
            Instruction::IMul => {
                let a = self.stack.pop()?;
                let b = self.stack.pop()?;
                let res = VMData::new_i64(b.as_i64() * a.as_i64());
                self.stack.push_with_rc(res, &mut self.object_map)?;
                self.pc += 1;
            }
            Instruction::FMul => {
                let a = self.stack.pop()?;
                let b = self.stack.pop()?;
                let res = VMData::new_f64(b.as_f64() * a.as_f64());
                self.stack.push_with_rc(res, &mut self.object_map)?;
                self.pc += 1;
            }
            Instruction::UIMul => {
                let a = self.stack.pop()?;
                let b = self.stack.pop()?;
                let res = VMData::new_u64(b.as_u64() * a.as_u64());
                self.stack.push_with_rc(res, &mut self.object_map)?;
                self.pc += 1;
            }
            Instruction::IDiv => {
                let a = self.stack.pop()?;
                if a == VMData::new_i64(0) {
                    return Err(RuntimeError::DivisionByZero);
                }
                let b = self.stack.pop()?;
                let res = VMData::new_i64(b.as_i64() / a.as_i64());
                self.stack.push_with_rc(res, &mut self.object_map)?;
                self.pc += 1;
            }
            Instruction::FDiv => {
                let a = self.stack.pop()?;
                if a == VMData::new_f64(0.0) {
                    return Err(RuntimeError::DivisionByZero);
                }
                let b = self.stack.pop()?;
                let res = VMData::new_f64(b.as_f64() / a.as_f64());
                self.stack.push_with_rc(res, &mut self.object_map)?;
                self.pc += 1;
            }
            Instruction::UIDiv => {
                let a = self.stack.pop()?;
                if a == VMData::new_u64(0) {
                    return Err(RuntimeError::DivisionByZero);
                }
                let b = self.stack.pop()?;
                let res = VMData::new_u64(b.as_u64() / a.as_u64());
                self.stack.push_with_rc(res, &mut self.object_map)?;
                self.pc += 1;
            }
            Instruction::IAdd => {
                let a = self.stack.pop()?;
                let b = self.stack.pop()?;
                let res = VMData::new_i64(b.as_i64() + a.as_i64());
                self.stack.push_with_rc(res, &mut self.object_map)?;
                self.pc += 1;
            }
            Instruction::FAdd => {
                let a = self.stack.pop()?;
                let b = self.stack.pop()?;
                let res = VMData::new_f64(b.as_f64() + a.as_f64());
                self.stack.push_with_rc(res, &mut self.object_map)?;
                self.pc += 1;
            }
            Instruction::UIAdd => {
                let a = self.stack.pop()?;
                let b = self.stack.pop()?;
                let res = VMData::new_u64(b.as_u64() + a.as_u64());
                self.stack.push_with_rc(res, &mut self.object_map)?;
                self.pc += 1;
            }
            Instruction::ISub => {
                let a = self.stack.pop()?;
                let b = self.stack.pop()?;
                let res = VMData::new_i64(b.as_i64() - a.as_i64());
                self.stack.push_with_rc(res, &mut self.object_map)?;
                self.pc += 1;
            }
            Instruction::FSub => {
                let a = self.stack.pop()?;
                let b = self.stack.pop()?;
                let res = VMData::new_f64(b.as_f64() - a.as_f64());
                self.stack.push_with_rc(res, &mut self.object_map)?;
                self.pc += 1;
            }
            Instruction::UISub => {
                let a = self.stack.pop()?;
                let b = self.stack.pop()?;
                let res = VMData::new_u64(b.as_u64() - a.as_u64());
                self.stack.push_with_rc(res, &mut self.object_map)?;
                self.pc += 1;
            }
            Instruction::IMod => {
                let a = self.stack.pop()?;
                let b = self.stack.pop()?;
                let res = VMData::new_i64(b.as_i64() % a.as_i64());
                self.stack.push_with_rc(res, &mut self.object_map)?;
                self.pc += 1;
            }
            Instruction::ListLoad => {
                let index = self.stack.pop()?;
                let list_ptr = self.stack.pop()?;
                let raw_list = self.object_map.get(list_ptr.as_object());
                let list = raw_list.list();
                let val = list[index.as_u64() as usize];
                self.stack.push_with_rc(val, &mut self.object_map)?;
                self.pc += 1;
            }
            Instruction::ListStore => {
                let val = self.stack.pop()?;
                let list_ptr = self.stack.pop()?;
                let index = self.stack.pop()?;
                let list = self.object_map.get_mut(list_ptr.as_object()).list_mut();
                list[index.as_u64() as usize] = val;
                self.pc += 1;
            }
            Instruction::NewList => {
                let size = self.stack.pop()?;
                let list = vec![VMData::new_unit(); size.as_u64() as usize];
                let ptr = self.object_map.put(ObjectKind::List(list))?;

                self.stack.push_with_rc(VMData::new_list(ptr), &mut self.object_map)?;
                self.pc += 1;
            }
            Instruction::ExternCall { name, .. } => {
                let consts = HashMap::new();
                let vm_state = runtime::vm_state::VMState::new(
                    &mut self.stack,
                    &mut self.object_map,
                    &consts,
                    self.var_map.last(),
                );
                let extern_fn: &CallBack = self.extern_fn.get::<&str>(&name.as_str()).unwrap();
                let res = extern_fn(vm_state)?;
                self.stack.push_with_rc(res, &mut self.object_map)?;
                self.pc += 1;
            }
            Instruction::DirectCall { pos, args } => {
                let fn_ptr = self.stack[pos];
                let (pc, sp) = (self.pc, self.stack.top - args as usize);
                self.stack_frame.push((pc, sp));
                self.var_map.push();
                self.pc = fn_ptr.as_fn_ptr();
            }
            Instruction::CallFunction { name, args } => {
                let label = self
                    .program
                    .labels
                    .iter()
                    .find(|label| label.name == name)
                    .unwrap();
                let (pc, sp) = (self.pc, self.stack.top - args as usize);
                self.stack_frame.push((pc, sp));
                self.var_map.push();
                self.pc = label.position;
            }
            Instruction::Return => {
                let (pc, sp) = self.stack_frame.pop().unwrap_or_else(|| {
                    eprintln!(
                        "No stack frame to return to {:?} @ {}",
                        self.stack.pop(),
                        self.pc
                    );
                    std::process::exit(1);
                });
                let ret = self.stack.last()?.clone();
                match ret.tag {
                    VMData::TAG_OBJECT | VMData::TAG_LIST | VMData::TAG_STR => {
                        self.object_map.rc_inc(ret.as_object());
                    }
                    _ => {}
                }
                self.stack.truncate(sp, &mut self.object_map);
                self.var_map.pop(&mut self.object_map);
                self.pc = pc + 1;
                self.stack.push(ret)?;
            }
            Instruction::Halt => {
                self.pc = self.program.len();
            }
            _ => unimplemented!("{:?}", instr),
        }
        Ok(())
    }
}
