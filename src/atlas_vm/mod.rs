pub mod errors;
pub mod libraries;
pub mod memory;
pub mod runtime;

use std::collections::HashMap;

use crate::atlas_vm::memory::stack::StackFrameInfo;
use crate::atlas_vm::memory::vm_data::VMTag;
use crate::atlas_vm::{
    errors::RuntimeError,
    libraries::{
        fs::FILE_FUNCTIONS, io::IO_FUNCTIONS, list::LIST_FUNCTIONS, math::MATH_FUNCTIONS,
        string::STRING_FUNCTIONS, time::TIME_FUNCTIONS,
    },
    memory::{
        object_map::Memory,
        object_map::{Class, ObjectKind},
        stack::Stack,
        vm_data::VMData,
    },
    runtime::{
        arena::RuntimeArena,
        instruction::{Instruction, ProgramDescriptor, Type},
        vm_state::VMState,
    },
};

pub type RuntimeResult<T> = Result<T, RuntimeError>;
pub type CallBack = fn(VMState) -> RuntimeResult<VMData>;

pub struct Atlas77VM<'run> {
    pub program: ProgramDescriptor<'run>,
    pub stack: Stack,
    stack_frame: Vec<StackFrameInfo>, //previous pc and previous stack top
    pub object_map: Memory<'run>,
    pub runtime_arena: RuntimeArena<'run>,
    pub extern_fn: HashMap<&'run str, CallBack>,
    pub pc: usize,
    pub base_ptr: usize,

    pub args: [VMData; 16],
}

impl<'run> Atlas77VM<'run> {
    pub fn new(program: ProgramDescriptor<'run>, runtime_arena: RuntimeArena<'run>) -> Self {
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
            object_map: Memory::new(256),
            runtime_arena,
            extern_fn,
            pc: 0,
            base_ptr: 0,
            args: [VMData::new_unit(); 16],
        }
    }
    pub fn reset(&mut self) {
        self.stack.clear();
        self.stack_frame.clear();
        self.object_map.clear();
        self.pc = 0;
        self.base_ptr = 0;
    }
    pub fn run(&mut self) -> RuntimeResult<VMData> {
        let position = self
            .program
            .functions
            .get(self.program.entry_point.as_str());

        self.stack.extends(
            &self
                .program
                .global
                .function_pool
                .iter()
                .map(|t| VMData::new_fn_ptr(*t))
                .collect::<Vec<_>>(),
        )?;
        if let Some(pos) = position {
            self.pc = *pos;
        } else {
            return Err(RuntimeError::EntryPointNotFound(
                self.program.entry_point.to_string(),
            ));
        }
        while self.pc < self.program.len() {
            println!("Instruction: {:?}", self.program[self.pc]);
            let instr = self.program[self.pc].clone();
            self.execute_instruction(instr.clone())?;
        }
        self.stack.top += 1;
        self.stack.last().cloned()
    }
}
impl<'run> Atlas77VM<'run> {
    /// TODO: Add check for unsigned int
    pub fn execute_instruction(&mut self, instr: Instruction<'run>) -> RuntimeResult<()> {
        match instr {
            Instruction::DeleteObj => {
                let obj = self.stack.pop()?;
                self.object_map.free(obj.as_object())?;
                self.pc += 1;
            }
            Instruction::NewObj { class_descriptor } => {
                let class = &self.program.classes[class_descriptor];
                let nb_fields = class.fields.len();
                let fields = self
                    .runtime_arena
                    .alloc(vec![VMData::new_unit(); nb_fields]);
                let class_ptr = self
                    .object_map
                    .put(ObjectKind::Class(Class::new(class_descriptor, fields)))?;
                self.stack.push(VMData::new_object(class_ptr))?;
                self.pc += 1;
            }
            Instruction::GetField { field } => {
                let obj = self.stack.pop()?;
                let obj_ptr = obj.as_object();
                let raw_obj = self.object_map.get(obj_ptr)?;
                let class = raw_obj.class();
                let field = class[field];
                self.stack.push_with_rc(field, &mut self.object_map)?;
                self.pc += 1;
            }
            Instruction::SetField { field } => {
                let val = self.stack.pop()?;
                match val.tag {
                    VMTag::Str | VMTag::List | VMTag::Object => {
                        self.object_map.rc_inc(val.as_object());
                    }
                    _ => {}
                }
                let obj = self.stack.pop()?;
                let obj_ptr = obj.as_object();
                let raw_obj = self.object_map.get_mut(obj_ptr)?;
                let class = raw_obj.class_mut();
                class[field] = val;
                self.pc += 1;
            }
            Instruction::PushInt(i) => {
                let val = VMData::new_i64(i);
                self.stack.push(val)?;
                self.pc += 1;
            }
            Instruction::PushFloat(f) => {
                let val = VMData::new_f64(f);
                self.stack.push(val)?;
                self.pc += 1;
            }
            Instruction::PushUnsignedInt(u) => {
                let val = VMData::new_u64(u);
                self.stack.push(val)?;
                self.pc += 1;
            }
            Instruction::PushChar(c) => {
                let val = VMData::new_char(c);
                self.stack.push(val)?;
                self.pc += 1;
            }
            Instruction::PushUnit => {
                let val = VMData::new_unit();
                self.stack.push(val)?;
                self.pc += 1;
            }
            Instruction::PushNull => {
                let val = VMData::new_none();
                self.stack.push(val)?;
                self.pc += 1;
            }
            Instruction::PushBool(b) => {
                let val = VMData::new_bool(b);
                self.stack.push(val)?;
                self.pc += 1;
            }
            Instruction::PushStr(u) => {
                let string = self.program.global.string_pool[u];
                let ptr = match self
                    .object_map
                    .put(ObjectKind::String(String::from(string)))
                {
                    Ok(ptr) => ptr,
                    Err(_) => return Err(RuntimeError::OutOfMemory),
                };
                self.stack.push(VMData::new_string(ptr))?;
                self.pc += 1;
            }
            Instruction::Lt => {
                let a = self.stack.pop()?;
                let b = self.stack.pop()?;
                let res = VMData::new_bool(b.as_i64() < a.as_i64());
                self.stack.push(res)?;
                self.pc += 1;
            }
            Instruction::Lte => {
                let a = self.stack.pop()?;
                let b = self.stack.pop()?;
                let res = VMData::new_bool(b.as_i64() <= a.as_i64());
                self.stack.push(res)?;
                self.pc += 1;
            }
            Instruction::Gt => {
                let a = self.stack.pop()?;
                let b = self.stack.pop()?;
                let res = VMData::new_bool(b.as_i64() > a.as_i64());
                self.stack.push(res)?;
                self.pc += 1;
            }
            Instruction::Gte => {
                let a = self.stack.pop()?;
                let b = self.stack.pop()?;
                let res = VMData::new_bool(b.as_i64() >= a.as_i64());
                self.stack.push(res)?;
                self.pc += 1;
            }
            Instruction::Eq => {
                let a = self.stack.pop()?;
                let b = self.stack.pop()?;
                let res = VMData::new_bool(b.as_i64() == a.as_i64());
                self.stack.push(res)?;
                self.pc += 1;
            }
            Instruction::Neq => {
                let a = self.stack.pop()?;
                let b = self.stack.pop()?;
                let res = VMData::new_bool(b.as_i64() != a.as_i64());
                self.stack.push(res)?;
                self.pc += 1;
            }
            Instruction::JmpZ { pos } => {
                let cond = self.stack.pop()?;
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
                            Ok(ptr) => ptr,
                            Err(_) => return Err(RuntimeError::OutOfMemory),
                        };
                        VMData::new_string(ptr)
                    }
                    Type::Char => match val.tag {
                        VMTag::Str => {
                            let raw_string = self.object_map.get(val.as_object())?;
                            let string = raw_string.string();
                            if string.len() != 1 {
                                return Err(RuntimeError::InvalidCast(val.tag, Type::Char));
                            }
                            VMData::new_char(string.chars().next().unwrap())
                        }
                        _ => VMData::new_char(val.as_char()),
                    },
                    Type::Boolean => match val.tag {
                        VMTag::Str => {
                            let raw_string = self.object_map.get(val.as_object())?;
                            let string = raw_string.string();
                            VMData::new_bool(string.parse::<bool>().unwrap())
                        }
                        _ => VMData::new_bool(val.as_bool()),
                    },
                    Type::Float => match val.tag {
                        VMTag::Str => {
                            let raw_string = self.object_map.get(val.as_object())?;
                            let string = raw_string.string();
                            VMData::new_f64(string.parse::<f64>().unwrap())
                        }
                        VMTag::UInt64 => VMData::new_f64(val.as_u64() as f64),
                        VMTag::Int64 => VMData::new_f64(val.as_i64() as f64),
                        VMTag::Bool => VMData::new_f64(val.as_bool() as i64 as f64),
                        VMTag::Char => VMData::new_f64(val.as_char() as i64 as f64),
                        _ => unreachable!("Invalid cast to float"),
                    },
                    Type::Integer => match val.tag {
                        VMTag::Str => {
                            let raw_string = self.object_map.get(val.as_object())?;
                            let string = raw_string.string();
                            VMData::new_i64(string.parse::<i64>().unwrap())
                        }
                        VMTag::UInt64 => VMData::new_i64(val.as_u64() as i64),
                        VMTag::Float64 => VMData::new_i64(val.as_f64() as i64),
                        VMTag::Bool => VMData::new_i64(val.as_bool() as i64),
                        VMTag::Char => VMData::new_i64(val.as_char() as i64),
                        _ => unreachable!("Invalid cast to integer"),
                    },
                    Type::UnsignedInteger => match val.tag {
                        VMTag::Str => {
                            let raw_string = self.object_map.get(val.as_object())?;
                            let string = raw_string.string();
                            VMData::new_u64(string.parse::<u64>().unwrap())
                        }
                        VMTag::Int64 => VMData::new_u64(val.as_i64() as u64),
                        VMTag::Float64 => VMData::new_u64(val.as_f64() as u64),
                        VMTag::Bool => VMData::new_u64(val.as_bool() as u64),
                        VMTag::Char => VMData::new_u64(val.as_char() as u64),
                        _ => unreachable!("Invalid cast to unsigned integer"),
                    },
                };
                self.stack.push(res)?;
                self.pc += 1;
            }
            Instruction::Jmp { pos } => {
                self.pc = (self.pc as isize + pos) as usize;
            }
            Instruction::StoreVar(pos) => {
                println!("Base ptr: {}", self.base_ptr);
                let val = self.stack.pop()?;
                println!("Before: {}", self.stack[self.base_ptr + pos]);
                self.stack[self.base_ptr + pos] = val;
                println!("After: {}", self.stack[self.base_ptr + pos]);
                self.pc += 1;
                println!("{}", self.stack);
            }
            Instruction::LoadVar(pos) => {
                let val = self.stack[self.base_ptr + pos];
                self.stack.push_with_rc(val, &mut self.object_map)?;
                self.pc += 1;
                println!("{}", self.stack);
            }
            Instruction::Pop => {
                self.stack.pop_with_rc(&mut self.object_map)?;
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
            Instruction::Mul => {
                let a = self.stack.pop()?;
                let b = self.stack.pop()?;
                self.stack.push(a * b)?;
                self.pc += 1;
            }
            Instruction::Div => {
                let a = self.stack.pop()?;
                if a.is_zero() {
                    return Err(RuntimeError::DivisionByZero);
                }
                let b = self.stack.pop()?;
                self.stack.push(b / a)?;
                self.pc += 1;
            }
            Instruction::Add => {
                let a = self.stack.pop()?;
                let b = self.stack.pop()?;
                self.stack.push(b + a)?;
                self.pc += 1;
            }
            Instruction::Sub => {
                let a = self.stack.pop()?;
                let b = self.stack.pop()?;
                self.stack.push(b - a)?;
                self.pc += 1;
            }
            Instruction::Mod => {
                let a = self.stack.pop()?;
                let b = self.stack.pop()?;
                self.stack.push(b % a)?;
                self.pc += 1;
            }
            Instruction::StringLoad => {
                let index = self.stack.pop()?;
                let str_ptr = self.stack.pop()?;
                let raw_string = self.object_map.get(str_ptr.as_object())?;
                let string = raw_string.string();
                let val = string.chars().nth(index.as_u64() as usize).unwrap();
                self.stack.push(VMData::new_char(val))?;
                self.pc += 1;
            }
            Instruction::ListLoad => {
                let index = self.stack.pop()?;
                let list_ptr = self.stack.pop()?;
                let raw_list = self.object_map.get(list_ptr.as_object())?;
                let list = raw_list.list();
                let val = list[index.as_u64() as usize];
                self.stack.push_with_rc(val, &mut self.object_map)?;
                self.pc += 1;
            }
            Instruction::StringStore => {
                let val = self.stack.pop()?.as_char();
                let str_ptr = self.stack.pop()?;
                let index = self.stack.pop()?;
                let string = self.object_map.get_mut(str_ptr.as_object())?.string_mut();
                string.remove(index.as_u64() as usize);
                string.insert(index.as_u64() as usize, val);
                self.pc += 1;
            }
            Instruction::ListStore => {
                let val = self.stack.pop()?;
                let list_ptr = self.stack.pop()?;
                let index = self.stack.pop()?;
                let list = self.object_map.get_mut(list_ptr.as_object())?.list_mut();
                list[index.as_u64() as usize] = val;
                self.pc += 1;
            }
            Instruction::NewList => {
                let size = self.stack.pop()?;
                let list = vec![VMData::new_unit(); size.as_u64() as usize];
                let ptr = self.object_map.put(ObjectKind::List(list))?;

                self.stack.push(VMData::new_list(ptr))?;
                self.pc += 1;
            }
            Instruction::ExternCall { function_name, .. } => {
                let consts = HashMap::new();
                let vm_state = VMState::new(
                    &mut self.stack,
                    &mut self.object_map,
                    &consts,
                    &mut self.runtime_arena,
                );
                let extern_fn = self.extern_fn.get::<&str>(&function_name).unwrap();
                let res = extern_fn(vm_state)?;
                self.stack.push_with_rc(res, &mut self.object_map)?;
                self.pc += 1;
            }
            Instruction::LocalSpace { nb_vars } => {
                self.stack.top += nb_vars as usize;
                self.pc += 1;
            }
            Instruction::LoadArg { index: nb_arg } => {
                let val = self.args[nb_arg as usize];
                eprintln!("Loading arg: {:?}", val);
                self.stack[self.base_ptr + nb_arg as usize] = val;
                self.pc += 1;
            }
            Instruction::FunctionCall {
                function_name,
                nb_args,
            } => {
                let position = *self.program.functions.get(function_name).unwrap();
                for arg in (0..nb_args).rev() {
                    self.args[arg as usize] = self.stack.pop()?;
                }
                let stack_frame = StackFrameInfo {
                    pc: self.pc,
                    base_ptr: self.stack.top,
                };
                self.stack_frame.push(stack_frame);
                self.pc = position;
                self.base_ptr = self.stack.top;
            }
            Instruction::Call { nb_args } => {
                let fn_ptr = self.stack.pop()?;
                let fn_ptr = fn_ptr.as_fn_ptr();
                let base_ptr = self.stack.top - nb_args as usize;
                let stack_frame = StackFrameInfo {
                    pc: self.pc,
                    base_ptr,
                };
                self.stack_frame.push(stack_frame);
                self.base_ptr = base_ptr;
                self.pc = fn_ptr;
            }
            Instruction::Return => {
                let previous_stack_frame = self.stack_frame.pop().unwrap_or_else(|| {
                    eprintln!(
                        "No stack frame to return from {:?} @ {}",
                        self.stack.last(),
                        self.pc
                    );
                    std::process::exit(1);
                });
                let ret = *self.stack.last()?;
                println!("Returning {:?}", ret);
                match ret.tag {
                    VMTag::Str | VMTag::List => {
                        self.object_map.rc_inc(ret.as_object());
                        self.object_map.rc_inc(ret.as_object());
                    }
                    VMTag::Object => {
                        self.object_map.rc_inc(ret.as_object());
                    }
                    _ => {}
                }
                self.stack
                    .truncate(previous_stack_frame.base_ptr, &mut self.object_map)?;
                self.pc = previous_stack_frame.pc + 1;
                self.base_ptr = self.stack.top;
                self.stack.push(ret)?;
                println!("{}", self.stack);
            }
            Instruction::Halt => {
                self.pc = self.program.len();
            }
            _ => unimplemented!("{:?}", instr),
        }
        Ok(())
    }
}
