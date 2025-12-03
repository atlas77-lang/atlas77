use crate::atlas_vm::error::RuntimeError;
use crate::atlas_vm::object::ObjectKind;
use crate::atlas_vm::runtime::vm_state::VMState;
use crate::atlas_vm::runtime::CallBack;
use crate::atlas_vm::vm_data::{VMData, VMTag};

pub const IO_FUNCTIONS: [(&str, CallBack); 4] = [
    ("println", println),
    ("print", print),
    ("input", input),
    ("panic", panic),
];
pub fn println<'lib>(state: VMState) -> Result<VMData, RuntimeError> {
    let val = state.stack.pop()?;
    match val.tag {
        VMTag::Unit
        | VMTag::Bool
        | VMTag::UInt64
        | VMTag::Int64
        | VMTag::Float64
        | VMTag::Char
        | VMTag::Null => {
            println!("{}", val)
        }
        VMTag::Str => {
            println!("{}", state.object_map.get(val.as_object())?.string())
        }
        _ => {
            println!("{}", state.object_map.get(val.as_object())?)
        }
    }
    Ok(VMData::new_unit())
}

pub fn print<'lib>(state: VMState) -> Result<VMData, RuntimeError> {
    let val = state.stack.pop()?;
    match val.tag {
        VMTag::Unit
        | VMTag::Bool
        | VMTag::UInt64
        | VMTag::Int64
        | VMTag::Float64
        | VMTag::Char
        | VMTag::Null => {
            print!("{}", val)
        }
        VMTag::Str => {
            print!("{}", state.object_map.get(val.as_object())?.string())
        }
        _ => {
            print!("{}", state.object_map.get(val.as_object())?)
        }
    }
    Ok(VMData::new_unit())
}

pub fn input<'lib>(state: VMState) -> Result<VMData, RuntimeError> {
    let mut input = String::new();
    std::io::stdin().read_line(&mut input).unwrap();
    let obj_index = state
        .object_map
        .put(ObjectKind::String(input.trim().to_string()));
    match obj_index {
        Ok(index) => Ok(VMData::new_string(index)),
        Err(_) => Err(RuntimeError::OutOfMemory),
    }
}

pub fn panic<'lib>(state: VMState) -> Result<VMData, RuntimeError> {
    let val = state.stack.pop()?;
    match val.tag {
        VMTag::Unit
        | VMTag::Bool
        | VMTag::UInt64
        | VMTag::Int64
        | VMTag::Float64
        | VMTag::Char
        | VMTag::Null => {
            println!("{}", val);
            std::process::exit(1);
        }
        VMTag::Str => {
            println!("{}", state.object_map.get(val.as_object())?.string());
            std::process::exit(1);
        }
        _ => {
            println!("{}", state.object_map.get(val.as_object())?);
            std::process::exit(1);
        }
    }
}
