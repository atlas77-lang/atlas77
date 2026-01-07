use crate::atlas_vm::error::RuntimeError;
use crate::atlas_vm::object::ObjectKind;
use crate::atlas_vm::runtime::CallBack;
use crate::atlas_vm::runtime::vm_state::VMState;
use crate::atlas_vm::vm_data::{VMData, VMTag};

pub const IO_FUNCTIONS: [(&str, CallBack); 4] = [
    ("println", println),
    ("print", print),
    ("input", input),
    ("panic", panic),
];
pub fn println(state: VMState) -> Result<VMData, RuntimeError> {
    let val = state.stack.pop()?;
    match val.tag {
        VMTag::Unit
        | VMTag::Boolean
        | VMTag::UInt64
        | VMTag::Int64
        | VMTag::Float64
        | VMTag::Char => {
            println!("{}", val)
        }
        VMTag::Ref => {
            let val = unsafe { &*val.as_ref() };
            if val.is_object() {
                println!("{}", state.object_map.get(val.as_object())?);
            } else {
                println!("{}", val)
            }
        }
        VMTag::Object => {
            println!("{}", state.object_map.get(val.as_object())?.structure());
            state.object_map.free(val.as_object())?;
        }
        VMTag::String => {
            let obj_kind = state.object_map.get(val.as_object())?;
            if let Some(s) = obj_kind.string() {
                println!("{}", s);
                state.object_map.free(val.as_object())?;
            } else {
                return Err(RuntimeError::InvalidObjectAccess(VMTag::String, obj_kind));
            }
        }
        _ => {
            println!("{}", state.object_map.get(val.as_object())?);
            state.object_map.free(val.as_object())?;
        }
    }
    Ok(VMData::new_unit())
}

pub fn print(state: VMState) -> Result<VMData, RuntimeError> {
    let val = state.stack.pop()?;
    match val.tag {
        VMTag::Unit
        | VMTag::Boolean
        | VMTag::UInt64
        | VMTag::Int64
        | VMTag::Float64
        | VMTag::Char => {
            print!("{}", val)
        }
        VMTag::Ref => {
            let val = unsafe { &*val.as_ref() };
            print!("{}", val)
        }
        VMTag::String => {
            let obj_kind = state.object_map.get(val.as_object())?;
            if let Some(s) = obj_kind.string() {
                print!("{}", s);
                state.object_map.free(val.as_object())?;
            } else {
                return Err(RuntimeError::InvalidObjectAccess(VMTag::String, obj_kind));
            }
        }
        VMTag::Object => {
            print!("{}", state.object_map.get(val.as_object())?.structure());
            state.object_map.free(val.as_object())?;
        }
        _ => {
            print!("{}", state.object_map.get(val.as_object())?);
            state.object_map.free(val.as_object())?;
        }
    }
    Ok(VMData::new_unit())
}

pub fn input(state: VMState) -> Result<VMData, RuntimeError> {
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

pub fn panic(state: VMState) -> Result<VMData, RuntimeError> {
    let val = state.stack.pop()?;
    match val.tag {
        VMTag::Unit
        | VMTag::Boolean
        | VMTag::UInt64
        | VMTag::Int64
        | VMTag::Float64
        | VMTag::Char => {
            println!("{}", val);
            std::process::exit(1);
        }
        VMTag::Ref => {
            let val = unsafe { &*val.as_ref() };
            println!("{}", val);
            std::process::exit(1);
        }
        //For the sake of cleaning up memory, we free the object before exiting
        //It's useless since the program is ending, but it's a good practice
        VMTag::String => {
            let obj_kind = state.object_map.get(val.as_object())?;
            if let Some(s) = obj_kind.string() {
                println!("{}", s);
                state.object_map.free(val.as_object())?;
                std::process::exit(1);
            } else {
                Err(RuntimeError::InvalidObjectAccess(VMTag::String, obj_kind))
            }
        }
        VMTag::Object => {
            println!("{}", state.object_map.get(val.as_object())?.structure());
            state.object_map.free(val.as_object())?;
            std::process::exit(1);
        }
        _ => {
            println!("{}", state.object_map.get(val.as_object())?);
            state.object_map.free(val.as_object())?;
            std::process::exit(1);
        }
    }
}
