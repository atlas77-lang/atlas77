use crate::atlas_vm::error::{RuntimeError, RuntimeResult};
use crate::atlas_vm::object::ObjectKind;
use crate::atlas_vm::runtime::CallBack;
use crate::atlas_vm::runtime::vm_state::VMState;
use crate::atlas_vm::vm_data::VMData;

pub const STRING_FUNCTIONS: [(&str, CallBack); 8] = [
    ("str_len", str_len),
    ("trim", trim),
    ("to_upper", to_upper),
    ("to_lower", to_lower),
    ("split", split),
    ("str_cmp", str_cmp),
    ("from_chars", from_chars),
    ("to_chars", to_chars),
];
// Now it takes a string reference, so we need to change accordingly
pub fn str_len(state: VMState) -> RuntimeResult<VMData> {
    let string_ref: *mut VMData = state.stack.pop()?.as_ref();
    let raw_string = state
        .object_map
        .get(unsafe { string_ref.as_ref().unwrap().as_object() })?;
    let string = raw_string.string();
    let len = string.len() as i64;
    Ok(VMData::new_i64(len))
}

// str_cmp(s1: &const string, s2: &const string) -> uint64
pub fn str_cmp(state: VMState) -> RuntimeResult<VMData> {
    let string2_ref: *mut VMData = state.stack.pop()?.as_ref();
    let string1_ref: *mut VMData = state.stack.pop()?.as_ref();

    let raw_string1 = state
        .object_map
        .get(unsafe { string1_ref.as_ref().unwrap().as_object() })?;
    let raw_string2 = state
        .object_map
        .get(unsafe { string2_ref.as_ref().unwrap().as_object() })?;

    let string1 = raw_string1.string();
    let string2 = raw_string2.string();

    let cmp_result = string1.cmp(string2) as i64;

    Ok(VMData::new_i64(cmp_result))
}
// This should just mutate the string in place rather than creating a new one
pub fn trim(state: VMState) -> RuntimeResult<VMData> {
    //Let's just mutate it, no need to create a new string
    let string_data = state.stack.pop()?;
    let string_ptr = string_data.as_object();
    let raw_string = state.object_map.get_mut(string_ptr)?;
    let string = raw_string.string_mut();
    let trimmed = string.trim().to_string();
    *string = trimmed;
    Ok(string_data)
}

// This should just mutate the string in place rather than creating a new one
pub fn to_upper(state: VMState) -> RuntimeResult<VMData> {
    //Let's just mutate it, no need to create a new string
    let string_data = state.stack.pop()?;
    let string_ptr = string_data.as_object();
    let raw_string = state.object_map.get_mut(string_ptr)?;
    let string = raw_string.string_mut();
    let upper = string.to_uppercase();
    *string = upper;
    Ok(string_data)
}

pub fn to_lower(state: VMState) -> RuntimeResult<VMData> {
    //Let's just mutate it, no need to create a new string
    let string_data = state.stack.pop()?;
    let string_ptr = string_data.as_object();
    let raw_string = state.object_map.get_mut(string_ptr)?;
    let string = raw_string.string_mut();
    let lower = string.to_lowercase();
    *string = lower;
    Ok(string_data)
}
// split(s: &string, sep: &string) -> [string]
pub fn split(state: VMState) -> RuntimeResult<VMData> {
    let sep_ref: *mut VMData = state.stack.pop()?.as_ref();
    let string_ref: *mut VMData = state.stack.pop()?.as_ref();

    let raw_string = state
        .object_map
        .get(unsafe { string_ref.as_ref().unwrap().as_object() })?;
    let raw_sep = state
        .object_map
        .get(unsafe { sep_ref.as_ref().unwrap().as_object() })?;

    let string = raw_string.string();
    let sep = raw_sep.string();

    let split_strings: Vec<VMData> = string
        .split(sep)
        .map(|s| {
            let obj_idx = state.object_map.put(ObjectKind::String(s.to_string()));
            match obj_idx {
                Ok(index) => VMData::new_string(index),
                Err(_) => panic!("Out of memory during string split"),
            }
        })
        .collect();

    let list_idx = state.object_map.put(ObjectKind::List(split_strings));
    match list_idx {
        Ok(index) => Ok(VMData::new_list(index)),
        Err(_) => Err(RuntimeError::OutOfMemory),
    }
}

// from_chars(s: [char]) -> string
pub fn from_chars(state: VMState) -> RuntimeResult<VMData> {
    let list_ptr = state.stack.pop()?.as_object();
    let raw_list = state.object_map.get(list_ptr)?;
    let list = raw_list.list().clone();

    let string: String = list.iter().map(|data| data.as_char()).collect();

    let obj_idx = state.object_map.put(ObjectKind::String(string));
    state.object_map.free(list_ptr)?;
    match obj_idx {
        Ok(index) => Ok(VMData::new_string(index)),
        Err(_) => Err(RuntimeError::OutOfMemory),
    }
}

// to_chars(s: &const string) -> [char]
pub fn to_chars(state: VMState) -> RuntimeResult<VMData> {
    let string_ref: *mut VMData = state.stack.pop()?.as_ref();

    let raw_string = state
        .object_map
        .get(unsafe { string_ref.as_ref().unwrap().as_object() })?;
    let string = raw_string.string();

    let char_data: Vec<VMData> = string.chars().map(|c| VMData::new_char(c)).collect();

    let list_idx = state.object_map.put(ObjectKind::List(char_data));
    match list_idx {
        Ok(index) => Ok(VMData::new_list(index)),
        Err(_) => Err(RuntimeError::OutOfMemory),
    }
}
