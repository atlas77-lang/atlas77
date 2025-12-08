use crate::atlas_vm::error::RuntimeError;
use crate::atlas_vm::object::ObjectKind;
use crate::atlas_vm::runtime::CallBack;
use crate::atlas_vm::runtime::vm_state::VMState;
use crate::atlas_vm::vm_data::VMData;

pub const LIST_FUNCTIONS: [(&str, CallBack); 2] = [("len", len), ("slice", slice)];

pub fn len(state: VMState) -> Result<VMData, RuntimeError> {
    let val = state.stack.pop()?;
    let list_ptr = val.as_object();
    let raw_list = state.object_map.get(list_ptr)?;
    let list = raw_list.list();
    Ok(VMData::new_i64(list.len() as i64))
}

pub fn slice(state: VMState) -> Result<VMData, RuntimeError> {
    let end = state.stack.pop_with_rc(state.object_map)?.as_i64();
    let start = state.stack.pop_with_rc(state.object_map)?.as_i64();
    let list_ptr = state.stack.pop_with_rc(state.object_map)?.as_object();
    let raw_list = state.object_map.get(list_ptr)?;
    let list = raw_list.list();
    let sliced = list[start as usize..end as usize].to_vec();
    let obj_idx = state.object_map.put(ObjectKind::List(sliced));
    match obj_idx {
        Ok(index) => Ok(VMData::new_list(index)),
        Err(_) => Err(RuntimeError::OutOfMemory),
    }
}
