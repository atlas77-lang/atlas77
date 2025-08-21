use crate::atlas_vm::CallBack;
use crate::atlas_vm::errors::RuntimeError;
use crate::atlas_vm::memory::object_map::ObjectKind;
use crate::atlas_vm::memory::vm_data::VMData;
use crate::atlas_vm::runtime::vm_state::VMState;

pub const LIST_FUNCTIONS: [(&str, CallBack); 2] = [("len", len), ("slice", slice)];

pub fn len<'lib>(state: VMState) -> Result<VMData, RuntimeError> {
    let list_ptr = state.stack.pop()?.as_object();
    let raw_list = state.object_map.get(list_ptr)?;
    let list = raw_list.list();
    Ok(VMData::new_i64(list.len() as i64))
}

pub fn slice<'lib>(state: VMState) -> Result<VMData, RuntimeError> {
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
