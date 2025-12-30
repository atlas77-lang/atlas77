use crate::atlas_vm::error::RuntimeError;
use crate::atlas_vm::object::ObjectKind;
use crate::atlas_vm::runtime::CallBack;
use crate::atlas_vm::runtime::vm_state::VMState;
use crate::atlas_vm::vm_data::VMData;

pub const VECTOR_FUNCTIONS: [(&str, CallBack); 2] = [("len", len), ("slice", slice)];

// len(l: &const [T]) -> uint64
pub fn len(state: VMState) -> Result<VMData, RuntimeError> {
    let list_ref: *mut VMData = state.stack.pop()?.as_ref();
    let raw_list = state
        .object_map
        .get(unsafe { list_ref.as_ref().unwrap().as_object() })?;
    let list = raw_list.list();
    let len = list.len() as u64;
    Ok(VMData::new_u64(len))
}

pub fn slice(state: VMState) -> Result<VMData, RuntimeError> {
    let end = state.stack.pop()?.as_i64();
    let start = state.stack.pop()?.as_i64();
    let list_ptr = state.stack.pop()?.as_object();
    let raw_list = state.object_map.get(list_ptr)?;
    let list = raw_list.list();
    let sliced = list[start as usize..end as usize].to_vec();
    let obj_idx = state.object_map.put(ObjectKind::List(sliced));
    state.object_map.free(list_ptr)?;
    match obj_idx {
        Ok(index) => Ok(VMData::new_list(index)),
        Err(_) => Err(RuntimeError::OutOfMemory),
    }
}
