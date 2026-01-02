use crate::atlas_vm::error::RuntimeError;
use crate::atlas_vm::runtime::CallBack;
use crate::atlas_vm::runtime::vm_state::VMState;
use crate::atlas_vm::vm_data::VMData;

pub const MEM_FUNCTIONS: [(&str, CallBack); 2] =
    [("memcpy", memcpy), ("delete_from_ref", delete_from_ref)];

pub fn delete_from_ref(state: VMState) -> Result<VMData, RuntimeError> {
    let data_ptr = state.stack.pop()?.as_ref();
    let src_data = unsafe { &*data_ptr };
    if src_data.is_ref() {
        return Err(RuntimeError::CannotDeleteReferenceDirectly);
    }
    if src_data.is_primitive() {
        // For primitive types, no action is needed
        return Ok(VMData::new_unit());
    }
    let obj_idx = src_data.as_object();
    state.object_map.free(obj_idx)?;
    Ok(VMData::new_unit())
}

pub fn memcpy(state: VMState) -> Result<VMData, RuntimeError> {
    let src_ptr = state.stack.pop()?.as_ref();
    // If it's a primitive type or a reference, just return a copy
    let src_data = unsafe { &*src_ptr };
    if src_data.is_primitive() || src_data.is_ref() {
        return Ok(src_data.clone());
    }
    // Otherwise, we need to do a shallow copy of the object
    let obj_idx = src_data.as_object();
    let raw_obj = state.object_map.get(obj_idx)?;
    let new_obj = match raw_obj {
        crate::atlas_vm::object::ObjectKind::List(list) => {
            crate::atlas_vm::object::ObjectKind::List(list.clone())
        }
        crate::atlas_vm::object::ObjectKind::String(s) => {
            crate::atlas_vm::object::ObjectKind::String(s.clone())
        }
        crate::atlas_vm::object::ObjectKind::Structure(fields) => {
            crate::atlas_vm::object::ObjectKind::Structure(fields.clone())
        }
        _ => {
            return Err(RuntimeError::InvalidMemCpySource);
        }
    };
    let new_obj_idx = state.object_map.put(new_obj)?;
    Ok(VMData::new_object(new_obj_idx))
}
