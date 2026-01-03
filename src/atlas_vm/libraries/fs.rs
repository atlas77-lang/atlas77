use crate::atlas_vm::error::RuntimeError;
use crate::atlas_vm::object::ObjectKind;
use crate::atlas_vm::runtime::CallBack;
use crate::atlas_vm::runtime::vm_state::VMState;
use crate::atlas_vm::vm_data::{VMData, VMTag};

pub const FILE_FUNCTIONS: [(&str, CallBack); 6] = [
    ("read_dir", read_dir),
    ("read_file", read_file),
    ("write_file", write_file),
    ("file_exists", file_exists),
    ("remove_file", remove_file),
    ("close_file", close_file),
];

// We need to manually close files to ensure data integrity
pub fn close_file(state: VMState) -> Result<VMData, RuntimeError> {
    let file_ptr = state.stack.pop()?.as_object();
    state.object_map.free(file_ptr)?;
    // In Rust, files are automatically closed when they go out of scope.
    // However, in this VM context, we can simulate closing a file by removing
    // its reference from the object map if needed.
    Ok(VMData::new_unit())
}

pub fn read_dir(state: VMState) -> Result<VMData, RuntimeError> {
    let path_ptr = state.stack.pop()?.as_object();
    let obj_kind = state.object_map.get(path_ptr)?;
    let path = if let Some(s) = obj_kind.string() {
        s
    } else {
        return Err(RuntimeError::InvalidObjectAccess(VMTag::String, obj_kind));
    };

    let entries = std::fs::read_dir(path).unwrap();
    let mut list = Vec::new();
    for entry in entries {
        let entry = entry.unwrap();
        let path = entry.path();
        let path_str = path.to_str().unwrap().to_string();
        let obj_idx = state.object_map.put(ObjectKind::String(path_str));
        match obj_idx {
            Ok(index) => list.push(VMData::new_string(index)),
            Err(_) => return Err(RuntimeError::OutOfMemory),
        }
    }

    let list_idx = state.object_map.put(ObjectKind::List(list));
    //state.object_map.free(path_ptr)?;
    match list_idx {
        Ok(index) => Ok(VMData::new_list(index)),
        Err(_) => Err(RuntimeError::OutOfMemory),
    }
}

pub fn read_file(state: VMState) -> Result<VMData, RuntimeError> {
    let path_ptr = state.stack.pop()?.as_object();
    let obj_kind = state.object_map.get(path_ptr)?;
    let path = if let Some(s) = obj_kind.string() {
        s
    } else {
        return Err(RuntimeError::InvalidObjectAccess(VMTag::String, obj_kind));
    };

    let content = std::fs::read_to_string(path).unwrap();
    let obj_idx = state.object_map.put(ObjectKind::String(content));

    //state.object_map.free(path_ptr)?;
    match obj_idx {
        Ok(index) => Ok(VMData::new_string(index)),
        Err(_) => Err(RuntimeError::OutOfMemory),
    }
}

pub fn write_file(state: VMState) -> Result<VMData, RuntimeError> {
    let content_ptr = state.stack.pop()?.as_object();
    let path_ptr = state.stack.pop()?.as_object();
    let obj_kind_path = state.object_map.get(path_ptr)?;
    let path = if let Some(s) = obj_kind_path.string() {
        s.clone()
    } else {
        return Err(RuntimeError::InvalidObjectAccess(
            VMTag::String,
            obj_kind_path,
        ));
    };
    let obj_kind_content = state.object_map.get(content_ptr)?;
    let content = if let Some(s) = obj_kind_content.string() {
        s.clone()
    } else {
        return Err(RuntimeError::InvalidObjectAccess(
            VMTag::String,
            obj_kind_content,
        ));
    };

    std::fs::write(path, content).unwrap();

    //state.object_map.free(path_ptr)?;
    //state.object_map.free(content_ptr)?;
    Ok(VMData::new_unit())
}

pub fn file_exists(state: VMState) -> Result<VMData, RuntimeError> {
    let path_ptr = state.stack.pop()?.as_object();
    let obj_kind = state.object_map.get(path_ptr)?;
    let path = if let Some(s) = obj_kind.string() {
        s
    } else {
        return Err(RuntimeError::InvalidObjectAccess(VMTag::String, obj_kind));
    };

    let exists = std::path::Path::new(&path).exists();
    //state.object_map.free(path_ptr)?;
    Ok(VMData::new_boolean(exists))
}

pub fn remove_file(state: VMState) -> Result<VMData, RuntimeError> {
    let path_ptr = state.stack.pop()?.as_object();
    let obj_kind = state.object_map.get(path_ptr)?;
    let path = if let Some(s) = obj_kind.string() {
        s
    } else {
        return Err(RuntimeError::InvalidObjectAccess(VMTag::String, obj_kind));
    };

    std::fs::remove_file(path).unwrap();
    //state.object_map.free(path_ptr)?;
    Ok(VMData::new_unit())
}
