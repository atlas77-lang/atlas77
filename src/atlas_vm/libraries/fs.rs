use crate::atlas_vm::CallBack;
use crate::atlas_vm::errors::RuntimeError;
use crate::atlas_vm::memory::object_map::ObjectKind;
use crate::atlas_vm::memory::vm_data::VMData;
use crate::atlas_vm::runtime::vm_state::VMState;

pub const FILE_FUNCTIONS: [(&str, CallBack); 5] = [
    ("read_dir", read_dir),
    ("read_file", read_file),
    ("write_file", write_file),
    ("file_exists", file_exists),
    ("remove_file", remove_file),
];

pub fn read_dir<'lib>(state: VMState) -> Result<VMData, RuntimeError> {
    let path_ptr = state.stack.pop_with_rc(state.object_map)?.as_object();
    let raw_path = state.object_map.get(path_ptr)?;
    let path = raw_path.string();

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
    match list_idx {
        Ok(index) => Ok(VMData::new_list(index)),
        Err(_) => Err(RuntimeError::OutOfMemory),
    }
}

pub fn read_file<'lib>(state: VMState) -> Result<VMData, RuntimeError> {
    let path_ptr = state.stack.pop_with_rc(state.object_map)?.as_object();
    let raw_path = state.object_map.get(path_ptr)?;
    let path = raw_path.string();

    let content = std::fs::read_to_string(path).unwrap();
    let obj_idx = state.object_map.put(ObjectKind::String(content));
    match obj_idx {
        Ok(index) => Ok(VMData::new_string(index)),
        Err(_) => Err(RuntimeError::OutOfMemory),
    }
}

pub fn write_file<'lib>(state: VMState) -> Result<VMData, RuntimeError> {
    let content_ptr = state.stack.pop_with_rc(state.object_map)?.as_object();
    let path_ptr = state.stack.pop_with_rc(state.object_map)?.as_object();

    let path = state.object_map.get(path_ptr)?.string().clone();
    let raw_content = state.object_map.get(content_ptr)?;
    let content = raw_content.string();

    std::fs::write(path, content).unwrap();
    Ok(VMData::new_unit())
}

pub fn file_exists<'lib>(state: VMState) -> Result<VMData, RuntimeError> {
    let path_ptr = state.stack.pop_with_rc(state.object_map)?.as_object();
    let raw_path = state.object_map.get(path_ptr)?;
    let path = raw_path.string();

    let exists = std::path::Path::new(&path).exists();
    Ok(VMData::new_bool(exists))
}

pub fn remove_file<'lib>(state: VMState) -> Result<VMData, RuntimeError> {
    let path_ptr = state.stack.pop_with_rc(state.object_map)?.as_object();
    let raw_path = state.object_map.get(path_ptr)?;
    let path = raw_path.string();

    std::fs::remove_file(path).unwrap();
    Ok(VMData::new_unit())
}
