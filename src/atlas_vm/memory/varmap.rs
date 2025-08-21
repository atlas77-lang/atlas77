use crate::atlas_vm::RuntimeResult;
use crate::atlas_vm::memory::object_map::Memory;
use crate::atlas_vm::memory::vm_data::{VMData, VMTag};
use std::collections::HashMap;

#[derive(Debug, Default)]
pub struct VarMap<'run> {
    pub var_map: HashMap<Key<'run>, VMData>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Key<'run> {
    pub scope: usize,
    pub name: &'run str,
}

impl<'run> VarMap<'run> {
    pub fn new() -> Self {
        VarMap {
            var_map: HashMap::new(),
        }
    }
    /// Insert doesn't need to increment the reference count of the value.
    /// Because stack.pop() doesn't decrement the reference count of the value.
    pub fn insert(
        &mut self,
        key: Key<'run>,
        value: VMData,
        mem: &mut Memory,
    ) -> RuntimeResult<VMData> {
        let old_data = self.var_map.insert(key, value);
        if let Some(old_data) = old_data {
            match old_data.tag {
                VMTag::Str | VMTag::List | VMTag::Object => {
                    mem.rc_dec(old_data.as_object())?;
                }
                _ => {}
            }
        }
        Ok(old_data.unwrap_or(VMData::new_unit()))
    }
    pub fn get(&self, key: &Key<'run>) -> Option<&VMData> {
        self.var_map.get(key)
    }
    pub fn last(&self) -> &HashMap<Key<'_>, VMData> {
        &self.var_map
    }
    pub fn clean_scope(&mut self, scope: usize, mem: &mut Memory) -> RuntimeResult<()> {
        let mut to_remove = Vec::new();
        self.var_map.retain(|key, value| {
            if key.scope == scope {
                match value.tag {
                    VMTag::Str | VMTag::List | VMTag::Object => {
                        to_remove.push(value.as_object());
                    }
                    _ => {}
                }
                false
            } else {
                true
            }
        });
        for obj in to_remove {
            mem.rc_dec(obj)?;
        }
        Ok(())
    }
}
