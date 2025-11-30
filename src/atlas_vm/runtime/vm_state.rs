use crate::atlas_vm::heap::Heap;
use crate::atlas_vm::stack::Stack;
use crate::atlas_vm::vm_data::VMData;
use std::collections::HashMap;

pub struct VMState<'state, 'run> {
    pub stack: &'state mut Stack,
    pub object_map: &'state mut Heap<'run>,
    pub consts: &'state HashMap<&'run str, VMData>,
}

impl<'state, 'run> VMState<'state, 'run> {
    pub fn new(
        stack: &'state mut Stack,
        object_map: &'state mut Heap<'run>,
        consts: &'state HashMap<&'run str, VMData>,
    ) -> Self {
        Self {
            stack,
            object_map,
            consts,
        }
    }
}
