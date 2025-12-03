use crate::atlas_vm::heap::Heap;
use crate::atlas_vm::stack::Stack;
use std::marker::PhantomData;

pub struct VMState<'state, 'run> {
    pub stack: &'state mut Stack,
    pub object_map: &'state mut Heap,
    ///To keep the `'run` lifetime, it is not used right now
    phantom_data: PhantomData<&'run ()>,
    //pub consts: &'state HashMap<&'run str, VMData>,
}

impl<'state, 'run> VMState<'state, 'run> {
    pub fn new(
        stack: &'state mut Stack,
        object_map: &'state mut Heap,
        //consts: &'state HashMap<&'run str, VMData>,
    ) -> Self {
        Self {
            stack,
            object_map,
            phantom_data: PhantomData,
            //consts,
        }
    }
}
