use std::ops::Index;
use crate::atlas_vm_new::object::ObjectDescriptor;

pub struct Heap<'heap> {
    memory: Vec<Object<'heap>>,
}

pub struct Object<'heap> {
    pub fields: RawObject<'heap>,
    //Maybe should be a static reference

    pub obj_descriptor: &'heap ObjectDescriptor,
}

impl<'heap> Object<'heap> {
    pub fn new(fields: &'heap mut [u8], obj_descriptor: &'heap ObjectDescriptor) -> Self {
        Self {
            fields: RawObject { ptr: fields },
            obj_descriptor,
        }
    }
}

impl<'heap> Index<usize> for Object<'heap> {
    type Output = [u8];
    fn index(&self, index: usize) -> &Self::Output {
        //This needs to be changed
        let mut position = 0;
        for i in 0..index {
            position += self.fields[position][i] as usize;
        }
        let size = self.obj_descriptor.fields[index].size as usize;
        &self.fields[position..position + size]
    }
}

#[repr(C)]
#[derive(Debug)]
pub struct RawObject<'heap> {
    pub ptr: &'heap mut [u8]
}

impl<'heap> Clone for RawObject<'heap> {
    fn clone(&self) -> Self {
        let ptr = Box::leak(Box::new(self.ptr.to_vec()))
            .as_mut_slice();
        RawObject { ptr }
    }
}