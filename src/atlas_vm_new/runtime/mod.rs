use crate::atlas_vm::memory::object_map::Memory;
use crate::atlas_vm::memory::stack::Stack;
use crate::atlas_vm::runtime::vm_state::VMState;
use crate::atlas_vm::RuntimeResult;
use crate::atlas_vm_new::instruction::Instruction;
use crate::atlas_vm_new::vm_data::VMData;

use std::collections::BTreeMap;

pub type CallBack = fn(VMState) -> RuntimeResult<VMData>;

pub struct AtlasRuntime<'run> {
    pub stack: Stack,
    pub heap: Memory<'run>,
    pub extern_fn: BTreeMap<&'run str, CallBack>,
    /// Program Counter
    pub pc: usize,
}


impl<'run> AtlasRuntime<'run> {
    pub fn execute_instruction(&mut self, instr: Instruction) -> RuntimeResult<()> {
        match instr {
            _ => unimplemented!("{:?}", instr),
        }
    }
}