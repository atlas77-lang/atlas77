mod vm_state;

use crate::atlas_vm::instruction::Instruction;
use crate::atlas_vm::runtime::vm_state::VMState;
use crate::atlas_vm::vm_data::VMData;

use crate::atlas_vm::error::RuntimeError;
use crate::atlas_vm::heap::Heap;
use crate::atlas_vm::stack::Stack;
use std::collections::BTreeMap;

pub type CallBack = fn(VMState) -> RuntimeResult<VMData>;
pub type RuntimeResult<T> = Result<T, RuntimeError>;

pub struct AtlasRuntime<'run> {
    pub stack: Stack,
    pub heap: Heap<'run>,
    pub extern_fn: BTreeMap<&'run str, CallBack>,
    /// Program Counter
    pub pc: usize,
}

impl<'run> AtlasRuntime<'run> {
    fn get_next_32_bits(&mut self) -> u32 {
        self.pc += 1;
        0
    }
    pub fn execute_instruction(&mut self, instr: Instruction) -> RuntimeResult<()> {
        match instr {
            _ => unimplemented!("{:?}", instr),
        }
    }
}
