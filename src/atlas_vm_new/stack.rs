use crate::atlas_vm_new::vm_data::VMData;

const STACK_SIZE: usize = 1024;

pub struct Stack {
    pub values: [StackFrame; STACK_SIZE],
    pub top: usize,
}

pub struct StackFrame {
    pub previous_pc: usize,
    /// All the variables & arguments in the current stack frame
    /// The first n variables are the arguments
    /// The rest are the local variables
    ///
    /// The size of the Vec is allocated at the beginning of the function
    pub variables: Vec<VMData>,
    pub exec_stack: Vec<VMData>,
}