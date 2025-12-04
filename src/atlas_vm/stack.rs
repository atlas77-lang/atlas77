use crate::atlas_vm::error::{RuntimeError, RuntimeResult};
use crate::atlas_vm::heap::Heap;
use crate::atlas_vm::vm_data::{VMData, VMTag};
use std::fmt::Display;
use std::ops::{Index, IndexMut};

/// The size of the stack in bytes
///
/// I'll try allocating the stack into the heap later on so
pub const STACK_SIZE: usize = 16 * 16384 / size_of::<VMData>();

/// The stack for the VM
///
/// The stack is a fixed-size array of [VMData] with a maximum size of [STACK_SIZE]
///
/// The stack is used to store values and objects during the execution of the program
///
/// Each time a function is called, a new stack frame is created.
/// A stack frame contains the values of the function's arguments and local variables
/// as well as the temporary values used during the execution of the function.
///
/// A stack frame takes this form:
/// - `[[previous_pc, previous_base_ptr], [arguments], [local_variables], [temporary_values]]`
/// ``arguments`` & ``local variables`` are fixed-size arrays as they are known at compile time.
#[derive(Debug)]
pub struct Stack {
    /// The stack values themselves
    values: [VMData; STACK_SIZE],
    /// The top of the stack (i.e. a pointer to the last value pushed)
    pub top: usize,
    /// A pointer to the base of the current stack
    pub base_ptr: usize,
}


#[derive(Debug)]
pub struct StackFrameInfo {
    pub pc: usize,
    pub base_ptr: usize,
}
impl Default for Stack {
    fn default() -> Self {
        Self::new()
    }
}

/// TODO: this implementation should be overhauled a bit cuz it's kinda clunky
impl Stack {
    pub fn new() -> Self {
        Self {
            values: [VMData::new_unit(); STACK_SIZE],
            top: 0,
            base_ptr: 0,
        }
    }

    /// Create a new stack frame.
    ///
    /// Layout we use at `base_ptr` after this call:
    ///   values[base_ptr]     = previous_pc
    ///   values[base_ptr + 1] = previous_base_ptr
    /// Locals / args start at base_ptr + 2
    pub fn new_stack_frame(&mut self, previous_pc: usize, previous_base: usize) {
        // Ensure we have space for the two frame metadata slots
        let need = 2;
        if self.top + need > STACK_SIZE {
            panic!("stack overflow while creating new frame");
        }

        // Write previous_pc then previous_base at current top
        self.values[self.top] = VMData::new_u64(previous_pc as u64);
        self.values[self.top + 1] = VMData::new_u64(previous_base as u64);

        // Set base_ptr to point to the metadata (previous frame info)
        self.base_ptr = self.top;

        // Advance top to point to the first free slot AFTER the metadata.
        self.top += need;
    }

    /// Returns (pc, bp, ret_val)
    pub fn return_from_stack_frame(&mut self) -> RuntimeResult<(usize, usize, VMData)> {
        // The current base_ptr points to the stored previous_pc at base_ptr
        let pc = self.values[self.base_ptr].as_u64() as usize;
        let bp = self.values[self.base_ptr + 1].as_u64() as usize;

        // Pop the return value (value on top of this frame)
        let ret_val = self.pop()?;

        // Shrink the stack back to the metadata slot of this frame,
        // effectively deallocating locals and temporaries.
        self.top = self.base_ptr;

        // Restore caller's base_ptr
        self.base_ptr = bp;

        Ok((pc, bp, ret_val))
    }
    /// Let you get the program counter of the previous stack frame that
    /// lives at the ``base_ptr`` location of the current stack frame.
    #[inline(always)]
    pub fn get_program_counter(&self) -> usize {
        self.values[self.base_ptr].as_u64() as usize
    }
    /// Let you set the program counter of the previous stack frame
    #[inline(always)]
    pub fn set_program_counter(&mut self, previous_pc: usize) {
        self.values[self.base_ptr] = VMData::new_u64(previous_pc as u64);
    }
    /// Let you get the base_pointer of the previous stack frame
    /// at the ``base_ptr + 1`` location of the current stack frame.
    #[inline(always)]
    pub fn get_base_pointer(&self) -> usize {
        self.values[self.base_ptr + 1].as_u64() as usize
    }
    #[inline(always)]
    pub fn set_base_pointer(&mut self, new_base_ptr: usize) {
        self.values[self.base_ptr + 1] = VMData::new_u64(new_base_ptr as u64);
    }

    /// Lets you get a value from the local space.
    ///
    /// It's used to streamline and standardize the `base_ptr + pos + 2`
    /// to avoid the `previous_program_counter` & `previous_base_ptr`
    /// that are the first 2 variables in the local space.
    #[inline(always)]
    pub fn get_var(&self, pos: usize) -> VMData {
        self.values[self.base_ptr + pos + 2]
    }
    /// Lets you set a value to the local space.
    ///
    /// It's used to streamline and standardize the `base_ptr + pos + 2`
    /// to avoid the `previous_program_counter` & `previous_base_ptr`
    /// that are the first 2 variables in the local space.
    #[inline(always)]
    pub fn set_var(&mut self, pos: usize, data: VMData) {
        self.values[self.base_ptr + pos + 2] = data
    }
    /// Clear the stack (i.e. set the top of the stack to 0)
    ///
    /// NB: It does zero the stack at all.
    pub fn clear(&mut self) {
        self.top = 0;
        self.base_ptr = 0;
    }
    /// Clear the stack (set the top of the stack to 0 & then zero every cell of the stack)
    pub fn clear_and_zero(&mut self) {
        self.top = 0;
        self.base_ptr = 0;
        for i in 0..self.values.len() {
            self.values[i] = VMData::new_unit();
        }
    }

    pub fn push(&mut self, val: VMData) -> Result<(), RuntimeError> {
        if self.top < STACK_SIZE {
            self.values[self.top] = val;
            self.top += 1;
            Ok(())
        } else {
            Self::push_stack_overflow()
        }
    }
    pub fn push_with_rc(&mut self, val: VMData, mem: &mut Heap) -> Result<(), RuntimeError> {
        if self.top < STACK_SIZE {
            self.values[self.top] = val;
            match val.tag {
                VMTag::Str | VMTag::List | VMTag::Object => {
                    mem.rc_inc(val.as_object());
                }
                _ => {}
            }
            self.top += 1;
            Ok(())
        } else {
            Self::push_stack_overflow()
        }
    }

    #[inline(always)]
    pub fn truncate(&mut self, new_top: usize, mem: &mut Heap) -> RuntimeResult<()> {
        for i in new_top..=self.top {
            match self.values[i].tag {
                VMTag::Str | VMTag::List | VMTag::Object => {
                    mem.rc_dec(self.values[i].as_object())?;
                }
                _ => {}
            }
        }
        self.top = new_top;
        Ok(())
    }

    #[inline(always)]
    pub fn pop(&mut self) -> Result<VMData, RuntimeError> {
        self.top = self.top.wrapping_sub(1); // Always decrement

        // If underflow happens, restore `self.top` and return an error.
        if self.top == usize::MAX {
            self.top = 0; // Restore previous state
            return Self::pop_stack_underflow();
        }
        Ok(self.values[self.top])
    }
    pub fn pop_with_rc(&mut self, mem: &mut Heap) -> Result<VMData, RuntimeError> {
        self.top = self.top.wrapping_sub(1); // Always decrement

        if self.top == usize::MAX {
            self.top = 0;
            return Self::pop_stack_underflow();
        }

        let r = self.values[self.top];
        match r.tag {
            VMTag::Str | VMTag::List | VMTag::Object => {
                mem.rc_dec(r.as_object())?;
            }
            _ => {}
        }
        Ok(r)
    }

    #[cold]
    #[inline(never)]
    fn pop_stack_underflow() -> Result<VMData, RuntimeError> {
        Err(RuntimeError::StackUnderflow)
    }

    #[cold]
    #[inline(never)]
    fn push_stack_overflow() -> Result<(), RuntimeError> {
        Err(RuntimeError::StackOverflow)
    }

    #[inline(always)]
    pub fn last(&self) -> Result<&VMData, RuntimeError> {
        if self.top != 0 {
            Ok(&self.values[self.top - 1])
        } else {
            Err(RuntimeError::StackUnderflow)
        }
    }

    pub fn extends(&mut self, values: &[VMData]) -> Result<(), RuntimeError> {
        if self.top + values.len() < STACK_SIZE {
            for val in values {
                self.values[self.top] = *val;
                self.top += 1;
            }
            Ok(())
        } else {
            Err(RuntimeError::StackOverflow)
        }
    }

    pub fn push_object(&mut self, _obj: &[VMData]) -> Result<(), RuntimeError> {
        unimplemented!("push_object(&mut self, obj: &[VMData])")
    }

    pub fn set(&mut self, _offset: usize) {}

    pub fn iter(&self) -> std::slice::Iter<VMData> {
        self.values[..self.top].iter()
    }
}

impl IntoIterator for Stack {
    type Item = VMData;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.values[..self.top].to_vec().into_iter()
    }
}

impl Index<usize> for Stack {
    type Output = VMData;
    fn index(&self, index: usize) -> &Self::Output {
        &self.values[index]
    }
}

impl IndexMut<usize> for Stack {
    fn index_mut(&mut self, index: usize) -> &mut VMData {
        &mut self.values[index]
    }
}

impl Index<StackFrameInfo> for Stack {
    type Output = [VMData];
    fn index(&self, index: StackFrameInfo) -> &Self::Output {
        &self.values[index.base_ptr..index.pc]
    }
}

impl Display for Stack {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Stack: {}", {
            let mut s = "[".to_string();
            for i in 0..self.top {
                s.push_str(&format!("{}, ", self.values[i]))
            }
            s.push(']');
            s
        }, )
    }
}
