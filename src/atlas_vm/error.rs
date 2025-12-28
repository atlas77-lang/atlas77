use crate::atlas_vm::vm_data::VMTag;

pub type RuntimeResult<T> = Result<T, RuntimeError>;

#[derive(Debug, Clone)]
pub enum RuntimeError {
    OutOfMemory,
    StackOverflow,
    StackUnderflow,
    NullReference(String),
    DivisionByZero,
    InvalidCast(VMTag, VMTag),
    IndexOutOfBounds,
    InvalidOperation,
    TypeMismatchError,
    EntryPointNotFound(String),
    ExternFunctionNotFound(String),
    FunctionNotFound(usize),
    InvalidConstantPoolPointer(usize),
    HaltEncountered,
    OutOfBoundProgram(usize),
    InvalidObjectAccess(VMTag),
}

impl std::fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use RuntimeError::*;
        match self {
            OutOfMemory => writeln!(f, "No more memory bozo"),
            StackOverflow => writeln!(f, "No more stack bozo"),
            StackUnderflow => writeln!(f, "Too little stack bozo"),
            NullReference(msg) => writeln!(f, "Null Reference error: {}", msg),
            DivisionByZero => writeln!(f, "There are no infinity, you can't divide by zero"),
            InvalidCast(from, to) => writeln!(f, "Invalid cast from {} to {:?}", from, to),
            IndexOutOfBounds => writeln!(f, "Index out of bounds"),
            InvalidOperation => writeln!(f, "Invalid Operation (default error)"),
            TypeMismatchError => writeln!(f, "Incorrect types bozo"),
            EntryPointNotFound(entry_point) => {
                writeln!(f, "Entry point {} not found", entry_point)
            }
            ExternFunctionNotFound(func_name) => {
                writeln!(f, "Extern function {} not found", func_name)
            }
            FunctionNotFound(func_ptr) => {
                writeln!(f, "Function not found at pointer: {}", func_ptr)
            }
            InvalidConstantPoolPointer(ptr) => {
                writeln!(f, "Invalid constant pool pointer: {}", ptr)
            }
            InvalidObjectAccess(tag) => writeln!(f, "Invalid object access: {}", tag),
            HaltEncountered => writeln!(f, "Halt instruction encountered"),
            OutOfBoundProgram(pos) => writeln!(f, "Program counter out of bounds: {}", pos),
        }
    }
}
