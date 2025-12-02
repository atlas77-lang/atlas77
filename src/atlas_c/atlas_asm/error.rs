use crate::declare_error_type;
use miette::Diagnostic;
use thiserror::Error;

declare_error_type! {
    #[error("assembler error: {0}")]
    pub enum ASMError {
        UnsupportedInstruction(UnsupportedInstructionError),
    }
}

pub type ASMResult<T> = Result<T, ASMError>;

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(asm::unsupported_instruction),
    help("The instruction is not supported by the assembler yet")
)]
#[error("Unsupported instruction: {details}")]
pub struct UnsupportedInstructionError {
    pub details: String,
}
