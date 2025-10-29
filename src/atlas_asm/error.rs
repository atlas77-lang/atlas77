use crate::declare_error_type;
use miette::Diagnostic;
use thiserror::Error;

declare_error_type! {
    #[error("asmbler error: {0}")]
    pub enum ASMError {
        WeirdStuff(WeirdStuffError),
    }
}

pub type ASMResult<T> = Result<T, ASMError>;

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(asm::weird_stuff))]
#[error("An unknown error occurred during assembly")]
pub struct WeirdStuffError {
    pub details: String,
}