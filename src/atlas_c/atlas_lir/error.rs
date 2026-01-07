use miette::{Diagnostic, NamedSource};
use thiserror::Error;

use crate::{atlas_c::utils::Span, declare_error_type};

declare_error_type! {
    #[error("lir_error: {0}")]
    pub enum LIRLoweringError {
        UnsupportedHirExpr(UnsupportedHirExprError),
        CurrentFunctionDoesntExist(CurrentFunctionDoesntExistError),
    }
}

pub type LIRResult<T> = Result<T, Box<LIRLoweringError>>;

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(lir_lowering::unsupported_hir_expr),
    help("The HIR expression is not supported for lowering to LIR")
)]
#[error("Unsupported HIR expression for LIR lowering")]
pub struct UnsupportedHirExprError {
    #[label = "unsupported HIR expression for LIR lowering"]
    pub span: Span,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(lir_lowering::current_function_doesnt_exist),
    help("Ensure that a function is being lowered before creating blocks")
)]
#[error("Current function does not exist when trying to create a new block")]
pub struct CurrentFunctionDoesntExistError;
