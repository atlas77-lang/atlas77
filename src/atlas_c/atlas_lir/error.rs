// For some reason I get unused assignment warnings in this file
#![allow(unused_assignments)]

use miette::{Diagnostic, NamedSource};
use thiserror::Error;

use crate::{atlas_c::utils::Span, declare_error_type};

declare_error_type! {
    #[error("lir_error: {0}")]
    pub enum LirLoweringError {
        UnsupportedHirExpr(UnsupportedHirExprError),
        CurrentFunctionDoesntExist(CurrentFunctionDoesntExistError),
        NoReturnInFunction(NoReturnInFunctionError),
        UnknownType(UnknownTypeError),
        IntrinsicCallShouldHaveBeenHandledEarlier(IntrinsicCallShouldHaveBeenHandledEarlierError),
    }
}

pub type LirResult<T> = Result<T, Box<LirLoweringError>>;

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(lir_lowering::intrinsic_call_should_have_been_handled_earlier),
    help(
        "This intrinsic call should have been handled earlier in the compiler passes. If you see this error, \
        it likely means there's a bug in the compiler itself where an intrinsic call is not being handled correctly."
    )
)]
#[error("Intrinsic call `{name}` should have been handled earlier in the compiler pipeline")]
pub struct IntrinsicCallShouldHaveBeenHandledEarlierError {
    pub name: String,
    #[label = "intrinsic call should have been handled earlier"]
    pub span: Span,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(lir_lowering::unsupported_hir_expr),
    help("Do not mind this error for now."),
    // It's just a warning for now, the Lir lowering pass isn't ready
    severity(warning)
)]
#[error("Unsupported HIR expression for Lir lowering: {expr}")]
pub struct UnsupportedHirExprError {
    #[label = "unsupported HIR expression for Lir lowering"]
    pub span: Span,
    #[source_code]
    pub src: NamedSource<String>,
    pub expr: String,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(lir_lowering::current_function_doesnt_exist),
    help("Ensure that a function is being lowered before creating blocks")
)]
#[error("Current function does not exist when trying to create a new block")]
pub struct CurrentFunctionDoesntExistError;

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(lir_lowering::no_return_in_function),
    help("All non-unit functions must have a return statement on all paths")
)]
#[error("No return statement in function `{name}`")]
pub struct NoReturnInFunctionError {
    pub name: String,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(lir_lowering::unknown_type),
    help(
        "The type may exist but was not materialized from inference yet. Try an explicit type annotation, e.g. `let x: MyType = expr;` (or assign to a typed temporary first)."
    )
)]
// It doesn't really mean the type is unknown, but that it's not managed by the LIR lowering pass yet
#[error("Unknown type: `{ty_name}`")]
pub struct UnknownTypeError {
    pub ty_name: String,
    #[label = "unknown type: `{ty_name}`"]
    pub span: Span,
    #[source_code]
    pub src: NamedSource<String>,
}
