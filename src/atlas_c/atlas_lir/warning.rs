// For some reason I get unused assignment warnings in this file
#![allow(unused_assignments)]

use miette::{Diagnostic, NamedSource};
use thiserror::Error;

use crate::{atlas_c::utils::Span, declare_error_type};

declare_error_type! {
    #[error("lir_warning: {0}")]
    pub enum LirLoweringWarning {
        UnknownType(UnknownTypeWarning),
    }
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(lir_lowering::unknown_type),
    help("Ensure that the type is defined before using it"),
    severity(warning)
)]
// It doesn't really mean the type is unknown, but that it's not managed by the LIR lowering pass yet
#[error("Unknown type: `{ty_name}`")]
pub struct UnknownTypeWarning {
    pub ty_name: String,
    #[label = "unknown type: `{ty_name}`"]
    pub span: Span,
    #[source_code]
    pub src: NamedSource<String>,
}
