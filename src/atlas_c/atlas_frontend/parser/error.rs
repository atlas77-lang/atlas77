use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

use crate::atlas_c::atlas_frontend::lexer::token::Token;
use crate::declare_error_type;

declare_error_type! {
    #[error("Parse error: {0}")]
    pub enum ParseError {
        UnexpectedEndOfFile(UnexpectedEndOfFileError),
        UnexpectedToken(UnexpectedTokenError),
        OnlyOneConstructorAllowed(OnlyOneConstructorAllowedError),
        NoFieldInClass(NoFieldInClassError),
    }
}

pub type ParseResult<T> = Result<T, ParseError>;

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(syntax::no_field_in_class), help("Add fields to the class"))]
#[error("No fields in class")]
pub struct NoFieldInClassError {
    #[label = "no fields in class"]
    pub span: SourceSpan,
    #[source_code]
    pub src: String,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(syntax::only_one_constructor_allowed),
    help("Only one constructor or destructor is allowed for classes")
)]
#[error("Only one constructor or destructor is allowed for classes")]
pub struct OnlyOneConstructorAllowedError {
    #[label = "only one constructor or destructor is allowed for classes"]
    pub span: SourceSpan,
    #[source_code]
    pub src: String,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(syntax::unexpected_end_of_file),
    help("Add more input to form a valid program")
)]
#[error("expected more characters after this")]
pub struct UnexpectedEndOfFileError {
    #[label = "required more input to parse"]
    pub span: SourceSpan,
    #[source_code]
    pub src: String,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(syntax::unexpected_token))]
#[error("Found unexpected token during parsing")]
pub struct UnexpectedTokenError {
    pub token: Token,
    pub expected: crate::atlas_c::atlas_frontend::lexer::TokenVec,
    #[label("was not expecting to find '{token}' in this position, expected one of: {expected}")]
    pub span: SourceSpan,
    #[source_code]
    pub src: String,
}
