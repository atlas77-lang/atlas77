use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

use crate::atlas_c::atlas_frontend::lexer::TokenVec;
use crate::atlas_c::atlas_frontend::lexer::token::{LexingError, Token};
use crate::declare_error_type;

declare_error_type! {
    #[error("Parse error: {0}")]
    pub enum SyntaxError {
        UnexpectedEndOfFile(UnexpectedEndOfFileError),
        UnexpectedToken(UnexpectedTokenError),
        OnlyOneConstructorAllowed(OnlyOneConstructorAllowedError),
        NoFieldInStruct(NoFieldInStructError),
        InvalidCharacter(InvalidCharacterError),
    }
}

pub type ParseResult<T> = Result<T, SyntaxError>;

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(syntax::no_field_in_class), help("Add fields to the struct"))]
#[error("No fields in struct")]
pub struct NoFieldInStructError {
    #[label = "no fields in struct"]
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
    pub expected: TokenVec,
    #[label("was not expecting to find '{token}' in this position, expected one of: {expected}")]
    pub span: SourceSpan,
    #[source_code]
    pub src: String,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(lexer::invalid_character))]
#[error("invalid stuff {kind:?} found during lexing")]
pub struct InvalidCharacterError {
    #[source_code]
    pub src: String,
    #[label("invalid character found here")]
    pub span: SourceSpan,
    pub kind: LexingError,
}
