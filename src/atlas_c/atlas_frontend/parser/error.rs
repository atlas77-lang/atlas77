use miette::{Diagnostic, NamedSource};
use thiserror::Error;

use crate::atlas_c::atlas_frontend::lexer::TokenVec;
use crate::atlas_c::atlas_frontend::lexer::token::{LexingError, Token};
use crate::atlas_c::utils::Span;
use crate::declare_error_type;

declare_error_type! {
    #[error("Parse error: {0}")]
    pub enum SyntaxError {
        UnexpectedEndOfFile(UnexpectedEndOfFileError),
        UnexpectedToken(UnexpectedTokenError),
        OnlyOneConstructorAllowed(OnlyOneConstructorAllowedError),
        NoFieldInStruct(NoFieldInStructError),
        InvalidCharacter(InvalidCharacterError),
        DestructorWithParameters(DestructorWithParametersError),
    }
}

pub type ParseResult<T> = Result<T, Box<SyntaxError>>;

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(syntax::destructor_with_parameters), help("Remove the parameters from the destructor"))]
#[error("Destructor cannot have parameters")]
pub struct DestructorWithParametersError {
    #[label = "destructor cannot have parameters"]
    pub span: Span,
    #[source_code]
    pub src: NamedSource<String>,
}


#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(syntax::no_field_in_class), help("Add fields to the struct"))]
#[error("No fields in struct")]
pub struct NoFieldInStructError {
    #[label = "no fields in struct"]
    pub span: Span,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(syntax::only_one_constructor_allowed),
    help(
        "Try removing that constructor/destructor or make it a static method (e.g. `fun init(...) -> Self)`"
    )
)]
#[error("Only one constructor or destructor is allowed per struct")]
//This should also have a label pointing to the 1st constructor/destructor
pub struct OnlyOneConstructorAllowedError {
    #[label = "only one constructor or destructor is allowed per struct"]
    pub span: Span,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(syntax::unexpected_end_of_file),
    help("Add more input to form a valid program")
)]
#[error("expected more characters after this")]
pub struct UnexpectedEndOfFileError {
    #[label = "required more input to parse"]
    pub span: Span,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(syntax::unexpected_token))]
#[error("Found unexpected token during parsing")]
pub struct UnexpectedTokenError {
    pub token: Token,
    pub expected: TokenVec,
    #[label("was not expecting to find '{token}' in this position, expected one of: {expected}")]
    pub span: Span,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(lexer::invalid_character))]
#[error("invalid stuff {kind:?} found during lexing")]
pub struct InvalidCharacterError {
    #[source_code]
    pub src: NamedSource<String>,
    #[label("invalid character found here")]
    pub span: Span,
    pub kind: LexingError,
}
