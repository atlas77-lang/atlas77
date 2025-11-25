use crate::declare_error_type;
use miette::{Diagnostic, SourceSpan as Span};
use std::fmt;
use std::fmt::Formatter;
use thiserror::Error;
//todo: Implement my own error type, because miette doesn't let me return just warnings
declare_error_type! {
    #[error("semantic error: {0}")]
    pub enum HirError {
        UnknownType(UnknownTypeError),
        BreakOutsideLoop(BreakOutsideLoopError),
        ContinueOutsideLoop(ContinueOutsideLoopError),
        TypeMismatch(TypeMismatchError),
        FunctionTypeMismatch(FunctionTypeMismatchError),
        UnsupportedStatement(UnsupportedStatement),
        UnsupportedExpr(UnsupportedExpr),
        UnsupportedType(UnsupportedTypeError),
        TryingToNegateUnsigned(TryingToNegateUnsignedError),
        TryingToMutateImmutableVariable(TryingToMutateImmutableVariableError),
        EmptyListLiteral(EmptyListLiteralError),
        AccessingClassFieldOutsideClass(AccessingClassFieldOutsideClassError),
        AccessingPrivateField(AccessingPrivateFieldError),
        NonConstantValue(NonConstantValueError),
        ConstTyToNonConstTy(ConstTyToNonConstTyError),
    }
}

/// Handy type alias for all HIR-related errors.
pub type HirResult<T> = Result<T, HirError>;

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::const_ty_to_non_const_ty))]
#[error("Can't assign a constant type to a non constant type")]
pub struct ConstTyToNonConstTyError {
    #[label("This is of type {const_type} which is a constant type")]
    pub const_val: Span,
    pub const_type: String,
    #[label("This is of type {non_const_type} which is not a constant type")]
    pub non_const_val: Span,
    pub non_const_type: String,
    #[source_code]
    pub src: String,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::non_constant_value))]
#[error("You can't assign a non-constant value to a constant field")]
pub struct NonConstantValueError {
    #[label("Trying to assign a non-constant value to a constant field")]
    pub span: Span,
    #[source_code]
    pub src: String,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::self_access_outside_class))]
#[error("Can't access fields of self outside of a class")]
pub struct AccessingPrivateFieldError {
    #[label("Trying to access a private {kind}")]
    pub span: Span,
    pub kind: FieldKind,
    #[source_code]
    pub src: String,
}

#[derive(Debug)]
pub enum FieldKind {
    Function,
    Field,
    Constant,
}
impl fmt::Display for FieldKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            FieldKind::Function => write!(f, "function"),
            FieldKind::Field => write!(f, "field"),
            FieldKind::Constant => write!(f, "constant"),
        }
    }
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::self_access_outside_class))]
#[error("Can't access fields of self outside of a class")]
pub struct AccessingClassFieldOutsideClassError {
    #[label("Trying to access a class field from `self` while outside of a class")]
    pub span: Span,
    #[source_code]
    pub src: String,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::empty_list_literal))]
#[error("empty list literals are not allowed")]
pub struct EmptyListLiteralError {
    pub span: Span,
    #[source_code]
    pub src: String,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::trying_to_mutate_immutable))]
#[error("trying to mutate an immutable variable")]
pub struct TryingToMutateImmutableVariableError {
    #[label = "{var_name} is immutable, try to use `let` instead"]
    pub const_loc: Span,
    pub var_name: String,
    #[label = "cannot mutate an immutable variable"]
    pub span: Span,
    #[source_code]
    pub src: String,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::trying_to_negate_unsigned))]
#[error("trying to negate an unsigned integer")]
pub struct TryingToNegateUnsignedError {
    #[label = "unsigned integers cannot be negated"]
    pub span: Span,
    #[source_code]
    pub src: String,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::unsupported_expr))]
#[error("{expr} isn't supported yet")]
pub struct UnsupportedExpr {
    #[label = "unsupported expr"]
    pub span: Span,
    pub expr: String,
    #[source_code]
    pub src: String,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::unsupported_type))]
#[error("{ty} isn't supported yet")]
pub struct UnsupportedTypeError {
    #[label = "unsupported type"]
    pub span: Span,
    pub ty: String,
    #[source_code]
    pub src: String,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::unsupported_stmt))]
#[error("{stmt} isn't supported yet")]
pub struct UnsupportedStatement {
    #[label = "unsupported statement"]
    pub span: Span,
    pub stmt: String,
    #[source_code]
    pub src: String,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::unknown_type))]
#[error("{name} is not a known type")]
pub struct UnknownTypeError {
    pub name: String,
    #[label = "could not find type {name}"]
    pub span: Span,
    #[source_code]
    pub src: String,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::break_outside_loop))]
#[error("break statement outside of loop")]
pub struct BreakOutsideLoopError {
    #[label = "there is no enclosing loop"]
    pub span: Span,
    #[source_code]
    pub src: String,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::continue_outside_loop))]
#[error("continue statement outside of loop")]
pub struct ContinueOutsideLoopError {
    #[label = "there is no enclosing loop"]
    pub span: Span,
    #[source_code]
    pub src: String,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::type_mismatch))]
#[error("type mismatch")]
pub struct TypeMismatchError {
    pub actual_type: String,
    pub expected_type: String,
    #[label = "the expression has type {actual_type}"]
    pub actual_loc: Span,
    #[label = "expected type {expected_type}"]
    pub expected_loc: Span,
    #[source_code]
    pub src: String,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::function_type_mismatch))]
#[error("function types do not take the same number of arguments")]
pub struct FunctionTypeMismatchError {
    pub expected_ty: String,
    #[label = "the function has type {expected_ty}"]
    pub span: Span,
    #[source_code]
    pub src: String,
}
