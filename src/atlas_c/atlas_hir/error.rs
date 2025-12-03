use crate::atlas_c::utils::Span;
use crate::declare_error_type;
use miette::{Diagnostic, NamedSource, SourceSpan};
use std::fmt;
use std::fmt::Formatter;
use thiserror::Error;

//todo: Implement my own error type, because miette doesn't let me return just warnings
declare_error_type! {
    #[error("semantic error: {0}")]
    pub enum HirError {
        UnknownFileImport(UnknownFileImportError),
        NotEnoughGenerics(NotEnoughGenericsError),
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
        AccessingPrivateConstructor(AccessingPrivateConstructorError),
        NonConstantValue(NonConstantValueError),
        ConstTyToNonConstTy(ConstTyToNonConstTyError),
        CanOnlyConstructStructs(CanOnlyConstructStructsError),
        TryingToIndexNonIndexableType(TryingToIndexNonIndexableTypeError),
        UselessError(UselessError),
        InvalidReadOnlyType(InvalidReadOnlyTypeError),
        CannotDeletePrimitiveType(CannotDeletePrimitiveTypeError),
        StructNameCannotBeOneLetter(StructNameCannotBeOneLetterError),
        NoReturnInFunction(NoReturnInFunctionError),
    }
}

/// Handy type alias for all HIR-related errors.
pub type HirResult<T> = Result<T, HirError>;

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(sema::no_return_in_function),
    help("Add a return statement at the end of the function")
)]
#[error("a function that is not of type `unit` must end with a return statement")]
pub struct NoReturnInFunctionError {
    #[label("function {func_name} requires a return statement")]
    pub span: SourceSpan,
    #[source_code]
    pub src: NamedSource<String>,
    pub func_name: String,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::struct_name_cannot_be_one_letter))]
#[error("Struct names cannot be a single letter.")]
pub struct StructNameCannotBeOneLetterError {
    #[source_code]
    pub src: NamedSource<String>,
    #[label = "Struct names cannot be a single letter. One letter name is reserved for generic type parameters."]
    pub span: SourceSpan,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::cannot_delete_primitive_type))]
#[error("cannot delete a value of primitive type {ty}")]
pub struct CannotDeletePrimitiveTypeError {
    #[label("cannot delete a value of primitive type")]
    pub span: SourceSpan,
    #[source_code]
    pub src: NamedSource<String>,
    pub ty: String,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::accessing_private_constructor))]
#[error("Can't access private {kind} outside of its class")]
pub struct AccessingPrivateConstructorError {
    #[label("Trying to access a private {kind}")]
    pub span: SourceSpan,
    #[source_code]
    pub src: NamedSource<String>,
    //Either "constructor" or "destructor"
    pub kind: String,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(sema::invalid_read_only_type),
    help("Try using this instead `const &{ty}`")
)]
#[error("only reference types can be const")]
//A const type can only hold a reference. It doesn't make sense to have a `const T` where T is not a reference.
pub struct InvalidReadOnlyTypeError {
    #[label = "only reference types can be const"]
    pub span: SourceSpan,
    #[source_code]
    pub src: NamedSource<String>,
    pub ty: String,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::this_should_not_appear))]
#[error("This is just a useless error that should not appear")]
pub struct UselessError {}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::trying_to_index_non_indexable_type))]
#[error("trying to index a non-indexable type {ty}")]
pub struct TryingToIndexNonIndexableTypeError {
    #[label = "type {ty} is not indexable"]
    pub span: SourceSpan,
    pub ty: String,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::not_valid_struct_construction))]
#[error("You cannot construct non-struct types")]
pub struct CanOnlyConstructStructsError {
    #[label = "only struct types can be constructed"]
    pub span: SourceSpan,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::unknown_file_import))]
#[error("imported file {file_name} could not be found")]
pub struct UnknownFileImportError {
    pub file_name: String,
    #[label = "could not find import file {file_name}"]
    pub span: SourceSpan,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::not_enough_generics))]
#[error(
    "not enough generics provided {ty_name} requires {expected} generics, but only {found} were provided"
)]
///TODO: Find a way to use 2 #[source_code] for both spans (declaration and error).
/// Because they could be in different files.
///
/// Maybe I could do something like having a Vec<NamedSource> and then have a mapping from span to source?
pub struct NotEnoughGenericsError {
    pub ty_name: String,
    pub expected: usize,
    pub found: usize,
    #[label = "type declared here"]
    pub declaration_span: SourceSpan,
    #[label = "here"]
    pub error_span: SourceSpan,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::const_ty_to_non_const_ty))]
#[error("Can't assign a constant type to a non constant type")]
pub struct ConstTyToNonConstTyError {
    #[label("This is of type {const_type} which is a constant type")]
    pub const_val: SourceSpan,
    pub const_type: String,
    #[label("This is of type {non_const_type} which is not a constant type")]
    pub non_const_val: SourceSpan,
    pub non_const_type: String,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::non_constant_value))]
#[error("You can't assign a non-constant value to a constant field")]
pub struct NonConstantValueError {
    #[label("Trying to assign a non-constant value to a constant field")]
    pub span: SourceSpan,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::self_access_outside_class))]
#[error("Can't access fields of self outside of a class")]
pub struct AccessingPrivateFieldError {
    #[label("Trying to access a private {kind}")]
    pub span: SourceSpan,
    pub kind: FieldKind,
    #[source_code]
    pub src: NamedSource<String>,
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
    pub span: SourceSpan,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::empty_list_literal))]
#[error("empty list literals are not allowed")]
pub struct EmptyListLiteralError {
    pub span: SourceSpan,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::trying_to_mutate_immutable))]
#[error("trying to mutate an immutable variable")]
pub struct TryingToMutateImmutableVariableError {
    #[label = "{var_name} is immutable, try to use `let` instead"]
    pub const_loc: SourceSpan,
    pub var_name: String,
    #[label = "cannot mutate an immutable variable"]
    pub span: SourceSpan,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::trying_to_negate_unsigned))]
#[error("trying to negate an unsigned integer")]
pub struct TryingToNegateUnsignedError {
    #[label = "unsigned integers cannot be negated"]
    pub span: SourceSpan,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::unsupported_expr))]
#[error("{expr} isn't supported yet")]
pub struct UnsupportedExpr {
    #[label = "unsupported expr"]
    pub span: SourceSpan,
    pub expr: String,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::unsupported_type))]
#[error("{ty} isn't supported yet")]
pub struct UnsupportedTypeError {
    #[label = "unsupported type"]
    pub span: SourceSpan,
    pub ty: String,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::unsupported_stmt))]
#[error("{stmt} isn't supported yet")]
pub struct UnsupportedStatement {
    #[label = "unsupported statement"]
    pub span: SourceSpan,
    pub stmt: String,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::unknown_type))]
#[error("{name} is not a known type")]
pub struct UnknownTypeError {
    pub name: String,
    #[label = "could not find type {name}"]
    pub span: SourceSpan,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::break_outside_loop))]
#[error("break statement outside of loop")]
pub struct BreakOutsideLoopError {
    #[label = "there is no enclosing loop"]
    pub span: SourceSpan,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::continue_outside_loop))]
#[error("continue statement outside of loop")]
pub struct ContinueOutsideLoopError {
    #[label = "there is no enclosing loop"]
    pub span: SourceSpan,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::type_mismatch), help("ensure that both types are the same"))]
#[error("type mismatch error, found `{}` but expected `{expected_ty}`", actual.actual_ty)]
pub struct TypeMismatchError {
    #[label("expected {expected_ty}")]
    pub span: Span,
    pub expected_ty: String,
    #[source_code]
    pub src: NamedSource<String>,
    #[source]
    #[diagnostic_source]
    pub actual: TypeMismatchActual,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic()]
#[error("")]
pub struct TypeMismatchActual {
    pub actual_ty: String,
    #[label = "found {actual_ty}"]
    pub span: Span,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::function_type_mismatch))]
#[error("function type mismatch: expected {expected_ty}")]
pub struct FunctionTypeMismatchError {
    pub expected_ty: String,
    #[label = "the function has type {expected_ty}"]
    pub span: SourceSpan,
    #[source_code]
    pub src: NamedSource<String>,
}
