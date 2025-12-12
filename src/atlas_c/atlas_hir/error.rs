use crate::atlas_c::utils::Span;
use crate::declare_error_type;
use logos::source;
use miette::{Diagnostic, NamedSource};
use std::fmt;
use std::fmt::Formatter;
use thiserror::Error;

/// Handy type alias for all HIR-related errors.
pub type HirResult<T> = Result<T, HirError>;

//todo: Implement my own error type, because miette doesn't let me return just warnings
declare_error_type! {
    #[error("semantic error: {0}")]
    pub enum HirError {
        UnknownFileImport(UnknownFileImportError),
        NotEnoughGenerics(NotEnoughGenericsError),
        NotEnoughArguments(NotEnoughArgumentsError),
        UnknownType(UnknownTypeError),
        BreakOutsideLoop(BreakOutsideLoopError),
        ContinueOutsideLoop(ContinueOutsideLoopError),
        TypeMismatch(TypeMismatchError),
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
        AccessingPrivateStruct(AccessingPrivateStructError),
        IllegalOperation(IllegalOperationError),
        IllegalUnaryOperation(IllegalUnaryOperationError),
        AccessingPrivateFunction(AccessingPrivateFunctionError),
        UnsupportedItem(UnsupportedItemError),
        TryingToAccessFieldOnNonObjectType(TryingToAccessFieldOnNonObjectTypeError),
        NullableTypeRequiresStdLibrary(NullableTypeRequiresStdLibraryError),
        TryingToAccessAMovedValue(TryingToAccessAMovedValueError),
        TryingToAccessADeletedValue(TryingToAccessADeletedValueError),
        CallingNonConstMethodOnConstReference(CallingNonConstMethodOnConstReferenceError),
        TryingToMutateConstReference(TryingToMutateConstReferenceError),
    }
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(sema::trying_to_mutate_const_reference),
    help("consider using a mutable reference instead")
)]
#[error("trying to mutate a const reference")]
pub struct TryingToMutateConstReferenceError {
    #[label = "cannot mutate `{ty}` as it is a const reference"]
    pub span: Span,
    pub ty: String,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(sema::calling_non_const_method_on_const_reference),
    help("consider making the method const, or using a mutable reference")
)]
#[error("calling a non-const method on a const reference")]
pub struct CallingNonConstMethodOnConstReferenceError {
    #[label = "method called on const reference here"]
    pub call_span: Span,
    #[source_code]
    pub src: NamedSource<String>,
    #[source]
    #[diagnostic_source]
    pub origin: CallingNonConstMethodOnConstReferenceOrigin,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic()]
#[error("")]
pub struct CallingNonConstMethodOnConstReferenceOrigin {
    #[label = "method is not marked as const here"]
    pub method_span: Span,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(sema::trying_to_access_a_moved_value),
    help("consider cloning the value before moving it, or using a reference")
)]
#[error("trying to access a moved value")]
pub struct TryingToAccessADeletedValueError {
    #[label = "value was deleted here"]
    pub delete_span: Span,
    #[label = "trying to access deleted value here"]
    pub access_span: Span,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(sema::trying_to_access_a_moved_value),
    help("consider cloning the value before moving it, or using a reference")
)]
#[error("trying to access a moved value")]
pub struct TryingToAccessAMovedValueError {
    #[label = "value was moved here"]
    pub move_span: Span,
    #[label = "trying to access moved value here"]
    pub access_span: Span,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::nullable_type_requires_std_library))]
#[error("nullable types require the standard library")]
pub struct NullableTypeRequiresStdLibraryError {
    #[label = "nullable types require the standard library"]
    pub span: Span,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::trying_to_access_field_on_non_object_type))]
#[error("Trying to access field on non-object type: {ty}")]
pub struct TryingToAccessFieldOnNonObjectTypeError {
    #[label = "trying to access field on non-object type"]
    pub span: Span,
    pub ty: String,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::unsupported_item))]
#[error("{item} isn't supported yet")]
pub struct UnsupportedItemError {
    #[label = "unsupported item"]
    pub span: Span,
    pub item: String,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(sema::not_enough_arguments),
    help("Provide the required number of arguments")
)]
#[error("Not enough arguments provided to {kind}, expected {} but found {found}", origin.expected)]
pub struct NotEnoughArgumentsError {
    //The kind of callable (function, method, constructor, destructor etc.)
    pub kind: String,
    pub found: usize,
    #[label = "only {found} were provided"]
    pub span: Span,
    #[source_code]
    pub src: NamedSource<String>,
    #[source]
    #[diagnostic_source]
    pub origin: NotEnoughArgumentsOrigin,
}
#[derive(Error, Diagnostic, Debug)]
#[error("")]
pub struct NotEnoughArgumentsOrigin {
    pub expected: usize,
    #[label = "function requires {expected} arguments"]
    pub span: Span,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(sema::trying_to_access_private_function),
    help("Mark the function {name} as public")
)]
#[error("{name} is marked as private, so you cannot call it outside of its file.")]
pub struct AccessingPrivateFunctionError {
    pub name: String,
    #[source_code]
    pub src: NamedSource<String>,
    #[label = "trying to call a private function"]
    pub span: Span,
    #[source]
    #[diagnostic_source]
    pub origin: AccessingPrivateFunctionOrigin,
}
#[derive(Error, Diagnostic, Debug)]
#[diagnostic()]
#[error("")]
pub struct AccessingPrivateFunctionOrigin {
    #[label = "You marked it as private"]
    pub span: Span,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(sema::illegal_operation),
    help("ensure that the operation is valid for the given type")
)]
#[error("Incompatible {operation} on {ty}")]
pub struct IllegalUnaryOperationError {
    pub operation: String,
    pub ty: String,
    #[label("Incompatible {operation} on {ty}")]
    pub expr_span: Span,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(sema::illegal_operation),
    help("ensure that the operation is valid for the given types")
)]
#[error("Incompatible {operation} on {ty1} & {ty2}")]
pub struct IllegalOperationError {
    #[source_code]
    pub src: NamedSource<String>,
    pub operation: String,
    pub ty1: String,
    #[label("Incompatible {operation} on {ty1} & {ty2}")]
    pub expr_span: Span,
    pub ty2: String,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(sema::trying_to_access_private_struct),
    help("Mark {name} the struct as public")
)]
#[error(
    "{name} is marked as private, so you cannot accessing it from outside of its declaration file."
)]
pub struct AccessingPrivateStructError {
    pub name: String,
    #[source_code]
    pub src: NamedSource<String>,
    #[label = "trying to access a private struct"]
    pub span: Span,
    #[source]
    #[diagnostic_source]
    pub origin: AccessingPrivateStructOrigin,
}
#[derive(Error, Diagnostic, Debug)]
#[error("")]
pub struct AccessingPrivateStructOrigin {
    #[label = "You marked it as private"]
    pub span: Span,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(sema::no_return_in_function),
    help("Add a return statement at the end of the function")
)]
#[error(
    "a function that is not of type `unit` must end with a return statement. NB: the compiler won't notice if you actually return in a loop. We still don't do Control Flow Graph analysis to check that."
)]
pub struct NoReturnInFunctionError {
    #[label("function {func_name} requires a return statement")]
    pub span: Span,
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
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::cannot_delete_primitive_type))]
#[error("cannot delete a value of primitive type {ty}")]
pub struct CannotDeletePrimitiveTypeError {
    #[label("cannot delete a value of primitive type")]
    pub span: Span,
    #[source_code]
    pub src: NamedSource<String>,
    pub ty: String,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::accessing_private_constructor))]
#[error("Can't access private {kind} outside of its class")]
pub struct AccessingPrivateConstructorError {
    #[label("Trying to access a private {kind}")]
    pub span: Span,
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
    pub span: Span,
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
    pub span: Span,
    pub ty: String,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::not_valid_struct_construction))]
#[error("You cannot construct non-struct types")]
pub struct CanOnlyConstructStructsError {
    #[label = "only struct types can be constructed"]
    pub span: Span,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::unknown_file_import))]
#[error("imported file {file_name} could not be found")]
pub struct UnknownFileImportError {
    pub file_name: String,
    #[label = "could not find import file {file_name}"]
    pub span: Span,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::not_enough_generics))]
#[error(
    "not enough generics provided {ty_name} requires {} generics, but only {found} were provided", origin.expected
)]
pub struct NotEnoughGenericsError {
    pub ty_name: String,
    pub found: usize,
    #[label = "only {found} generics were provided"]
    pub error_span: Span,
    #[source_code]
    pub src: NamedSource<String>,
    #[source]
    #[diagnostic_source]
    pub origin: NotEnoughGenericsOrigin,
}
#[derive(Error, Diagnostic, Debug)]
#[error("")]
pub struct NotEnoughGenericsOrigin {
    pub expected: usize,
    #[label = "{expected} generics were expected"]
    pub declaration_span: Span,
    #[source_code]
    pub src: NamedSource<String>,
}

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
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::non_constant_value))]
#[error("You can't assign a non-constant value to a constant field")]
pub struct NonConstantValueError {
    #[label("Trying to assign a non-constant value to a constant field")]
    pub span: Span,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::self_access_outside_class))]
#[error("Can't access private {kind} `{field_name}` outside of its class")]
pub struct AccessingPrivateFieldError {
    #[label("Trying to access private {kind} `{field_name}` from outside its class")]
    pub span: Span,
    pub kind: FieldKind,
    #[source_code]
    pub src: NamedSource<String>,
    pub field_name: String,
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
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::empty_list_literal))]
#[error("empty list literals are not allowed")]
pub struct EmptyListLiteralError {
    pub span: Span,
    #[source_code]
    pub src: NamedSource<String>,
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
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::trying_to_negate_unsigned))]
#[error("trying to negate an unsigned integer")]
pub struct TryingToNegateUnsignedError {
    #[label = "unsigned integers cannot be negated"]
    pub span: Span,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::unsupported_expr))]
#[error("{expr} isn't supported yet")]
pub struct UnsupportedExpr {
    #[label = "unsupported expr"]
    pub span: Span,
    pub expr: String,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::unsupported_type))]
#[error("{ty} isn't supported yet")]
pub struct UnsupportedTypeError {
    #[label = "unsupported type"]
    pub span: Span,
    pub ty: String,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::unsupported_stmt))]
#[error("{stmt} isn't supported yet")]
pub struct UnsupportedStatement {
    #[label = "unsupported statement"]
    pub span: Span,
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
    pub span: Span,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::break_outside_loop))]
#[error("break statement outside of loop")]
pub struct BreakOutsideLoopError {
    #[label = "there is no enclosing loop"]
    pub span: Span,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::continue_outside_loop))]
#[error("continue statement outside of loop")]
pub struct ContinueOutsideLoopError {
    #[label = "there is no enclosing loop"]
    pub span: Span,
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
