// For some reason I get unused assignment warnings in this file
#![allow(unused_assignments)]

use crate::atlas_c::utils::Span;
use crate::declare_error_type;
use miette::{Diagnostic, NamedSource};
use std::fmt;
use std::fmt::Formatter;
use thiserror::Error;

/// Handy type alias for all HIR-related errors.
pub type HirResult<T> = Result<T, HirError>;

//todo: Implement my own error type, because miette doesn't let me return just warnings
declare_error_type! {
    #[error("semantic error: {0}")]
    pub(crate) enum HirError {
        UnknownFileImport(UnknownFileImportError),
        NotEnoughGenerics(NotEnoughGenericsError),
        NotEnoughArguments(NotEnoughArgumentsError),
        UnknownType(UnknownTypeError),
        BreakOutsideLoop(BreakOutsideLoopError),
        ContinueOutsideLoop(ContinueOutsideLoopError),
        TypeMismatch(TypeMismatchError),
        UnsupportedStatement(UnsupportedStatementError),
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
        ThisStructDoesNotHaveACopyConstructor(ThisStructDoesNotHaveACopyConstructorError),
        ThisStructDoesNotHaveAMoveConstructor(ThisStructDoesNotHaveAMoveConstructorError),
        TryingToIndexNonIndexableType(TryingToIndexNonIndexableTypeError),
        UselessError(UselessError),
        InvalidReadOnlyType(InvalidReadOnlyTypeError),
        CannotDeletePrimitiveType(CannotDeletePrimitiveTypeError),
        StructNameCannotBeOneLetter(StructNameCannotBeOneLetterError),
        NoReturnInFunction(NoReturnInFunctionError),
        AccessingPrivateStruct(AccessingPrivateStructError),
        AccessingPrivateUnion(AccessingPrivateUnionError),
        IllegalOperation(IllegalOperationError),
        IllegalUnaryOperation(IllegalUnaryOperationError),
        AccessingPrivateFunction(AccessingPrivateFunctionError),
        UnsupportedItem(UnsupportedItemError),
        TryingToAccessFieldOnNonObjectType(TryingToAccessFieldOnNonObjectTypeError),
        NullableTypeRequiresStdLibrary(NullableTypeRequiresStdLibraryError),
        TryingToAccessAMovedValue(TryingToAccessAMovedValueError),
        TryingToAccessAPotentiallyMovedValue(TryingToAccessAPotentiallyMovedValueError),
        TryingToAccessADeletedValue(TryingToAccessADeletedValueError),
        CannotMoveOutOfLoop(CannotMoveOutOfLoopError),
        CannotDeleteOutOfLoop(CannotDeleteOutOfLoopError),
        CallingNonConstMethodOnConstReference(CallingNonConstMethodOnConstReferenceError),
        CallingConsumingMethodOnMutableReference(CallingConsumingMethodOnMutableReferenceError),
        TryingToMutateConstReference(TryingToMutateConstReferenceError),
        TryingToCreateAnUnionWithMoreThanOneActiveField(TryingToCreateAnUnionWithMoreThanOneActiveFieldError),
        TypeDoesNotImplementRequiredConstraint(TypeDoesNotImplementRequiredConstraintError),
        InvalidSpecialMethodSignature(InvalidSpecialMethodSignatureError),
        ReturningReferenceToLocalVariable(ReturningReferenceToLocalVariableError),
        VariableNameAlreadyDefined(VariableNameAlreadyDefinedError),
        TryingToCopyNonCopyableType(TryingToCopyNonCopyableTypeError),
        DoubleMoveError(DoubleMoveError),
        UnknownIdentifier(UnknownIdentifierError),
        UnknownField(UnknownFieldError),
        UnknownMethod(UnknownMethodError),
        CannotTransferOwnershipInBorrowingMethod(CannotTransferOwnershipInBorrowingMethodError),
        CannotMoveOutOfContainer(CannotMoveOutOfContainerError),
        CannotMoveOutOfReference(CannotMoveOutOfReferenceError),
        TypeIsNotMoveable(TypeIsNotMoveableError),
        RecursiveCopyConstructor(RecursiveCopyConstructorError),
        StdNonCopyableStructCannotHaveCopyConstructor(StdNonCopyableStructCannotHaveCopyConstructorError),
        CopyConstructorParameterMustBeCopyable(CopyConstructorParameterMustBeCopyableError),
        StructCannotHaveAFieldOfItsOwnType(StructCannotHaveAFieldOfItsOwnTypeError),
        UnionMustHaveAtLeastTwoVariant(UnionMustHaveAtLeastTwoVariantError),
        UnionVariantDefinedMultipleTimes(UnionVariantDefinedMultipleTimesError),
        LifetimeDependencyViolation(LifetimeDependencyViolationError),
        ReturningValueWithLocalLifetimeDependency(ReturningValueWithLocalLifetimeDependencyError),
        ConstructorCannotHaveAWhereClause(ConstructorCannotHaveAWhereClauseError),
        MethodConstraintNotSatisfied(MethodConstraintNotSatisfiedError),
        TooManyReferenceLevels(TooManyReferenceLevelsError),
        AssignmentCannotBeAnExpression(AssignmentCannotBeAnExpressionError),
        CannotGenerateADestructorForThisType(CannotGenerateADestructorForThisTypeError),
        CannotImplicitlyCopyNonCopyableValue(CannotImplicitlyCopyNonCopyableValueError),
        CannotMoveFromRvalue(CannotMoveFromRvalueError),
        TypeIsNotCopyable(TypeIsNotCopyableError),
        ListIndexOutOfBounds(ListIndexOutOfBoundsError),
        IncorrectIntrinsicCallArguments(IncorrectIntrinsicCallArgumentsError),
        //T&& cannot become T&
        RvalueReferenceToLvalueReferenceError(RvalueReferenceToLvalueReferenceError),
    }
}

//We need an enum that tells the compiler up to where it could go based on the error gravity
pub enum HirPass {
    SyntaxLowering = 0,
    Monomorphization = 1,
    TypeCheck = 2,
    LifetimeAnalysis = 3,
    ConstantFolding = 4,
    DeadCodeElimination = 5,
}

pub enum HirErrorGravity {
    //The error is not critical, the compiler can go up to a certain pass
    CanGoUpTo(HirPass),
    //The error is not critical, the compiler can finish the current pass but not continue
    CanFinishCurrentPassButNotContinue,
    //The error is critical, the compiler should stop immediately
    Critical,
}

impl HirError {
    pub fn gravity(&self) -> HirErrorGravity {
        match self {
            HirError::UnsupportedExpr(_) => HirErrorGravity::CanFinishCurrentPassButNotContinue,
            HirError::UnsupportedType(_) => HirErrorGravity::CanFinishCurrentPassButNotContinue,
            HirError::UnsupportedStatement(_) => {
                HirErrorGravity::CanFinishCurrentPassButNotContinue
            }
            HirError::UnsupportedItem(_) => HirErrorGravity::CanFinishCurrentPassButNotContinue,
            HirError::UnknownFileImport(_) => HirErrorGravity::CanGoUpTo(HirPass::SyntaxLowering),
            _ => HirErrorGravity::Critical,
        }
    }
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(sema::list_index_out_of_bounds),
    help("ensure the index is within the bounds of the list")
)]
#[error("list index {index} is out of bounds for list of size {size}")]
pub(crate) struct ListIndexOutOfBoundsError {
    #[label = "index {index} is out of bounds for list of size {size}"]
    pub span: Span,
    pub index: usize,
    pub size: usize,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(sema::too_many_reference_levels),
    help("reduce the number of reference levels")
)]
#[error("type has too many reference levels")]
pub(crate) struct TooManyReferenceLevelsError {
    #[label = "type has too many reference levels"]
    pub span: Span,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(sema::returning_reference_to_local_variable),
    help(
        "references to local variables cannot be returned because the variable will be dropped when the function returns"
    )
)]
#[error("cannot return reference to local variable `{var_name}`")]
pub(crate) struct ReturningReferenceToLocalVariableError {
    #[label = "returns a reference to local variable `{var_name}`"]
    pub span: Span,
    pub var_name: String,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(sema::variable_name_already_defined),
    help("consider renaming one of the variables")
)]
#[error("variable name `{name}` is already defined")]
pub(crate) struct VariableNameAlreadyDefinedError {
    pub name: String,
    pub first_definition_span: Span,
    pub second_definition_span: Span,
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(sema::invalid_special_method_signature),
    help("Ensure special methods have the correct signature, try {expected}")
)]
#[error(
    "Invalid special method signature for method '{method_name}': expected {expected} but found {actual}"
)]
pub(crate) struct InvalidSpecialMethodSignatureError {
    #[label = "invalid special method signature"]
    pub span: Span,
    pub expected: String,
    pub actual: String,
    #[source_code]
    pub src: NamedSource<String>,
    pub method_name: String,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(sema::type_does_not_implement_required_constraint),
    help("implement the required constraint for this type")
)]
#[error("type `{ty}` does not implement required constraint `{constraint}`")]
pub(crate) struct TypeDoesNotImplementRequiredConstraintError {
    #[label = "type `{ty}` does not implement required constraint `{constraint}`"]
    pub span: Span,
    pub ty: String,
    pub constraint: String,
    #[source_code]
    pub src: NamedSource<String>,
    #[source]
    #[diagnostic_source]
    pub origin: TypeDoesNotImplementRequiredConstraintOrigin,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic()]
#[error("")]
pub(crate) struct TypeDoesNotImplementRequiredConstraintOrigin {
    #[label = "the constraint is required here"]
    pub span: Span,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::trying_to_create_an_union_with_more_than_one_active_field))]
#[error("trying to create an union with more than one active field")]
pub(crate) struct TryingToCreateAnUnionWithMoreThanOneActiveFieldError {
    #[label = "multiple active fields were provided here"]
    pub span: Span,
    #[source_code]
    pub src: NamedSource<String>,
    #[source]
    #[diagnostic_source]
    pub origin: TryingToCreateAnUnionWithMoreThanOneActiveFieldOrigin,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic()]
#[error("")]
pub(crate) struct TryingToCreateAnUnionWithMoreThanOneActiveFieldOrigin {
    #[label = "unions can only have one active field at a time"]
    pub span: Span,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(sema::trying_to_mutate_const_reference),
    help("consider using a mutable reference instead")
)]
#[error("trying to mutate a const reference")]
pub(crate) struct TryingToMutateConstReferenceError {
    #[label = "cannot mutate `{ty}` as it is a const reference"]
    pub span: Span,
    pub ty: String,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(sema::calling_consuming_method_on_mutable_reference),
    help(
        "consider using an owned value instead (You can use the DeRef operator `*` to get an owned value from a mutable reference)"
    )
)]
#[error("calling a consuming method on a mutable reference")]
pub(crate) struct CallingConsumingMethodOnMutableReferenceError {
    #[label = "method called on mutable reference here"]
    pub call_span: Span,
    #[source_code]
    pub src: NamedSource<String>,
    #[source]
    #[diagnostic_source]
    pub origin: CallingConsumingMethodOnMutableReferenceOrigin,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic()]
#[error("")]
pub(crate) struct CallingConsumingMethodOnMutableReferenceOrigin {
    #[label = "method is marked as consuming here"]
    pub method_span: Span,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(sema::calling_non_const_method_on_const_reference),
    help("consider making the method const, or using a mutable reference")
)]
#[error("calling a non-const method on a const reference")]
pub(crate) struct CallingNonConstMethodOnConstReferenceError {
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
pub(crate) struct CallingNonConstMethodOnConstReferenceOrigin {
    #[label = "method is not marked as const here"]
    pub method_span: Span,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(sema::trying_to_access_a_deleted_value),
    help("consider cloning the value before deleting it, or using a reference")
)]
#[error("trying to access a deleted value")]
pub(crate) struct TryingToAccessADeletedValueError {
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
pub(crate) struct TryingToAccessAMovedValueError {
    #[label = "value was moved here"]
    pub move_span: Span,
    #[label = "trying to access moved value here"]
    pub access_span: Span,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(sema::trying_to_access_a_potentially_moved_value),
    help("consider cloning the value before moving it, or using a reference")
)]
#[error("trying to access a potentially moved value")]
pub(crate) struct TryingToAccessAPotentiallyMovedValueError {
    #[label = "value was potentially moved here"]
    pub move_span: Span,
    #[label = "trying to access potentially moved value here"]
    pub access_span: Span,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::nullable_type_requires_std_library))]
#[error("nullable types require the standard library")]
pub(crate) struct NullableTypeRequiresStdLibraryError {
    #[label = "nullable types require the standard library"]
    pub span: Span,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::trying_to_access_field_on_non_object_type))]
#[error("Trying to access field on non-object type: {ty}")]
pub(crate) struct TryingToAccessFieldOnNonObjectTypeError {
    #[label = "trying to access field on non-object type"]
    pub span: Span,
    pub ty: String,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::unsupported_item))]
#[error("{item} aren't supported yet")]
pub(crate) struct UnsupportedItemError {
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
pub(crate) struct NotEnoughArgumentsError {
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
pub(crate) struct NotEnoughArgumentsOrigin {
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
pub(crate) struct AccessingPrivateFunctionError {
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
pub(crate) struct AccessingPrivateFunctionOrigin {
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
pub(crate) struct IllegalUnaryOperationError {
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
pub(crate) struct IllegalOperationError {
    #[source_code]
    pub src: NamedSource<String>,
    pub operation: String,
    pub ty1: String,
    #[label("Incompatible {operation} on {ty1} & {ty2}")]
    pub expr_span: Span,
    pub ty2: String,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::trying_to_access_private_struct))]
#[error(
    "{name} is marked as private, so you cannot accessing it from outside of its declaration file."
)]
pub(crate) struct AccessingPrivateStructError {
    pub name: String,
    #[source_code]
    pub src: NamedSource<String>,
    #[label = "trying to access a private struct"]
    pub span: Span,
    #[source]
    #[diagnostic_source]
    pub origin: AccessingPrivateObjectOrigin,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::trying_to_access_private_union))]
#[error(
    "{name} is marked as private, so you cannot accessing it from outside of its declaration file."
)]
pub(crate) struct AccessingPrivateUnionError {
    pub name: String,
    #[source_code]
    pub src: NamedSource<String>,
    #[label = "trying to access a private union"]
    pub span: Span,
    #[source]
    #[diagnostic_source]
    pub origin: AccessingPrivateObjectOrigin,
}

#[derive(Error, Diagnostic, Debug)]
#[error("")]
pub(crate) struct AccessingPrivateObjectOrigin {
    #[label = "It's marked as private here"]
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
pub(crate) struct NoReturnInFunctionError {
    #[label("function {func_name} requires a return statement")]
    pub span: Span,
    #[source_code]
    pub src: NamedSource<String>,
    pub func_name: String,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::struct_name_cannot_be_one_letter))]
#[error("Struct names cannot be a single letter.")]
pub(crate) struct StructNameCannotBeOneLetterError {
    #[source_code]
    pub src: NamedSource<String>,
    #[label = "Struct names cannot be a single letter. One letter name is reserved for generic type parameters."]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::cannot_delete_primitive_type))]
#[error("cannot delete a value of primitive type {ty}")]
pub(crate) struct CannotDeletePrimitiveTypeError {
    #[label("cannot delete a value of primitive type")]
    pub span: Span,
    #[source_code]
    pub src: NamedSource<String>,
    pub ty: String,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::accessing_private_constructor))]
#[error("Can't access private {kind} outside of its class")]
pub(crate) struct AccessingPrivateConstructorError {
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
pub(crate) struct InvalidReadOnlyTypeError {
    #[label = "only reference types can be const"]
    pub span: Span,
    #[source_code]
    pub src: NamedSource<String>,
    pub ty: String,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::this_should_not_appear))]
#[error("This is just a useless error that should not appear")]
pub(crate) struct UselessError {
    #[label = "useless error"]
    pub span: Span,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::trying_to_index_non_indexable_type))]
#[error("trying to index a non-indexable type {ty}")]
pub(crate) struct TryingToIndexNonIndexableTypeError {
    #[label = "type {ty} is not indexable"]
    pub span: Span,
    pub ty: String,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::not_valid_struct_construction))]
#[error("You cannot construct non-struct types")]
pub(crate) struct CanOnlyConstructStructsError {
    #[label = "only struct types can be constructed"]
    pub span: Span,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::this_struct_does_not_have_a_copy_constructor))]
#[error(
    "It seems like you are trying to use a copy constructor on a struct that does not have one."
)]
pub(crate) struct ThisStructDoesNotHaveACopyConstructorError {
    #[label = "trying to use copy constructor, but this struct does not have one defined"]
    pub span: Span,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::this_struct_does_not_have_a_move_constructor))]
#[error(
    "It seems like you are trying to use a move constructor on a struct that does not have one."
)]
pub(crate) struct ThisStructDoesNotHaveAMoveConstructorError {
    #[label = "trying to use move constructor, but this struct does not have one defined"]
    pub span: Span,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::unknown_file_import))]
#[error("imported file {file_name} could not be found")]
pub(crate) struct UnknownFileImportError {
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
pub(crate) struct NotEnoughGenericsError {
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
pub(crate) struct NotEnoughGenericsOrigin {
    pub expected: usize,
    #[label = "{expected} generics were expected"]
    pub declaration_span: Span,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::const_ty_to_non_const_ty))]
#[error("Can't assign a constant type to a non constant type")]
pub(crate) struct ConstTyToNonConstTyError {
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
pub(crate) struct NonConstantValueError {
    #[label("Trying to assign a non-constant value to a constant field")]
    pub span: Span,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::self_access_outside_class))]
#[error("Can't access private {kind} `{field_name}` outside of its class")]
pub(crate) struct AccessingPrivateFieldError {
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
pub(crate) struct AccessingClassFieldOutsideClassError {
    #[label("Trying to access a class field from `self` while outside of a class")]
    pub span: Span,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::empty_list_literal))]
#[error("empty list literals are not allowed")]
pub(crate) struct EmptyListLiteralError {
    pub span: Span,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::trying_to_mutate_immutable))]
#[error("trying to mutate an immutable variable")]
pub(crate) struct TryingToMutateImmutableVariableError {
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
pub(crate) struct TryingToNegateUnsignedError {
    #[label = "unsigned integers cannot be negated"]
    pub span: Span,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::unsupported_expr))]
#[error("{expr} isn't supported yet")]
pub(crate) struct UnsupportedExpr {
    #[label = "unsupported expr"]
    pub span: Span,
    pub expr: String,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::unsupported_type))]
#[error("{ty} isn't supported yet")]
pub(crate) struct UnsupportedTypeError {
    #[label = "unsupported type"]
    pub span: Span,
    pub ty: String,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::unsupported_stmt))]
#[error("{stmt} isn't supported yet")]
pub(crate) struct UnsupportedStatementError {
    #[label = "unsupported statement"]
    pub span: Span,
    pub stmt: String,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::unknown_type))]
#[error("{name} is not a known type")]
pub(crate) struct UnknownTypeError {
    pub name: String,
    #[label = "could not find type {name}"]
    pub span: Span,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::break_outside_loop))]
#[error("break statement outside of loop")]
pub(crate) struct BreakOutsideLoopError {
    #[label = "there is no enclosing loop"]
    pub span: Span,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::continue_outside_loop))]
#[error("continue statement outside of loop")]
pub(crate) struct ContinueOutsideLoopError {
    #[label = "there is no enclosing loop"]
    pub span: Span,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::type_mismatch), help("ensure that both types are the same"))]
#[error("type mismatch error, found `{}` but expected `{expected_ty}`", actual.actual_ty)]
pub(crate) struct TypeMismatchError {
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
pub(crate) struct TypeMismatchActual {
    pub actual_ty: String,
    #[label = "found {actual_ty}"]
    pub span: Span,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(sema::trying_to_copy_non_copyable_type),
    help(
        "type `{ty}` does not implement a copy constructor (`_copy` method). Consider moving the value instead, or implement a `_copy` method for the type."
    )
)]
#[error("cannot copy value of type `{ty}` because it does not implement a copy constructor")]
pub(crate) struct TryingToCopyNonCopyableTypeError {
    #[label = "trying to copy this value"]
    pub span: Span,
    pub ty: String,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(sema::double_move),
    help(
        "a value can only be moved once. Consider cloning the value before the first move if you need to use it multiple times."
    )
)]
#[error("value has already been moved")]
pub(crate) struct DoubleMoveError {
    #[label = "value was first moved here"]
    pub first_move_span: Span,
    #[label = "trying to move again here"]
    pub second_move_span: Span,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(sema::unknown_identifier),
    help("check the variable name for typos, or ensure it is declared before use")
)]
#[error("cannot find value `{name}` in this scope")]
pub(crate) struct UnknownIdentifierError {
    pub name: String,
    #[label = "not found in this scope"]
    pub span: Span,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(sema::unknown_field),
    help("check the field name for typos, or ensure the struct has this field")
)]
#[error("no field `{field_name}` on type `{ty_name}`")]
pub(crate) struct UnknownFieldError {
    pub field_name: String,
    pub ty_name: String,
    #[label = "unknown field"]
    pub span: Span,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(sema::unknown_method),
    help("check the method name for typos, or ensure the type has this method")
)]
#[error("no method `{method_name}` found for type `{ty_name}`")]
pub(crate) struct UnknownMethodError {
    pub method_name: String,
    pub ty_name: String,
    #[label = "method not found"]
    pub span: Span,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(sema::method_constraint_not_satisfied),
    help(
        "this member has a where clause constraint that is not satisfied by the concrete type used in this instantiation"
    )
)]
#[error(
    "{member_kind} `{member_name}` is not available on `{ty_name}` because its constraints are not satisfied"
)]
pub(crate) struct MethodConstraintNotSatisfiedError {
    pub member_kind: String,
    pub member_name: String,
    pub ty_name: String,
    #[label = "{member_kind} not available due to unsatisfied constraints"]
    pub span: Span,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(sema::cannot_transfer_ownership_in_borrowing_method),
    help(
        "change the method to use `this` instead of `&this` if it needs to transfer ownership, or copy the value if the type is copyable"
    )
)]
#[error("cannot transfer ownership of `{value_name}` in a borrowing method")]
pub(crate) struct CannotTransferOwnershipInBorrowingMethodError {
    #[label = "this method borrows `this` (uses `&this`), it does not own it"]
    pub method_span: Span,
    #[label = "trying to transfer ownership here"]
    pub transfer_span: Span,
    pub value_name: String,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(sema::cannot_move_out_of_container),
    help(
        "consider returning a reference (`&T` or `&const T`) instead, or implement `_copy` for this type to make it copyable"
    )
)]
#[error("cannot move non-copyable type `{ty_name}` out of container")]
pub(crate) struct CannotMoveOutOfContainerError {
    #[label = "attempting to move `{ty_name}` out of array/container here"]
    pub span: Span,
    pub ty_name: String,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(sema::cannot_move_out_of_reference),
    help(
        "cannot dereference (copy/move out of) a reference to a non-copyable type. \nTo fix this:\n  - Implement a copy constructor for `{ty_name}` if it should be copyable\n  - Or work with the reference directly without dereferencing it"
    )
)]
#[error("cannot dereference non-copyable type `{ty_name}` from a reference")]
pub(crate) struct CannotMoveOutOfReferenceError {
    #[label = "trying to dereference `{ty_name}` here, but it's not copyable"]
    pub span: Span,
    pub ty_name: String,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(sema::cannot_move_value),
    help(
        "type `{ty_name}` is not moveable. Consider implementing a move constructor (`{ty_name}(dying_obj: {ty_name}&&)` constructor) for this type if it should be moveable"
    )
)]
#[error("Trying to move a non-moveable type `{ty_name}`")]
pub(crate) struct TypeIsNotMoveableError {
    #[label = "trying to move a non-moveable type here"]
    pub span: Span,
    pub ty_name: String,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(sema::cannot_move_out_of_loop),
    help(
        "variables cannot be moved inside loops because the loop could iterate multiple times, causing use-after-move. Consider moving the variable before the loop, or restructuring your code"
    )
)]
#[error("cannot move variable `{var_name}` inside loop")]
pub(crate) struct CannotMoveOutOfLoopError {
    #[label = "loop starts here"]
    pub loop_span: Span,
    #[label = "variable `{var_name}` is moved here"]
    pub move_span: Span,
    pub var_name: String,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(sema::cannot_delete_out_of_loop),
    help(
        "variables cannot be deleted inside loops because the loop could iterate multiple times, causing use-after-delete. Consider deleting the variable before the loop, or restructuring your code"
    )
)]
#[error("cannot delete variable `{var_name}` inside loop")]
pub(crate) struct CannotDeleteOutOfLoopError {
    #[label = "loop starts here"]
    pub loop_span: Span,
    #[label = "variable `{var_name}` is deleted here"]
    pub delete_span: Span,
    pub var_name: String,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(sema::recursive_copy_constructor),
    help(
        "a copy constructor cannot copy the same type it's constructing, as this would cause infinite recursion."
    )
)]
#[error("recursive copy detected in the Copy constructor for type `{type_name}`")]
pub(crate) struct RecursiveCopyConstructorError {
    #[label = "copy constructor defined here"]
    pub method_span: Span,
    #[label = "attempting to copy `{type_name}` inside its own copy constructor"]
    pub copy_span: Span,
    pub type_name: String,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(sema::std_non_copyable_struct_cannot_have_copy_constructor),
    help(
        "structs marked as `std::non_copyable` are not allowed to have copy constructors. Remove either the copy constructor or the `std::non_copyable` attribute."
    )
)]
#[error(
    "struct `{struct_name}` is marked as `std::non_copyable` and cannot have a copy constructor"
)]
pub(crate) struct StdNonCopyableStructCannotHaveCopyConstructorError {
    #[label = "copy constructor defined here"]
    pub copy_ctor_span: Span,
    #[label = "`std::non_copyable` flag set here"]
    pub flag_span: Span,
    #[source_code]
    pub src: NamedSource<String>,
    pub(crate) struct_name: String,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(sema::copy_constructor_parameter_must_be_copyable),
    help(
        "a copy constructor copies the fields of the source object. If the type contains non-copyable fields, you cannot implement a copy constructor. Consider implementing a move constructor or restructuring your type."
    )
)]
#[error(
    "copy constructor for `{struct_name}` cannot be used because it contains non-copyable fields"
)]
pub(crate) struct CopyConstructorParameterMustBeCopyableError {
    #[label = "copy constructor defined here"]
    pub copy_ctor_span: Span,
    #[label = "but `{struct_name}` contains fields that are not copyable"]
    pub(crate) struct_span: Span,
    pub(crate) struct_name: String,
    #[source_code]
    pub src: NamedSource<String>,
}
#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(sema::struct_cannot_have_a_field_of_its_own_type),
    help(
        "A struct cannot directly or indirectly contain itself as a field, as this would create infinite size.
This error occurs when:
  - A struct has a field of its own type (direct cycle): `struct A {{ a: A }}`
  - A struct contains another struct that eventually contains the first struct (indirect cycle): `struct A {{ b: B }}` where `struct B {{ a: A }}`

Solutions:
  - Use a reference: `&T` or `&const T` (references are fixed-size pointers)
  - Use an indirection type: `optional<T>` or `expected<T, E>` (these allow null/empty states)
  - Redesign the data structure to avoid the cycle"
    )
)]
#[error("struct `{struct_name}` contains a cyclic reference to itself")]
pub(crate) struct StructCannotHaveAFieldOfItsOwnTypeError {
    pub(crate) struct_name: String,
    #[label = "struct `{struct_name}` defined here"]
    pub(crate) struct_span: Span,
    #[label(collection)]
    pub cycle_path: Vec<miette::LabeledSpan>,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(sema::union_must_have_at_least_two_variant),
    help(
        "A union must have at least two variants to be valid, add a `unit` variant if you need a nullable state."
    )
)]
#[error("{union_name} must have at least two variants")]
pub(crate) struct UnionMustHaveAtLeastTwoVariantError {
    pub union_name: String,
    #[label = "{union_name} must have at least two variants"]
    pub span: Span,
    #[source_code]
    pub src: NamedSource<String>,
}
#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(sema::union_variant_defined_multiple_times),
    help(
        "Each variant in a union must have a unique name. Rename one of the variants to resolve the conflict."
    )
)]
#[error("union `{union_name}` has a variant of type `{variant_ty}` defined multiple times")]
pub(crate) struct UnionVariantDefinedMultipleTimesError {
    pub union_name: String,
    pub variant_ty: String,
    #[label = "first definition of variant of type `{variant_ty}`"]
    pub first_span: Span,
    #[label = "second definition of variant of type `{variant_ty}`"]
    pub second_span: Span,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(sema::lifetime_dependency_violation),
    help(
        "The value `{value_name}` depends on `{origin_name}` which has been deleted or moved. \
        Consider restructuring your code to avoid this lifetime dependency, or ensure `{origin_name}` \
        outlives `{value_name}`."
    )
)]
#[error("`{value_name}`'s lifetime is tied to `{origin_name}`'s lifetime")]
pub(crate) struct LifetimeDependencyViolationError {
    pub value_name: String,
    pub origin_name: String,
    #[label = "`{origin_name}` was deleted/moved here"]
    pub origin_invalidation_span: Span,
    #[label = "but `{value_name}` (which depends on `{origin_name}`) is accessed here"]
    pub access_span: Span,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(sema::returning_value_with_local_lifetime_dependency),
    help(
        "Cannot return `{value_name}` because its lifetime is tied to `{origin_name}`, \
        which is a local variable that will be destroyed when the function returns. \
        Consider passing `{origin_name}` as a parameter, or restructuring your code \
        to avoid this dependency."
    )
)]
#[error("`{value_name}`'s lifetime is tied to local variable `{origin_name}`")]
pub(crate) struct ReturningValueWithLocalLifetimeDependencyError {
    pub value_name: String,
    pub origin_name: String,
    #[label = "`{origin_name}` is declared here as a local variable"]
    pub origin_declaration_span: Span,
    #[label = "cannot return `{value_name}` here because `{origin_name}` will be destroyed"]
    pub return_span: Span,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(sema::constructor_cannot_have_a_where_clause),
    help(
        "constructors cannot have where clauses, maybe you meant to use that clause on the copy constructor?"
    )
)]
#[error("constructors cannot have where clauses, they aren't conditionally defined")]
pub(crate) struct ConstructorCannotHaveAWhereClauseError {
    #[label = "constructors cannot have where clauses"]
    pub span: Span,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(sema::assignment_cannot_be_an_expression),
    help("assignments are statements and do not produce a value")
)]
#[error("assignments cannot be used as expressions")]
pub(crate) struct AssignmentCannotBeAnExpressionError {
    #[label = "assignments cannot be used as expressions"]
    pub span: Span,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(sema::cannot_generate_a_destructor_for_this_type),
    severity(error),
    help(
        "the type has a field that requires a custom destructor, but the type itself does not define one. \
        Consider implementing a destructor for this type to properly clean up its resources."
    )
)]
#[error("cannot automatically generate a destructor for type `{type_name}`")]
pub(crate) struct CannotGenerateADestructorForThisTypeError {
    #[source_code]
    pub src: NamedSource<String>,
    #[label = "field requiring custom destructor is defined here"]
    pub conflicting_field: Span,
    #[label("Type `{type_name}` declared here")]
    pub name_span: Span,
    pub type_name: String,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(sema::cannot_implicitly_copy_non_copyable_value),
    help(
        "the value `{var_name}` of type `{ty_name}` cannot be implicitly copied because \
        `{ty_name}` does not implement a copy constructor. Consider moving the value instead, \
        with `move(&{var_name})`."
    )
)]
#[error("cannot implicitly copy non-copyable value `{var_name}` of type `{ty_name}`")]
pub(crate) struct CannotImplicitlyCopyNonCopyableValueError {
    pub var_name: String,
    pub ty_name: String,
    #[label = "attempting to implicitly copy `{var_name}` here"]
    pub span: Span,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[error("Cannot move from rvalue")]
#[diagnostic(code(atlas::ownership::cannot_move_from_rvalue))]
pub(crate) struct CannotMoveFromRvalueError {
    #[source_code]
    pub src: NamedSource<String>,

    #[label("Cannot move from this expression")]
    pub span: Span,

    #[help]
    pub hint: String,
}

#[derive(Error, Diagnostic, Debug)]
#[error("Type is not copyable")]
#[diagnostic(code(atlas::ownership::type_not_copyable))]
pub(crate) struct TypeIsNotCopyableError {
    #[source_code]
    pub src: NamedSource<String>,

    #[label("Type '{type_name}' doesn't implement std::copyable")]
    pub span: Span,

    pub type_name: String,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(sema::incorrect_intrinsic_call_arguments),
    help("provide the correct number of arguments ({expected}) to the intrinsic function")
)]
#[error("intrinsic function `{name}` expected {expected} arguments, but found {found}")]
pub(crate) struct IncorrectIntrinsicCallArgumentsError {
    pub expected: usize,
    pub found: usize,
    pub name: String,
    #[label = "only {found} were provided"]
    pub span: Span,
    #[source_code]
    pub src: NamedSource<String>,
}

/*
TODO: this should work accross different files
*/
#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(sema::rvalue_reference_assigned_to_lvalue_reference),
    help("an rvalue reference cannot be assigned to an lvalue reference")
)]
#[error("cannot assign rvalue reference to lvalue reference")]
pub(crate) struct RvalueReferenceToLvalueReferenceError {
    #[label = "rvalue reference found here"]
    pub r_val_span: Span,
    #[label = "lvalue reference expected here"]
    pub l_val_span: Span,
    #[source_code]
    pub src: NamedSource<String>,
}
