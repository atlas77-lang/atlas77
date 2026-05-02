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
    pub enum HirError {
        InvalidListSize(InvalidListSizeError),
        NonConstantListSize(NonConstantListSizeError),
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
        AccessingPrivateDestructor(AccessingPrivateDestructorError),
        NonConstantValue(NonConstantValueError),
        ConstTyToNonConstTy(ConstTyToNonConstTyError),
        CanOnlyConstructStructs(CanOnlyConstructStructsError),
        TryingToIndexNonIndexableType(TryingToIndexNonIndexableTypeError),
        UselessError(UselessError),
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
        TryingToAccessAMovedValue(TryingToAccessAMovedValueError),
        TryingToAccessAConsumedValue(TryingToAccessAConsumedValueError),
        TryingToAccessAPotentiallyMovedValue(TryingToAccessAPotentiallyMovedValueError),
        TryingToAccessAPotentiallyDeletedValue(TryingToAccessAPotentiallyDeletedValueError),
        TryingToAccessAPotentiallyConsumedValue(TryingToAccessAPotentiallyConsumedValueError),
        TryingToAccessADeletedValue(TryingToAccessADeletedValueError),
        CannotMoveOutOfLoop(CannotMoveOutOfLoopError),
        CannotDeleteOutOfLoop(CannotDeleteOutOfLoopError),
        CallingNonConstMethodOnConstReference(CallingNonConstMethodOnConstReferenceError),
        CallingConsumingMethodOnMutableReference(CallingConsumingMethodOnMutableReferenceError),
        TryingToMutateConstPointer(TryingToMutateConstPointerError),
        TryingToCreateAnUnionWithMoreThanOneActiveField(TryingToCreateAnUnionWithMoreThanOneActiveFieldError),
        TypeDoesNotImplementRequiredConstraint(TypeDoesNotImplementRequiredConstraintError),
        InvalidSpecialMethodSignature(InvalidSpecialMethodSignatureError),
        ReturningReferenceToLocalVariable(ReturningPointerToLocalVariableError),
        VariableNameAlreadyDefined(VariableNameAlreadyDefinedError),
        DoubleMoveError(DoubleMoveError),
        UnknownIdentifier(UnknownIdentifierError),
        UnknownField(UnknownFieldError),
        UnknownMethod(UnknownMethodError),
        StructCannotHaveAFieldOfItsOwnType(StructCannotHaveAFieldOfItsOwnTypeError),
        UnionMustHaveAtLeastTwoVariant(UnionMustHaveAtLeastTwoVariantError),
        UnionVariantDefinedMultipleTimes(UnionVariantDefinedMultipleTimesError),
        LifetimeDependencyViolation(LifetimeDependencyViolationError),
        ReturningValueWithLocalLifetimeDependency(ReturningValueWithLocalLifetimeDependencyError),
        MethodConstraintNotSatisfied(MethodConstraintNotSatisfiedError),
        AssignmentCannotBeAnExpression(AssignmentCannotBeAnExpressionError),
        CannotGenerateADestructorForThisType(CannotGenerateADestructorForThisTypeError),
        CannotMoveFromRvalue(CannotMoveFromRvalueError),
        TypeIsNotCopyable(TypeIsNotCopyableError),
        TypeIsNotTriviallyCopyable(TypeIsNotTriviallyCopyableError),
        OwnershipAnalysisFailed(OwnershipAnalysisFailedError),
        TypeCheckFailed(TypeCheckFailedError),
        SemanticAnalysisFailed(SemanticAnalysisFailedError),
        ListIndexOutOfBounds(ListIndexOutOfBoundsError),
        IncorrectIntrinsicCallArguments(IncorrectIntrinsicCallArgumentsError),
        CannotAccessFieldOfPointers(CannotAccessFieldOfPointersError),
        ReservedVariableName(ReservedVariableNameError),
        // Operators
        UnknownOverloadableOperator(UnknownOverloadableOperatorError),
        OperatorIsNotImplementedForThisType(OperatorIsNotImplementedForThisTypeError),
        OperatorMustUseConstThisModifier(OperatorMustUseConstThisModifierError),
        OperatorSecondArgumentMustBeConstPointerToSelf(OperatorSecondArgumentMustBeConstPointerToSelfError),
        OperatorOverloadDoesNotHaveRequiredAmountOfArgs(OperatorOverloadDoesNotHaveRequiredAmountOfArgsError),
    }
}

//We need an enum that tells the compiler up to where it could go based on the error gravity
pub enum HirPass {
    SyntaxLowering = 0,
    Monomorphization = 1,
    TypeCheck = 2,
    OwnershipPass = 3,
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
            HirError::TypeCheckFailed(_) => HirErrorGravity::CanGoUpTo(HirPass::OwnershipPass),
            HirError::OwnershipAnalysisFailed(_) => {
                HirErrorGravity::CanFinishCurrentPassButNotContinue
            }
            _ => HirErrorGravity::Critical,
        }
    }
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(sema::invalid_list_size),
    help("list size must be a non-negative and non-zero integer")
)]
#[error("invalid list size: {size}")]
pub struct InvalidListSizeError {
    #[label = "list size must be a non-negative and non-zero integer"]
    pub span: Span,
    pub size: usize,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(sema::non_constant_list_size),
    help("Only literal integers can be used as list size for now")
)]
#[error("list size must be a constant expression")]
pub struct NonConstantListSizeError {
    #[label = "list size must be a constant expression"]
    pub span: Span,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::reserved_variable_name), help("Try renaming your variable"))]
#[error("This kind of variable as a special behaviour, please rename it")]
pub struct ReservedVariableNameError {
    #[label = "{name} is a reserved name for variable"]
    pub span: Span,
    #[source_code]
    pub src: NamedSource<String>,
    pub name: String,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(sema::list_index_out_of_bounds),
    help("ensure the index is within the bounds of the list")
)]
#[error("list index {index} is out of bounds for list of size {size}")]
pub struct ListIndexOutOfBoundsError {
    #[label = "index {index} is out of bounds for list of size {size}"]
    pub span: Span,
    pub index: usize,
    pub size: usize,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(sema::returning_pointer_to_local_variable),
    help(
        "pointers to local variables cannot be returned because the variable will be dropped when the function returns"
    )
)]
#[error("cannot return pointers to local variable `{var_name}`")]
pub struct ReturningPointerToLocalVariableError {
    #[label = "returns a pointer to local variable `{var_name}`"]
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
pub struct VariableNameAlreadyDefinedError {
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
pub struct InvalidSpecialMethodSignatureError {
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
pub struct TypeDoesNotImplementRequiredConstraintError {
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
pub struct TypeDoesNotImplementRequiredConstraintOrigin {
    #[label = "the constraint is required here"]
    pub span: Span,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::trying_to_create_an_union_with_more_than_one_active_field))]
#[error("trying to create an union with more than one active field")]
pub struct TryingToCreateAnUnionWithMoreThanOneActiveFieldError {
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
pub struct TryingToCreateAnUnionWithMoreThanOneActiveFieldOrigin {
    #[label = "unions can only have one active field at a time"]
    pub span: Span,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(sema::trying_to_mutate_const_pointer),
    help("consider using a mutable pointer instead")
)]
#[error("trying to mutate a const pointer")]
pub struct TryingToMutateConstPointerError {
    #[label = "cannot mutate `{ty}` as it is a const pointer"]
    pub span: Span,
    pub ty: String,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(sema::calling_consuming_method_on_mutable_reference),
    help(
        "consider using an owned value instead (You can use the DeRef operator `*` to get an owned value from a mutable reference, though that might create a copy)"
    )
)]
#[error("calling a consuming method on a mutable reference")]
pub struct CallingConsumingMethodOnMutableReferenceError {
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
pub struct CallingConsumingMethodOnMutableReferenceOrigin {
    #[label = "method is marked as consuming here"]
    pub method_span: Span,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(sema::calling_non_const_method_on_const_reference),
    help("Try using a non-const reference or mark the method as const")
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
    code(sema::trying_to_access_a_deleted_value),
    help("consider copying the value before deleting it, or using a reference")
)]
#[error("trying to access a deleted value")]
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
    help("consider copying the value before moving it, or using a reference")
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
#[diagnostic(
    code(sema::trying_to_access_a_consumed_value),
    help("the value has already been consumed on all control-flow paths")
)]
#[error("trying to access a consumed value")]
pub struct TryingToAccessAConsumedValueError {
    #[label(collection, "value was moved/deleted here")]
    pub consume_spans: Vec<Span>,
    #[label(primary, "trying to access consumed value here")]
    pub access_span: Span,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(sema::trying_to_access_a_potentially_moved_value),
    help("consider copying the value before moving it, or using a reference")
)]
#[error("trying to access a potentially moved value")]
pub struct TryingToAccessAPotentiallyMovedValueError {
    #[label = "value was conditionally moved here"]
    pub move_span: Span,
    #[label = "trying to access potentially moved value here"]
    pub access_span: Span,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(sema::trying_to_access_a_potentially_deleted_value),
    help("ensure the value is not conditionally deleted before this access")
)]
#[error("trying to access a potentially deleted value")]
pub struct TryingToAccessAPotentiallyDeletedValueError {
    #[label = "value was conditionally deleted here"]
    pub delete_span: Span,
    #[label = "trying to access potentially deleted value here"]
    pub access_span: Span,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(sema::trying_to_access_a_potentially_consumed_value),
    help(
        "the value is consumed across control-flow branches (moved in one branch and/or deleted in another)"
    )
)]
#[error("trying to access a potentially consumed value")]
pub struct TryingToAccessAPotentiallyConsumedValueError {
    #[label(collection, "value was conditionally moved/deleted here")]
    pub consume_spans: Vec<Span>,
    #[label(primary, "trying to access potentially consumed value here")]
    pub access_span: Span,
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
#[error("{item} aren't supported yet")]
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
    //The kind of callable (function or method, etc.)
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
#[diagnostic(code(sema::trying_to_access_private_struct))]
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
    pub origin: AccessingPrivateObjectOrigin,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::trying_to_access_private_union))]
#[error(
    "{name} is marked as private, so you cannot accessing it from outside of its declaration file."
)]
pub struct AccessingPrivateUnionError {
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
pub struct AccessingPrivateObjectOrigin {
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
#[diagnostic(code(sema::accessing_private_destructor))]
#[error("Can't access the private destructor of {ty} outside of its class")]
pub struct AccessingPrivateDestructorError {
    #[label("Trying to access the private destructor here")]
    pub span: Span,
    #[source_code]
    pub src: NamedSource<String>,
    pub ty: String,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::this_should_not_appear))]
#[error("This is just a useless error that should not appear")]
pub struct UselessError {
    #[label = "useless error"]
    pub span: Span,
    #[source_code]
    pub src: NamedSource<String>,
}

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
#[diagnostic(code(sema::this_access_outside_class))]
#[error("Can't access fields of this outside of a class")]
pub struct AccessingClassFieldOutsideClassError {
    #[label("Trying to access a class field from `this` while outside of a class")]
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
pub struct UnsupportedStatementError {
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

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(sema::double_move),
    help(
        "a value can only be moved once. Consider cloning the value before the first move if you need to use it multiple times."
    )
)]
#[error("value has already been moved")]
pub struct DoubleMoveError {
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
pub struct UnknownIdentifierError {
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
pub struct UnknownFieldError {
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
pub struct UnknownMethodError {
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
pub struct MethodConstraintNotSatisfiedError {
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
    code(sema::cannot_move_out_of_loop),
    help(
        "variables cannot be moved inside loops because the loop could iterate multiple times, causing use-after-move. Consider moving the variable before the loop, or restructuring your code"
    )
)]
#[error("cannot move variable `{var_name}` inside loop")]
pub struct CannotMoveOutOfLoopError {
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
pub struct CannotDeleteOutOfLoopError {
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
    code(sema::struct_cannot_have_a_field_of_its_own_type),
    help(
        "A struct cannot directly or indirectly contain itself as a field, as this would create infinite size.
This error occurs when:
  - A struct has a field of its own type (direct cycle): `struct A {{ a: A }}`
  - A struct contains another struct that eventually contains the first struct (indirect cycle): `struct A {{ b: B }}` where `struct B {{ a: A }}`

Solutions:
  - Use a pointer: `*T` or `*const T` (pointers are fixed-size (8 bytes))
  - Use an indirection type: `optional<T>` or `expected<T, E>` (these allow null/empty states)
  - Redesign the data structure to avoid the cycle"
    )
)]
#[error("struct `{struct_name}` contains a cyclic reference to itself")]
pub struct StructCannotHaveAFieldOfItsOwnTypeError {
    pub struct_name: String,
    #[label = "struct `{struct_name}` defined here"]
    pub struct_span: Span,
    #[label(collection)]
    pub cycle_path: Vec<miette::LabeledSpan>,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(sema::union_must_have_at_least_two_variant),
    help(
        "A union must have at least two variants to be valid, add a `std::empty` variant if you need a nullable state."
    )
)]
#[error("{union_name} must have at least two variants")]
pub struct UnionMustHaveAtLeastTwoVariantError {
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
pub struct UnionVariantDefinedMultipleTimesError {
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
pub struct LifetimeDependencyViolationError {
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
pub struct ReturningValueWithLocalLifetimeDependencyError {
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
    code(sema::assignment_cannot_be_an_expression),
    help("assignments are statements and do not produce a value")
)]
#[error("assignments cannot be used as expressions")]
pub struct AssignmentCannotBeAnExpressionError {
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
pub struct CannotGenerateADestructorForThisTypeError {
    #[source_code]
    pub src: NamedSource<String>,
    #[label = "field requiring custom destructor is defined here"]
    pub conflicting_field: Span,
    #[label("Type `{type_name}` declared here")]
    pub name_span: Span,
    pub type_name: String,
}

#[derive(Error, Diagnostic, Debug)]
#[error("Cannot move from rvalue")]
#[diagnostic(code(sema::cannot_move_from_rvalue))]
pub struct CannotMoveFromRvalueError {
    #[source_code]
    pub src: NamedSource<String>,

    #[label("Cannot move from this expression")]
    pub span: Span,

    #[help]
    pub hint: String,
}

#[derive(Error, Diagnostic, Debug)]
#[error("Type is not copyable")]
#[diagnostic(code(sema::type_not_copyable))]
pub struct TypeIsNotCopyableError {
    #[source_code]
    pub src: NamedSource<String>,

    #[label("Type '{type_name}' doesn't implement std::copyable")]
    pub span: Span,

    pub type_name: String,
}

#[derive(Error, Diagnostic, Debug)]
#[error("Type is not trivially copyable")]
#[diagnostic(
    code(sema::type_not_trivially_copyable),
    help(
        "implicit copies (e.g. `let foo = bar`) require `std::trivially_copyable`; use `std::move(&p)`/`std::take(&p)` to transfer ownership, or `p.copy()`/`std::copy(&p)` for an explicit copy when available"
    )
)]
pub struct TypeIsNotTriviallyCopyableError {
    #[source_code]
    pub src: NamedSource<String>,

    #[label("Type '{type_name}' doesn't implement std::trivially_copyable")]
    pub span: Span,

    pub type_name: String,
}

#[derive(Error, Diagnostic, Debug)]
#[error("Ownership analysis found {error_count} error(s)")]
#[diagnostic(code(sema::ownership_analysis_failed))]
pub struct OwnershipAnalysisFailedError {
    pub error_count: usize,

    #[related]
    pub errors: Vec<HirError>,
}

#[derive(Error, Diagnostic, Debug)]
#[error("Type checking found {error_count} error(s)")]
#[diagnostic(code(sema::type_check_failed))]
pub struct TypeCheckFailedError {
    pub error_count: usize,

    #[related]
    pub errors: Vec<HirError>,
}

#[derive(Error, Diagnostic, Debug)]
#[error("Semantic analysis found {error_count} error(s)")]
#[diagnostic(code(sema::semantic_analysis_failed))]
pub struct SemanticAnalysisFailedError {
    pub error_count: usize,

    #[related]
    pub errors: Vec<HirError>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(sema::incorrect_intrinsic_call_arguments),
    help("provide the correct number of arguments ({expected}) to the intrinsic function")
)]
#[error("intrinsic function `{name}` expected {expected} arguments, but found {found}")]
pub struct IncorrectIntrinsicCallArgumentsError {
    pub expected: usize,
    pub found: usize,
    pub name: String,
    #[label = "only {found} were provided"]
    pub span: Span,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(sema::trying_to_access_field_of_pointer),
    help("Try using the `->` operator to dereference and access the field.")
)]
#[error("Cannot access a struct field directly from a pointer")]
pub struct CannotAccessFieldOfPointersError {
    #[label = "Try using `->` instead of `.`"]
    pub span: Span,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(sema::unknown_overloadable_operator),
    help("ensure the operator is one of the allowed overloadable operators")
)]
#[error(
    "The operator `{operator}` is not a recognized overloadable operator in Atlas. Only the following operators can be overloaded: 'add', 'sub', 'mul', 'div', 'rem', 'neg', 'not', 'and', 'or', 'xor', 'shl', 'shr', 'equal', 'not_equal', 'less', 'less_equal', 'greater', 'greater_equal',"
)]
pub struct UnknownOverloadableOperatorError {
    pub operator: String,
    #[label = "unknown overloadable operator `{operator}`"]
    pub span: Span,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(sema::operator_not_implemented_for_this_type),
    help("ensure the type implements the operator, or implement it if it's a user-defined type")
)]
#[error("The operator `{operator}` is not implemented for type `{ty}`")]
pub struct OperatorIsNotImplementedForThisTypeError {
    pub operator: String,
    pub ty: String,
    #[label = "operator `{operator}` is not implemented for type `{ty}`"]
    pub span: Span,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(sema::operator_must_use_const_this_modifier),
    help("use a const receiver for operator overloads")
)]
#[error("operator overloads must use a const `this` receiver")]
pub struct OperatorMustUseConstThisModifierError {
    #[label = "operator overload receiver must be `*const this`"]
    pub span: Span,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(sema::operator_second_argument_must_be_const_pointer_to_self),
    help(
        "the second operator argument must be a const pointer to the enclosing struct \
    \n\tNB: This is currently a hard limitation of the compiler. It will probably be removed at one point"
    )
)]
#[error(
    "operator overloads must take `*const T` as their second argument, where `T` is the enclosing type"
)]
pub struct OperatorSecondArgumentMustBeConstPointerToSelfError {
    #[label = "second argument must be `*const {ty_name}`"]
    pub span: Span,
    pub ty_name: String,
    #[source_code]
    pub src: NamedSource<String>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(sema::operator_overload_does_not_have_required_amount_of_args),
    help(
        "{kind} operator overloads must take exactly {expected} arguments (the `this` receiver{context})"
    )
)]
#[error("operator overloads must take exactly {expected} arguments (the `this` receiver{context})")]
pub struct OperatorOverloadDoesNotHaveRequiredAmountOfArgsError {
    pub kind: String,
    pub expected: usize,
    #[label = "this operator overload takes {found} arguments but should actually only have {expected}"]
    pub span: Span,
    pub found: usize,
    #[source_code]
    pub src: NamedSource<String>,
    pub context: String,
}
