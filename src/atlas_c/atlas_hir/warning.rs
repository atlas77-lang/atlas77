use crate::{atlas_c::utils::Span, declare_warning_type};
use miette::{Diagnostic, NamedSource};
use thiserror::Error;

declare_warning_type!(
    #[warning("semantic warning: {0}")]
    pub enum HirWarning {
        ThisTypeIsStillUnstable(ThisTypeIsStillUnstableWarning),
        NameShouldBeInDifferentCase(NameShouldBeInDifferentCaseWarning),
        TryingToCastToTheSameType(TryingToCastToTheSameTypeWarning),
        ConsumingMethodMayLeakThis(ConsumingMethodMayLeakThisWarning),
        CannotGenerateACopyConstructorForThisType(CannotGenerateACopyConstructorForThisTypeWarning),
        UnnecessaryCopyDueToLaterBorrows(UnnecessaryCopyDueToLaterBorrowsWarning),
        ReferenceEscapesToConstructor(ReferenceEscapesToConstructorWarning),
        UnionFieldCannotBeAutomaticallyDeleted(UnionFieldCannotBeAutomaticallyDeletedWarning),
    }
);

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(sema::cannot_generate_a_copy_constructor_for_this_type),
    severity(warning),
    help(
        "Consider implementing a custom copy constructor for this type, or making its fields copyable"
    )
)]
#[error(
    "Type `{type_name}` is marked as std::copyable, but a copy constructor could not be automatically generated"
)]
pub struct CannotGenerateACopyConstructorForThisTypeWarning {
    #[source_code]
    pub src: NamedSource<String>,
    #[label = "Type `{type_name}` is marked as std::copyable here"]
    pub flag_span: Span,
    #[label("Type `{type_name}` declared here")]
    pub name_span: Span,
    pub type_name: String,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(sema::trying_to_cast_to_the_same_type),
    severity(warning),
    help("Remove the cast (`as {ty}`) as it is redundant")
)]
#[error("Trying to cast something which is of type `{ty}` to `{ty}`")]
pub struct TryingToCastToTheSameTypeWarning {
    #[source_code]
    pub src: NamedSource<String>,
    #[label = "Casting to the same type is redundant"]
    pub span: Span,
    pub ty: String,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(sema::name_should_be_in_a_different_case),
    severity(warning),
    help("Consider renaming the {item_kind} to follow the {case_kind} case convention")
)]
#[error("{item_kind} should be in {case_kind} case")]
pub struct NameShouldBeInDifferentCaseWarning {
    #[source_code]
    pub src: NamedSource<String>,
    #[label = "Name `{name}` should be in {case_kind} case: `{expected_name}`"]
    pub span: Span,
    //The kind of case that is expected
    pub case_kind: String,
    //The kind of item (function, struct, variable, etc.)
    pub item_kind: String,
    //The name that triggered the warning
    pub name: String,
    //The expected name in the correct case
    pub expected_name: String,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::type_is_still_unstable), severity(warning))]
#[error("{type_name} is still unstable. {info}")]
pub struct ThisTypeIsStillUnstableWarning {
    #[source_code]
    pub src: NamedSource<String>,
    #[label = "There is no guarantee of this working properly. Beware."]
    pub span: Span,
    pub type_name: String,
    //Additional info about why it's unstable
    pub info: String,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(sema::consuming_method_may_leak_this),
    severity(warning),
    help(
        "Add `delete this;` before returning, or change to `&this` / `&const this` if you don't need to consume ownership"
    )
)]
#[error("Consuming method `{method_signature}` does not explicitly delete `this`")]
pub struct ConsumingMethodMayLeakThisWarning {
    #[source_code]
    pub src: NamedSource<String>,
    #[label = "This method takes ownership of `this` but doesn't delete it, which may cause a memory leak"]
    pub span: Span,
    pub method_signature: String,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(sema::unnecessary_copy_due_to_later_borrows),
    severity(warning),
    help("Consider reordering statements to move `{var_name}` last")
)]
#[error("Variable `{var_name}` is copied here but only borrowed later")]
pub struct UnnecessaryCopyDueToLaterBorrowsWarning {
    #[source_code]
    pub src: NamedSource<String>,
    #[label(
        primary,
        "This copies `{var_name}` because it's used later, but all later uses are just borrows"
    )]
    pub span: Span,
    pub var_name: String,
    #[label(collection, "Borrowed here")]
    pub borrow_uses: Vec<Span>,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(sema::reference_escapes_to_constructor),
    severity(warning),
    help(
        "If `{origin_var}` is deleted or consumed before the constructed object, \
        accessing the stored reference will be a use-after-free bug. \
        Consider updating the reference before using it if the origin changes."
    )
)]
#[error("Reference `{ref_var}` (originating from `{origin_var}`) is passed to a constructor")]
pub struct ReferenceEscapesToConstructorWarning {
    #[source_code]
    pub src: NamedSource<String>,
    #[label(
        primary,
        "Reference escapes here - it may be stored in `{constructed_type}`"
    )]
    pub span: Span,
    #[label("Reference originates from this variable")]
    pub origin_span: Span,
    pub ref_var: String,
    pub origin_var: String,
    pub constructed_type: String,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(sema::union_field_cannot_be_automatically_deleted),
    severity(warning),
    help(
        "Unions require special handling for deletion. Consider implementing a custom destructor for this type."
    )
)]
#[error(
    "The compiler cannot automatically delete the union field `{field_name}` of struct `{struct_name}` as it may lead to undefined behavior"
)]
pub struct UnionFieldCannotBeAutomaticallyDeletedWarning {
    #[source_code]
    pub src: NamedSource<String>,
    #[label = "Union field `{field_name}` cannot be automatically deleted here"]
    pub span: Span,
    pub field_name: String,
    pub struct_name: String,
}
