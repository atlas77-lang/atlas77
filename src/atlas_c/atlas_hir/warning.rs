use crate::{atlas_c::utils::Span, declare_warning_type};
use miette::{Diagnostic, NamedSource};
use thiserror::Error;

declare_warning_type!(
    #[warning("semantic warning: {0}")]
    pub enum HirWarning {
        ThisTypeIsStillUnstable(ThisTypeIsStillUnstableWarning),
        DeletingReferenceIsNotSafe(DeletingReferenceMightLeadToUB),
        NameShouldBeInDifferentCase(NameShouldBeInDifferentCaseWarning),
        TryingToCastToTheSameType(TryingToCastToTheSameTypeWarning),
    }
);

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
#[diagnostic(code(sema::deleting_reference_is_not_safe), severity(warning))]
#[error("Deleting a reference might lead to undefined behavior")]
pub struct DeletingReferenceMightLeadToUB {
    #[source_code]
    pub src: NamedSource<String>,
    #[label = "Deleting references might lead to undefined behavior. Use with caution."]
    pub span: Span,
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
