use crate::declare_warning_type;
use miette::{Diagnostic, NamedSource, SourceSpan};
use thiserror::Error;

declare_warning_type!(
    #[warning("semantic warning: {0}")]
    pub enum HirWarning {
        NullableTypesAreUnstable(NullableTypesAreUnstableWarning),
        DeletingReferenceIsUnstable(DeletingReferenceIsUnstableWarning),
        NameShouldBeInDifferentCase(NameShouldBeInDifferentCaseWarning),
    }
);

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
    pub span: SourceSpan,
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
#[diagnostic(code(sema::deleting_reference_is_unstable), severity(warning))]
#[error("Deleting references is still unstable. They may lead to unexpected behavior.")]
pub struct DeletingReferenceIsUnstableWarning {
    #[source_code]
    pub src: NamedSource<String>,
    #[label = "Deleting references is still unstable. Use with caution."]
    pub span: SourceSpan,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::nullable_types_are_unstable), severity(warning))]
#[error("Nullable types are still unstable. They will later be replaced by a stable `Option<T>`")]
pub struct NullableTypesAreUnstableWarning {
    #[source_code]
    pub src: NamedSource<String>,
    #[label = "There is no guarantee of this working properly. Beware."]
    pub span: SourceSpan,
}
