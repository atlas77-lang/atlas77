use crate::declare_warning_type;
use miette::{Diagnostic, NamedSource, SourceSpan};
use thiserror::Error;

declare_warning_type!(
    #[warning("Semantic Warning")]
    pub enum HirWarning {
        NullableTypesAreUnstable(NullableTypesAreUnstableWarning),
        DeletingReferenceIsUnstable(DeletingReferenceIsUnstableWarning),
    }
);


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
