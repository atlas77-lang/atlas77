use miette::{Diagnostic, SourceSpan};
use thiserror::Error;
use crate::declare_warning_type;

declare_warning_type!(
    #[warning("Semantic Warning")]
    pub enum HirWarning {
        NullableTypesAreUnstable(NullableTypesAreUnstableWarning),
    }
);
#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(warning::nullabel_types_are_unstable))]
#[error("Nullable types are still unstable. They will later be replaced by a stable `Option<T>`")]
pub struct NullableTypesAreUnstableWarning {
    #[source_code]
    pub src: String,
    #[label = "There is no guarantee of this working. Beware."]
    pub span: SourceSpan,
}
