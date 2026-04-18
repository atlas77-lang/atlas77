// For some reason I get unused assignment warnings in this file
#![allow(unused_assignments)]

use crate::{atlas_c::utils::Span, declare_warning_type};
use miette::{Diagnostic, NamedSource};
use thiserror::Error;

declare_warning_type!(
    #[warning("semantic warning: {0}")]
    pub enum HirWarning {
        ThisTypeIsStillUnstable(ThisTypeIsStillUnstableWarning),
        TryingToCastToTheSameType(TryingToCastToTheSameTypeWarning),
        ConsumingMethodMayLeakThis(ConsumingMethodMayLeakThisWarning),
        UnnecessaryCopyDueToLaterBorrows(UnnecessaryCopyDueToLaterBorrowsWarning),
        UnionFieldCannotBeAutomaticallyDeleted(UnionFieldCannotBeAutomaticallyDeletedWarning),
        UnsafeRawPointerStruct(UnsafeRawPointerStructWarning),
        SpecialMethodMightHaveWrongSignature(SpecialMethodMightHaveWrongSignatureWarning),
    }
);

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(
    code(sema::non_trivially_copyable_struct_holds_a_raw_pointer_with_no_custom_destructor),
    severity(warning),
    help(
        "Consider marking the struct as `std::trivially_copyable` if it is safe to do so, or implement a custom destructor to properly manage the raw pointer's memory"
    )
)]
#[error(
    "Struct `{struct_name}` is not marked as `std::trivially_copyable` but holds a raw pointer and does not have a custom destructor, which may lead to memory safety issues if the struct is copied or moved without proper handling"
)]
pub struct UnsafeRawPointerStructWarning {
    #[source_code]
    pub src: NamedSource<String>,
    #[label = "Struct not marked as `std::trivially_copyable` declared here"]
    pub struct_span: Span,
    #[label = "Raw pointer field declared here"]
    pub pointer_span: Span,
    pub struct_name: String,
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
    code(sema::union_field_cannot_be_automatically_deleted),
    severity(warning),
    help(
        "Unions require special handling for deletion. Consider implementing a custom destructor for your struct."
    )
)]
#[error(
    "The compiler cannot automatically delete the union field `{variant_name}` because it requires drop and the compiler cannot know which variant is active at compile time"
)]
pub struct UnionFieldCannotBeAutomaticallyDeletedWarning {
    #[source_code]
    pub src: NamedSource<String>,

    #[label = "Union field `{variant_name}` cannot be automatically deleted here."]
    pub variant_span: Span,
    pub variant_name: String,

    #[label = "Union `{union_name}` declared here"]
    pub union_span: Span,
    pub union_name: String,

    #[label = "{usage_loc_type} `{variant_name}` is used here, which may lead to it not being properly deleted if it's the active variant when the struct is dropped"]
    pub usage_loc_span: Span,
    pub usage_loc_type: String,
}

#[derive(Error, Diagnostic, Debug)]
#[diagnostic(code(sema::potential_wrong_signature), severity(warning))]
#[error(
    "Special method `{method_name}` might have the wrong signature `{signature}`\n\t(expected `{expected_signature}`)"
)]
pub struct SpecialMethodMightHaveWrongSignatureWarning {
    pub signature: String,
    pub expected_signature: String,
    pub method_name: String,
    #[source_code]
    pub src: NamedSource<String>,
    #[label = "Method `{method_name}` is a special method but its signature `{signature}` does not match the expected signature `{expected_signature}` for this method, which may lead to it not being recognized as a special method and not being called in certain situations"]
    pub span: Span,
}
