use super::{signature::HirFunctionSignature, stmt::HirBlock};
use crate::atlas_c::atlas_hir::signature::{
    HirFlag, HirFunctionParameterSignature, HirStructConstructorSignature, HirStructFieldSignature,
    HirStructMethodSignature, HirStructSignature, HirTypeParameterItemSignature, HirUnionSignature,
    HirVisibility,
};
use crate::atlas_c::utils::Span;

#[derive(Debug, Clone)]
pub struct HirGlobalConst<'hir> {
    pub span: Span,
    pub name: &'hir str,
    pub name_span: Span,
    pub ty: &'hir str,
    pub ty_span: Span,
    pub value: &'hir str,
    pub value_span: Span,
    pub vis: HirVisibility,
}

#[derive(Debug, Clone)]
pub struct HirFunction<'hir> {
    pub span: Span,
    pub name: &'hir str,
    pub name_span: Span,
    pub signature: &'hir HirFunctionSignature<'hir>,
    pub body: HirBlock<'hir>,
}

/// Used by the type checker to import the API Signature of a module.
#[derive(Debug, Clone)]
pub struct HirImport<'hir> {
    pub span: Span,
    pub path: &'hir str,
    pub path_span: Span,

    /// As of now the alias is unsupported.
    pub alias: Option<&'hir str>,
    pub alias_span: Option<Span>,
}

#[derive(Debug, Clone)]
pub struct HirUnion<'hir> {
    pub span: Span,
    pub name: &'hir str,
    pub name_span: Span,
    pub variants: Vec<HirStructFieldSignature<'hir>>,
    pub signature: HirUnionSignature<'hir>,
    pub vis: HirVisibility,
}

#[derive(Debug, Clone)]
pub struct HirStruct<'hir> {
    pub span: Span,
    pub name: &'hir str,
    pub name_span: Span,
    pub signature: HirStructSignature<'hir>,
    pub methods: Vec<HirStructMethod<'hir>>,
    pub fields: Vec<HirStructFieldSignature<'hir>>,
    pub constructor: HirStructConstructor<'hir>,
    pub copy_constructor: Option<HirStructConstructor<'hir>>,
    pub destructor: HirStructConstructor<'hir>,
    pub vis: HirVisibility,
    pub flag: HirFlag,
}

#[derive(Debug, Clone)]
pub struct HirEnum<'hir> {
    pub span: Span,
    pub name: &'hir str,
    pub name_span: Span,
    pub variants: Vec<HirEnumVariant<'hir>>,
    pub vis: HirVisibility,
}

#[derive(Debug, Clone)]
pub struct HirEnumVariant<'hir> {
    pub span: Span,
    pub name: &'hir str,
    pub name_span: Span,
    //Only supporting discriminant values for now
    pub value: u64,
}

#[derive(Debug, Clone)]
pub struct HirStructMethod<'hir> {
    pub span: Span,
    pub name: &'hir str,
    pub name_span: Span,
    pub signature: &'hir HirStructMethodSignature<'hir>,
    pub body: HirBlock<'hir>,
}

#[derive(Debug, Clone)]
/// Also used for the destructor
pub struct HirStructConstructor<'hir> {
    pub span: Span,
    pub signature: &'hir HirStructConstructorSignature<'hir>,
    pub params: Vec<HirFunctionParameterSignature<'hir>>,
    pub type_params: Vec<HirTypeParameterItemSignature<'hir>>,
    pub body: HirBlock<'hir>,
    pub vis: HirVisibility,
}

#[derive(Debug, Clone)]
/// Represents a package path declaration like `package my_project::my_module;`
pub struct HirPackage<'hir> {
    pub span: Span,
    pub path: &'hir [&'hir str],
}
