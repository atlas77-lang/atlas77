use std::collections::BTreeMap;

use crate::atlas_c::atlas_hir::item::{HirEnum, HirStruct, HirUnion};
use item::{HirFunction, HirImport};
use signature::HirModuleSignature;

//Should try to run even with a faulty AST
/// Always run
pub mod arena;
/// Passes not run in debug mode
pub mod constant_folding_pass;
pub mod dead_code_elimination_pass;
pub mod error;
pub mod monomorphization_pass;
/// Ownership analysis pass: implements MOVE/COPY semantics and destructor insertion
pub mod ownership_pass;
/// Pass not run in debug mode
pub mod syntax_lowering_pass;
pub mod type_check_pass;
//todo: The Hir needs a little rework to correctly define what is an item, a statement, an expression, a type, etc.
pub mod expr;
pub mod item;
pub mod pretty_print;
mod scope;
pub mod signature;
pub mod stmt;
pub mod ty;
mod warning;

#[derive(Debug, Clone, Default)]
pub struct HirModuleGraph<'hir> {
    pub modules: BTreeMap<HirModuleId<'hir>, HirModule<'hir>>,
}

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct HirModuleId<'hir> {
    pub name: &'hir str,
}

#[derive(Debug, Clone, Default)]
pub struct HirModuleBody<'hir> {
    pub functions: BTreeMap<&'hir str, HirFunction<'hir>>,
    pub structs: BTreeMap<&'hir str, HirStruct<'hir>>,
    pub imports: Vec<&'hir HirImport<'hir>>,
    // Not really useful for the current version, but I might add methods to enums later
    pub enums: BTreeMap<&'hir str, HirEnum<'hir>>,
    // Not really useful for the current version, but I might add methods to unions later
    pub unions: BTreeMap<&'hir str, HirUnion<'hir>>,
}

#[derive(Debug, Clone, Default)]
/// A module is
pub struct HirModule<'hir> {
    pub body: HirModuleBody<'hir>,
    pub signature: HirModuleSignature<'hir>,
}
