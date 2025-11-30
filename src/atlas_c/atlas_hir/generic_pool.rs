use crate::atlas_c::atlas_hir::arena::HirArena;
use crate::atlas_c::atlas_hir::ty::{HirGenericTy, HirTy};
use logos::Span;
use std::collections::BTreeMap;
use std::fmt::Debug;

//TODO: Add generic methods
#[derive(Clone)]
pub struct HirGenericPool<'hir> {
    /// Mapped mangled generic struct name to its instance
    pub structs: BTreeMap<&'hir str, HirGenericInstance<'hir>>,
    pub methods: BTreeMap<&'hir str, HirGenericInstance<'hir>>,
    pub functions: BTreeMap<&'hir str, HirGenericInstance<'hir>>,
    pub arena: &'hir HirArena<'hir>,
}

impl Debug for HirGenericPool<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("HirGenericPool")
            .field("structs", &self.structs)
            .field("methods", &self.methods)
            .field("functions", &self.functions)
            .finish()
    }
}

#[derive(Debug, Clone)]
pub struct HirGenericInstance<'hir> {
    /// Actual Struct name or function/method name
    /// e.g. "MyStruct", "my_function"
    pub name: &'hir str,
    pub args: Vec<HirTy<'hir>>,
    pub span: Span,
    pub is_done: bool,
}

impl<'hir> HirGenericPool<'hir> {
    pub fn new(arena: &'hir HirArena<'hir>) -> Self {
        Self {
            structs: BTreeMap::new(),
            methods: BTreeMap::new(),
            functions: BTreeMap::new(),
            arena,
        }
    }
    pub fn register_struct_instance(&mut self, generic: HirGenericTy<'hir>) {
        let name = self.mangle_generic_struct_name(generic.clone());
        self.structs.insert(
            name,
            HirGenericInstance {
                name: generic.name,
                args: generic.inner,
                is_done: false,
                span: generic.span,
            },
        );
    }

    pub fn register_function_instance(
        &mut self,
        generic_name: &'hir str,
        instance: Vec<HirTy<'hir>>,
    ) {
        self.functions.insert(
            generic_name,
            HirGenericInstance {
                name: generic_name,
                args: instance,
                is_done: false,
                span: Span::default(),
            },
        );
    }

    fn mangle_generic_struct_name(&self, generic: HirGenericTy<'hir>) -> &'hir str {
        let parts: Vec<String> = generic
            .inner
            .iter()
            .map(|t| match t {
                HirTy::Generic(g) => self.mangle_generic_struct_name(g.clone()).to_string(),
                _ => format!("{}", t),
            })
            .collect();
        let name = format!("__atlas77__struct__{}__{}", generic.name, parts.join("_"));
        self.arena.intern(name)
    }
}
