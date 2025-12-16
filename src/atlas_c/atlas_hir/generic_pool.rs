use crate::atlas_c::atlas_hir::arena::HirArena;
use crate::atlas_c::atlas_hir::ty::{HirGenericTy, HirTy};
use crate::atlas_c::utils::Span;
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
        //First we need to check if it's an instantiated generics or a generic definition e.g.: Vector<T> or Vector<uint64>
        if !self.is_generic_instantiated(&generic) {
            return;
        }
        let name = self.mangle_generic_struct_name(generic.clone(), "struct");
        self.structs.entry(name).or_insert(HirGenericInstance {
            name: generic.name,
            args: generic.inner,
            is_done: false,
            span: generic.span,
        });
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

    fn is_generic_instantiated(&mut self, generic: &HirGenericTy<'hir>) -> bool {
        let mut is_instantiated = true;
        for ty in generic.inner.iter() {
            match ty {
                HirTy::Named(n) => {
                    if n.name.len() == 1 {
                        is_instantiated = false;
                    }
                }
                HirTy::Generic(g) => {
                    //We register nested generics as well (e.g. MyStruct<Vector<uint64>>)
                    //This ensures that they are also monomorphized if it's the only instance
                    //But because the check is called in register_struct_instance it won't register generic definitions
                    self.register_struct_instance(g.clone());
                }
                _ => continue,
            }
        }
        is_instantiated
    }

    fn mangle_generic_struct_name(&self, generic: HirGenericTy<'hir>, kind: &str) -> &'hir str {
        let parts: Vec<String> = generic
            .inner
            .iter()
            .map(|t| match t {
                HirTy::Generic(g) => self.mangle_generic_struct_name(g.clone(), kind).to_string(),
                _ => format!("{}", t),
            })
            .collect();
        let name = format!("__atlas77__{}__{}__{}", kind, generic.name, parts.join("_"));
        self.arena.intern(name)
    }
}
