use miette::NamedSource;

use crate::atlas_c::atlas_hir::arena::HirArena;
use crate::atlas_c::atlas_hir::error::{
    TypeDoesNotImplementRequiredConstraintError, TypeDoesNotImplementRequiredConstraintOrigin,
};
use crate::atlas_c::atlas_hir::monomorphization_pass::MonomorphizationPass;
use crate::atlas_c::atlas_hir::signature::{
    HirGenericConstraint, HirGenericConstraintKind, HirModuleSignature,
};
use crate::atlas_c::atlas_hir::ty::{HirGenericTy, HirTy};
use crate::atlas_c::utils::{self, Span};
use std::collections::BTreeMap;
use std::fmt::Debug;

//TODO: Add generic methods
#[derive(Clone)]
pub struct HirGenericPool<'hir> {
    /// Mapped mangled generic struct name to its instance
    pub structs: BTreeMap<&'hir str, HirGenericInstance<'hir>>,
    pub methods: BTreeMap<&'hir str, HirGenericInstance<'hir>>,
    pub functions: BTreeMap<&'hir str, HirGenericInstance<'hir>>,
    pub unions: BTreeMap<&'hir str, HirGenericInstance<'hir>>,
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
            unions: BTreeMap::new(),
            arena,
        }
    }
    pub fn register_struct_instance(
        &mut self,
        generic: HirGenericTy<'hir>,
        module: &HirModuleSignature<'hir>,
    ) {
        //Let's now check for constraints:
        let declaration_span;
        let constraints: Vec<&HirGenericConstraint<'_>>;
        if let Some(struct_sig) = module.structs.get(generic.name) {
            declaration_span = struct_sig.name_span;
            constraints = struct_sig.generics.clone();
            if !self.check_constraint_satisfaction(module, &generic, constraints, declaration_span)
            {
                std::process::exit(1);
            }
        } else if let Some(union_sig) = module.unions.get(generic.name) {
            declaration_span = union_sig.name_span;
            constraints = union_sig.generics.clone();
            if !self.check_constraint_satisfaction(module, &generic, constraints, declaration_span)
            {
                std::process::exit(1);
            }
        }
        //We need to check if it's an instantiated generics or a generic definition e.g.: Vector<T> or Vector<uint64>
        //We check only after checking constraints so if a type is registered during syntax lowering we still check constraints afterwards
        if !self.is_generic_instantiated(&generic, module) {
            return;
        }

        let name = self.mangle_generic_object_name(generic.clone(), "struct");
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

    fn is_generic_instantiated(
        &mut self,
        generic: &HirGenericTy<'hir>,
        module: &HirModuleSignature<'hir>,
    ) -> bool {
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
                    self.register_struct_instance(g.clone(), module);
                }
                _ => continue,
            }
        }
        is_instantiated
    }

    fn mangle_generic_object_name(&self, generic: HirGenericTy<'hir>, kind: &str) -> &'hir str {
        let parts: Vec<String> = generic
            .inner
            .iter()
            .map(|t| match t {
                HirTy::Generic(g) => self.mangle_generic_object_name(g.clone(), kind).to_string(),
                _ => format!("{}", t),
            })
            .collect();
        let name = format!("__atlas77__{}__{}__{}", kind, generic.name, parts.join("_"));
        self.arena.intern(name)
    }

    fn check_constraint_satisfaction(
        &self,
        module: &HirModuleSignature<'hir>,
        instantiated_generic: &HirGenericTy<'hir>,
        constraints: Vec<&HirGenericConstraint<'hir>>,
        declaration_span: Span,
    ) -> bool {
        let mut are_constraints_satisfied = true;
        for (instantiated_ty, constraint) in
            instantiated_generic.inner.iter().zip(constraints.iter())
        {
            for kind in constraint.kind.iter() {
                match kind {
                    HirGenericConstraintKind::Std {
                        name: "copyable",
                        span,
                    } => {
                        if !self.implements_std_copyable(module, instantiated_ty) {
                            let origin_path = declaration_span.path;
                            let origin_src = utils::get_file_content(origin_path).unwrap();
                            let origin = TypeDoesNotImplementRequiredConstraintOrigin {
                                span: *span,
                                src: NamedSource::new(origin_path, origin_src),
                            };
                            let err_path = instantiated_generic.span.path;
                            let err_src = utils::get_file_content(err_path).unwrap();
                            let err = TypeDoesNotImplementRequiredConstraintError {
                                ty: format!("{}", instantiated_ty),
                                span: instantiated_generic.span,
                                constraint: format!("{}", kind),
                                src: NamedSource::new(err_path, err_src),
                                origin,
                            };
                            eprintln!("{:?}", Into::<miette::Report>::into(err));
                            are_constraints_satisfied = false;
                        } else {
                            continue;
                        }
                    }
                    _ => {
                        //Other constraints not implemented yet
                        continue;
                    }
                }
            }
        }
        are_constraints_satisfied
    }

    /// This is currently the only generic constraint supported.
    /// Checks if a type implements `std::copyable` e.g. If it's a primitive type or a struct that has the `_copy` method.
    fn implements_std_copyable(&self, module: &HirModuleSignature<'hir>, ty: &HirTy<'hir>) -> bool {
        match ty {
            HirTy::Boolean(_)
            | HirTy::Int64(_)
            | HirTy::Float64(_)
            | HirTy::Char(_)
            | HirTy::String(_)
            | HirTy::UInt64(_) => true,
            HirTy::List(l) => self.implements_std_copyable(module, l.inner),
            HirTy::Named(n) => match module.structs.get(n.name) {
                Some(struct_sig) => struct_sig.methods.contains_key("_copy"),
                None => false,
            },
            HirTy::Generic(g) => {
                let name = MonomorphizationPass::mangle_generic_object_name(self.arena, g);
                match module.structs.get(name) {
                    Some(struct_sig) => struct_sig.methods.contains_key("_copy"),
                    None => false,
                }
            }
            _ => false,
        }
    }
}
