use miette::NamedSource;

use crate::atlas_c::atlas_hir::arena::HirArena;
use crate::atlas_c::atlas_hir::error::{
    HirError, TooManyReferenceLevelsError, TypeDoesNotImplementRequiredConstraintError,
    TypeDoesNotImplementRequiredConstraintOrigin,
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
            .field("unions", &self.unions)
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
        //We need to check if it's an instantiated generics or a generic definition e.g.: Vector<T> or Vector<uint64>
        if !self.is_generic_instantiated(&generic, module) {
            return;
        }
        // Let's add a temporary check. References cannot be more than 2 levels deep (e.g.: &&T/&&const T/&const &T/&const &const T)
        fn count_reference_levels<'hir>(ty: &'hir HirTy<'hir>, current: usize) -> Option<usize> {
            if current > 2 {
                //Failed the check
                return None;
            }
            match ty {
                HirTy::ReadOnlyReference(inner) => count_reference_levels(inner.inner, current + 1),
                HirTy::MutableReference(inner) => count_reference_levels(inner.inner, current + 1),
                HirTy::PtrTy(inner) => count_reference_levels(inner.inner, current + 1),
                _ => Some(0),
            }
        }
        for node in generic.inner.iter() {
            if let Some(levels) = count_reference_levels(node, 0) {
                if levels > 2 {
                    let path = generic.span.path;
                    let src = crate::atlas_c::utils::get_file_content(path).unwrap();
                    let report: miette::ErrReport =
                        HirError::TooManyReferenceLevels(TooManyReferenceLevelsError {
                            span: generic.span,
                            src: NamedSource::new(path, src),
                        })
                        .into();
                    eprintln!("{:?}", report);
                    std::process::exit(1);
                }
            } else {
                let path = generic.span.path;
                let src = crate::atlas_c::utils::get_file_content(path).unwrap();
                let report: miette::ErrReport =
                    HirError::TooManyReferenceLevels(TooManyReferenceLevelsError {
                        span: generic.span,
                        src: NamedSource::new(path, src),
                    })
                    .into();
                eprintln!("{:?}", report);
                std::process::exit(1);
            }
        }

        //TODO: Differentiate between struct and union here
        let name = MonomorphizationPass::generate_mangled_name(self.arena, &generic, "struct");
        self.structs.entry(name).or_insert(HirGenericInstance {
            name: generic.name,
            args: generic.inner,
            is_done: false,
            span: generic.span,
        });
    }

    pub fn register_union_instance(
        &mut self,
        generic: &HirGenericTy<'hir>,
        module: &HirModuleSignature<'hir>,
    ) {
        //We need to check if it's an instantiated generics or a generic definition e.g.: Result<T> or Result<uint64>
        if !self.is_generic_instantiated(generic, module) {
            return;
        }
        let name = MonomorphizationPass::generate_mangled_name(self.arena, generic, "union");
        self.unions.entry(name).or_insert(HirGenericInstance {
            name: generic.name,
            args: generic.inner.clone(),
            is_done: false,
            span: generic.span,
        });
    }

    pub fn register_function_instance(
        &mut self,
        generic: HirGenericTy<'hir>,
        module: &HirModuleSignature<'hir>,
    ) {
        // Check for constraints if function has generics
        let mut is_external = false;
        if let Some(func_sig) = module.functions.get(generic.name)
            && !func_sig.generics.is_empty()
            && func_sig.generics.len() == generic.inner.len()
        {
            is_external = func_sig.is_external;
            // TODO: Validate that the concrete types satisfy the generic constraints
            // This stub implementation currently skips constraint checking
            for _param in func_sig.generics.iter() {
                // TODO: Check if each concrete type in generic.inner[i] satisfies constraints for _param
            }
        }

        if is_external {
            // External functions don't need monomorphization
            return;
        }
        // Check if this is an instantiated generic or a generic definition
        let is_instantiated = self.is_generic_instantiated(&generic, module);
        if !is_instantiated {
            return;
        }

        let mangled_name =
            MonomorphizationPass::generate_mangled_name(self.arena, &generic, "function");
        self.functions
            .entry(mangled_name)
            .or_insert(HirGenericInstance {
                name: generic.name,
                args: generic.inner,
                is_done: false,
                span: generic.span,
            });
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
                    // Check if this is actually a defined struct/union in the module
                    // If it's only 1 letter AND not defined as a struct/union, it's a generic type parameter
                    if n.name.len() == 1
                        && !module.structs.contains_key(n.name)
                        && !module.unions.contains_key(n.name)
                    {
                        is_instantiated = false;
                    }
                }
                HirTy::Generic(g) => {
                    //We register nested generics as well (e.g. MyStruct<Vector<uint64>>)
                    //This ensures that they are also monomorphized if it's the only instance
                    //But because the check is called in register_struct_instance it won't register generic definitions
                    //Check if the nested generic is itself instantiated
                    if !self.is_generic_instantiated(g, module) {
                        is_instantiated = false;
                    } else {
                        self.register_struct_instance(g.clone(), module);
                    }
                }
                HirTy::ReadOnlyReference(r) => match r.inner {
                    HirTy::Named(n) => {
                        // Check if this is actually a defined struct/union in the module
                        if n.name.len() == 1
                            && !module.structs.contains_key(n.name)
                            && !module.unions.contains_key(n.name)
                        {
                            is_instantiated = false;
                        }
                    }
                    HirTy::Generic(g) => {
                        if !self.is_generic_instantiated(g, module) {
                            is_instantiated = false;
                        } else {
                            self.register_struct_instance(g.clone(), module);
                        }
                    }
                    _ => continue,
                },
                HirTy::MutableReference(r) => match r.inner {
                    HirTy::Named(n) => {
                        // Check if this is actually a defined struct/union in the module
                        if n.name.len() == 1
                            && !module.structs.contains_key(n.name)
                            && !module.unions.contains_key(n.name)
                        {
                            is_instantiated = false;
                        }
                    }
                    HirTy::Generic(g) => {
                        if !self.is_generic_instantiated(g, module) {
                            is_instantiated = false;
                        } else {
                            self.register_struct_instance(g.clone(), module);
                        }
                    }
                    _ => continue,
                },
                HirTy::PtrTy(p) => match p.inner {
                    HirTy::Named(n) => {
                        // Check if this is actually a defined struct/union in the module
                        if n.name.len() == 1
                            && !module.structs.contains_key(n.name)
                            && !module.unions.contains_key(n.name)
                        {
                            is_instantiated = false;
                        }
                    }
                    HirTy::Generic(g) => {
                        if !self.is_generic_instantiated(g, module) {
                            is_instantiated = false;
                        } else {
                            self.register_struct_instance(g.clone(), module);
                        }
                    }
                    _ => continue,
                },
                _ => continue,
            }
        }
        is_instantiated
    }

    pub fn check_constraint_satisfaction(
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
                    HirGenericConstraintKind::Std {
                        name: "moveable",
                        span,
                    } => {
                        if !self.implements_std_moveable(module, instantiated_ty) {
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
                    HirGenericConstraintKind::Std { name: _, span } => {
                        //Other std constraints not implemented yet
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
    pub fn implements_std_copyable(
        &self,
        module: &HirModuleSignature<'hir>,
        ty: &HirTy<'hir>,
    ) -> bool {
        match ty {
            HirTy::Boolean(_)
            | HirTy::Int64(_)
            | HirTy::Float64(_)
            | HirTy::Char(_)
            | HirTy::String(_)
            | HirTy::UInt64(_)
            // References are copyable as they are just pointers
            | HirTy::ReadOnlyReference(_)
            | HirTy::MutableReference(_)
            | HirTy::PtrTy(_)
            // Function pointers are copyable, though I am still not sure if I want this behavior...
            // Maybe closures that capture environment shouldn't be copyable?
            | HirTy::Function(_) => true,
            // Lists are copyable by default, they are just a pointer to the heap data
            // THIS IS ONLY TEMPORARY. Lists need to be owned types, and if people want a reference to them,
            // They'll need to do &const [T] or &[T]
            // I just need to make c_vec works properly first (I still don't have the ptr<T> type implemented)
            HirTy::List(_) => true,
            HirTy::Named(n) => match module.structs.get(n.name) {
                Some(struct_sig) => {
                    struct_sig.copy_constructor.is_some()
                },
                None => {
                    false
                },
            },
            HirTy::Generic(g) => {
                let name = MonomorphizationPass::generate_mangled_name(self.arena, g, "struct");
                match module.structs.get(name) {
                    Some(struct_sig) => {
                        struct_sig.copy_constructor.is_some()
                    },
                    None => {
                        false
                    }
                }
            }
            _ => false,
        }
    }

    fn implements_std_moveable(&self, module: &HirModuleSignature<'hir>, ty: &HirTy<'hir>) -> bool {
        match ty {
            // For now we consider all primitive types as moveable
            HirTy::Boolean(_)
            | HirTy::Int64(_)
            | HirTy::Float64(_)
            | HirTy::Char(_)
            | HirTy::String(_)
            | HirTy::UInt64(_)
            // References are moveable as they are just pointers
            | HirTy::ReadOnlyReference(_)
            | HirTy::MutableReference(_)
            // Function pointers are moveable
            | HirTy::Function(_) => true,
            // For now we consider lists as moveable until we have a better way to handle them
            HirTy::List(_l) => true /*self.implements_std_moveable(module, l.inner)*/,
            HirTy::Named(n) => match module.structs.get(n.name) {
                Some(struct_sig) => {
                    struct_sig.move_constructor.is_some()
                },
                None => {
                    false
                },
            },
            HirTy::Generic(g) => {
                let name = MonomorphizationPass::generate_mangled_name(self.arena, g, "struct");
                match module.structs.get(name) {
                    Some(struct_sig) => {
                        struct_sig.move_constructor.is_some()
                    },
                    None => {
                        false
                    }
                }
            }
            _ => false,
        }
    }
}
