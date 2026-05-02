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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum StdCapability {
    Copyable,
    Default,
    Hashable,
    TriviallyCopyable,
}

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
    /// When false, instantiate fields/signature only and skip method expansion.
    pub monomorphize_methods: bool,
}

impl<'hir> HirGenericPool<'hir> {
    fn std_capability_from_name(name: &str) -> Option<StdCapability> {
        match name {
            "copyable" => Some(StdCapability::Copyable),
            "default" => Some(StdCapability::Default),
            "hashable" => Some(StdCapability::Hashable),
            "trivially_copyable" => Some(StdCapability::TriviallyCopyable),
            _ => None,
        }
    }

    fn type_is_primitive_capability(ty: &HirTy<'hir>, capability: StdCapability) -> bool {
        match capability {
            StdCapability::Copyable => matches!(
                ty,
                HirTy::Boolean(_)
                    | HirTy::Integer(_)
                    | HirTy::Float(_)
                    | HirTy::Char(_)
                    | HirTy::String(_)
                    | HirTy::UnsignedInteger(_)
                    | HirTy::PtrTy(_)
                    | HirTy::Function(_)
                    | HirTy::Slice(_)
                    | HirTy::Unit(_)
                    | HirTy::LiteralInteger(_)
                    | HirTy::LiteralUnsignedInteger(_)
                    | HirTy::LiteralFloat(_)
            ),
            StdCapability::Default | StdCapability::Hashable | StdCapability::TriviallyCopyable => {
                matches!(
                    ty,
                    HirTy::Boolean(_)
                        | HirTy::Integer(_)
                        | HirTy::Float(_)
                        | HirTy::Char(_)
                        | HirTy::String(_)
                        | HirTy::UnsignedInteger(_)
                        | HirTy::PtrTy(_)
                        | HirTy::Function(_)
                        | HirTy::Slice(_)
                        | HirTy::Unit(_)
                        | HirTy::LiteralInteger(_)
                        | HirTy::LiteralUnsignedInteger(_)
                        | HirTy::LiteralFloat(_)
                )
            }
        }
    }

    fn struct_implements_capability(
        module: &HirModuleSignature<'hir>,
        ty: &HirTy<'hir>,
        capability: StdCapability,
    ) -> bool {
        let get_signature_capability = |name: &str| {
            module
                .structs
                .get(name)
                .is_some_and(|sig| match capability {
                    StdCapability::Copyable => sig.is_std_copyable,
                    StdCapability::Default => sig.is_std_default,
                    StdCapability::Hashable => sig.is_std_hashable,
                    StdCapability::TriviallyCopyable => sig.is_trivially_copyable,
                })
        };

        match ty {
            HirTy::Named(n) => get_signature_capability(n.name),
            HirTy::Generic(g) => get_signature_capability(g.name),
            _ => false,
        }
    }

    fn implements_std_capability(
        &self,
        module: &HirModuleSignature<'hir>,
        ty: &HirTy<'hir>,
        capability: StdCapability,
    ) -> bool {
        if matches!(capability, StdCapability::Copyable) {
            // Keep copyability semantics centralized in HirTy for ownership rules.
            return ty.is_copyable(module);
        }

        if Self::type_is_primitive_capability(ty, capability) {
            return true;
        }

        if Self::struct_implements_capability(module, ty, capability) {
            return true;
        }

        match ty {
            HirTy::InlineArray(arr) => {
                self.implements_std_capability(module, arr.inner, capability)
            }
            _ => false,
        }
    }

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
        self.register_struct_instance_with_policy(generic, module, true);
    }

    pub fn register_struct_instance_with_policy(
        &mut self,
        generic: HirGenericTy<'hir>,
        module: &HirModuleSignature<'hir>,
        monomorphize_methods: bool,
    ) {
        //We need to check if it's an instantiated generics or a generic definition e.g.: Vector<T> or Vector<uint64>
        if !self.is_generic_instantiated(&generic, module) {
            return;
        }

        //TODO: Differentiate between struct and union here
        let name = MonomorphizationPass::generate_mangled_name(self.arena, &generic, "struct");
        if let Some(existing) = self.structs.get_mut(name) {
            existing.monomorphize_methods = existing.monomorphize_methods || monomorphize_methods;
            // Keep the earliest source span so errors point closer to the generic type
            // declaration/use site rather than nested field expressions discovered later.
            if generic.span.path == existing.span.path && generic.span.start < existing.span.start {
                existing.span = generic.span;
            }
            return;
        }

        self.structs.insert(
            name,
            HirGenericInstance {
                name: generic.name,
                args: generic.inner,
                is_done: false,
                span: generic.span,
                monomorphize_methods,
            },
        );
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
        if let Some(existing) = self.unions.get_mut(name) {
            if generic.span.path == existing.span.path && generic.span.start < existing.span.start {
                existing.span = generic.span;
            }
            return;
        }
        self.unions.insert(
            name,
            HirGenericInstance {
                name: generic.name,
                args: generic.inner.clone(),
                is_done: false,
                span: generic.span,
                monomorphize_methods: false,
            },
        );
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
        if let Some(existing) = self.functions.get_mut(mangled_name) {
            if generic.span.path == existing.span.path && generic.span.start < existing.span.start {
                existing.span = generic.span;
            }
            return;
        }
        self.functions.insert(
            mangled_name,
            HirGenericInstance {
                name: generic.name,
                args: generic.inner,
                is_done: false,
                span: generic.span,
                monomorphize_methods: false,
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
                    HirGenericConstraintKind::Std { name, span } => {
                        let Some(std_constraint) = Self::std_capability_from_name(name) else {
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
                            continue;
                        };

                        if !self.implements_std_capability(module, instantiated_ty, std_constraint)
                        {
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
    /// Checks if a type implements `std::copyable` e.g. If it's a primitive type or TBD.
    pub fn implements_std_copyable(
        &self,
        module: &HirModuleSignature<'hir>,
        ty: &HirTy<'hir>,
    ) -> bool {
        self.implements_std_capability(module, ty, StdCapability::Copyable)
    }

    pub fn implements_std_hashable(
        &self,
        module: &HirModuleSignature<'hir>,
        ty: &HirTy<'hir>,
    ) -> bool {
        self.implements_std_capability(module, ty, StdCapability::Hashable)
    }

    pub fn implements_std_default(
        &self,
        module: &HirModuleSignature<'hir>,
        ty: &HirTy<'hir>,
    ) -> bool {
        self.implements_std_capability(module, ty, StdCapability::Default)
    }

    pub fn implements_std_trivially_copyable(
        &self,
        module: &HirModuleSignature<'hir>,
        ty: &HirTy<'hir>,
    ) -> bool {
        self.implements_std_capability(module, ty, StdCapability::TriviallyCopyable)
    }
}
