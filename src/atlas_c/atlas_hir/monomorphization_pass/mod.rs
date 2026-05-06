use crate::atlas_c::{
    atlas_hir::{
        HirModule,
        arena::HirArena,
        error::{
            HirError::{self, UnknownType},
            HirResult, NotEnoughGenericsError, NotEnoughGenericsOrigin, UnknownTypeError,
        },
        expr::HirExpr,
        item::{HirStruct, HirStructDestructor, HirUnion},
        monomorphization_pass::generic_pool::HirGenericPool,
        signature::{HirGenericConstraint, HirModuleSignature},
        stmt::HirStatement,
        ty::{HirFunctionTy, HirGenericTy, HirInlineArrayTy, HirPtrTy, HirSliceTy, HirTy},
    },
    utils::{self, Span},
};
use miette::NamedSource;
pub mod generic_pool;

#[derive(Debug, Clone)]
pub struct MethodMonomorphizationRequest<'hir> {
    pub owner_name: &'hir str,
    pub method_name: &'hir str,
    pub generic_args: Vec<&'hir HirTy<'hir>>,
    pub span: Span,
}

//Maybe all the passes should share a common trait? Or be linked to a common context struct?
pub struct MonomorphizationPass<'hir> {
    arena: &'hir HirArena<'hir>,
    generic_pool: HirGenericPool<'hir>,
}

impl<'hir> MonomorphizationPass<'hir> {
    pub fn new(arena: &'hir HirArena<'hir>, generic_pool: HirGenericPool<'hir>) -> Self {
        Self {
            arena,
            generic_pool,
        }
    }
    /// Clears all the generic structs & functions from the module body and signature.
    pub fn clear_generic(&mut self, module: &mut HirModule<'hir>) {
        // Clear generic structs
        for (_, instance) in self.generic_pool.structs.iter() {
            module.body.structs.remove(instance.name);
            module.signature.structs.remove(instance.name);
        }
        for (name, signature) in module.signature.structs.clone().iter() {
            if !signature.is_instantiated {
                module.signature.structs.remove(name);
                module.body.structs.remove(name);
            }
        }

        // Clear generic functions
        for (_, instance) in self.generic_pool.functions.iter() {
            // Remove the ORIGINAL generic function definition, not the monomorphized version
            module.body.functions.remove(instance.name);
            module.signature.functions.remove(instance.name);
        }
        for (name, signature) in module.signature.functions.clone().iter() {
            if !signature.is_instantiated && !signature.is_external && !signature.is_intrinsic {
                module.signature.functions.remove(name);
                module.body.functions.remove(name);
            }
        }

        // Clear generic unions
        for (_, instance) in self.generic_pool.unions.iter() {
            module.body.unions.remove(instance.name);
            module.signature.unions.remove(instance.name);
        }
        for (name, signature) in module.signature.unions.clone().iter() {
            if !signature.is_instantiated {
                module.signature.unions.remove(name);
                module.body.unions.remove(name);
            }
        }

        // Remove unresolved generic method templates from concrete structs.
        // Concrete specializations generated on-demand remain because their
        // method signatures have `generics = None`.
        let struct_names: Vec<&'hir str> = module.body.structs.keys().copied().collect();
        for struct_name in struct_names {
            if let Some(struct_item) = module.body.structs.get_mut(struct_name) {
                struct_item
                    .methods
                    .retain(|method| method.signature.generics.is_none());
                struct_item
                    .signature
                    .methods
                    .retain(|_, method_sig| method_sig.generics.is_none());

                module.signature.structs.insert(
                    struct_name,
                    self.arena.intern(struct_item.signature.clone()),
                );
            }
        }
    }

    pub fn monomorphize(
        &mut self,
        module: &'hir mut HirModule<'hir>,
    ) -> HirResult<&'hir mut HirModule<'hir>> {
        //1. Generate only the signatures of the generic structs and functions
        while !self.process_pending_generics(module)? {}
        //2. If you encounter a generic struct or function instantiation (e.g. in the return type), register it to the pool
        //3. Generate the actual bodies of the structs & functions in the pool, if you encounter new instantiations while generating, register them too
        //4. Generic template cleanup is deferred until the caller finishes any
        //   on-demand monomorphization rounds.

        Ok(module)
    }

    pub fn monomorphize_requested_methods(
        &mut self,
        module: &mut HirModule<'hir>,
        requests: Vec<MethodMonomorphizationRequest<'hir>>,
    ) -> HirResult<bool> {
        let mut changed = false;
        for request in requests.iter() {
            if self.instantiate_method_from_request(module, request)? {
                changed = true;
            }
        }

        if changed {
            while !self.process_pending_generics(module)? {}
        }

        Ok(changed)
    }

    fn instantiate_method_from_request(
        &mut self,
        module: &mut HirModule<'hir>,
        request: &MethodMonomorphizationRequest<'hir>,
    ) -> HirResult<bool> {
        let mut owner = match module.body.structs.get(request.owner_name) {
            Some(s) => s.clone(),
            None => return Ok(false),
        };

        let (template_owner, struct_types_to_change): (
            HirStruct<'hir>,
            Vec<(&'hir str, &'hir HirTy<'hir>)>,
        ) = if let Some(pre_mangled_ty) = owner.signature.pre_mangled_ty {
            let template_owner = match module.body.structs.get(pre_mangled_ty.name) {
                Some(s) => s.clone(),
                None => return Ok(false),
            };
            let template_owner_sig = match module.signature.structs.get(pre_mangled_ty.name) {
                Some(s) => *s,
                None => return Ok(false),
            };
            if template_owner_sig.generics.len() != pre_mangled_ty.inner.len() {
                return Ok(false);
            }
            let types_to_change = template_owner_sig
                .generics
                .iter()
                .enumerate()
                .map(|(i, generic_constraint)| {
                    (
                        generic_constraint.generic_name,
                        self.arena.intern(pre_mangled_ty.inner[i].clone()) as &'hir HirTy<'hir>,
                    )
                })
                .collect();
            (template_owner, types_to_change)
        } else {
            (owner.clone(), vec![])
        };

        let template_sig = match template_owner.signature.methods.get(request.method_name) {
            Some(s) => s.clone(),
            None => return Ok(false),
        };
        let template_method = match template_owner
            .methods
            .iter()
            .find(|m| m.name == request.method_name)
        {
            Some(m) => m.clone(),
            None => return Ok(false),
        };

        let (materialized_method_name, method_types_to_change) =
            if let Some(method_generics) = template_sig.generics.clone() {
                if method_generics.len() != request.generic_args.len() {
                    return Ok(false);
                }

                let mangled_method_name = Self::generate_mangled_name(
                    self.arena,
                    &HirGenericTy {
                        name: request.method_name,
                        inner: request.generic_args.iter().map(|g| (*g).clone()).collect(),
                        span: request.span,
                    },
                    "method",
                );

                let types_to_change: Vec<(&'hir str, &'hir HirTy<'hir>)> = method_generics
                    .iter()
                    .enumerate()
                    .map(|(i, generic_constraint)| {
                        (generic_constraint.generic_name, request.generic_args[i])
                    })
                    .collect();
                (mangled_method_name, types_to_change)
            } else {
                if !request.generic_args.is_empty() {
                    return Ok(false);
                }
                (request.method_name, vec![])
            };

        let already_instantiated = owner
            .signature
            .methods
            .get(materialized_method_name)
            .map(|sig| sig.is_instantiated)
            .unwrap_or(false)
            || owner
                .methods
                .iter()
                .any(|m| m.name == materialized_method_name);
        if already_instantiated {
            return Ok(false);
        }

        let mut all_types_to_change = struct_types_to_change;
        all_types_to_change.extend(method_types_to_change);

        let mut new_sig = template_sig.clone();
        for param in new_sig.params.iter_mut() {
            param.ty = self.swap_generic_types_in_ty(param.ty, all_types_to_change.clone());
        }
        let return_ty = self.swap_generic_types_in_ty(
            self.arena.intern(new_sig.return_ty.clone()),
            all_types_to_change.clone(),
        );
        new_sig.return_ty = return_ty.clone();
        new_sig.generics = None;

        let is_constraint_satisfied = if let (Some(where_clause), Some(pre_mangled_ty)) =
            (&new_sig.where_clause, owner.signature.pre_mangled_ty)
        {
            if let Some(base_sig) = module.signature.structs.get(pre_mangled_ty.name) {
                self.check_where_constraints_on_method(
                    where_clause,
                    &base_sig.generics,
                    pre_mangled_ty,
                    &module.signature,
                )
            } else {
                true
            }
        } else {
            true
        };
        new_sig.is_constraint_satisfied = is_constraint_satisfied;
        new_sig.is_instantiated = is_constraint_satisfied;

        owner
            .signature
            .methods
            .insert(materialized_method_name, new_sig.clone());

        if is_constraint_satisfied {
            let mut new_method = template_method.clone();
            new_method.name = materialized_method_name;
            new_method.signature = self.arena.intern(new_sig.clone());

            for statement in new_method.body.statements.iter_mut() {
                self.monomorphize_statement(statement, all_types_to_change.clone(), module)?;
            }

            owner.methods.push(new_method);
        }

        module.signature.structs.insert(
            request.owner_name,
            self.arena.intern(owner.signature.clone()),
        );
        module.body.structs.insert(request.owner_name, owner);

        Ok(true)
    }

    fn process_pending_generics(&mut self, module: &mut HirModule<'hir>) -> HirResult<bool> {
        let mut is_done = true;

        // First, monomorphize all non-generic function bodies to discover generic instantiations
        // We need to be careful about borrowing - collect the function names first
        let non_generic_functions: Vec<_> = module
            .body
            .functions
            .iter()
            .filter(|(_, func)| {
                func.signature.generics.is_empty()
                    && !func.signature.is_external
                    && !func.signature.is_intrinsic
            })
            .map(|(_, func)| func.name)
            .collect();

        // Monomorphize each function's body
        for func_name in non_generic_functions.iter() {
            // Extract and clone the statements so we can process them
            let statements = if let Some(func) = module.body.functions.get(func_name) {
                func.body.statements.clone()
            } else {
                continue;
            };

            // Process the cloned statements and put them back
            let mut processed_stmts = statements;
            for statement in processed_stmts.iter_mut() {
                self.monomorphize_statement(statement, vec![], module)?;
            }

            // Update the function with the processed statements
            if let Some(func) = module.body.functions.get_mut(func_name) {
                func.body.statements = processed_stmts;
            }
        }

        // Also scan non-generic struct members to discover generic instantiations
        // used inside methods of concrete structs.
        let non_generic_structs: Vec<_> = module
            .body
            .structs
            .iter()
            .filter(|(_, s)| s.signature.generics.is_empty())
            .map(|(name, _)| *name)
            .collect();

        for struct_name in non_generic_structs.iter() {
            let mut updated_struct = if let Some(s) = module.body.structs.get(struct_name) {
                s.clone()
            } else {
                continue;
            };

            if let Some(dtor) = updated_struct.destructor.as_mut() {
                for statement in dtor.body.statements.iter_mut() {
                    self.monomorphize_statement(statement, vec![], module)?;
                }
            }

            for method in updated_struct.methods.iter_mut() {
                for statement in method.body.statements.iter_mut() {
                    self.monomorphize_statement(statement, vec![], module)?;
                }
            }

            module.body.structs.insert(*struct_name, updated_struct);
        }

        let mut generic_pool_clone = self.generic_pool.structs.clone();
        for (_, instance) in generic_pool_clone.iter_mut() {
            if !instance.is_done {
                let generic_ty = self.arena.intern(HirGenericTy {
                    name: instance.name,
                    inner: instance.args.clone(),
                    span: instance.span,
                });
                if let Some(struct_sig) = module.signature.structs.get(instance.name) {
                    let constraints = struct_sig.generics.clone();
                    if !constraints.is_empty()
                        && !self.generic_pool.check_constraint_satisfaction(
                            &module.signature,
                            generic_ty,
                            constraints,
                            struct_sig.name_span,
                        )
                    {
                        std::process::exit(1);
                    }
                }
                self.monomorphize_object(module, generic_ty, instance.span)?;
                instance.is_done = true;
                is_done = false;
            }
        }
        self.generic_pool.structs.append(&mut generic_pool_clone);

        let mut union_pool_clone = self.generic_pool.unions.clone();
        for (_, instance) in union_pool_clone.iter_mut() {
            if !instance.is_done {
                let generic_ty = self.arena.intern(HirGenericTy {
                    name: instance.name,
                    inner: instance.args.clone(),
                    span: instance.span,
                });
                if let Some(union_sig) = module.signature.unions.get(instance.name) {
                    let constraints = union_sig.generics.clone();
                    if !constraints.is_empty()
                        && !self.generic_pool.check_constraint_satisfaction(
                            &module.signature,
                            generic_ty,
                            constraints,
                            union_sig.name_span,
                        )
                    {
                        std::process::exit(1);
                    }
                }
                self.monomorphize_object(module, generic_ty, instance.span)?;
                instance.is_done = true;
                is_done = false;
            }
        }
        self.generic_pool.unions.append(&mut union_pool_clone);

        // Process pending generic functions
        let mut function_pool_clone = self.generic_pool.functions.clone();
        for (_, instance) in function_pool_clone.iter_mut() {
            if !instance.is_done {
                let generic_ty = self.arena.intern(HirGenericTy {
                    name: instance.name,
                    inner: instance.args.clone(),
                    span: instance.span,
                });
                if let Some(func_sig) = module.signature.functions.get(instance.name) {
                    let constraints = func_sig.generics.clone();
                    if !constraints.is_empty()
                        && !self.generic_pool.check_constraint_satisfaction(
                            &module.signature,
                            generic_ty,
                            constraints,
                            func_sig.span,
                        )
                    {
                        std::process::exit(1);
                    }
                }
                self.monomorphize_function(module, generic_ty, instance.span)?;
                instance.is_done = true;
                is_done = false;
            }
        }
        self.generic_pool.functions.append(&mut function_pool_clone);

        Ok(is_done)
    }

    //TODO: Add support for unions
    pub fn monomorphize_object(
        &mut self,
        module: &mut HirModule<'hir>,
        actual_type: &'hir HirGenericTy<'hir>,
        span: Span,
    ) -> HirResult<&'hir HirTy<'hir>> {
        let mangled_name =
            MonomorphizationPass::generate_mangled_name(self.arena, actual_type, "struct");
        if module.body.structs.contains_key(&mangled_name)
            || module.body.unions.contains_key(mangled_name)
        {
            //Already monomorphized
            return Ok(self.arena.types().get_named_ty(mangled_name, span));
        }

        let base_name = actual_type.name;
        match module.body.structs.get(base_name) {
            Some(template) => {
                let template_clone = template.clone();
                self.monomorphize_struct(module, template_clone, actual_type, mangled_name, span)
            }
            None => {
                if let Some(template) = module.body.unions.get(base_name) {
                    let template_clone = template.clone();
                    let union_mangled_name = MonomorphizationPass::generate_mangled_name(
                        self.arena,
                        actual_type,
                        "union",
                    );
                    return self.monomorphize_union(
                        module,
                        template_clone,
                        actual_type,
                        union_mangled_name,
                        span,
                    );
                }
                let path = span.path;
                let src = crate::atlas_c::utils::get_file_content(path).unwrap();
                Err(UnknownType(UnknownTypeError {
                    name: base_name.to_string(),
                    span,
                    src: NamedSource::new(path, src),
                }))
            }
        }
    }

    fn monomorphize_union(
        &mut self,
        module: &mut HirModule<'hir>,
        template: HirUnion<'hir>,
        actual_type: &'hir HirGenericTy<'hir>,
        mangled_name: &'hir str,
        span: Span,
    ) -> HirResult<&'hir HirTy<'hir>> {
        let base_name = actual_type.name;
        let mut new_union = template.clone();
        new_union.pre_mangled_ty = Some(actual_type);
        new_union.signature.pre_mangled_ty = Some(actual_type);
        new_union.signature.is_instantiated = true;
        //Collect generic names
        let generic_constraints = template.signature.generics.clone();
        if generic_constraints.len() != actual_type.inner.len() {
            let declaration_span = template.name_span;
            return Err(Self::not_enough_generics_err(
                base_name,
                actual_type.inner.len(),
                span,
                generic_constraints.len(),
                declaration_span,
            ));
        }

        for (_, variant_signature) in new_union.signature.variants.iter_mut() {
            for (i, generic_constraint) in generic_constraints.iter().enumerate() {
                variant_signature.ty = self.change_inner_type(
                    variant_signature.ty,
                    generic_constraint.generic_name,
                    actual_type.inner[i].clone(),
                    module,
                );
            }
        }
        // Keep the concrete body variants in sync with signature variants.
        for variant in new_union.variants.iter_mut() {
            for (i, generic_constraint) in generic_constraints.iter().enumerate() {
                variant.ty = self.change_inner_type(
                    variant.ty,
                    generic_constraint.generic_name,
                    actual_type.inner[i].clone(),
                    module,
                );
            }
        }
        new_union.signature.generics = vec![];
        new_union.name = mangled_name;
        new_union.signature.name = mangled_name;
        module
            .signature
            .unions
            .insert(mangled_name, self.arena.intern(new_union.signature.clone()));
        module.body.unions.insert(mangled_name, new_union);
        Ok(self.arena.types().get_named_ty(mangled_name, span))
    }

    fn monomorphize_struct(
        &mut self,
        module: &mut HirModule<'hir>,
        template: HirStruct<'hir>,
        actual_type: &'hir HirGenericTy<'hir>,
        mangled_name: &'hir str,
        span: Span,
    ) -> HirResult<&'hir HirTy<'hir>> {
        let base_name = actual_type.name;
        let mut new_struct = template.clone();
        new_struct.pre_mangled_ty = Some(actual_type);
        new_struct.signature.pre_mangled_ty = Some(actual_type);
        //Collect generic names
        let generics = template.signature.generics.clone();
        if generics.len() != actual_type.inner.len() {
            let declaration_span = template.name_span;
            return Err(Self::not_enough_generics_err(
                base_name,
                actual_type.inner.len(),
                span,
                generics.len(),
                declaration_span,
            ));
        }

        let types_to_change: Vec<(&'hir str, &'hir HirTy<'hir>)> = generics
            .iter()
            .enumerate()
            .map(|(i, generic_constraint)| {
                (
                    generic_constraint.generic_name,
                    self.arena.intern(actual_type.inner[i].clone()) as &'hir HirTy<'hir>,
                )
            })
            .collect::<Vec<(&'hir str, &'hir HirTy<'hir>)>>();

        self.monomorphize_fields(&mut new_struct, &generics, actual_type, module)?;

        if let Some(destructor) = new_struct.destructor.as_mut() {
            self.monomorphize_destructor(destructor, types_to_change.clone(), module)?;
        }

        // Check and mark methods based on where_clause constraints
        let mut methods_constraint_status: std::collections::HashMap<&str, bool> =
            std::collections::HashMap::new();
        for (name, func) in new_struct.signature.methods.iter() {
            // Check where_clause constraints (struct-level generics only)
            let is_satisfied = if let Some(where_clause) = &func.where_clause {
                self.check_where_constraints_on_method(
                    where_clause,
                    &generics,
                    actual_type,
                    &module.signature,
                )
            } else {
                true
            };

            methods_constraint_status.insert(name, is_satisfied);
        }

        // Mark methods with unsatisfied constraints in signature
        for (name, func) in new_struct.signature.methods.iter_mut() {
            if let Some(&is_satisfied) = methods_constraint_status.get(name) {
                let mut new_func = func.clone();
                for param in new_func.params.iter_mut() {
                    param.ty = self.swap_generic_types_in_ty(param.ty, types_to_change.clone());
                }
                let return_ty = self.swap_generic_types_in_ty(
                    self.arena.intern(new_func.return_ty.clone()),
                    types_to_change.clone(),
                );
                new_func.return_ty = return_ty.clone();
                new_func.is_constraint_satisfied = is_satisfied;
                new_func.is_instantiated = false;
                *func = new_func;
            }
        }

        // Methods on concrete generic structs are materialized lazily on demand.
        // Keep signatures for type checking/diagnostics, but defer body creation.
        new_struct.methods.clear();

        for (i, field) in new_struct.fields.clone().iter().enumerate() {
            new_struct.fields[i] = new_struct.signature.fields.get(field.name).unwrap().clone();
        }

        //new_struct.signature.generics = vec![];
        new_struct.name = mangled_name;
        new_struct.signature.name = mangled_name;
        new_struct.signature.is_instantiated = true;

        module.signature.structs.insert(
            mangled_name,
            self.arena.intern(new_struct.signature.clone()),
        );
        module.body.structs.insert(mangled_name, new_struct);

        Ok(self.arena.types().get_named_ty(mangled_name, span))
    }

    fn monomorphize_function(
        &mut self,
        module: &mut HirModule<'hir>,
        actual_type: &'hir HirGenericTy<'hir>,
        span: Span,
    ) -> HirResult<()> {
        let mangled_name =
            MonomorphizationPass::generate_mangled_name(self.arena, actual_type, "function");
        if module.body.functions.contains_key(mangled_name)
            || module.signature.functions.contains_key(mangled_name)
        {
            //Already monomorphized
            return Ok(());
        }

        let base_name = actual_type.name;
        let template = match module.body.functions.get(base_name) {
            Some(func) => func.clone(),
            None => {
                //Maybe it's an external function
                if module.signature.functions.contains_key(base_name) {
                    return Ok(());
                }
                let path = span.path;
                let src = crate::atlas_c::utils::get_file_content(path).unwrap();
                return Err(UnknownType(UnknownTypeError {
                    name: base_name.to_string(),
                    span,
                    src: NamedSource::new(path, src),
                }));
            }
        };

        let mut new_function = template.clone();
        let generics = new_function.signature.generics.clone();

        new_function.pre_mangled_ty = Some(actual_type);

        if !generics.is_empty() {
            let generic_params = &generics;
            if generic_params.len() != actual_type.inner.len() {
                let declaration_span = new_function.name_span;
                return Err(Self::not_enough_generics_err(
                    base_name,
                    actual_type.inner.len(),
                    span,
                    generic_params.len(),
                    declaration_span,
                ));
            }

            let types_to_change: Vec<(&'hir str, &'hir HirTy<'hir>)> = generic_params
                .iter()
                .enumerate()
                .map(|(i, generic_param)| {
                    (
                        generic_param.generic_name,
                        self.arena.intern(actual_type.inner[i].clone()) as &'hir HirTy<'hir>,
                    )
                })
                .collect();

            // Monomorphize parameter types by applying all type substitutions
            let mut new_params = new_function.signature.params.clone();
            for param in new_params.iter_mut() {
                for (j, generic_param) in generic_params.iter().enumerate() {
                    param.ty = self.change_inner_type(
                        param.ty,
                        generic_param.generic_name,
                        actual_type.inner[j].clone(),
                        module,
                    );
                }
            }

            // Monomorphize return type - intern it first, then apply all substitutions in sequence
            let mut new_return_ty: &'hir HirTy<'hir> =
                self.arena.intern(new_function.signature.return_ty.clone()) as &'hir HirTy<'hir>;
            for (j, generic_param) in generic_params.iter().enumerate() {
                new_return_ty = self.change_inner_type(
                    new_return_ty,
                    generic_param.generic_name,
                    actual_type.inner[j].clone(),
                    module,
                );
            }

            // Create new signature with monomorphized types and no generics
            let mut new_sig = new_function.signature.clone();
            new_sig.params = new_params;
            new_sig.return_ty = new_return_ty.clone();
            //new_sig.generics = vec![];
            new_sig.pre_mangled_ty = Some(actual_type);
            new_sig.is_instantiated = true;

            // Update the function's signature
            new_function.signature = self.arena.intern(new_sig);

            // Monomorphize function body
            for statement in new_function.body.statements.iter_mut() {
                self.monomorphize_statement(statement, types_to_change.clone(), module)?;
            }
        }

        new_function.name = mangled_name;

        let new_sig = self.arena.intern(new_function.signature.clone());
        module.signature.functions.insert(mangled_name, new_sig);
        module.body.functions.insert(mangled_name, new_function);

        Ok(())
    }

    fn monomorphize_destructor(
        &mut self,
        dtor: &mut HirStructDestructor<'hir>,
        types_to_change: Vec<(&'hir str, &'hir HirTy<'hir>)>,
        module: &HirModule<'hir>,
    ) -> HirResult<()> {
        //Monomorphize body
        for statement in dtor.body.statements.iter_mut() {
            self.monomorphize_statement(statement, types_to_change.clone(), module)?;
        }
        Ok(())
    }

    fn monomorphize_fields(
        &mut self,
        new_struct: &mut HirStruct<'hir>,
        generics: &Vec<&'hir HirGenericConstraint<'hir>>,
        actual_type: &'hir HirGenericTy<'hir>,
        module: &HirModule<'hir>,
    ) -> HirResult<()> {
        for (_, field_signature) in new_struct.signature.fields.iter_mut() {
            for (i, generic_name) in generics.iter().enumerate() {
                field_signature.ty = self.change_inner_type(
                    field_signature.ty,
                    generic_name.generic_name,
                    actual_type.inner[i].clone(),
                    module,
                );
            }
        }
        Ok(())
    }

    fn monomorphize_statement(
        &mut self,
        statement: &mut HirStatement<'hir>,
        types_to_change: Vec<(&'hir str, &'hir HirTy<'hir>)>,
        module: &HirModule<'hir>,
    ) -> HirResult<()> {
        match statement {
            HirStatement::Expr(expr_stmt) => {
                self.monomorphize_expression(&mut expr_stmt.expr, types_to_change, module)?;
            }
            HirStatement::Let(let_stmt) => {
                //Let's monomorphize the type if it's not uninitialized
                if let_stmt.ty != self.arena.types().get_uninitialized_ty() {
                    let monomorphized_ty =
                        self.swap_generic_types_in_ty(let_stmt.ty, types_to_change.clone());
                    let_stmt.ty = monomorphized_ty;
                }
                self.monomorphize_expression(&mut let_stmt.value, types_to_change, module)?;
            }
            HirStatement::Assign(assign_stmt) => {
                self.monomorphize_expression(
                    &mut assign_stmt.dst,
                    types_to_change.clone(),
                    module,
                )?;
                self.monomorphize_expression(&mut assign_stmt.val, types_to_change, module)?;
            }
            HirStatement::While(while_stmt) => {
                for stmt in while_stmt.body.statements.iter_mut() {
                    self.monomorphize_statement(stmt, types_to_change.clone(), module)?;
                }
                self.monomorphize_expression(&mut while_stmt.condition, types_to_change, module)?;
            }
            HirStatement::IfElse(if_else_stmt) => {
                for stmt in if_else_stmt.then_branch.statements.iter_mut() {
                    self.monomorphize_statement(stmt, types_to_change.clone(), module)?;
                }
                if let Some(else_branch) = &mut if_else_stmt.else_branch {
                    for stmt in else_branch.statements.iter_mut() {
                        self.monomorphize_statement(stmt, types_to_change.clone(), module)?;
                    }
                }
                self.monomorphize_expression(&mut if_else_stmt.condition, types_to_change, module)?;
            }
            HirStatement::Return(return_stmt) => {
                if let Some(expr) = &mut return_stmt.value {
                    self.monomorphize_expression(expr, types_to_change, module)?;
                }
            }
            HirStatement::Block(block_stmt) => {
                for stmt in block_stmt.statements.iter_mut() {
                    self.monomorphize_statement(stmt, types_to_change.clone(), module)?;
                }
            }
            _ => {}
        }
        Ok(())
    }

    fn monomorphize_expression(
        &mut self,
        expr: &mut HirExpr<'hir>,
        types_to_change: Vec<(&'hir str, &'hir HirTy<'hir>)>,
        module: &HirModule<'hir>,
    ) -> HirResult<()> {
        match expr {
            HirExpr::Ident(_)
            | HirExpr::FloatLiteral(_)
            | HirExpr::CharLiteral(_)
            | HirExpr::IntegerLiteral(_)
            | HirExpr::UnsignedIntegerLiteral(_)
            | HirExpr::UnitLiteral(_)
            | HirExpr::BooleanLiteral(_)
            | HirExpr::ThisLiteral(_)
            | HirExpr::StringLiteral(_)
            | HirExpr::NullLiteral(_)
            | HirExpr::FieldAccess(_) => {}
            HirExpr::ObjLiteral(obj_lit_expr) => {
                if let HirTy::Generic(_g) = obj_lit_expr.ty {
                    let monomorphized_ty =
                        self.swap_generic_types_in_ty(obj_lit_expr.ty, types_to_change.clone());

                    // Register the monomorphized union, not the generic one with unresolved parameters
                    if let HirTy::Generic(g_mono) = monomorphized_ty {
                        self.generic_pool
                            .register_union_instance(g_mono, &module.signature);
                    }

                    obj_lit_expr.ty = monomorphized_ty;
                }
                for field_init in obj_lit_expr.fields.iter_mut() {
                    self.monomorphize_expression(
                        &mut field_init.value,
                        types_to_change.clone(),
                        module,
                    )?;
                }
            }
            HirExpr::Indexing(idx_expr) => {
                self.monomorphize_expression(
                    &mut idx_expr.target,
                    types_to_change.clone(),
                    module,
                )?;
                self.monomorphize_expression(&mut idx_expr.index, types_to_change, module)?;
            }
            HirExpr::Unary(unary_expr) => {
                self.monomorphize_expression(&mut unary_expr.expr, types_to_change, module)?;
            }
            HirExpr::HirBinaryOperation(binary_expr) => {
                self.monomorphize_expression(
                    &mut binary_expr.lhs,
                    types_to_change.clone(),
                    module,
                )?;
                self.monomorphize_expression(&mut binary_expr.rhs, types_to_change, module)?;
            }
            HirExpr::Call(call_expr) => {
                // First, check if this is an external function call
                let is_external = if let HirExpr::Ident(ident_expr) = &*call_expr.callee {
                    module
                        .signature
                        .functions
                        .get(ident_expr.name)
                        .map(|sig| sig.is_external)
                        .unwrap_or(false)
                } else {
                    false
                };

                // Always monomorphize arguments
                for arg in call_expr.args.iter_mut() {
                    self.monomorphize_expression(arg, types_to_change.clone(), module)?;
                }

                // Always monomorphize generic type arguments (for both external and non-external)
                for generic in call_expr.generics.iter_mut() {
                    let monomorphized_ty =
                        self.swap_generic_types_in_ty(generic, types_to_change.clone());
                    *generic = monomorphized_ty;
                }

                // For external functions, skip registration and callee monomorphization
                // (which would mangle the function name)
                if !is_external {
                    // Register generic function instances if the callee is a generic function
                    if !call_expr.generics.is_empty()
                        && let HirExpr::Ident(ident_expr) = &*call_expr.callee
                        && let Some(func_sig) = module.signature.functions.get(ident_expr.name)
                        && !func_sig.generics.is_empty()
                    {
                        let generic_ty = HirGenericTy {
                            name: ident_expr.name,
                            inner: call_expr.generics.iter().map(|t| (*t).clone()).collect(),
                            span: call_expr.span,
                        };
                        self.generic_pool
                            .register_function_instance(generic_ty, &module.signature);
                    }

                    // Monomorphize the callee itself (potentially mangles the name)
                    self.monomorphize_expression(&mut call_expr.callee, types_to_change, module)?;
                }
            }
            HirExpr::Casting(casting_expr) => {
                self.monomorphize_expression(
                    &mut casting_expr.expr,
                    types_to_change.clone(),
                    module,
                )?;
                let monomorphized_ty =
                    self.swap_generic_types_in_ty(casting_expr.target_ty, types_to_change);
                casting_expr.target_ty = monomorphized_ty;
            }
            HirExpr::Delete(delete_expr) => {
                self.monomorphize_expression(&mut delete_expr.expr, types_to_change, module)?;
            }
            HirExpr::ListLiteral(list_expr) => {
                for item in list_expr.items.iter_mut() {
                    self.monomorphize_expression(item, types_to_change.clone(), module)?;
                }
            }
            HirExpr::ListLiteralWithSize(list_expr) => {
                self.monomorphize_expression(&mut list_expr.item, types_to_change, module)?;
            }
            HirExpr::StaticAccess(static_access) => {
                let monomorphized_ty =
                    self.swap_generic_types_in_ty(static_access.target, types_to_change.clone());

                if let HirTy::Generic(generic_ty) = monomorphized_ty {
                    if module.signature.structs.contains_key(generic_ty.name) {
                        self.generic_pool
                            .register_struct_instance(generic_ty.clone(), &module.signature);
                    } else if module.signature.unions.contains_key(generic_ty.name) {
                        self.generic_pool
                            .register_union_instance(generic_ty, &module.signature);
                    }
                }

                static_access.target = monomorphized_ty;
            }
            HirExpr::IntrinsicCall(intrinsic) => {
                for arg_ty in intrinsic.args_ty.iter_mut() {
                    let monomorphized_ty =
                        self.swap_generic_types_in_ty(arg_ty, types_to_change.clone());
                    *arg_ty = monomorphized_ty;
                }
                for arg in intrinsic.args.iter_mut() {
                    self.monomorphize_expression(arg, types_to_change.clone(), module)?;
                }
            }
        }

        Ok(())
    }

    fn not_enough_generics_err(
        ty_name: &str,
        found: usize,
        error_span: Span,
        expected: usize,
        declaration_span: Span,
    ) -> HirError {
        let expected_path = declaration_span.path;
        let expected_src = utils::get_file_content(expected_path).unwrap();
        let origin = NotEnoughGenericsOrigin {
            expected,
            declaration_span,
            src: NamedSource::new(expected_path, expected_src),
        };
        let found_path = error_span.path;
        let found_src = utils::get_file_content(found_path).unwrap();
        HirError::NotEnoughGenerics(NotEnoughGenericsError {
            ty_name: ty_name.to_string(),
            origin,
            found,
            error_span,
            src: NamedSource::new(found_path, found_src),
        })
    }

    //This function swaps generic types in a given type according to the provided mapping.
    //It does not mangle the name, it just replaces the generic types with the actual types.
    fn swap_generic_types_in_ty(
        &self,
        ty: &'hir HirTy<'hir>,
        types_to_change: Vec<(&'hir str, &'hir HirTy<'hir>)>,
        //module: &HirModule<'hir>,
    ) -> &'hir HirTy<'hir> {
        match ty {
            HirTy::Named(n) => {
                for (generic_name, actual_ty) in types_to_change.iter() {
                    if n.name == *generic_name {
                        return actual_ty;
                    }
                }
                ty
            }
            HirTy::Slice(l) => {
                let new_inner = self.swap_generic_types_in_ty(l.inner, types_to_change);
                self.arena
                    .intern(HirTy::Slice(HirSliceTy { inner: new_inner }))
            }
            HirTy::InlineArray(arr) => {
                let new_inner = self.swap_generic_types_in_ty(arr.inner, types_to_change);
                self.arena.intern(HirTy::InlineArray(HirInlineArrayTy {
                    inner: new_inner,
                    size: arr.size,
                }))
            }
            HirTy::Generic(g) => {
                let new_inner_types: Vec<HirTy<'hir>> = g
                    .inner
                    .iter()
                    .map(|inner_ty| {
                        self.swap_generic_types_in_ty(inner_ty, types_to_change.clone())
                            .clone()
                    })
                    .collect();
                self.arena.intern(HirTy::Generic(HirGenericTy {
                    name: g.name,
                    inner: new_inner_types,
                    span: g.span,
                }))
            }
            HirTy::PtrTy(ptr_ty) => {
                let new_inner = self.swap_generic_types_in_ty(ptr_ty.inner, types_to_change);
                self.arena.intern(HirTy::PtrTy(HirPtrTy {
                    inner: new_inner,
                    is_const: ptr_ty.is_const,
                    span: ptr_ty.span,
                }))
            }
            HirTy::Function(fn_ty) => {
                let new_ret = self.swap_generic_types_in_ty(fn_ty.ret_ty, types_to_change.clone());
                let new_params = fn_ty
                    .params
                    .iter()
                    .map(|p| {
                        self.swap_generic_types_in_ty(p, types_to_change.clone())
                            .clone()
                    })
                    .collect();
                self.arena.intern(HirTy::Function(HirFunctionTy {
                    ret_ty: new_ret,
                    ret_ty_span: fn_ty.ret_ty_span,
                    params: new_params,
                    param_spans: fn_ty.param_spans.clone(),
                    span: fn_ty.span,
                }))
            }
            _ => ty,
        }
    }

    /// Produce a stable mangled name for a generic instantiation.
    ///
    /// Format: __atlas77__<base_name>__<type1>_<type2>_..._<typeN>
    // TODO: It should take an enum for the kind instead of a &str
    pub fn generate_mangled_name(
        arena: &'hir HirArena<'hir>,
        generic: &HirGenericTy<'_>,
        kind: &str,
    ) -> &'hir str {
        let parts: Vec<String> = generic
            .inner
            .iter()
            .map(|t| match t {
                HirTy::Generic(g) => Self::generate_mangled_name(arena, g, kind).to_string(),
                _ => t.get_valid_c_string(),
            })
            .collect();
        let name = format!("_{}_{}_{}_A77", kind, generic.name, parts.join("_"));
        arena.intern(name)
    }

    /// Check if a method's where_clause constraints are satisfied by the concrete types.
    /// This only checks struct-level generic constraints.
    fn check_where_constraints_on_method(
        &self,
        where_clause: &[&'hir HirGenericConstraint<'hir>],
        struct_generics: &[&'hir HirGenericConstraint<'hir>],
        actual_type: &'hir HirGenericTy<'hir>,
        module_sig: &HirModuleSignature<'hir>,
    ) -> bool {
        // For each constraint in the where_clause, find the corresponding concrete type
        for constraint in where_clause {
            // Find which generic parameter index this constraint refers to
            let generic_index = struct_generics
                .iter()
                .position(|g| g.generic_name == constraint.generic_name);

            if let Some(index) = generic_index {
                if index >= actual_type.inner.len() {
                    // Generic index out of bounds - constraint can't be satisfied
                    return false;
                }

                let concrete_type = &actual_type.inner[index];

                // Check each constraint kind
                for constraint_kind in &constraint.kind {
                    if !self.generic_pool.is_constraint_kind_satisfied(
                        module_sig,
                        concrete_type,
                        constraint_kind,
                    ) {
                        return false;
                    }
                }
            }
        }

        true
    }

    fn change_inner_type(
        &mut self,
        type_to_change: &'hir HirTy<'hir>,
        generic_name: &'hir str,
        new_type: HirTy<'hir>,
        module: &HirModule<'hir>,
    ) -> &'hir HirTy<'hir> {
        match type_to_change {
            HirTy::Named(n) => {
                if n.name == generic_name {
                    self.arena.intern(new_type)
                } else {
                    type_to_change
                }
            }
            HirTy::Slice(l) => self.arena.intern(HirTy::Slice(HirSliceTy {
                inner: self.change_inner_type(l.inner, generic_name, new_type, module),
            })),
            HirTy::InlineArray(arr) => self.arena.intern(HirTy::InlineArray(HirInlineArrayTy {
                inner: self.change_inner_type(arr.inner, generic_name, new_type, module),
                size: arr.size,
            })),
            HirTy::Generic(g) => {
                let new_inner_types: Vec<HirTy<'hir>> = g
                    .inner
                    .iter()
                    .map(|inner_ty| {
                        self.change_inner_type(inner_ty, generic_name, new_type.clone(), module)
                            .clone()
                    })
                    .collect();
                let generic_ty = HirGenericTy {
                    name: g.name,
                    inner: new_inner_types,
                    span: g.span,
                };
                let res = self.arena.intern(HirTy::Generic(generic_ty.clone()));
                // An `something<T>` could be either an union or a struct, we need to check it here:
                if module.signature.structs.contains_key(g.name) {
                    self.generic_pool
                        .register_struct_instance(generic_ty, &module.signature);
                } else if module.signature.unions.contains_key(g.name) {
                    self.generic_pool
                        .register_union_instance(&generic_ty, &module.signature);
                }
                res
            }
            HirTy::Function(func) => {
                let new_ret =
                    self.change_inner_type(func.ret_ty, generic_name, new_type.clone(), module);
                let new_params = func
                    .params
                    .iter()
                    .map(|p| {
                        self.change_inner_type(p, generic_name, new_type.clone(), module)
                            .clone()
                    })
                    .collect();
                self.arena.intern(HirTy::Function(HirFunctionTy {
                    ret_ty: new_ret,
                    ret_ty_span: func.ret_ty_span,
                    params: new_params,
                    param_spans: func.param_spans.clone(),
                    span: func.span,
                }))
            }
            HirTy::PtrTy(ptr_ty) => self.arena.intern(HirTy::PtrTy(HirPtrTy {
                inner: self.change_inner_type(ptr_ty.inner, generic_name, new_type, module),
                is_const: ptr_ty.is_const,
                span: ptr_ty.span,
            })),
            _ => type_to_change,
        }
    }
}
