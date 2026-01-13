use std::collections::HashSet;

use crate::atlas_c::{
    atlas_hir::{
        HirModule,
        arena::HirArena,
        error::{
            HirError::{self, UnknownType},
            HirResult, NotEnoughGenericsError, NotEnoughGenericsOrigin, UnknownTypeError,
        },
        expr::{
            HirAssignExpr, HirExpr, HirFieldAccessExpr, HirIdentExpr, HirThisLiteral, HirUnaryOp,
            UnaryOpExpr,
        },
        item::{HirStruct, HirStructConstructor, HirUnion},
        monomorphization_pass::generic_pool::HirGenericPool,
        signature::{
            HirFunctionParameterSignature, HirGenericConstraint, HirGenericConstraintKind,
            HirModuleSignature, HirStructConstructorSignature, HirStructFieldSignature,
            HirTypeParameterItemSignature, HirVisibility,
        },
        stmt::{HirBlock, HirExprStmt, HirStatement},
        ty::{HirGenericTy, HirListTy, HirMutableReferenceTy, HirReadOnlyReferenceTy, HirTy},
        warning::{CannotGenerateACopyConstructorForThisTypeWarning, HirWarning},
    },
    utils::{self, Span, get_file_content},
};
use miette::{ErrReport, NamedSource};
pub(crate) mod generic_pool;

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
            if !signature.generics.is_empty() {
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
            if !signature.generics.is_empty() && !signature.is_external {
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
            if !signature.generics.is_empty() {
                module.signature.unions.remove(name);
                module.body.unions.remove(name);
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
        //4. Clear the generic structs & functions from the module body and signature
        self.clear_generic(module);

        Ok(module)
    }

    fn process_pending_generics(&mut self, module: &mut HirModule<'hir>) -> HirResult<bool> {
        let mut is_done = true;

        // First, monomorphize all non-generic function bodies to discover generic instantiations
        // We need to be careful about borrowing - collect the function names first
        let non_generic_functions: Vec<_> = module
            .body
            .functions
            .iter()
            .filter(|(_, func)| func.signature.generics.is_empty() && !func.signature.is_external)
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

        let mut generic_pool_clone = self.generic_pool.structs.clone();
        for (_, instance) in generic_pool_clone.iter_mut() {
            if !instance.is_done {
                let generic_ty = self.arena.intern(HirGenericTy {
                    name: instance.name,
                    inner: instance.args.clone(),
                    span: instance.span,
                });
                // Check constraints before monomorphizing (copy constructors are now available)
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
                // Check constraints before monomorphizing (copy constructors are now available)
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
                // Check constraints before monomorphizing (copy constructors are now available)
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

        self.monomorphize_constructor(
            &mut new_struct.constructor,
            types_to_change.clone(),
            module,
        )?;
        self.monomorphize_constructor(&mut new_struct.destructor, types_to_change.clone(), module)?;

        // Monomorphize copy constructor signature params first, then sync body params
        if let Some(copy_ctor_sig) = new_struct.signature.copy_constructor.as_mut() {
            // Check where_clause constraints for copy constructor
            if let Some(where_clause) = &copy_ctor_sig.where_clause {
                let is_satisfied = self.check_where_constraints_on_method(
                    where_clause,
                    &generics,
                    actual_type,
                    &module.signature,
                );
                copy_ctor_sig.is_constraint_satisfied = is_satisfied;

                // If constraints are not satisfied, remove the copy constructor body
                if !is_satisfied {
                    new_struct.copy_constructor = None;
                }
            }

            for arg in copy_ctor_sig.params.iter_mut() {
                for (j, generic) in generics.iter().enumerate() {
                    arg.ty = self.change_inner_type(
                        arg.ty,
                        generic.generic_name,
                        actual_type.inner[j].clone(),
                        module,
                    );
                }
            }
        }

        // Monomorphize copy constructor body (only if constraints are satisfied)
        if let Some(copy_ctor) = new_struct.copy_constructor.as_mut() {
            self.monomorphize_constructor(copy_ctor, types_to_change.clone(), module)?;
            // Sync body params from signature
            for (i, _) in copy_ctor.params.clone().iter().enumerate() {
                copy_ctor.params[i] = new_struct
                    .signature
                    .copy_constructor
                    .as_ref()
                    .unwrap()
                    .params[i]
                    .clone();
            }
        }

        for arg in new_struct.signature.constructor.params.iter_mut() {
            for (j, generic) in generics.iter().enumerate() {
                arg.ty = self.change_inner_type(
                    arg.ty,
                    generic.generic_name,
                    actual_type.inner[j].clone(),
                    module,
                );
            }
        }

        // Check and mark methods based on where_clause constraints
        let mut methods_constraint_status: std::collections::HashMap<&str, bool> =
            std::collections::HashMap::new();
        for (name, func) in new_struct.signature.methods.iter() {
            // Skip methods with their own generics for now
            if func.generics.is_some() {
                eprintln!(
                    "Warning: Skipping monomorphization of method {} because it has its own generics",
                    name
                );
                methods_constraint_status.insert(name, false);
                continue;
            }

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
                new_func.is_constraint_satisfied = is_satisfied;
                *func = new_func;
            }
        }

        // Remove unsatisfied methods from body only (keep in signature for error reporting)
        new_struct.methods.retain(|method| {
            methods_constraint_status
                .get(&method.name)
                .copied()
                .unwrap_or(true)
        });

        // Monomorphize remaining satisfied methods
        for (name, func) in new_struct.signature.methods.iter_mut() {
            if !methods_constraint_status.get(name).copied().unwrap_or(true) {
                continue; // Skip unsatisfied methods
            }

            //args:
            for param in func.params.iter_mut() {
                for (i, generic_name) in generics.iter().enumerate() {
                    param.ty = self.change_inner_type(
                        param.ty,
                        generic_name.generic_name,
                        actual_type.inner[i].clone(),
                        module,
                    );
                }
            }

            //ret_type:
            for (i, generic_name) in generics.iter().enumerate() {
                let type_to_change = self.arena.intern(func.return_ty.clone());
                func.return_ty = self
                    .change_inner_type(
                        type_to_change,
                        generic_name.generic_name,
                        actual_type.inner[i].clone(),
                        module,
                    )
                    .clone();
            }
            func.generics = None;
        }

        //At this point the signature is fully monomorphized, but the actual struct itself isn't.
        //We still need to change the new_struct.fields/new_struct.methods/new_struct.constructor

        for (i, field) in new_struct.fields.clone().iter().enumerate() {
            new_struct.fields[i] = new_struct.signature.fields.get(field.name).unwrap().clone();
        }
        for (i, method) in new_struct.methods.clone().iter().enumerate() {
            new_struct.methods[i].signature = self.arena.intern(
                new_struct
                    .signature
                    .methods
                    .get(method.name)
                    .unwrap()
                    .clone(),
            );
        }

        for (i, _) in new_struct.constructor.params.clone().iter().enumerate() {
            new_struct.constructor.params[i] = new_struct.signature.constructor.params[i].clone();
        }

        //And lastly, we need to update the statements inside the constructor and methods to reflect the new types.
        //It's mostly changing the name of every `new Struct<Generic>` to `new __atlas77__Struct__actual_types`

        for method in new_struct.methods.iter_mut() {
            for statement in method.body.statements.iter_mut() {
                self.monomorphize_statement(statement, types_to_change.clone(), module)?;
            }
        }

        new_struct.signature.generics = vec![];
        new_struct.name = mangled_name;
        new_struct.signature.name = mangled_name;

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
                if let Some(func) = module.signature.functions.get(base_name) {
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
            new_sig.generics = vec![];
            new_sig.pre_mangled_ty = Some(actual_type);
            new_function.signature = &*self.arena.intern(new_sig);

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

    fn monomorphize_constructor(
        &mut self,
        constructor: &mut HirStructConstructor<'hir>,
        types_to_change: Vec<(&'hir str, &'hir HirTy<'hir>)>,
        module: &HirModule<'hir>,
    ) -> HirResult<()> {
        //Monomorphize params
        for param in constructor.params.iter_mut() {
            param.ty = self.swap_generic_types_in_ty(param.ty, types_to_change.clone());
        }

        //Monomorphize body
        for statement in constructor.body.statements.iter_mut() {
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
                self.monomorphize_expression(&mut return_stmt.value, types_to_change, module)?;
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
            HirExpr::NewObj(new_obj_expr) => {
                if let HirTy::Generic(_g) = new_obj_expr.ty {
                    let monomorphized_ty =
                        self.swap_generic_types_in_ty(new_obj_expr.ty, types_to_change.clone());

                    // Register the monomorphized struct, not the generic one with unresolved parameters
                    if let HirTy::Generic(g_mono) = monomorphized_ty {
                        self.generic_pool
                            .register_struct_instance(g_mono.clone(), &module.signature);
                    }

                    new_obj_expr.ty = monomorphized_ty;
                }
                for arg in new_obj_expr.args.iter_mut() {
                    self.monomorphize_expression(arg, types_to_change.clone(), module)?;
                }
            }
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
            HirExpr::Assign(assign_expr) => {
                self.monomorphize_expression(
                    &mut assign_expr.lhs,
                    types_to_change.clone(),
                    module,
                )?;
                self.monomorphize_expression(&mut assign_expr.rhs, types_to_change, module)?;
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
                self.monomorphize_expression(&mut casting_expr.expr, types_to_change, module)?;
            }
            HirExpr::Delete(delete_expr) => {
                self.monomorphize_expression(&mut delete_expr.expr, types_to_change, module)?;
            }
            HirExpr::ListLiteral(list_expr) => {
                for item in list_expr.items.iter_mut() {
                    self.monomorphize_expression(item, types_to_change.clone(), module)?;
                }
            }
            HirExpr::NewArray(new_array_expr) => {
                if let HirTy::List(_l) = new_array_expr.ty {
                    let ty =
                        self.swap_generic_types_in_ty(new_array_expr.ty, types_to_change.clone());

                    if let HirTy::List(l_mono) = ty
                        && let HirTy::Generic(g_mono) = l_mono.inner
                    {
                        self.generic_pool
                            .register_struct_instance(g_mono.clone(), &module.signature);
                    }
                    new_array_expr.ty = ty;
                }
                self.monomorphize_expression(&mut new_array_expr.size, types_to_change, module)?;
            }
            HirExpr::StaticAccess(static_access) => {
                let monomorphized_ty =
                    self.swap_generic_types_in_ty(static_access.target, types_to_change.clone());

                static_access.target = monomorphized_ty;
            }
            _ => {}
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
            HirTy::List(l) => {
                let new_inner = self.swap_generic_types_in_ty(l.inner, types_to_change);
                self.arena
                    .intern(HirTy::List(HirListTy { inner: new_inner }))
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
            HirTy::MutableReference(m) => {
                let new_inner = self.swap_generic_types_in_ty(m.inner, types_to_change.clone());
                self.arena
                    .intern(HirTy::MutableReference(HirMutableReferenceTy {
                        inner: new_inner,
                    }))
            }
            HirTy::ReadOnlyReference(r) => {
                let new_inner = self.swap_generic_types_in_ty(r.inner, types_to_change.clone());
                self.arena
                    .intern(HirTy::ReadOnlyReference(HirReadOnlyReferenceTy {
                        inner: new_inner,
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
                _ => format!("{}", t),
            })
            .collect();
        let name = format!("__atlas77__{}__{}__{}", kind, generic.name, parts.join("_"));
        arena.intern(name)
    }

    /// Compute a stable mangled name for a monomorphized function given its base name
    /// and the actual type arguments.
    #[inline]
    fn _mangle_function_name(&self, base_name: &str, actual_tys: &[&'hir HirTy<'hir>]) -> String {
        let parts: Vec<String> = actual_tys.iter().map(|t| format!("{}", t)).collect();
        format!("__atlas77__fun__{}__{}", base_name, parts.join("_"))
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
                    match constraint_kind {
                        HirGenericConstraintKind::Std { name, .. } => {
                            // Check std::copyable constraint
                            if *name == "copyable"
                                && !self
                                    .generic_pool
                                    .implements_std_copyable(module_sig, concrete_type)
                            {
                                return false;
                            }
                        }
                        // Add more std constraints here as needed
                        // Once there is more std constraints, consider refactoring to a match statement

                        // Handle other constraint kinds as needed
                        _ => {
                            // For now, unknown constraints pass
                        }
                    }
                }
            }
        }

        true
    }

    /// Add a new struct signature to the module signature.
    ///
    /// The name of the new struct will be `__atlas77__StructType_actual_type_names`, so it's actually mangled.
    ///
    /// Helper function to change the inner type of generic type recursively if matches.
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
            HirTy::List(l) => self.arena.intern(HirTy::List(HirListTy {
                inner: self.change_inner_type(l.inner, generic_name, new_type, module),
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
            HirTy::MutableReference(m) => {
                self.arena
                    .intern(HirTy::MutableReference(HirMutableReferenceTy {
                        inner: self.change_inner_type(m.inner, generic_name, new_type, module),
                    }))
            }
            HirTy::ReadOnlyReference(r) => {
                self.arena
                    .intern(HirTy::ReadOnlyReference(HirReadOnlyReferenceTy {
                        inner: self.change_inner_type(r.inner, generic_name, new_type, module),
                    }))
            }
            _ => type_to_change,
        }
    }
}
