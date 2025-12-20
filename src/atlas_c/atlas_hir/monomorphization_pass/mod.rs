use crate::atlas_c::{
    atlas_hir::{
        HirModule,
        arena::HirArena,
        error::{
            HirError::{self, UnknownType},
            HirResult, NotEnoughGenericsError, NotEnoughGenericsOrigin, UnknownTypeError,
        },
        expr::HirExpr,
        item::{HirStruct, HirStructConstructor, HirUnion},
        monomorphization_pass::generic_pool::HirGenericPool,
        signature::HirGenericConstraint,
        stmt::HirStatement,
        ty::{HirGenericTy, HirListTy, HirMutableReferenceTy, HirReadOnlyReferenceTy, HirTy},
    },
    utils::{self, Span},
};
use miette::NamedSource;
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

        for (function_name, _) in self.generic_pool.functions.iter() {
            module.body.functions.remove(function_name);
            module.signature.functions.remove(function_name);
        }
    }

    pub fn monomorphize(
        &mut self,
        module: &'hir mut HirModule<'hir>,
    ) -> HirResult<&'hir mut HirModule<'hir>> {
        eprintln!("DEBUG: Starting monomorphization");
        //1. Generate only the signatures of the generic structs and functions
        while !self.process_pending_generics(module)? {
            eprintln!("DEBUG: Processing another round of generics");
        }
        eprintln!("DEBUG: Done processing generics");
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
        let non_generic_functions: Vec<&'hir str> = module.body.functions
            .iter()
            .filter(|(_, func)| func.signature.generics.is_none())
            .map(|(_, func)| func.name)
            .collect();
        
        eprintln!("DEBUG: Processing {} non-generic functions for monomorphization", non_generic_functions.len());
        
        // Debug: Print all functions in module body and signature
        eprintln!("DEBUG: Functions in module.body:");
        for (name, func) in &module.body.functions {
            eprintln!("  - {} signature.generics: {} signature.type_params: {}", name, func.signature.generics.is_some(), func.signature.type_params.len());
        }
        
        eprintln!("DEBUG: Functions in module.signature:");
        for (name, sig) in &module.signature.functions {
            eprintln!("  - {} has generics: {} type_params: {}", name, sig.generics.is_some(), sig.type_params.len());
            if let Some(gparams) = &sig.generics {
                for gp in gparams {
                    eprintln!("      generics - {}", gp.name);
                }
            }
            for tp in &sig.type_params {
                eprintln!("      type_params - {}", tp.name);
            }
        }
        
        // Monomorphize each function's body
        for func_name in non_generic_functions {
            eprintln!("DEBUG: Processing function {}", func_name);
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
                self.monomorphize_object(module, generic_ty, instance.span)?;
                instance.is_done = true;
                is_done = false;
            }
        }
        self.generic_pool.structs.append(&mut generic_pool_clone);

        // Process pending generic functions
        let mut function_pool_clone = self.generic_pool.functions.clone();
        for (_, instance) in function_pool_clone.iter_mut() {
            if !instance.is_done {
                let generic_ty = self.arena.intern(HirGenericTy {
                    name: instance.name,
                    inner: instance.args.clone(),
                    span: instance.span,
                });
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
            MonomorphizationPass::mangle_generic_object_name(self.arena, actual_type);
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
                    return self.monomorphize_union(
                        module,
                        template_clone,
                        actual_type,
                        mangled_name,
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

        for (name, func) in new_struct.signature.methods.iter_mut() {
            if func.generics.is_some() {
                // We just ignore methods with their own generics for now
                eprintln!(
                    "Warning: Skipping monomorphization of method {} because it has its own generics",
                    name
                );
                continue;
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
            MonomorphizationPass::mangle_generic_object_name(self.arena, actual_type);
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

        if let Some(generic_params) = generics {
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
                        generic_param.name,
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
                        generic_param.name,
                        actual_type.inner[j].clone(),
                        module,
                    );
                }
            }

            // Monomorphize return type - intern it first, then apply all substitutions in sequence
            let mut new_return_ty: &'hir HirTy<'hir> = self.arena.intern(new_function.signature.return_ty.clone()) as &'hir HirTy<'hir>;
            for (j, generic_param) in generic_params.iter().enumerate() {
                new_return_ty = self.change_inner_type(
                    new_return_ty,
                    generic_param.name,
                    actual_type.inner[j].clone(),
                    module,
                );
            }

            // Create new signature with monomorphized types and no generics
            let mut new_sig = new_function.signature.clone();
            new_sig.params = new_params;
            new_sig.return_ty = new_return_ty.clone();
            new_sig.generics = None;
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
                //TODO: Make LetStmt ty not optional then monomorphize it here
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
                if let HirTy::Generic(g) = new_obj_expr.ty {
                    self.generic_pool
                        .register_struct_instance(g.clone(), &module.signature);
                    let monomorphized_ty =
                        self.swap_generic_types_in_ty(new_obj_expr.ty, types_to_change.clone());

                    new_obj_expr.ty = monomorphized_ty;
                }
                for arg in new_obj_expr.args.iter_mut() {
                    self.monomorphize_expression(arg, types_to_change.clone(), module)?;
                }
            }
            HirExpr::ObjLiteral(obj_lit_expr) => {
                if let HirTy::Generic(g) = obj_lit_expr.ty {
                    self.generic_pool
                        .register_struct_instance(g.clone(), &module.signature);
                    let monomorphized_ty =
                        self.swap_generic_types_in_ty(obj_lit_expr.ty, types_to_change.clone());

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
                eprintln!("DEBUG: Processing Call expression");
                for arg in call_expr.args.iter_mut() {
                    self.monomorphize_expression(arg, types_to_change.clone(), module)?;
                }

                for generic in call_expr.generics.iter_mut() {
                    let monomorphized_ty =
                        self.swap_generic_types_in_ty(generic, types_to_change.clone());
                    *generic = monomorphized_ty;
                }

                eprintln!("DEBUG: Call has {} generic args", call_expr.generics.len());
                // Register generic function instances if the callee is a generic function
                if !call_expr.generics.is_empty() {
                    if let HirExpr::Ident(ident_expr) = &*call_expr.callee {
                        eprintln!("DEBUG: Call to function {}", ident_expr.name);
                        if let Some(func_sig) = module.signature.functions.get(ident_expr.name) {
                            eprintln!("DEBUG: Function {} found in signature, has generics: {}", ident_expr.name, func_sig.generics.is_some());
                            if let Some(generics) = &func_sig.generics {
                                eprintln!("DEBUG:   Generic params count: {}", generics.len());
                            }
                            if func_sig.generics.is_some() {
                                let generic_ty = HirGenericTy {
                                    name: ident_expr.name,
                                    inner: call_expr.generics.iter().map(|t| (*t).clone()).collect(),
                                    span: call_expr.span,
                                };
                                eprintln!("DEBUG: Registering generic function call: {}", ident_expr.name);
                                self.generic_pool
                                    .register_function_instance(generic_ty, &module.signature);
                            }
                        }
                    }
                }

                self.monomorphize_expression(&mut call_expr.callee, types_to_change, module)?;
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
                if let HirTy::List(l) = new_array_expr.ty {
                    if let HirTy::Generic(g) = l.inner {
                        self.generic_pool
                            .register_struct_instance(g.clone(), &module.signature);
                    } else if let HirTy::Named(n) = l.inner
                        && n.name.len() == 1
                    {
                        let ty = self
                            .swap_generic_types_in_ty(new_array_expr.ty, types_to_change.clone());
                        new_array_expr.ty = ty;
                    }
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
    pub fn mangle_generic_object_name(
        arena: &'hir HirArena<'hir>,
        generic: &HirGenericTy<'_>,
    ) -> &'hir str {
        let parts: Vec<String> = generic
            .inner
            .iter()
            .map(|t| match t {
                HirTy::Generic(g) => {
                    MonomorphizationPass::mangle_generic_object_name(arena, g).to_string()
                }
                _ => format!("{}", t),
            })
            .collect();
        let name = format!("__atlas77__struct__{}__{}", generic.name, parts.join("_"));
        arena.intern(name)
    }

    /// Compute a stable mangled name for a monomorphized function given its base name
    /// and the actual type arguments.
    #[inline]
    fn _mangle_function_name(&self, base_name: &str, actual_tys: &[&'hir HirTy<'hir>]) -> String {
        let parts: Vec<String> = actual_tys.iter().map(|t| format!("{}", t)).collect();
        format!("__atlas77__fun__{}__{}", base_name, parts.join("_"))
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
                self.generic_pool
                    .register_struct_instance(generic_ty, &module.signature);
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
