use crate::atlas_c::{
    atlas_hir::{
        HirModule,
        arena::HirArena,
        error::{
            HirError::{self, UnknownType},
            HirResult, NotEnoughGenericsError, NotEnoughGenericsOrigin, UnknownTypeError,
        },
        expr::HirExpr,
        generic_pool::HirGenericPool,
        item::{HirStruct, HirStructConstructor},
        signature::HirFunctionParameterSignature,
        stmt::HirStatement,
        ty::{
            HirGenericTy, HirListTy, HirMutableReferenceTy, HirNamedTy, HirReadOnlyReferenceTy,
            HirTy,
        },
    },
    utils::{self, Span},
};
use miette::NamedSource;

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
        let mut generic_pool_clone = self.generic_pool.structs.clone();
        for (_, instance) in generic_pool_clone.iter_mut() {
            if !instance.is_done {
                let generic_ty = self.arena.intern(HirGenericTy {
                    name: instance.name,
                    inner: instance.args.clone(),
                    span: instance.span,
                });
                self.monomorphize_struct(module, generic_ty, instance.span)?;
                instance.is_done = true;
                is_done = false;
            }
        }
        self.generic_pool.structs.append(&mut generic_pool_clone);
        Ok(is_done)
    }

    pub fn monomorphize_struct(
        &mut self,
        module: &mut HirModule<'hir>,
        actual_type: &'hir HirGenericTy<'hir>,
        span: Span,
    ) -> HirResult<&'hir HirTy<'hir>> {
        let mangled_name =
            MonomorphizationPass::mangle_generic_struct_name(self.arena, actual_type);
        if module.body.structs.contains_key(&mangled_name) {
            //Already monomorphized
            return Ok(self.arena.types().get_named_ty(mangled_name, span));
        }

        let base_name = actual_type.name;
        let template = match module.body.structs.get(base_name) {
            Some(s) => s,
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

        let mut new_struct = template.clone();
        //Collect generic names
        let generic_names = template.signature.generics.clone();
        if generic_names.len() != actual_type.inner.len() {
            let declaration_span = template.name_span;
            return Err(Self::not_enough_generics_err(
                base_name,
                actual_type.inner.len(),
                span,
                generic_names.len(),
                declaration_span,
            ));
        }

        let types_to_change: Vec<(&'hir str, &'hir HirTy<'hir>)> = generic_names
            .iter()
            .enumerate()
            .map(|(i, generic_name)| {
                (
                    *generic_name,
                    self.arena.intern(actual_type.inner[i].clone()) as &'hir HirTy<'hir>,
                )
            })
            .collect::<Vec<(&'hir str, &'hir HirTy<'hir>)>>();

        self.monomorphize_fields(&mut new_struct, &generic_names, actual_type)?;

        self.monomorphize_constructor(&mut new_struct.constructor, types_to_change.clone())?;
        self.monomorphize_constructor(&mut new_struct.destructor, types_to_change.clone())?;

        for (i, arg) in new_struct
            .signature
            .constructor
            .params
            .clone()
            .iter()
            .enumerate()
        {
            for (j, generic_name) in generic_names.iter().enumerate() {
                let new_ty =
                    self.change_inner_type(arg.ty, generic_name, actual_type.inner[j].clone());
                let new_ty = if let HirTy::Generic(g) = new_ty {
                    self.arena.intern(HirTy::Named(HirNamedTy {
                        name: MonomorphizationPass::mangle_generic_struct_name(
                            self.arena,
                            self.arena.intern(g),
                        ),
                        span: arg.span,
                    }))
                } else {
                    new_ty
                };
                //Update only if changed
                if new_ty != arg.ty {
                    new_struct.signature.constructor.params[i] = HirFunctionParameterSignature {
                        name: arg.name,
                        name_span: arg.name_span,
                        span: arg.span,
                        ty: new_ty,
                        ty_span: arg.ty_span,
                    };
                }
            }
        }

        for (_, func) in new_struct.signature.methods.iter_mut() {
            //args:
            for param in func.params.iter_mut() {
                for (i, generic_name) in generic_names.iter().enumerate() {
                    let type_to_change = self.arena.intern(param.ty.clone());
                    let new_ty = self
                        .change_inner_type(
                            type_to_change,
                            generic_name,
                            actual_type.inner[i].clone(),
                        )
                        .clone();
                    let new_ty = if let HirTy::Generic(g) = new_ty {
                        HirTy::Named(HirNamedTy {
                            name: MonomorphizationPass::mangle_generic_struct_name(
                                self.arena,
                                self.arena.intern(g),
                            ),
                            span: param.span,
                        })
                    } else {
                        new_ty
                    };
                    param.ty = self.arena.intern(new_ty);
                }
            }

            //ret_type:
            for (i, generic_name) in generic_names.iter().enumerate() {
                let type_to_change = self.arena.intern(func.return_ty.clone());
                func.return_ty = self
                    .change_inner_type(type_to_change, generic_name, actual_type.inner[i].clone())
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
                self.monomorphize_statement(statement, types_to_change.clone())?;
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

    fn monomorphize_constructor(
        &mut self,
        constructor: &mut HirStructConstructor<'hir>,
        types_to_change: Vec<(&'hir str, &'hir HirTy<'hir>)>,
    ) -> HirResult<()> {
        //Monomorphize params
        for param in constructor.params.iter_mut() {
            param.ty = self.swap_generic_types_in_ty(param.ty, types_to_change.clone());
        }

        //Monomorphize body
        for statement in constructor.body.statements.iter_mut() {
            self.monomorphize_statement(statement, types_to_change.clone())?;
        }
        Ok(())
    }

    fn monomorphize_fields(
        &mut self,
        new_struct: &mut HirStruct<'hir>,
        generic_names: &Vec<&'hir str>,
        actual_type: &'hir HirGenericTy<'hir>,
    ) -> HirResult<()> {
        for (_, field_signature) in new_struct.signature.fields.iter_mut() {
            for (i, generic_name) in generic_names.iter().enumerate() {
                field_signature.ty = self.change_inner_type(
                    field_signature.ty,
                    generic_name,
                    actual_type.inner[i].clone(),
                );
            }
        }
        Ok(())
    }

    fn monomorphize_statement(
        &mut self,
        statement: &mut HirStatement<'hir>,
        types_to_change: Vec<(&'hir str, &'hir HirTy<'hir>)>,
    ) -> HirResult<()> {
        match statement {
            HirStatement::Expr(expr_stmt) => {
                self.monomorphize_expression(&mut expr_stmt.expr, types_to_change)?;
            }
            HirStatement::Let(let_stmt) => {
                self.monomorphize_expression(&mut let_stmt.value, types_to_change)?;
            }
            HirStatement::While(while_stmt) => {
                for stmt in while_stmt.body.statements.iter_mut() {
                    self.monomorphize_statement(stmt, types_to_change.clone())?;
                }
                self.monomorphize_expression(&mut while_stmt.condition, types_to_change)?;
            }
            HirStatement::IfElse(if_else_stmt) => {
                for stmt in if_else_stmt.then_branch.statements.iter_mut() {
                    self.monomorphize_statement(stmt, types_to_change.clone())?;
                }
                if let Some(else_branch) = &mut if_else_stmt.else_branch {
                    for stmt in else_branch.statements.iter_mut() {
                        self.monomorphize_statement(stmt, types_to_change.clone())?;
                    }
                }
                self.monomorphize_expression(&mut if_else_stmt.condition, types_to_change)?;
            }
            HirStatement::Return(return_stmt) => {
                self.monomorphize_expression(&mut return_stmt.value, types_to_change)?;
            }
            _ => {}
        }
        Ok(())
    }

    fn monomorphize_expression(
        &mut self,
        expr: &mut HirExpr<'hir>,
        types_to_change: Vec<(&'hir str, &'hir HirTy<'hir>)>,
    ) -> HirResult<()> {
        match expr {
            HirExpr::NewObj(new_obj_expr) => {
                if let HirTy::Generic(g) = new_obj_expr.ty {
                    self.generic_pool.register_struct_instance(g.clone());
                    let monomorphized_ty =
                        self.swap_generic_types_in_ty(new_obj_expr.ty, types_to_change.clone());

                    new_obj_expr.ty = monomorphized_ty;
                }
                for arg in new_obj_expr.args.iter_mut() {
                    self.monomorphize_expression(arg, types_to_change.clone())?;
                }
            }
            HirExpr::Indexing(idx_expr) => {
                self.monomorphize_expression(&mut idx_expr.target, types_to_change.clone())?;
                self.monomorphize_expression(&mut idx_expr.index, types_to_change)?;
            }
            HirExpr::Assign(assign_expr) => {
                self.monomorphize_expression(&mut assign_expr.lhs, types_to_change.clone())?;
                self.monomorphize_expression(&mut assign_expr.rhs, types_to_change)?;
            }
            HirExpr::Unary(unary_expr) => {
                self.monomorphize_expression(&mut unary_expr.expr, types_to_change)?;
            }
            HirExpr::HirBinaryOperation(binary_expr) => {
                self.monomorphize_expression(&mut binary_expr.lhs, types_to_change.clone())?;
                self.monomorphize_expression(&mut binary_expr.rhs, types_to_change)?;
            }
            HirExpr::Call(call_expr) => {
                for arg in call_expr.args.iter_mut() {
                    self.monomorphize_expression(arg, types_to_change.clone())?;
                }
                self.monomorphize_expression(&mut call_expr.callee, types_to_change)?;
            }
            HirExpr::Casting(casting_expr) => {
                self.monomorphize_expression(&mut casting_expr.expr, types_to_change)?;
            }
            HirExpr::Delete(delete_expr) => {
                self.monomorphize_expression(&mut delete_expr.expr, types_to_change)?;
            }
            HirExpr::ListLiteral(list_expr) => {
                for item in list_expr.items.iter_mut() {
                    self.monomorphize_expression(item, types_to_change.clone())?;
                }
            }
            HirExpr::NewArray(new_array_expr) => {
                if let HirTy::List(l) = new_array_expr.ty {
                    if let HirTy::Generic(g) = l.inner {
                        self.generic_pool.register_struct_instance(g.clone());
                    } else if let HirTy::Named(n) = l.inner
                        && n.name.len() == 1
                    {
                        let ty = self
                            .swap_generic_types_in_ty(new_array_expr.ty, types_to_change.clone());
                        new_array_expr.ty = ty;
                    }
                }
                self.monomorphize_expression(&mut new_array_expr.size, types_to_change)?;
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
    pub fn mangle_generic_struct_name(
        arena: &'hir HirArena<'hir>,
        generic: &HirGenericTy<'_>,
    ) -> &'hir str {
        let parts: Vec<String> = generic
            .inner
            .iter()
            .map(|t| match t {
                HirTy::Generic(g) => {
                    MonomorphizationPass::mangle_generic_struct_name(arena, g).to_string()
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
                inner: self.change_inner_type(l.inner, generic_name, new_type),
            })),
            HirTy::Generic(g) => {
                let new_inner_types: Vec<HirTy<'hir>> = g
                    .inner
                    .iter()
                    .map(|inner_ty| {
                        self.change_inner_type(inner_ty, generic_name, new_type.clone())
                            .clone()
                    })
                    .collect();
                let generic_ty = HirGenericTy {
                    name: g.name,
                    inner: new_inner_types,
                    span: g.span,
                };
                let res = self.arena.intern(HirTy::Generic(generic_ty.clone()));
                self.generic_pool.register_struct_instance(generic_ty);
                res
            }
            HirTy::MutableReference(m) => {
                self.arena
                    .intern(HirTy::MutableReference(HirMutableReferenceTy {
                        inner: self.change_inner_type(m.inner, generic_name, new_type),
                    }))
            }
            HirTy::ReadOnlyReference(r) => {
                self.arena
                    .intern(HirTy::ReadOnlyReference(HirReadOnlyReferenceTy {
                        inner: self.change_inner_type(r.inner, generic_name, new_type),
                    }))
            }
            _ => type_to_change,
        }
    }
}
