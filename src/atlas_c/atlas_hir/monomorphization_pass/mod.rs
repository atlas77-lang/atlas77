use crate::atlas_c::atlas_hir::HirModule;
use crate::atlas_c::atlas_hir::arena::HirArena;
use crate::atlas_c::atlas_hir::error::HirError::UnknownType;
use crate::atlas_c::atlas_hir::error::{
    HirError, HirResult, NotEnoughGenericsError, UnknownTypeError,
};
use crate::atlas_c::atlas_hir::expr::HirExpr;
use crate::atlas_c::atlas_hir::generic_pool::HirGenericPool;
use crate::atlas_c::atlas_hir::signature::HirFunctionParameterSignature;
use crate::atlas_c::atlas_hir::stmt::HirStatement;
use crate::atlas_c::atlas_hir::ty::{HirGenericTy, HirListTy, HirNamedTy, HirTy};
use logos::Span;
use miette::{SourceOffset, SourceSpan};

//Maybe all the passes should share a common trait? Or be linked to a common context struct?
pub struct MonomorphizationPass<'hir> {
    arena: &'hir HirArena<'hir>,
    generic_pool: HirGenericPool<'hir>,
    //source code
    src: String,
}

impl<'hir> MonomorphizationPass<'hir> {
    pub fn new(
        arena: &'hir HirArena<'hir>,
        generic_pool: HirGenericPool<'hir>,
        src: String,
    ) -> Self {
        Self {
            arena,
            src,
            generic_pool,
        }
    }
    /// Clears all the generic structs & functions from the module body and signature.
    pub fn clear_generic(&mut self, module: &mut HirModule<'hir>) {
        for (_, instance) in self.generic_pool.structs.iter() {
            module.body.structs.remove(instance.name);
            module.signature.structs.remove(instance.name);
        }

        println!(
            "Module Body struct names: {:#?}",
            module.body.structs.keys()
        );

        for (function_name, _) in self.generic_pool.functions.iter() {
            module.body.functions.remove(function_name);
            module.signature.functions.remove(function_name);
        }
    }

    pub fn monomorphize(&mut self, module: &mut HirModule<'hir>) -> HirResult<()> {
        println!("Starting monomorphization pass... {:?}", self.generic_pool);
        //1. Generate only the signatures of the generic structs and functions
        println!(
            "self.generic_pool.structs.len() BEFORE = {}",
            self.generic_pool.structs.len()
        );
        while !self.just_do_it(module)? {}
        println!(
            "self.generic_pool.structs.len() AFTER = {}",
            self.generic_pool.structs.len()
        );
        //println!("After generating struct signatures: {:#?}", module.signature.structs);
        //2. If you encounter a generic struct or function instantiation (e.g. in the return type), register it to the pool
        //3. Generate the actual bodies of the structs & functions in the pool, if you encounter new instantiations while generating, register them too
        //4. Clear the generic structs & functions from the module body and signature
        self.clear_generic(module);

        Ok(())
    }

    fn just_do_it(&mut self, module: &mut HirModule<'hir>) -> HirResult<bool> {
        let mut is_done = true;
        let mut generic_pool_clone = self.generic_pool.structs.clone();
        for (name, instance) in generic_pool_clone.iter_mut() {
            if !instance.is_done {
                println!("1. NAME: {} & IS_DONE: {}", name, instance.is_done);
                let generic_ty = self.arena.intern(HirGenericTy {
                    name: instance.name,
                    inner: instance.args.clone(),
                    span: instance.span.clone(),
                });
                self.monomorphize_struct(module, generic_ty, instance.span.clone())?;
                instance.is_done = true;
                println!("2. NAME: {} & IS_DONE: {}", name, instance.is_done);
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
            return Ok(self.arena.types().get_named_ty(mangled_name, span.clone()));
        }

        let base_name = actual_type.name;
        let template = match module.body.structs.get(base_name) {
            Some(s) => s,
            None => {
                return Err(UnknownType(UnknownTypeError {
                    name: base_name.to_string(),
                    span: SourceSpan::new(SourceOffset::from(span.start), span.end - span.start),
                    src: self.src.clone(),
                }));
            }
        };

        let mut new_struct = template.clone();
        //Collect generic names
        let generic_names = template.signature.generics.clone();
        if generic_names.len() != actual_type.inner.len() {
            let declaration_span = template.name_span.clone();
            return Err(HirError::NotEnoughGenerics(NotEnoughGenericsError {
                ty_name: base_name.to_string(),
                expected: generic_names.len(),
                found: actual_type.inner.len(),
                declaration_span: SourceSpan::new(
                    SourceOffset::from(declaration_span.start),
                    declaration_span.end - declaration_span.start,
                ),
                error_span: SourceSpan::new(SourceOffset::from(span.start), span.end - span.start),
                src: self.src.clone(),
            }));
        }

        //TODO: Right now we can't have nested generics (i.e. Box<T> -> Vector<Box<int64>>)
        //TODO: Add a bunch of helper functions for that
        //TODO: NB: This would require calling the "monomorphize_struct" function recursively.
        //TODO: I'll do it once the basic monomorphization is working and stable enough.

        for (_, field_signature) in new_struct.signature.fields.iter_mut() {
            for (i, generic_name) in generic_names.iter().enumerate() {
                field_signature.ty = self.change_inner_type(
                    field_signature.ty,
                    generic_name,
                    actual_type.inner[i].clone(),
                );
            }
        }

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
                        span: arg.span.clone(),
                    }))
                } else {
                    new_ty
                };
                if new_ty != arg.ty {
                    new_struct.signature.constructor.params[i] = HirFunctionParameterSignature {
                        name: arg.name,
                        name_span: arg.name_span.clone(),
                        span: arg.span.clone(),
                        ty: new_ty,
                        ty_span: arg.ty_span.clone(),
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
                            span: param.span.clone(),
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

        let type_to_change: Vec<(&'hir str, &'hir HirTy<'hir>)> = generic_names
            .iter()
            .enumerate()
            .map(|(i, generic_name)| {
                (
                    *generic_name,
                    self.arena.intern(actual_type.inner[i].clone()) as &'hir HirTy<'hir>,
                )
            })
            .collect::<Vec<(&'hir str, &'hir HirTy<'hir>)>>();

        for method in new_struct.methods.iter_mut() {
            for statement in method.body.statements.iter_mut() {
                self.monomorphize_statement(module, statement, type_to_change.clone())?;
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

        Ok(self.arena.types().get_named_ty(mangled_name, span.clone()))
    }

    fn monomorphize_statement(
        &mut self,
        module: &mut HirModule<'hir>,
        statement: &mut HirStatement<'hir>,
        types_to_change: Vec<(&'hir str, &'hir HirTy<'hir>)>,
    ) -> HirResult<()> {
        match statement {
            HirStatement::Expr(expr_stmt) => {
                self.monomorphize_expression(module, &mut expr_stmt.expr, types_to_change)?;
            }
            HirStatement::Let(let_stmt) => {
                self.monomorphize_expression(module, &mut let_stmt.value, types_to_change)?;
            }
            HirStatement::While(while_stmt) => {
                for stmt in while_stmt.body.statements.iter_mut() {
                    self.monomorphize_statement(module, stmt, types_to_change.clone())?;
                }
                self.monomorphize_expression(module, &mut while_stmt.condition, types_to_change)?;
            }
            HirStatement::IfElse(if_else_stmt) => {
                for stmt in if_else_stmt.then_branch.statements.iter_mut() {
                    self.monomorphize_statement(module, stmt, types_to_change.clone())?;
                }
                if let Some(else_branch) = &mut if_else_stmt.else_branch {
                    for stmt in else_branch.statements.iter_mut() {
                        self.monomorphize_statement(module, stmt, types_to_change.clone())?;
                    }
                }
                self.monomorphize_expression(module, &mut if_else_stmt.condition, types_to_change)?;
            }
            HirStatement::Return(return_stmt) => {
                self.monomorphize_expression(module, &mut return_stmt.value, types_to_change)?;
            }
            _ => {}
        }
        Ok(())
    }

    fn monomorphize_expression(
        &mut self,
        module: &mut HirModule<'hir>,
        expr: &mut HirExpr<'hir>,
        types_to_change: Vec<(&'hir str, &'hir HirTy<'hir>)>,
    ) -> HirResult<()> {
        match expr {
            HirExpr::NewObj(new_obj_expr) => {
                if let HirTy::Generic(g) = new_obj_expr.ty {
                    self.generic_pool.register_struct_instance(g.clone());
                    let monomorphized_ty =
                        self.swap_generic_types_in_ty(new_obj_expr.ty, types_to_change.clone());
                    let monomorphized_ty = if let HirTy::Generic(g) = monomorphized_ty {
                        self.arena.intern(HirTy::Named(HirNamedTy {
                            name: MonomorphizationPass::mangle_generic_struct_name(self.arena, g),
                            span: new_obj_expr.span.clone(),
                        }))
                    } else {
                        monomorphized_ty
                    };
                    new_obj_expr.ty = monomorphized_ty;
                }
            }
            HirExpr::Indexing(idx_expr) => {
                self.monomorphize_expression(
                    module,
                    &mut idx_expr.target,
                    types_to_change.clone(),
                )?;
                self.monomorphize_expression(module, &mut idx_expr.index, types_to_change)?;
            }
            HirExpr::Assign(assign_expr) => {
                self.monomorphize_expression(
                    module,
                    &mut assign_expr.lhs,
                    types_to_change.clone(),
                )?;
                self.monomorphize_expression(module, &mut assign_expr.rhs, types_to_change)?;
            }
            HirExpr::Unary(unary_expr) => {
                self.monomorphize_expression(module, &mut unary_expr.expr, types_to_change)?;
            }
            HirExpr::HirBinaryOp(binary_expr) => {
                self.monomorphize_expression(
                    module,
                    &mut binary_expr.lhs,
                    types_to_change.clone(),
                )?;
                self.monomorphize_expression(module, &mut binary_expr.rhs, types_to_change)?;
            }
            HirExpr::Call(call_expr) => {
                for arg in call_expr.args.iter_mut() {
                    self.monomorphize_expression(module, arg, types_to_change.clone())?;
                }
                self.monomorphize_expression(module, &mut call_expr.callee, types_to_change)?;
            }
            HirExpr::Casting(casting_expr) => {
                self.monomorphize_expression(module, &mut casting_expr.expr, types_to_change)?;
            }
            HirExpr::Delete(delete_expr) => {
                self.monomorphize_expression(module, &mut delete_expr.expr, types_to_change)?;
            }
            HirExpr::ListLiteral(list_expr) => {
                for item in list_expr.items.iter_mut() {
                    self.monomorphize_expression(module, item, types_to_change.clone())?;
                }
            }
            HirExpr::NewArray(new_array_expr) => {
                if let HirTy::Generic(g) = new_array_expr.ty {
                    self.generic_pool.register_struct_instance(g.clone());
                }
                self.monomorphize_expression(module, &mut new_array_expr.size, types_to_change)?;
            }
            _ => {}
        }

        Ok(())
    }

    //This function swaps generic types in a given type according to the provided mapping.
    //It does not mangle the name, it just replaces the generic types with the actual types.
    fn swap_generic_types_in_ty(
        &self,
        ty: &'hir HirTy<'hir>,
        types_to_change: Vec<(&'hir str, &'hir HirTy<'hir>)>,
    ) -> &'hir HirTy<'hir> {
        println!(
            "Swapping in type: {}. types_to_change: {:?}",
            ty, types_to_change
        );
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
                    span: g.span.clone(),
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
        generic: &'hir HirGenericTy<'hir>,
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
    fn mangle_function_name(&self, base_name: &str, actual_tys: &[&'hir HirTy<'hir>]) -> String {
        let parts: Vec<String> = actual_tys.iter().map(|t| format!("{}", t)).collect();
        format!("__atlas77__fun__{}__{}", base_name, parts.join("_"))
    }

    /// Add a new struct signature to the module signature.
    ///
    /// The name of the new struct will be `__atlas77__StructType_actual_type_names`, so it's actually mangled.

    /// Helper function to change the inner type of generic type recursively if matches.
    fn change_inner_type(
        &self,
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
                self.arena.intern(HirTy::Generic(HirGenericTy {
                    name: g.name,
                    inner: new_inner_types,
                    span: g.span.clone(),
                }))
            }
            _ => type_to_change,
        }
    }
}
