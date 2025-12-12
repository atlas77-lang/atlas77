mod context;
pub use context::{ScopeMap, VarData, VarMap, VarStatus};

use crate::atlas_c::{
    atlas_hir::{
        HirModule,
        error::{HirError, HirResult},
        expr::{HirDeleteExpr, HirExpr, HirIdentExpr},
        item::HirFunction,
        lifetime_pass::context::VarKind,
        stmt::{HirExprStmt, HirStatement},
        ty::{HirTy, HirUnitTy},
    },
    utils::Span,
};
/// TODO: Implement "COPY" semantics for types that have a copy constructor.
/// For now it copy is only applied to primitive types (int, float, bool, char) as well as ref types.
///
/// Check moves and copies to ensure lifetimes are respected
/// Will be renamed later.
/// Also add a `delete my_var;` statement at the end of each scope to explicitly delete variables.
#[derive(Debug)]
pub struct LifeTimePass<'hir> {
    pub scope_map: ScopeMap<'hir>,
    // Collect errors during the pass
    pub errors: Vec<HirError>,
}

impl Default for LifeTimePass<'_> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'hir> LifeTimePass<'hir> {
    pub fn new() -> Self {
        Self {
            scope_map: ScopeMap::new(),
            errors: Vec::new(),
        }
    }
    //TODO: Add a way to know if an error is fatal or not
    pub fn run(&mut self, hir: &'hir mut HirModule<'hir>) -> HirResult<&'hir mut HirModule<'hir>> {
        for func in hir.body.functions.values_mut() {
            match self.analyze_function(func) {
                Ok(_) => {}
                Err(e) => self.errors.push(e),
            }
        }
        Ok(hir)
    }

    pub fn analyze_function(&mut self, func: &mut HirFunction<'hir>) -> HirResult<()> {
        for arg in func.signature.params.iter() {
            let kind = match arg.ty {
                HirTy::Boolean(_)
                | HirTy::Int64(_)
                | HirTy::Float64(_)
                | HirTy::Char(_)
                | HirTy::UInt64(_) => VarKind::Primitive,
                HirTy::ReadOnlyReference(_) | HirTy::MutableReference(_) => VarKind::Reference,
                HirTy::String(_) | HirTy::List(_) | HirTy::Named(_) | HirTy::Generic(_) => {
                    VarKind::Object
                }
                _ => {
                    unimplemented!("Type kind not implemented yet in lifetime analysis")
                }
            };
            self.scope_map.insert(
                arg.name,
                VarData {
                    name: arg.name,
                    status: VarStatus::Valid,
                    span: arg.span,
                    kind,
                },
            );
        }
        for stmt in func.body.statements.iter_mut() {
            match self.analyze_stmt(stmt) {
                Ok(_) => {}
                Err(e) => self.errors.push(e),
            }
        }
        Ok(())
    }

    /// Returns Ok(true) if the statement is a return statement
    ///
    /// So we can track variables that need to be deleted before returning
    pub fn analyze_stmt(&mut self, stmt: &mut HirStatement<'hir>) -> HirResult<bool> {
        match stmt {
            HirStatement::IfElse(if_else) => {
                // Let's store the current scope map so we can restore it for the else branch
                let snapshot = self.scope_map.clone();
                // Analyze the if branch
                self.analyze_expr(&if_else.condition)?;
                for stmt in if_else.then_branch.statements.iter_mut() {
                    match self.analyze_stmt(stmt) {
                        Ok(true) => {
                            let ret_stmt = if_else.then_branch.statements.pop().unwrap();
                            self.scope_map.get_valid_vars().iter().for_each(|var_name| {
                                if_else
                                    .then_branch
                                    .statements
                                    .push(Self::delete_stmt(var_name.name, ret_stmt.span()));
                            });
                            break;
                        }
                        Ok(false) => {}
                        Err(e) => self.errors.push(e),
                    }
                }
                // Restore the scope map for the else branch
                self.scope_map = snapshot;
                if let Some(else_branch) = &mut if_else.else_branch {
                    for stmt in else_branch.statements.iter_mut() {
                        match self.analyze_stmt(stmt) {
                            Ok(_) => {}
                            Err(e) => self.errors.push(e),
                        }
                    }
                }
            }
            HirStatement::Return(ret) => {
                self.analyze_expr(&ret.value)?;
                // Before returning, we need to delete all variables that are still valid
                return Ok(true);
            }
            HirStatement::Expr(expr) => {
                self.analyze_expr(&expr.expr)?;
            }
            _ => {
                unimplemented!()
            }
        }
        Ok(false)
    }

    pub fn analyze_expr(&mut self, expr: &HirExpr<'hir>) -> HirResult<()> {
        match expr {
            _ => {
                // For now, we do nothing
            }
        }
        Ok(())
    }

    fn delete_stmt(var_name: &'hir str, span: Span) -> HirStatement<'hir> {
        HirStatement::Expr(HirExprStmt {
            span,
            expr: HirExpr::Delete(HirDeleteExpr {
                span,
                expr: Box::new(HirExpr::Ident(HirIdentExpr {
                    name: var_name,
                    span,
                    ty: &HirTy::Unit(HirUnitTy {}),
                })),
            }),
        })
    }

    fn is_copyable(&self, ty: &HirTy<'hir>) -> bool {
        matches!(
            ty,
            HirTy::Boolean(_)
                | HirTy::Int64(_)
                | HirTy::Float64(_)
                | HirTy::Char(_)
                | HirTy::UInt64(_)
                | HirTy::ReadOnlyReference(_)
                | HirTy::MutableReference(_)
        )
    }
}
