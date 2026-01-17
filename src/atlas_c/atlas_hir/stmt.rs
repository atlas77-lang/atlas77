use super::{expr::HirExpr, ty::HirTy};
use crate::atlas_c::utils::Span;
/// Most of the statements could actually be replaced with
///
/// Statement::Expr(HirExpr)
/// Only the HirBlock & HirReturn is useful
#[derive(Debug, Clone)]
pub enum HirStatement<'hir> {
    Block(HirBlock<'hir>),
    Return(HirReturn<'hir>),
    Expr(HirExprStmt<'hir>),
    Let(HirVariableStmt<'hir>),
    Assign(HirAssignStmt<'hir>),
    //tbf, no need to use anything else than a LetStmt for the const
    Const(HirVariableStmt<'hir>),
    IfElse(HirIfElseStmt<'hir>),
    While(HirWhileStmt<'hir>),
    Break(Span),
    Continue(Span),
}

impl HirStatement<'_> {
    pub fn span(&self) -> Span {
        match self {
            HirStatement::Block(block) => block.span,
            HirStatement::Return(ret) => ret.span,
            HirStatement::Expr(expr) => expr.span,
            HirStatement::Let(let_stmt) => let_stmt.span,
            HirStatement::Assign(assign_stmt) => assign_stmt.span,
            HirStatement::Const(const_stmt) => const_stmt.span,
            HirStatement::IfElse(if_else) => if_else.span,
            HirStatement::While(while_stmt) => while_stmt.span,
            HirStatement::Break(span) => *span,
            HirStatement::Continue(span) => *span,
        }
    }
}

#[derive(Debug, Clone)]
pub struct HirExprStmt<'hir> {
    pub span: Span,
    pub expr: HirExpr<'hir>,
}
#[derive(Debug, Clone)]
pub struct HirWhileStmt<'hir> {
    pub span: Span,
    pub condition: HirExpr<'hir>,
    pub body: HirBlock<'hir>,
}

#[derive(Debug, Clone)]
pub struct HirVariableStmt<'hir> {
    pub span: Span,
    pub name: &'hir str,
    pub name_span: Span,
    pub ty: &'hir HirTy<'hir>,
    pub ty_span: Option<Span>,
    pub value: HirExpr<'hir>,
}

#[derive(Debug, Clone)]
pub struct HirAssignStmt<'hir> {
    pub span: Span,
    pub dst: HirExpr<'hir>,
    pub val: HirExpr<'hir>,
    pub ty: &'hir HirTy<'hir>,
}

#[derive(Debug, Clone)]
pub struct HirIfElseStmt<'hir> {
    pub span: Span,
    pub condition: HirExpr<'hir>,
    pub then_branch: HirBlock<'hir>,
    pub else_branch: Option<HirBlock<'hir>>,
}
#[derive(Debug, Clone)]
pub struct HirReturn<'hir> {
    pub span: Span,
    pub value: HirExpr<'hir>,
    pub ty: &'hir HirTy<'hir>,
}
#[derive(Debug, Clone)]
pub struct HirBlock<'hir> {
    pub span: Span,
    pub statements: Vec<HirStatement<'hir>>,
}
