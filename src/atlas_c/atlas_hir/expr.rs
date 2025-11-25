use super::ty::{HirTy, HirUnitTy};
use logos::Span;
use serde::Serialize;

#[derive(Debug, Clone, Serialize)]
//todo: Add arrays/struct & class init literal
pub enum HirExpr<'hir> {
    Assign(HirAssignExpr<'hir>),
    HirBinaryOp(HirBinaryOpExpr<'hir>),
    Call(HirFunctionCallExpr<'hir>),
    Ident(HirIdentExpr<'hir>),
    Unary(UnaryOpExpr<'hir>),
    Casting(HirCastExpr<'hir>),
    FloatLiteral(HirFloatLiteralExpr<'hir>),
    CharLiteral(HirCharLiteralExpr<'hir>),
    IntegerLiteral(HirIntegerLiteralExpr<'hir>),
    UnitLiteral(HirUnitLiteralExpr<'hir>),
    BooleanLiteral(HirBooleanLiteralExpr<'hir>),
    UnsignedIntegerLiteral(HirUnsignedIntegerLiteralExpr<'hir>),
    ThisLiteral(HirThisLiteral<'hir>),
    NoneLiteral(HirNoneLiteral<'hir>),
    StringLiteral(HirStringLiteralExpr<'hir>),
    ListLiteral(HirListLiteralExpr<'hir>),
    NewArray(HirNewArrayExpr<'hir>),
    NewObj(HirNewObjExpr<'hir>),
    Delete(HirDeleteExpr<'hir>),
    FieldAccess(HirFieldAccessExpr<'hir>),
    Indexing(HirIndexingExpr<'hir>),
    StaticAccess(HirStaticAccessExpr<'hir>),
    ConstructorExpr(HirConstructorExpr<'hir>),
}

pub fn is_self_access(field_access_expr: &HirFieldAccessExpr) -> bool {
    matches!(field_access_expr.target.as_ref(), HirExpr::ThisLiteral(_))
}

impl HirExpr<'_> {
    pub(crate) fn span(&self) -> Span {
        match self {
            HirExpr::Ident(expr) => expr.span.clone(),
            HirExpr::IntegerLiteral(expr) => expr.span.clone(),
            HirExpr::UnsignedIntegerLiteral(expr) => expr.span.clone(),
            HirExpr::BooleanLiteral(expr) => expr.span.clone(),
            HirExpr::FloatLiteral(expr) => expr.span.clone(),
            HirExpr::CharLiteral(expr) => expr.span.clone(),
            HirExpr::UnitLiteral(expr) => expr.span.clone(),
            HirExpr::ThisLiteral(expr) => expr.span.clone(),
            HirExpr::NoneLiteral(expr) => expr.span.clone(),
            HirExpr::Unary(expr) => expr.span.clone(),
            HirExpr::Casting(expr) => expr.span.clone(),
            HirExpr::HirBinaryOp(expr) => expr.span.clone(),
            HirExpr::Call(expr) => expr.span.clone(),
            HirExpr::Assign(expr) => expr.span.clone(),
            HirExpr::StringLiteral(expr) => expr.span.clone(),
            HirExpr::ListLiteral(expr) => expr.span.clone(),
            HirExpr::NewArray(expr) => expr.span.clone(),
            HirExpr::NewObj(expr) => expr.span.clone(),
            HirExpr::Delete(expr) => expr.span.clone(),
            HirExpr::FieldAccess(expr) => expr.span.clone(),
            HirExpr::Indexing(expr) => expr.span.clone(),
            HirExpr::StaticAccess(expr) => expr.span.clone(),
            HirExpr::ConstructorExpr(expr) => expr.span.clone(),
        }
    }
}

impl<'hir> HirExpr<'hir> {
    pub fn ty(&self) -> &'hir HirTy<'hir> {
        match self {
            HirExpr::Ident(expr) => expr.ty,
            HirExpr::IntegerLiteral(expr) => expr.ty,
            HirExpr::UnsignedIntegerLiteral(expr) => expr.ty,
            HirExpr::BooleanLiteral(expr) => expr.ty,
            HirExpr::FloatLiteral(expr) => expr.ty,
            HirExpr::CharLiteral(expr) => expr.ty,
            HirExpr::UnitLiteral(expr) => expr.ty,
            HirExpr::ThisLiteral(expr) => expr.ty,
            HirExpr::NoneLiteral(expr) => expr.ty,
            HirExpr::Unary(expr) => expr.ty,
            HirExpr::Casting(expr) => expr.ty,
            HirExpr::HirBinaryOp(expr) => expr.ty,
            HirExpr::Call(expr) => expr.ty,
            HirExpr::Assign(expr) => expr.ty,
            HirExpr::StringLiteral(expr) => expr.ty,
            HirExpr::ListLiteral(expr) => expr.ty,
            HirExpr::NewArray(expr) => expr.ty,
            HirExpr::NewObj(expr) => expr.ty,
            HirExpr::Delete(_) => &HirTy::Unit(HirUnitTy {}),
            HirExpr::FieldAccess(expr) => expr.ty,
            HirExpr::Indexing(expr) => expr.ty,
            HirExpr::StaticAccess(expr) => expr.ty,
            HirExpr::ConstructorExpr(expr) => expr.ty,
        }
    }
}

#[derive(Debug, Clone, Serialize)]
/// Represents the default C-like constructor expression for structs.
///
/// Example:
/// ```
/// Point {
///    x: 10,
///    y: 20,
/// }
/// ```
pub struct HirConstructorExpr<'hir> {
    pub name: &'hir str,
    pub span: Span,
    pub ty: &'hir HirTy<'hir>,
    pub fields: Vec<HirFieldInit<'hir>>,
}

#[derive(Debug, Clone, Serialize)]
pub struct HirFieldInit<'hir> {
    pub span: Span,
    pub name: Box<HirIdentExpr<'hir>>,
    pub ty: &'hir HirTy<'hir>,
    pub value: Box<HirExpr<'hir>>,
}

#[derive(Debug, Clone, Serialize)]
pub struct HirNoneLiteral<'hir> {
    pub span: Span,
    pub ty: &'hir HirTy<'hir>,
}

#[derive(Debug, Clone, Serialize)]
pub struct HirThisLiteral<'hir> {
    pub span: Span,
    pub ty: &'hir HirTy<'hir>,
}

#[derive(Debug, Clone, Serialize)]
pub struct HirStaticAccessExpr<'hir> {
    pub span: Span,
    pub target: Box<HirIdentExpr<'hir>>,
    pub field: Box<HirIdentExpr<'hir>>,
    pub ty: &'hir HirTy<'hir>,
}

#[derive(Debug, Clone, Serialize)]
pub struct HirFieldAccessExpr<'hir> {
    pub span: Span,
    pub target: Box<HirExpr<'hir>>,
    pub field: Box<HirIdentExpr<'hir>>,
    pub ty: &'hir HirTy<'hir>,
}

#[derive(Debug, Clone, Serialize)]
pub struct HirCharLiteralExpr<'hir> {
    pub value: char,
    pub span: Span,
    pub ty: &'hir HirTy<'hir>,
}

#[derive(Debug, Clone, Serialize)]
pub struct HirUnitLiteralExpr<'hir> {
    pub span: Span,
    pub ty: &'hir HirTy<'hir>,
}

#[derive(Debug, Clone, Serialize)]
pub struct HirNewArrayExpr<'hir> {
    pub span: Span,
    pub ty: &'hir HirTy<'hir>,
    pub size: Box<HirExpr<'hir>>,
}

#[derive(Debug, Clone, Serialize)]
pub struct HirDeleteExpr<'hir> {
    pub span: Span,
    pub expr: Box<HirExpr<'hir>>,
}

#[derive(Debug, Clone, Serialize)]
pub struct HirNewObjExpr<'hir> {
    pub span: Span,
    pub ty: &'hir HirTy<'hir>,
    pub args: Vec<HirExpr<'hir>>,
    pub args_ty: Vec<&'hir HirTy<'hir>>,
}

#[derive(Debug, Clone, Serialize)]
pub struct HirListLiteralExpr<'hir> {
    pub span: Span,
    pub items: Vec<HirExpr<'hir>>,
    pub ty: &'hir HirTy<'hir>,
}

#[derive(Debug, Clone, Serialize)]
pub struct HirIndexingExpr<'hir> {
    pub span: Span,
    pub target: Box<HirExpr<'hir>>,
    pub index: Box<HirExpr<'hir>>,
    pub ty: &'hir HirTy<'hir>,
}

#[derive(Debug, Clone, Serialize)]
pub struct HirCastExpr<'hir> {
    pub span: Span,
    pub expr: Box<HirExpr<'hir>>,
    pub ty: &'hir HirTy<'hir>,
}

#[derive(Debug, Clone, Serialize)]
pub struct HirBooleanLiteralExpr<'hir> {
    pub value: bool,
    pub span: Span,
    pub ty: &'hir HirTy<'hir>,
}

#[derive(Debug, Clone, Serialize)]
pub struct HirStringLiteralExpr<'hir> {
    pub value: &'hir str,
    pub span: Span,
    pub ty: &'hir HirTy<'hir>,
}

#[derive(Debug, Clone, Serialize)]
pub struct HirAssignExpr<'hir> {
    pub span: Span,
    pub lhs: Box<HirExpr<'hir>>,
    pub rhs: Box<HirExpr<'hir>>,
    pub ty: &'hir HirTy<'hir>,
}

#[derive(Debug, Clone, Serialize)]
pub struct HirFunctionCallExpr<'hir> {
    pub span: Span,
    /// The callee can be any kind of expression (e.g. ``Rectangle::new()`` or ``MyStruct.some_fn_ptr()`` or ``MyOtherStruct.some_array_of_fn[0]()``)
    pub callee: Box<HirExpr<'hir>>,
    pub callee_span: Span,
    pub args: Vec<HirExpr<'hir>>,
    pub args_ty: Vec<&'hir HirTy<'hir>>,
    /// Result type of the call
    pub ty: &'hir HirTy<'hir>,
}

#[derive(Debug, Clone, Serialize)]
pub struct HirBinaryOpExpr<'hir> {
    pub span: Span,
    pub op: HirBinaryOp,
    pub op_span: Span,
    pub lhs: Box<HirExpr<'hir>>,
    pub rhs: Box<HirExpr<'hir>>,
    /// The type of the result of the expression.
    pub ty: &'hir HirTy<'hir>,
}

#[derive(Debug, Clone, Serialize)]
pub enum HirBinaryOp {
    Add,
    And,
    Div,
    Eq,
    Gt,
    Gte,
    Lt,
    Lte,
    Mod,
    Mul,
    Neq,
    Or,
    Sub,
}

#[derive(Debug, Clone, Serialize)]
pub struct UnaryOpExpr<'hir> {
    pub span: Span,
    pub op: Option<HirUnaryOp>,
    pub expr: Box<HirExpr<'hir>>,
    /// The type of the result of the expression.
    pub ty: &'hir HirTy<'hir>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum HirUnaryOp {
    Neg,
    Not,
}

#[derive(Debug, Clone, Serialize)]
pub struct HirFloatLiteralExpr<'hir> {
    pub value: f64,
    pub span: Span,
    pub ty: &'hir HirTy<'hir>,
}

#[derive(Debug, Clone, Serialize)]
pub struct HirUnsignedIntegerLiteralExpr<'hir> {
    pub value: u64,
    pub span: Span,
    pub ty: &'hir HirTy<'hir>,
}

#[derive(Debug, Clone, Serialize)]
pub struct HirIntegerLiteralExpr<'hir> {
    pub value: i64,
    pub span: Span,
    pub ty: &'hir HirTy<'hir>,
}

#[derive(Debug, Clone, Serialize)]
pub struct HirIdentExpr<'hir> {
    pub name: &'hir str,
    pub span: Span,
    pub ty: &'hir HirTy<'hir>,
}
