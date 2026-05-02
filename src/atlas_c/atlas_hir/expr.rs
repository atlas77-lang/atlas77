use core::fmt;

use super::ty::{HirTy, HirUnitTy};
use crate::atlas_c::utils::Span;

#[derive(Debug, Clone)]
//todo: Add arrays/struct & class init literal
pub enum HirExpr<'hir> {
    HirBinaryOperation(HirBinaryOpExpr<'hir>),
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
    StringLiteral(HirStringLiteralExpr<'hir>),
    ListLiteral(HirListLiteralExpr<'hir>),
    ListLiteralWithSize(HirListLiteralWithSizeExpr<'hir>),
    NullLiteral(HirNullLiteralExpr<'hir>),
    ObjLiteral(HirObjLiteralExpr<'hir>),
    Delete(HirDeleteExpr<'hir>),
    FieldAccess(HirFieldAccessExpr<'hir>),
    Indexing(HirIndexingExpr<'hir>),
    StaticAccess(HirStaticAccessExpr<'hir>),
    IntrinsicCall(HirIntrinsicCallExpr<'hir>),
}

impl<'hir> HirExpr<'hir> {
    pub fn span(&self) -> Span {
        match self {
            HirExpr::Ident(expr) => expr.span,
            HirExpr::IntegerLiteral(expr) => expr.span,
            HirExpr::UnsignedIntegerLiteral(expr) => expr.span,
            HirExpr::BooleanLiteral(expr) => expr.span,
            HirExpr::FloatLiteral(expr) => expr.span,
            HirExpr::CharLiteral(expr) => expr.span,
            HirExpr::UnitLiteral(expr) => expr.span,
            HirExpr::ThisLiteral(expr) => expr.span,
            HirExpr::NullLiteral(expr) => expr.span,
            HirExpr::Unary(expr) => expr.span,
            HirExpr::Casting(expr) => expr.span,
            HirExpr::HirBinaryOperation(expr) => expr.span,
            HirExpr::Call(expr) => expr.span,
            HirExpr::StringLiteral(expr) => expr.span,
            HirExpr::ListLiteral(expr) => expr.span,
            HirExpr::ListLiteralWithSize(expr) => expr.span,
            HirExpr::ObjLiteral(expr) => expr.span,
            HirExpr::Delete(expr) => expr.span,
            HirExpr::FieldAccess(expr) => expr.span,
            HirExpr::Indexing(expr) => expr.span,
            HirExpr::StaticAccess(expr) => expr.span,
            HirExpr::IntrinsicCall(expr) => expr.span,
        }
    }

    pub fn kind(&self) -> &'static str {
        match self {
            HirExpr::Ident(_) => "Identifier",
            HirExpr::IntegerLiteral(_) => "Integer Literal",
            HirExpr::UnsignedIntegerLiteral(_) => "Unsigned Integer Literal",
            HirExpr::BooleanLiteral(_) => "Boolean Literal",
            HirExpr::FloatLiteral(_) => "Float Literal",
            HirExpr::CharLiteral(_) => "Char Literal",
            HirExpr::UnitLiteral(_) => "Unit Literal",
            HirExpr::ThisLiteral(_) => "This Literal",
            HirExpr::NullLiteral(_) => "Null Literal",
            HirExpr::Unary(_) => "Unary Expression",
            HirExpr::Casting(_) => "Casting Expression",
            HirExpr::HirBinaryOperation(_) => "Binary Operation Expression",
            HirExpr::Call(_) => "Function Call Expression",
            HirExpr::StringLiteral(_) => "String Literal",
            HirExpr::ListLiteral(_) => "List Literal",
            HirExpr::ListLiteralWithSize(_) => "List Literal with Size",
            HirExpr::ObjLiteral(_) => "Object Literal Expression",
            HirExpr::Delete(_) => "Delete Expression",
            HirExpr::FieldAccess(_) => "Field Access Expression",
            HirExpr::Indexing(_) => "Indexing Expression",
            HirExpr::StaticAccess(_) => "Static Access Expression",
            HirExpr::IntrinsicCall(_) => "Intrinsic Call Expression",
        }
    }

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
            HirExpr::NullLiteral(expr) => expr.ty,
            HirExpr::Unary(expr) => expr.ty,
            HirExpr::Casting(expr) => expr.target_ty,
            HirExpr::HirBinaryOperation(expr) => expr.ty,
            HirExpr::Call(expr) => expr.ty,
            HirExpr::StringLiteral(expr) => expr.ty,
            HirExpr::ListLiteral(expr) => expr.ty,
            HirExpr::ListLiteralWithSize(expr) => expr.ty,
            HirExpr::ObjLiteral(expr) => expr.ty,
            HirExpr::Delete(_) => &HirTy::Unit(HirUnitTy {}),
            HirExpr::FieldAccess(expr) => expr.ty,
            HirExpr::Indexing(expr) => expr.ty,
            HirExpr::StaticAccess(expr) => expr.ty,
            HirExpr::IntrinsicCall(expr) => expr.ty,
        }
    }
}

#[derive(Debug, Clone)]
pub struct HirNullLiteralExpr<'hir> {
    pub span: Span,
    pub ty: &'hir HirTy<'hir>,
}

#[derive(Debug, Clone)]
pub struct HirIntrinsicCallExpr<'hir> {
    pub span: Span,
    pub name: &'hir str,
    pub args: Vec<HirExpr<'hir>>,
    pub args_ty: Vec<&'hir HirTy<'hir>>,
    pub ty: &'hir HirTy<'hir>,
}

#[derive(Debug, Clone)]
pub struct HirObjLiteralExpr<'hir> {
    pub span: Span,
    pub ty: &'hir HirTy<'hir>,
    pub fields: Vec<HirFieldInit<'hir>>,
}

#[derive(Debug, Clone)]
pub struct HirFieldInit<'hir> {
    pub span: Span,
    pub name: &'hir str,
    pub name_span: Span,
    pub ty: &'hir HirTy<'hir>,
    pub value: Box<HirExpr<'hir>>,
}

#[derive(Debug, Clone)]
pub struct HirThisLiteral<'hir> {
    pub span: Span,
    pub ty: &'hir HirTy<'hir>,
}

#[derive(Debug, Clone)]
pub struct HirStaticAccessExpr<'hir> {
    pub span: Span,
    pub target: &'hir HirTy<'hir>,
    pub field: Box<HirIdentExpr<'hir>>,
    pub ty: &'hir HirTy<'hir>,
}

#[derive(Debug, Clone)]
pub struct HirFieldAccessExpr<'hir> {
    pub span: Span,
    pub target: Box<HirExpr<'hir>>,
    pub field: Box<HirIdentExpr<'hir>>,
    pub ty: &'hir HirTy<'hir>,
    /// foo.bar or foo->bar
    pub is_arrow: bool,
}

#[derive(Debug, Clone)]
pub struct HirCharLiteralExpr<'hir> {
    pub value: char,
    pub span: Span,
    pub ty: &'hir HirTy<'hir>,
}

#[derive(Debug, Clone)]
pub struct HirUnitLiteralExpr<'hir> {
    pub span: Span,
    pub ty: &'hir HirTy<'hir>,
}

#[derive(Debug, Clone)]
pub struct HirDeleteExpr<'hir> {
    pub span: Span,
    pub expr: Box<HirExpr<'hir>>,
}

#[derive(Debug, Clone)]
pub struct HirListLiteralExpr<'hir> {
    pub span: Span,
    pub items: Vec<HirExpr<'hir>>,
    pub ty: &'hir HirTy<'hir>,
}

#[derive(Debug, Clone)]
pub struct HirListLiteralWithSizeExpr<'hir> {
    pub span: Span,
    pub item: Box<HirExpr<'hir>>,
    pub size: Box<HirExpr<'hir>>,
    pub ty: &'hir HirTy<'hir>,
}

impl HirListLiteralWithSizeExpr<'_> {
    pub fn size_as_usize(&self) -> Option<usize> {
        match &*self.size {
            HirExpr::IntegerLiteral(i) => Some(i.value as usize),
            HirExpr::UnsignedIntegerLiteral(u) => Some(u.value as usize),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct HirIndexingExpr<'hir> {
    pub span: Span,
    pub target: Box<HirExpr<'hir>>,
    pub index: Box<HirExpr<'hir>>,
    pub ty: &'hir HirTy<'hir>,
}

#[derive(Debug, Clone)]
pub struct HirCastExpr<'hir> {
    pub span: Span,
    pub expr: Box<HirExpr<'hir>>,
    pub target_ty: &'hir HirTy<'hir>,
}

#[derive(Debug, Clone)]
pub struct HirBooleanLiteralExpr<'hir> {
    pub value: bool,
    pub span: Span,
    pub ty: &'hir HirTy<'hir>,
}

#[derive(Debug, Clone)]
pub struct HirStringLiteralExpr<'hir> {
    pub value: &'hir str,
    pub span: Span,
    pub ty: &'hir HirTy<'hir>,
}

#[derive(Debug, Clone)]
pub struct HirFunctionCallExpr<'hir> {
    pub span: Span,
    /// The callee can be any kind of expression (e.g. ``Rectangle::new()`` or ``MyStruct.some_fn_ptr()`` or ``MyOtherStruct.some_array_of_fn[0]()``)
    pub callee: Box<HirExpr<'hir>>,
    pub callee_span: Span,
    pub args: Vec<HirExpr<'hir>>,
    pub args_ty: Vec<&'hir HirTy<'hir>>,
    pub generics: Vec<&'hir HirTy<'hir>>,
    /// True when this expression denotes a generic function value reference
    /// (e.g. `add<int64>`) rather than an invocation.
    pub is_reference: bool,
    /// Result type of the call
    pub ty: &'hir HirTy<'hir>,
    pub kind: HirFunctionKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HirFunctionKind {
    Constructor,
    DefaultConstructor,
    MoveConstructor,
    CopyConstructor,
    Method,
    StaticMethod,
    Function,
    ExternFunction,
}

#[derive(Debug, Clone)]
pub struct HirBinaryOpExpr<'hir> {
    pub span: Span,
    pub op: HirBinaryOperator,
    pub op_span: Span,
    pub lhs: Box<HirExpr<'hir>>,
    pub rhs: Box<HirExpr<'hir>>,
    /// The type of the result of the expression.
    pub ty: &'hir HirTy<'hir>,
}

#[derive(Debug, Clone)]
pub enum HirBinaryOperator {
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
    ShL,
    ShR,
    BinAnd,
    BinOr,
    BinXor,
}

impl fmt::Display for HirBinaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let op_str = match self {
            HirBinaryOperator::Add => "+",
            HirBinaryOperator::And => "&&",
            HirBinaryOperator::Div => "/",
            HirBinaryOperator::Eq => "==",
            HirBinaryOperator::Gt => ">",
            HirBinaryOperator::Gte => ">=",
            HirBinaryOperator::Lt => "<",
            HirBinaryOperator::Lte => "<=",
            HirBinaryOperator::Mod => "%",
            HirBinaryOperator::Mul => "*",
            HirBinaryOperator::Neq => "!=",
            HirBinaryOperator::Or => "||",
            HirBinaryOperator::Sub => "-",
            HirBinaryOperator::ShL => "<<",
            HirBinaryOperator::ShR => ">>",
            HirBinaryOperator::BinAnd => "&",
            HirBinaryOperator::BinOr => "|",
            HirBinaryOperator::BinXor => "^",
        };
        write!(f, "{}", op_str)
    }
}

#[derive(Debug, Clone)]
pub struct UnaryOpExpr<'hir> {
    pub span: Span,
    pub op: Option<HirUnaryOp>,
    pub expr: Box<HirExpr<'hir>>,
    /// The type of the result of the expression.
    pub ty: &'hir HirTy<'hir>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum HirUnaryOp {
    Neg,
    Not,
    AsRef,
    Deref,
}

impl fmt::Display for HirUnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let op_str = match self {
            HirUnaryOp::Neg => "-",
            HirUnaryOp::Not => "!",
            HirUnaryOp::AsRef => "&",
            HirUnaryOp::Deref => "*",
        };
        write!(f, "{}", op_str)
    }
}

#[derive(Debug, Clone)]
pub struct HirFloatLiteralExpr<'hir> {
    pub value: f64,
    pub span: Span,
    pub ty: &'hir HirTy<'hir>,
}

#[derive(Debug, Clone)]
pub struct HirUnsignedIntegerLiteralExpr<'hir> {
    pub value: u64,
    pub span: Span,
    pub ty: &'hir HirTy<'hir>,
}

#[derive(Debug, Clone)]
pub struct HirIntegerLiteralExpr<'hir> {
    pub value: i64,
    pub span: Span,
    pub ty: &'hir HirTy<'hir>,
}

#[derive(Debug, Clone)]
pub struct HirIdentExpr<'hir> {
    pub name: &'hir str,
    pub span: Span,
    pub ty: &'hir HirTy<'hir>,
}
