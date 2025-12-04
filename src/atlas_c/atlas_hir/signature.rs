use super::ty::{HirTy, HirUnitTy};
use crate::atlas_c::atlas_frontend::parser::ast::AstVisibility;
use crate::atlas_c::atlas_hir::expr::HirUnaryOp;
use crate::atlas_c::atlas_hir::expr::{HirBinaryOp, HirExpr};
use crate::atlas_c::utils::Span;
use std::collections::BTreeMap;
use std::fmt::Display;

/// An HirModuleSignature represents the API of a module.
///
/// Currently only functions exist in the language.
#[derive(Debug, Clone, Default)]
pub struct HirModuleSignature<'hir> {
    pub functions: BTreeMap<&'hir str, &'hir HirFunctionSignature<'hir>>,
    pub structs: BTreeMap<&'hir str, &'hir HirStructSignature<'hir>>,
}

#[derive(Debug, Clone)]
/// As of now, structs don't inherit concepts.
pub struct HirStructSignature<'hir> {
    pub declaration_span: Span,
    pub vis: HirVisibility,
    pub name: &'hir str,
    pub name_span: Span,
    pub methods: BTreeMap<&'hir str, HirStructMethodSignature<'hir>>,
    pub fields: BTreeMap<&'hir str, HirStructFieldSignature<'hir>>,
    /// Generic type parameter names
    pub generics: Vec<&'hir str>,
    /// This is enough to know if the class implement them or not
    pub operators: Vec<HirBinaryOp>,
    pub constants: BTreeMap<&'hir str, &'hir HirStructConstantSignature<'hir>>,
    pub constructor: HirStructConstructorSignature<'hir>,
    pub destructor: HirStructConstructorSignature<'hir>,
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum HirVisibility {
    Public,
    Private,
}
impl Default for HirVisibility {
    fn default() -> Self {
        Self::Public
    }
}
impl From<AstVisibility> for HirVisibility {
    fn from(ast_vis: AstVisibility) -> Self {
        match ast_vis {
            AstVisibility::Public => HirVisibility::Public,
            AstVisibility::Private => HirVisibility::Private,
        }
    }
}

#[derive(Debug, Clone)]
//Also used for the destructor
pub struct HirStructConstructorSignature<'hir> {
    pub span: Span,
    pub params: Vec<HirFunctionParameterSignature<'hir>>,
    pub type_params: Vec<HirTypeParameterItemSignature<'hir>>,
    pub vis: HirVisibility,
}

#[derive(Debug, Clone)]
pub struct HirStructConstantSignature<'hir> {
    pub span: Span,
    pub vis: HirVisibility,
    pub name: &'hir str,
    pub name_span: Span,
    pub ty: &'hir HirTy<'hir>,
    pub ty_span: Span,
    pub value: &'hir ConstantValue,
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Default)]
pub enum ConstantValue {
    Int(i64),
    Float(f64),
    UInt(u64),
    String(String),
    Bool(bool),
    Char(char),
    #[default]
    Unit,
    List(Vec<ConstantValue>),
}

impl Display for ConstantValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ConstantValue::Int(i) => write!(f, "int64 : {}", i),
            ConstantValue::Float(fl) => write!(f, "float64 : {}", fl),
            ConstantValue::UInt(u) => write!(f, "uint64 : {}", u),
            ConstantValue::String(s) => write!(f, "string : \"{}\"", s),
            ConstantValue::Bool(b) => write!(f, "bool : {}", b),
            ConstantValue::Char(c) => write!(f, "char : '{}'", c),
            ConstantValue::Unit => write!(f, "()"),
            ConstantValue::List(l) => {
                let elements: Vec<String> = l.iter().map(|elem| format!("{}", elem)).collect();
                write!(f, "[{}]", elements.join(", "))
            }
        }
    }
}

impl TryFrom<HirExpr<'_>> for ConstantValue {
    type Error = ();
    fn try_from(value: HirExpr) -> Result<Self, Self::Error> {
        match value {
            HirExpr::CharLiteral(c) => Ok(ConstantValue::Char(c.value)),
            HirExpr::IntegerLiteral(i) => Ok(ConstantValue::Int(i.value)),
            HirExpr::UnsignedIntegerLiteral(u) => Ok(ConstantValue::UInt(u.value)),
            HirExpr::FloatLiteral(f) => Ok(ConstantValue::Float(f.value)),
            HirExpr::StringLiteral(s) => Ok(ConstantValue::String(String::from(s.value))),
            HirExpr::BooleanLiteral(b) => Ok(ConstantValue::Bool(b.value)),
            HirExpr::Unary(u) => {
                if u.op == Some(HirUnaryOp::Neg) {
                    match *u.expr {
                        HirExpr::IntegerLiteral(i) => Ok(ConstantValue::Int(-i.value)),
                        HirExpr::FloatLiteral(f) => Ok(ConstantValue::Float(-f.value)),
                        _ => Err(()),
                    }
                } else if u.op == Some(HirUnaryOp::Not) {
                    match *u.expr {
                        HirExpr::BooleanLiteral(b) => Ok(ConstantValue::Bool(!b.value)),
                        _ => Err(()),
                    }
                } else {
                    ConstantValue::try_from(*u.expr)
                }
            }
            _ => Err(()),
        }
    }
}

#[derive(Debug, Clone)]
pub struct HirStructFieldSignature<'hir> {
    pub span: Span,
    pub vis: HirVisibility,
    pub name: &'hir str,
    pub name_span: Span,
    pub ty: &'hir HirTy<'hir>,
    pub ty_span: Span,
}

#[derive(Debug, Clone)]
pub struct HirStructMethodSignature<'hir> {
    pub span: Span,
    pub vis: HirVisibility,
    pub modifier: HirStructMethodModifier,
    pub params: Vec<HirFunctionParameterSignature<'hir>>,
    pub generics: Option<Vec<&'hir HirTypeParameterItemSignature<'hir>>>,
    pub type_params: Vec<&'hir HirTypeParameterItemSignature<'hir>>,
    pub return_ty: HirTy<'hir>,
    pub return_ty_span: Option<Span>,
}

#[derive(Debug, Default, Clone, PartialEq)]
pub enum HirStructMethodModifier {
    Static,
    Const,
    #[default]
    None,
}

#[derive(Debug, Clone)]
pub struct HirFunctionSignature<'hir> {
    pub span: Span,
    pub vis: HirVisibility,
    pub params: Vec<HirFunctionParameterSignature<'hir>>,
    pub generics: Option<Vec<&'hir HirTypeParameterItemSignature<'hir>>>,
    pub type_params: Vec<&'hir HirTypeParameterItemSignature<'hir>>,
    /// The user can declare a function without a return type, in which case the return type is `()`.
    pub return_ty: HirTy<'hir>,
    /// The span of the return type, if it exists.
    pub return_ty_span: Option<Span>,
    pub is_external: bool,
}

impl Default for HirFunctionSignature<'_> {
    fn default() -> Self {
        Self {
            span: Span::default(),
            vis: HirVisibility::Public,
            params: Vec::new(),
            generics: None,
            type_params: Vec::new(),
            return_ty: HirTy::Unit(HirUnitTy {}),
            return_ty_span: None,
            is_external: false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct HirTypeParameterItemSignature<'hir> {
    pub span: Span,
    pub name: &'hir str,
    pub name_span: Span,
}

#[derive(Debug, Clone)]
pub struct HirFunctionParameterSignature<'hir> {
    pub span: Span,
    pub name: &'hir str,
    pub name_span: Span,
    pub ty: &'hir HirTy<'hir>,
    pub ty_span: Span,
}
