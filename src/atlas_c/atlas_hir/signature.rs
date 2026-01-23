use super::ty::HirTy;
use crate::atlas_c::atlas_frontend::parser::ast::{AstFlag, AstVisibility};
use crate::atlas_c::atlas_hir::expr::HirUnaryOp;
use crate::atlas_c::atlas_hir::expr::{HirBinaryOperator, HirExpr};
use crate::atlas_c::atlas_hir::item::HirEnum;
use crate::atlas_c::atlas_hir::ty::HirGenericTy;
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
    //No need for enum signatures for now
    pub enums: BTreeMap<&'hir str, &'hir HirEnum<'hir>>,
    pub unions: BTreeMap<&'hir str, &'hir HirUnionSignature<'hir>>,
}

#[derive(Debug, Clone)]
/// As of now, structs don't inherit concepts.
pub struct HirStructSignature<'hir> {
    pub declaration_span: Span,
    pub vis: HirVisibility,
    pub flag: HirFlag,
    pub name: &'hir str,
    /// If the struct name is mangled, this contains the pre-mangled type
    pub pre_mangled_ty: Option<&'hir HirGenericTy<'hir>>,
    pub name_span: Span,
    pub methods: BTreeMap<&'hir str, HirStructMethodSignature<'hir>>,
    pub fields: BTreeMap<&'hir str, HirStructFieldSignature<'hir>>,
    /// Generic type parameter names
    pub generics: Vec<&'hir HirGenericConstraint<'hir>>,
    /// This is enough to know if the class implement them or not
    pub operators: Vec<HirBinaryOperator>,
    pub constants: BTreeMap<&'hir str, &'hir HirStructConstantSignature<'hir>>,
    pub constructor: HirStructConstructorSignature<'hir>,
    pub copy_constructor: Option<HirStructConstructorSignature<'hir>>,
    pub move_constructor: Option<HirStructConstructorSignature<'hir>>,
    pub default_constructor: Option<HirStructConstructorSignature<'hir>>,
    /// THERE IS a destructor in this option. It's only used, because compiler
    /// generated destructors are made at the end of the syntax lowering pass.
    pub destructor: Option<HirStructConstructorSignature<'hir>>,
    pub had_user_defined_constructor: bool,
    pub had_user_defined_destructor: bool,
    pub had_user_defined_copy_constructor: bool,
    pub had_user_defined_move_constructor: bool,
    pub had_user_defined_default_constructor: bool,
    pub docstring: Option<&'hir str>,
}

#[derive(Debug, Clone)]
pub struct HirGenericConstraint<'hir> {
    pub span: Span,
    pub generic_name: &'hir str,
    // For now only `std::copyable`
    pub kind: Vec<&'hir HirGenericConstraintKind<'hir>>,
}

#[derive(Debug, Clone)]
pub enum HirGenericConstraintKind<'hir> {
    // e.g. std::copyable
    Std { name: &'hir str, span: Span },
    // e.g. operator overloading
    Operator { op: HirBinaryOperator, span: Span },
    // e.g. user-defined concepts
    Concept { name: &'hir str, span: Span },
}

impl Display for HirGenericConstraintKind<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            HirGenericConstraintKind::Std { name, .. } => write!(f, "std::{}", name),
            HirGenericConstraintKind::Operator { op, .. } => write!(f, "operator {:?}", op),
            HirGenericConstraintKind::Concept { name, .. } => {
                write!(f, "{}", name)
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct HirUnionSignature<'hir> {
    pub declaration_span: Span,
    pub vis: HirVisibility,
    pub name: &'hir str,
    pub name_span: Span,
    pub variants: BTreeMap<&'hir str, HirStructFieldSignature<'hir>>,
    /// Generic type parameter names
    pub generics: Vec<&'hir HirGenericConstraint<'hir>>,
    /// If the union name is mangled, this contains the pre-mangled type
    pub pre_mangled_ty: Option<&'hir HirGenericTy<'hir>>,
    pub docstring: Option<&'hir str>,
}

#[derive(Debug, Clone, PartialEq, Copy, Default)]
pub enum HirVisibility {
    #[default]
    Public,
    Private,
}
impl From<AstVisibility> for HirVisibility {
    fn from(ast_vis: AstVisibility) -> Self {
        match ast_vis {
            AstVisibility::Public => HirVisibility::Public,
            AstVisibility::Private => HirVisibility::Private,
        }
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub enum HirFlag {
    Copyable(Span),
    NonCopyable(Span),
    #[default]
    None,
}

impl From<AstFlag> for HirFlag {
    fn from(ast_flag: AstFlag) -> Self {
        match ast_flag {
            AstFlag::Copyable(span) => HirFlag::Copyable(span),
            AstFlag::NonCopyable(span) => HirFlag::NonCopyable(span),
            AstFlag::None => HirFlag::None,
        }
    }
}

impl HirFlag {
    pub fn span(&self) -> Option<Span> {
        match self {
            HirFlag::Copyable(span) => Some(*span),
            HirFlag::NonCopyable(span) => Some(*span),
            HirFlag::None => None,
        }
    }
    pub fn is_non_copyable(&self) -> bool {
        matches!(self, HirFlag::NonCopyable(_))
    }
    pub fn is_copyable(&self) -> bool {
        matches!(self, HirFlag::Copyable(_))
    }
    pub fn is_no_flag(&self) -> bool {
        matches!(self, HirFlag::None)
    }
}

#[derive(Debug, Clone)]
//Also used for the destructor
pub struct HirStructConstructorSignature<'hir> {
    pub span: Span,
    pub params: Vec<HirFunctionParameterSignature<'hir>>,
    pub type_params: Vec<HirTypeParameterItemSignature<'hir>>,
    pub vis: HirVisibility,
    pub where_clause: Option<Vec<&'hir HirGenericConstraint<'hir>>>,
    /// Whether the constructor's where_clause constraints are satisfied by the concrete types.
    /// Only used for copy constructors. Set to false during monomorphization if constraints aren't met.
    pub is_constraint_satisfied: bool,
    pub docstring: Option<&'hir str>,
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
    pub docstring: Option<&'hir str>,
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
            ConstantValue::Int(i) => write!(f, "{}", i),
            ConstantValue::Float(fl) => write!(f, "{}", fl),
            ConstantValue::UInt(u) => write!(f, "{}", u),
            ConstantValue::String(s) => write!(f, "\"{}\"", s.escape_default()),
            ConstantValue::Bool(b) => write!(f, "{}", b),
            ConstantValue::Char(c) => write!(f, "'{}'", c),
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
    pub docstring: Option<&'hir str>,
}

#[derive(Debug, Clone)]
pub struct HirStructMethodSignature<'hir> {
    pub span: Span,
    pub vis: HirVisibility,
    pub modifier: HirStructMethodModifier,
    pub params: Vec<HirFunctionParameterSignature<'hir>>,
    pub generics: Option<Vec<&'hir HirGenericConstraint<'hir>>>,
    pub type_params: Vec<&'hir HirTypeParameterItemSignature<'hir>>,
    pub return_ty: HirTy<'hir>,
    pub return_ty_span: Option<Span>,
    /// Optional where clause only containing constraints on struct generics.
    pub where_clause: Option<Vec<&'hir HirGenericConstraint<'hir>>>,
    /// Whether the method's where_clause constraints are satisfied by the concrete types.
    /// Set to false during monomorphization if constraints aren't met.
    pub is_constraint_satisfied: bool,
    pub docstring: Option<&'hir str>,
}

#[derive(Debug, Default, Clone, PartialEq)]
pub enum HirStructMethodModifier {
    /// Static method - no `this` parameter
    Static,
    /// Method that takes immutable reference to `this` (&const this)
    Const,
    /// Method that takes mutable reference to `this` (&this)
    Mutable,
    /// Method that consumes ownership of `this` (this)
    #[default]
    Consuming,
}

#[derive(Debug, Clone)]
pub struct HirFunctionSignature<'hir> {
    pub span: Span,
    pub vis: HirVisibility,
    pub params: Vec<HirFunctionParameterSignature<'hir>>,
    pub generics: Vec<&'hir HirGenericConstraint<'hir>>,
    pub type_params: Vec<&'hir HirTypeParameterItemSignature<'hir>>,
    /// The user can declare a function without a return type, in which case the return type is `()`.
    pub return_ty: HirTy<'hir>,
    /// The span of the return type, if it exists.
    pub return_ty_span: Option<Span>,
    pub is_external: bool,
    /// If the function name is mangled, this contains the pre-mangled type
    pub pre_mangled_ty: Option<&'hir HirGenericTy<'hir>>,
    pub docstring: Option<&'hir str>,
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
