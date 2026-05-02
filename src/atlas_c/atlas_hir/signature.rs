use super::ty::HirTy;
use crate::atlas_c::atlas_frontend::parser::ast::{
    AstFlag, AstMethodAttribute, AstNullablePredicateSemantics, AstVisibility,
};
use crate::atlas_c::atlas_hir::expr::{HirBinaryOperator, HirExpr, HirUnaryOp};
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
    pub docstring: Option<&'hir str>,
    /// Name of the module (e.g.: `package name;`)
    pub module_name: &'hir str,
    /// Imported modules and their signatures
    pub imported_modules: BTreeMap<&'hir str, &'hir HirModuleSignature<'hir>>,
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
    pub operators: BTreeMap<HirOverloadableOperatorKind, HirStructMethodSignature<'hir>>,
    pub constants: BTreeMap<&'hir str, &'hir HirStructConstantSignature<'hir>>,
    /// This optional is always Some() after the syntax lowering pass.
    /// It's only optional, because at the beginning of the pass, the destructor might not exist yet
    pub destructor: Option<HirStructDestructorSignature<'hir>>,
    pub had_user_defined_destructor: bool,
    /// True when the struct provides a userland `copy(*const this) -> This`-style API
    /// (or legacy copyable flag during transition).
    pub is_std_copyable: bool,
    /// True when the struct provides a userland `default() -> This` static API.
    pub is_std_default: bool,
    /// True when the struct provides a userland `hash(*const this) -> uint64`-style API.
    pub is_std_hashable: bool,
    /// True when this type is explicitly marked as trivially copyable.
    pub is_trivially_copyable: bool,
    /// Marker set when the struct declaration includes `#[std::nullable]`.
    pub nullable_attribute_span: Option<Span>,
    pub is_instantiated: bool,
    pub docstring: Option<&'hir str>,
    pub is_extern: bool,
    /// Optional C type name override for extern structs.
    pub c_name: Option<&'hir str>,
}

#[derive(Debug, Clone, Copy)]
pub struct HirOverloadableOperator {
    // Where it's implemented or requested
    pub span: Span,
    pub kind: HirOverloadableOperatorKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum HirOverloadableOperatorKind {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    And,
    Or,
    Shl,
    Shr,
    Eq,
    NEq,
    Lt,
    Lte,
    Gt,
    Gte,
    // Binary
    BinXor,
    BinAnd,
    BinOr,

    // Unary
    AsRef,
    DeRef,
    Neg,
    Not,
}

impl From<HirBinaryOperator> for HirOverloadableOperatorKind {
    fn from(op: HirBinaryOperator) -> Self {
        match op {
            HirBinaryOperator::Add => HirOverloadableOperatorKind::Add,
            HirBinaryOperator::Sub => HirOverloadableOperatorKind::Sub,
            HirBinaryOperator::Mul => HirOverloadableOperatorKind::Mul,
            HirBinaryOperator::Div => HirOverloadableOperatorKind::Div,
            HirBinaryOperator::Mod => HirOverloadableOperatorKind::Mod,
            HirBinaryOperator::And => HirOverloadableOperatorKind::And,
            HirBinaryOperator::Or => HirOverloadableOperatorKind::Or,
            HirBinaryOperator::BinXor => HirOverloadableOperatorKind::BinXor,
            HirBinaryOperator::BinAnd => HirOverloadableOperatorKind::BinAnd,
            HirBinaryOperator::BinOr => HirOverloadableOperatorKind::BinOr,
            HirBinaryOperator::ShL => HirOverloadableOperatorKind::Shl,
            HirBinaryOperator::ShR => HirOverloadableOperatorKind::Shr,
            HirBinaryOperator::Eq => HirOverloadableOperatorKind::Eq,
            HirBinaryOperator::Neq => HirOverloadableOperatorKind::NEq,
            HirBinaryOperator::Lt => HirOverloadableOperatorKind::Lt,
            HirBinaryOperator::Lte => HirOverloadableOperatorKind::Lte,
            HirBinaryOperator::Gt => HirOverloadableOperatorKind::Gt,
            HirBinaryOperator::Gte => HirOverloadableOperatorKind::Gte,
        }
    }
}

impl From<HirUnaryOp> for HirOverloadableOperatorKind {
    fn from(op: HirUnaryOp) -> Self {
        match op {
            HirUnaryOp::Neg => HirOverloadableOperatorKind::Neg,
            HirUnaryOp::Not => HirOverloadableOperatorKind::Not,
            HirUnaryOp::AsRef => HirOverloadableOperatorKind::AsRef,
            HirUnaryOp::Deref => HirOverloadableOperatorKind::DeRef,
        }
    }
}

impl Into<String> for HirOverloadableOperatorKind {
    fn into(self) -> String {
        match self {
            HirOverloadableOperatorKind::Add => "add".to_string(),
            HirOverloadableOperatorKind::Sub => "sub".to_string(),
            HirOverloadableOperatorKind::Mul => "mul".to_string(),
            HirOverloadableOperatorKind::Div => "div".to_string(),
            HirOverloadableOperatorKind::Mod => "mod".to_string(),

            HirOverloadableOperatorKind::Shl => "shl".to_string(),
            HirOverloadableOperatorKind::Shr => "shr".to_string(),

            HirOverloadableOperatorKind::And => "and".to_string(),
            HirOverloadableOperatorKind::Or => "or".to_string(),
            HirOverloadableOperatorKind::Eq => "equal".to_string(),
            HirOverloadableOperatorKind::NEq => "not_equal".to_string(),
            HirOverloadableOperatorKind::Lt => "less".to_string(),
            HirOverloadableOperatorKind::Lte => "less_equal".to_string(),
            HirOverloadableOperatorKind::Gt => "greater".to_string(),
            HirOverloadableOperatorKind::Gte => "greater_equal".to_string(),

            HirOverloadableOperatorKind::BinXor => "bin_xor".to_string(),
            HirOverloadableOperatorKind::BinAnd => "bin_and".to_string(),
            HirOverloadableOperatorKind::BinOr => "bin_or".to_string(),

            HirOverloadableOperatorKind::Neg => "neg".to_string(),
            HirOverloadableOperatorKind::Not => "not".to_string(),
            HirOverloadableOperatorKind::AsRef => "as_ref".to_string(),
            HirOverloadableOperatorKind::DeRef => "de_ref".to_string(),
        }
    }
}

impl TryFrom<&str> for HirOverloadableOperatorKind {
    type Error = ();
    fn try_from(s: &str) -> Result<Self, Self::Error> {
        Ok(match s {
            "add" => HirOverloadableOperatorKind::Add,
            "sub" => HirOverloadableOperatorKind::Sub,
            "mul" => HirOverloadableOperatorKind::Mul,
            "div" => HirOverloadableOperatorKind::Div,
            "mod" => HirOverloadableOperatorKind::Mod,

            "shl" => HirOverloadableOperatorKind::Shl,
            "shr" => HirOverloadableOperatorKind::Shr,

            "and" => HirOverloadableOperatorKind::And,
            "or" => HirOverloadableOperatorKind::Or,
            "equal" => HirOverloadableOperatorKind::Eq,
            "not_equal" => HirOverloadableOperatorKind::NEq,
            "less" => HirOverloadableOperatorKind::Lt,
            "less_equal" => HirOverloadableOperatorKind::Lte,
            "greater" => HirOverloadableOperatorKind::Gt,
            "greater_equal" => HirOverloadableOperatorKind::Gte,

            "bin_and" => HirOverloadableOperatorKind::BinAnd,
            "bin_or" => HirOverloadableOperatorKind::BinOr,
            "bin_xor" => HirOverloadableOperatorKind::BinXor,

            "neg" => HirOverloadableOperatorKind::Neg,
            "not" => HirOverloadableOperatorKind::Not,
            "as_ref" => HirOverloadableOperatorKind::AsRef,
            "de_ref" => HirOverloadableOperatorKind::DeRef,
            _ => return Err(()),
        })
    }
}

impl HirOverloadableOperatorKind {
    pub fn is_binary(&self) -> bool {
        !self.is_unary()
    }
    pub fn is_unary(&self) -> bool {
        use HirOverloadableOperatorKind::*;
        matches!(self, Not | Neg)
    }
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
    Std {
        name: &'hir str,
        span: Span,
    },
    // e.g. operator overloading
    Operator {
        op: HirOverloadableOperator,
        span: Span,
    },
    // e.g. user-defined concepts
    Concept {
        name: &'hir str,
        span: Span,
    },
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
    pub is_instantiated: bool,
    pub is_extern: bool,
    /// Optional C type name override for extern unions.
    pub c_name: Option<&'hir str>,
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
    TriviallyCopyable(Span),
    NonCopyable(Span),
    Default(Span),
    Hashable(Span),
    NonMoveable(Span),
    #[default]
    None,
}

impl From<AstFlag> for HirFlag {
    fn from(ast_flag: AstFlag) -> Self {
        match ast_flag {
            AstFlag::TriviallyCopyable(span) => HirFlag::TriviallyCopyable(span),
            AstFlag::Copyable(span) => HirFlag::Copyable(span),
            AstFlag::NonCopyable(span) => HirFlag::NonCopyable(span),
            AstFlag::Default(span) => HirFlag::Default(span),
            AstFlag::Hashable(span) => HirFlag::Hashable(span),
            AstFlag::Intrinsic(_) => HirFlag::None,
            AstFlag::None => HirFlag::None,
        }
    }
}

impl HirFlag {
    pub fn span(&self) -> Option<Span> {
        match self {
            HirFlag::Copyable(span) => Some(*span),
            HirFlag::NonCopyable(span) => Some(*span),
            HirFlag::NonMoveable(span) => Some(*span),
            HirFlag::Default(span) => Some(*span),
            HirFlag::Hashable(span) => Some(*span),
            HirFlag::TriviallyCopyable(span) => Some(*span),
            HirFlag::None => None,
        }
    }
    pub fn is_non_copyable(&self) -> bool {
        matches!(self, HirFlag::NonCopyable(_))
    }
    pub fn is_trivially_copyable(&self) -> bool {
        matches!(self, HirFlag::TriviallyCopyable(_))
    }
    pub fn is_copyable(&self) -> bool {
        matches!(self, HirFlag::Copyable(_))
    }
    pub fn is_non_moveable(&self) -> bool {
        matches!(self, HirFlag::NonMoveable(_))
    }
    pub fn is_no_flag(&self) -> bool {
        matches!(self, HirFlag::None)
    }
    pub fn is_std_default(&self) -> bool {
        matches!(self, HirFlag::Default(_))
    }
    pub fn is_std_hashable(&self) -> bool {
        matches!(self, HirFlag::Hashable(_))
    }
}

#[derive(Debug, Clone)]
//Also used for the destructor
pub struct HirStructDestructorSignature<'hir> {
    pub span: Span,
    pub vis: HirVisibility,
    pub where_clause: Option<Vec<&'hir HirGenericConstraint<'hir>>>,
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
    pub attributes: Vec<HirMethodAttribute>,
    /// True when the method body has been materialized in the owning struct.
    /// For instantiated generic structs, methods can be signature-only until
    /// requested by the type checker.
    pub is_instantiated: bool,
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HirNullablePredicateSemantics {
    Empty,
    Present,
}

impl From<AstNullablePredicateSemantics> for HirNullablePredicateSemantics {
    fn from(value: AstNullablePredicateSemantics) -> Self {
        match value {
            AstNullablePredicateSemantics::Empty => HirNullablePredicateSemantics::Empty,
            AstNullablePredicateSemantics::Present => HirNullablePredicateSemantics::Present,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HirMethodAttribute {
    Nullable(Span),
    NullablePredicate {
        span: Span,
        semantics: HirNullablePredicateSemantics,
    },
    NullableGuarded(Span),
    NullableInfallible(Span),
}

impl From<AstMethodAttribute> for HirMethodAttribute {
    fn from(value: AstMethodAttribute) -> Self {
        match value {
            AstMethodAttribute::Nullable(span) => HirMethodAttribute::Nullable(span),
            AstMethodAttribute::NullablePredicate { span, semantics } => {
                HirMethodAttribute::NullablePredicate {
                    span,
                    semantics: semantics.into(),
                }
            }
            AstMethodAttribute::NullableGuarded(span) => HirMethodAttribute::NullableGuarded(span),
            AstMethodAttribute::NullableInfallible(span) => {
                HirMethodAttribute::NullableInfallible(span)
            }
        }
    }
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
    pub is_intrinsic: bool,
    /// If the function name is mangled, this contains the pre-mangled type
    pub pre_mangled_ty: Option<&'hir HirGenericTy<'hir>>,
    pub docstring: Option<&'hir str>,
    pub is_instantiated: bool,
    /// Optional C symbol name override for extern functions.
    pub c_name: Option<&'hir str>,
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
