use logos::Span;
use serde::Serialize;
use std::fmt;
use std::fmt::Formatter;
use std::hash::{DefaultHasher, Hash, Hasher};

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Copy)]
pub struct HirTyId(u64);

const NULL_TY_ID: u8 = 0x00;
const INTEGER64_TY_ID: u8 = 0x01;
const FLOAT64_TY_ID: u8 = 0x02;
const UNSIGNED_INTEGER_TY_ID: u8 = 0x03;
const BOOLEAN_TY_ID: u8 = 0x04;
const UNIT_TY_ID: u8 = 0x05;
const CHAR_TY_ID: u8 = 0x06;
const STR_TY_ID: u8 = 0x10;
const FUNCTION_TY_ID: u8 = 0x28;
const LIST_TY_ID: u8 = 0x39;
const NULLABLE_TY_ID: u8 = 0x40;
const READONLY_TY_ID: u8 = 0x41;
const UNINITIALIZED_TY_ID: u8 = 0x50;
const NAMED_TY_ID: u8 = 0x60;
const GENERIC_TY_ID: u8 = 0x70;

impl HirTyId {
    pub fn compute_null_ty_id() -> Self {
        let mut hasher = DefaultHasher::new();
        NULL_TY_ID.hash(&mut hasher);
        Self(hasher.finish())
    }
    pub fn compute_integer64_ty_id() -> Self {
        let mut hasher = DefaultHasher::new();
        INTEGER64_TY_ID.hash(&mut hasher);
        Self(hasher.finish())
    }

    pub fn compute_float64_ty_id() -> Self {
        let mut hasher = DefaultHasher::new();
        FLOAT64_TY_ID.hash(&mut hasher);
        Self(hasher.finish())
    }

    pub fn compute_uint64_ty_id() -> Self {
        let mut hasher = DefaultHasher::new();
        UNSIGNED_INTEGER_TY_ID.hash(&mut hasher);
        Self(hasher.finish())
    }

    pub fn compute_boolean_ty_id() -> Self {
        let mut hasher = DefaultHasher::new();
        BOOLEAN_TY_ID.hash(&mut hasher);
        Self(hasher.finish())
    }

    pub fn compute_unit_ty_id() -> Self {
        let mut hasher = DefaultHasher::new();
        UNIT_TY_ID.hash(&mut hasher);
        Self(hasher.finish())
    }

    pub fn compute_char_ty_id() -> Self {
        let mut hasher = DefaultHasher::new();
        CHAR_TY_ID.hash(&mut hasher);
        Self(hasher.finish())
    }

    pub fn compute_str_ty_id() -> Self {
        let mut hasher = DefaultHasher::new();
        STR_TY_ID.hash(&mut hasher);
        Self(hasher.finish())
    }

    pub fn compute_function_ty_id(ret_ty: &HirTyId, params: &[HirTyId]) -> Self {
        let mut hasher = DefaultHasher::new();

        (FUNCTION_TY_ID, ret_ty, params).hash(&mut hasher);
        Self(hasher.finish())
    }

    pub fn compute_list_ty_id(ty: &HirTyId) -> Self {
        let mut hasher = DefaultHasher::new();
        (LIST_TY_ID, ty).hash(&mut hasher);
        Self(hasher.finish())
    }

    pub fn compute_nullable_ty_id(inner: &HirTyId) -> Self {
        let mut hasher = DefaultHasher::new();
        (NULLABLE_TY_ID, inner).hash(&mut hasher);
        Self(hasher.finish())
    }

    /// Not used in type system yet.
    pub fn compute_readonly_ty_id(inner: &HirTyId) -> Self {
        let mut hasher = DefaultHasher::new();
        (READONLY_TY_ID, inner).hash(&mut hasher);
        Self(hasher.finish())
    }

    pub fn compute_uninitialized_ty_id() -> Self {
        let mut hasher = DefaultHasher::new();
        UNINITIALIZED_TY_ID.hash(&mut hasher);
        Self(hasher.finish())
    }

    pub fn compute_name_ty_id(name: &str) -> Self {
        let mut hasher = DefaultHasher::new();
        (NAMED_TY_ID, name).hash(&mut hasher);
        Self(hasher.finish())
    }

    pub fn compute_generic_ty_id(name: &str, params: &[HirTyId]) -> Self {
        let mut hasher = DefaultHasher::new();
        (GENERIC_TY_ID, name, params).hash(&mut hasher);
        Self(hasher.finish())
    }
}

impl<'hir> From<&'hir HirTy<'hir>> for HirTyId {
    fn from(value: &'hir HirTy<'hir>) -> Self {
        match value {
            HirTy::Null(_) => Self::compute_null_ty_id(),
            HirTy::Int64(_) => Self::compute_integer64_ty_id(),
            HirTy::Float64(_) => Self::compute_float64_ty_id(),
            HirTy::UInt64(_) => Self::compute_uint64_ty_id(),
            HirTy::Char(_) => Self::compute_char_ty_id(),
            HirTy::Boolean(_) => Self::compute_boolean_ty_id(),
            HirTy::Unit(_) => Self::compute_unit_ty_id(),
            HirTy::String(_) => Self::compute_str_ty_id(),
            HirTy::List(ty) => HirTyId::compute_list_ty_id(&HirTyId::from(ty.inner)),
            HirTy::Named(ty) => HirTyId::compute_name_ty_id(ty.name),
            HirTy::Uninitialized(_) => Self::compute_uninitialized_ty_id(),
            HirTy::Nullable(ty) => HirTyId::compute_nullable_ty_id(&HirTyId::from(ty.inner)),
            HirTy::Const(ty) => HirTyId::compute_readonly_ty_id(&HirTyId::from(ty.inner)),
            HirTy::Generic(g) => {
                let params = g.inner.iter().map(HirTyId::from).collect::<Vec<_>>();
                HirTyId::compute_generic_ty_id(g.name, &params)
            }
            HirTy::_Function(f) => {
                let parameters = f.params.iter().map(HirTyId::from).collect::<Vec<_>>();
                let ret_ty = HirTyId::from(f.ret_ty);
                HirTyId::compute_function_ty_id(&ret_ty, &parameters)
            }
        }
    }
}

#[derive(Debug, Clone, Serialize, Eq, Hash, PartialEq)]
pub enum HirTy<'hir> {
    Null(HirNullTy),
    Int64(HirIntegerTy),
    Float64(HirFloatTy),
    UInt64(HirUnsignedIntTy),
    Char(HirCharTy),
    Unit(HirUnitTy),
    Boolean(HirBooleanTy),
    String(HirStringTy),
    List(HirListTy<'hir>),
    Named(HirNamedTy<'hir>),
    Uninitialized(HirUninitializedTy),
    Nullable(HirNullableTy<'hir>),
    Const(HirConstTy<'hir>),
    Generic(HirGenericTy<'hir>),
    _Function(HirFunctionTy<'hir>),
}

impl HirTy<'_> {
    pub fn is_const(&self) -> bool {
        matches!(self, HirTy::Const(_))
    }
}

impl fmt::Display for HirTy<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            HirTy::Null(_) => write!(f, "null"),
            HirTy::Int64(_) => write!(f, "int64"),
            HirTy::Float64(_) => write!(f, "float64"),
            HirTy::UInt64(_) => write!(f, "uint64"),
            HirTy::Char(_) => write!(f, "char"),
            HirTy::Unit(_) => write!(f, "unit"),
            HirTy::Boolean(_) => write!(f, "bool"),
            HirTy::String(_) => write!(f, "string"),
            HirTy::List(ty) => write!(f, "[{}]", ty),
            HirTy::Named(ty) => write!(f, "{}", ty.name),
            HirTy::Uninitialized(_) => write!(f, "uninitialized"),
            HirTy::Nullable(ty) => write!(f, "{}?", ty.inner),
            HirTy::Const(ty) => write!(f, "const {}", ty.inner),
            HirTy::Generic(ty) => {
                if ty.inner.is_empty() {
                    write!(f, "{}", ty.name)
                } else {
                    let params = ty
                        .inner
                        .iter()
                        .map(|p| format!("{}", p))
                        .collect::<Vec<_>>()
                        .join(", ");
                    write!(f, "{}<{}>", ty.name, params)
                }
            }
            HirTy::_Function(func) => {
                let params = func
                    .params
                    .iter()
                    .map(|p| format!("{}", p))
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "({}) -> {}", params, func.ret_ty)
            }
        }
    }
}

#[derive(Debug, Clone, Serialize, Eq, Hash, PartialEq)]
pub struct HirConstTy<'hir> {
    pub inner: &'hir HirTy<'hir>,
}

#[derive(Debug, Clone, Serialize, Eq, Hash, PartialEq)]
pub struct HirNullTy {}

#[derive(Debug, Clone, Serialize, Eq, Hash, PartialEq)]
pub struct HirNullableTy<'hir> {
    pub inner: &'hir HirTy<'hir>,
}

/// The char type is a 32-bit Unicode code point.
///
/// It can be considered as a 4-byte integer.
#[derive(Debug, Clone, Serialize, Eq, Hash, PartialEq)]
pub struct HirCharTy {}

#[derive(Debug, Clone, Serialize, Eq, Hash, PartialEq)]
pub struct HirListTy<'hir> {
    pub inner: &'hir HirTy<'hir>,
}
impl fmt::Display for HirListTy<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.inner)
    }
}

// all the types should hold a span
#[derive(Debug, Clone, Serialize, Eq, Hash, PartialEq)]
pub struct HirUninitializedTy {}

#[derive(Debug, Clone, Serialize, Eq, Hash, PartialEq)]
pub struct HirIntegerTy {}

#[derive(Debug, Clone, Serialize, Eq, Hash, PartialEq)]
pub struct HirFloatTy {}

#[derive(Debug, Clone, Serialize, Eq, Hash, PartialEq)]
pub struct HirUnsignedIntTy {}

#[derive(Debug, Clone, Serialize, Eq, Hash, PartialEq)]
pub struct HirUnitTy {}

#[derive(Debug, Clone, Serialize, Eq, Hash, PartialEq)]
pub struct HirBooleanTy {}

#[derive(Debug, Clone, Serialize, Eq, Hash, PartialEq)]
pub struct HirStringTy {}

#[derive(Debug, Clone, Serialize, Eq, Hash, PartialEq)]
pub struct HirFunctionTy<'hir> {
    pub ret_ty: &'hir HirTy<'hir>,
    pub params: Vec<HirTy<'hir>>,
}

#[derive(Debug, Clone, Serialize, Eq, Hash, PartialEq)]
pub struct HirGenericTy<'hir> {
    pub name: &'hir str,
    pub inner: Vec<HirTy<'hir>>,
}

#[derive(Debug, Clone, Serialize, Eq, Hash, PartialEq)]
pub struct HirNamedTy<'hir> {
    pub name: &'hir str,
    /// Span of the name declaration.
    pub span: Span,
}
