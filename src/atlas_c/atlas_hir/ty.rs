use crate::atlas_c::atlas_frontend::parser::ast::AstReferenceKind;
use crate::atlas_c::utils::Span;
use std::fmt;
use std::fmt::Formatter;
use std::hash::{DefaultHasher, Hash, Hasher};

#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub struct HirTyId(u64);

const INTEGER_TY_ID: u8 = 0x01;
const FLOAT_TY_ID: u8 = 0x02;
const UNSIGNED_INTEGER_TY_ID: u8 = 0x03;
const BOOLEAN_TY_ID: u8 = 0x04;
const UNIT_TY_ID: u8 = 0x05;
const CHAR_TY_ID: u8 = 0x06;
const STR_TY_ID: u8 = 0x10;
const FUNCTION_TY_ID: u8 = 0x28;
const SLICE_TY_ID: u8 = 0x35;
const INLINE_ARRAY_TY_ID: u8 = 0x36;
const NULLABLE_TY_ID: u8 = 0x40;
const UNINITIALIZED_TY_ID: u8 = 0x50;
const NAMED_TY_ID: u8 = 0x60;
const GENERIC_TY_ID: u8 = 0x70;
const REFERENCE_TY_ID: u8 = 0x80;
#[deprecated(note = "Use Reference types instead")]
const MUT_REFERENCE_TY_ID: u8 = 0x80;
#[deprecated(note = "Use Reference types instead")]
const CONST_REFERENCE_TY_ID: u8 = 0x81;
const POINTER_TY_ID: u8 = 0x90;

impl HirTyId {
    pub fn compute_int_ty_id(size_in_bits: u8) -> Self {
        let mut hasher = DefaultHasher::new();
        (INTEGER_TY_ID, size_in_bits).hash(&mut hasher);
        Self(hasher.finish())
    }

    pub fn compute_float_ty_id(size_in_bits: u8) -> Self {
        let mut hasher = DefaultHasher::new();
        (FLOAT_TY_ID, size_in_bits).hash(&mut hasher);
        Self(hasher.finish())
    }

    pub fn compute_uint_ty_id(size_in_bits: u8) -> Self {
        let mut hasher = DefaultHasher::new();
        (UNSIGNED_INTEGER_TY_ID, size_in_bits).hash(&mut hasher);
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

    pub fn compute_slice_ty_id(ty: &HirTyId) -> Self {
        let mut hasher = DefaultHasher::new();
        (SLICE_TY_ID, ty).hash(&mut hasher);
        Self(hasher.finish())
    }

    pub fn compute_inline_arr_ty_id(ty: &HirTyId, size: usize) -> Self {
        let mut hasher = DefaultHasher::new();
        (INLINE_ARRAY_TY_ID, ty, size).hash(&mut hasher);
        Self(hasher.finish())
    }

    pub fn compute_nullable_ty_id(inner: &HirTyId) -> Self {
        let mut hasher = DefaultHasher::new();
        (NULLABLE_TY_ID, inner).hash(&mut hasher);
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

    pub fn compute_ref_ty_id(inner: &HirTyId, kind: HirReferenceKind) -> Self {
        let mut hasher = DefaultHasher::new();
        (REFERENCE_TY_ID, kind, inner).hash(&mut hasher);
        Self(hasher.finish())
    }

    pub fn compute_mut_ref_ty_id(inner: &HirTyId) -> Self {
        let mut hasher = DefaultHasher::new();
        (MUT_REFERENCE_TY_ID, inner).hash(&mut hasher);
        Self(hasher.finish())
    }

    pub fn compute_readonly_ref_ty_id(inner: &HirTyId) -> Self {
        let mut hasher = DefaultHasher::new();
        (CONST_REFERENCE_TY_ID, inner).hash(&mut hasher);
        Self(hasher.finish())
    }

    pub fn compute_pointer_ty_id(inner: &HirTyId) -> Self {
        let mut hasher = DefaultHasher::new();
        (POINTER_TY_ID, inner).hash(&mut hasher);
        Self(hasher.finish())
    }
}

impl<'hir> From<&'hir HirTy<'hir>> for HirTyId {
    fn from(value: &'hir HirTy<'hir>) -> Self {
        match value {
            HirTy::Integer(i) => Self::compute_int_ty_id(i.size_in_bits),
            HirTy::Float(f) => Self::compute_float_ty_id(f.size_in_bits),
            HirTy::UnsignedInteger(u) => Self::compute_uint_ty_id(u.size_in_bits),
            HirTy::Char(_) => Self::compute_char_ty_id(),
            HirTy::Boolean(_) => Self::compute_boolean_ty_id(),
            HirTy::Unit(_) => Self::compute_unit_ty_id(),
            HirTy::String(_) => Self::compute_str_ty_id(),
            HirTy::Slice(ty) => HirTyId::compute_slice_ty_id(&HirTyId::from(ty.inner)),
            HirTy::InlineArray(ty) => {
                HirTyId::compute_inline_arr_ty_id(&HirTyId::from(ty.inner), ty.size)
            }
            HirTy::Named(ty) => HirTyId::compute_name_ty_id(ty.name),
            HirTy::Uninitialized(_) => Self::compute_uninitialized_ty_id(),
            HirTy::Nullable(ty) => HirTyId::compute_nullable_ty_id(&HirTyId::from(ty.inner)),
            HirTy::Generic(g) => {
                let params = g.inner.iter().map(HirTyId::from).collect::<Vec<_>>();
                HirTyId::compute_generic_ty_id(g.name, &params)
            }
            HirTy::MutableReference(ty) => Self::compute_mut_ref_ty_id(&HirTyId::from(ty.inner)),
            HirTy::ReadOnlyReference(ty) => {
                Self::compute_readonly_ref_ty_id(&HirTyId::from(ty.inner))
            }
            HirTy::PtrTy(ptr_ty) => HirTyId::compute_pointer_ty_id(&HirTyId::from(ptr_ty.inner)),
            HirTy::Function(f) => {
                let parameters = f.params.iter().map(HirTyId::from).collect::<Vec<_>>();
                let ret_ty = HirTyId::from(f.ret_ty);
                HirTyId::compute_function_ty_id(&ret_ty, &parameters)
            }
            HirTy::Reference(r) => HirTyId::compute_ref_ty_id(&HirTyId::from(r.inner), r.kind),
        }
    }
}

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub enum HirTy<'hir> {
    Integer(HirIntegerTy),
    Float(HirFloatTy),
    UnsignedInteger(HirUnsignedIntTy),
    Char(HirCharTy),
    Unit(HirUnitTy),
    Boolean(HirBooleanTy),
    String(HirStringTy),
    Slice(HirSliceTy<'hir>),
    InlineArray(HirInlineArrayTy<'hir>),
    Named(HirNamedTy<'hir>),
    Uninitialized(HirUninitializedTy),
    #[deprecated(note = "Use Option types instead of Nullable types")]
    Nullable(HirNullableTy<'hir>),
    Generic(HirGenericTy<'hir>),
    #[deprecated(note = "Use Reference types instead")]
    MutableReference(HirMutableReferenceTy<'hir>),
    #[deprecated(note = "Use Reference types instead")]
    ReadOnlyReference(HirReadOnlyReferenceTy<'hir>),
    Function(HirFunctionTy<'hir>),
    PtrTy(HirPtrTy<'hir>),
    Reference(HirReferenceTy<'hir>),
}

impl HirTy<'_> {
    pub fn is_const(&self) -> bool {
        matches!(
            self,
            HirTy::Reference(HirReferenceTy {
                kind: HirReferenceKind::ReadOnly,
                ..
            })
        )
    }
    pub fn is_ref(&self) -> bool {
        matches!(self, HirTy::Reference(_))
    }
    pub fn get_inner_ref_ty(&self) -> Option<&HirTy<'_>> {
        match self {
            HirTy::Reference(ref_ty) => Some(ref_ty.inner),
            _ => None,
        }
    }
    pub fn is_unit(&self) -> bool {
        matches!(self, HirTy::Unit(_))
    }
    pub fn is_primitive(&self) -> bool {
        matches!(
            self,
            HirTy::Integer(_)
                | HirTy::Float(_)
                | HirTy::UnsignedInteger(_)
                | HirTy::Boolean(_)
                | HirTy::Unit(_)
                | HirTy::Char(_)
                // TODO: string should not be a primitive anymore
                | HirTy::String(_)
        )
    }
    pub fn is_raw_ptr(&self) -> bool {
        matches!(self, HirTy::PtrTy(_))
    }
    //TODO: Rename the function
    /// Used by the monomorphization pass to generate mangled names.
    /// It solves the issue of using HirTy.to_string(), which returns `Foo_&T`,
    /// Which is not a valid C identifier. It should returns `Foo_T_ptr` instead.
    pub fn get_valid_c_string(&self) -> String {
        match self {
            HirTy::Integer(_) => "int64".to_string(),
            HirTy::Float(_) => "float64".to_string(),
            HirTy::UnsignedInteger(_) => "uint64".to_string(),
            HirTy::Char(_) => "char".to_string(),
            HirTy::Unit(_) => "unit".to_string(),
            HirTy::Boolean(_) => "bool".to_string(),
            HirTy::String(_) => "string".to_string(),
            HirTy::Slice(ty) => format!("list_{}", ty.inner.get_valid_c_string()),
            HirTy::InlineArray(ty) => {
                format!("inlinearr_{}_{}", ty.inner.get_valid_c_string(), ty.size)
            }
            HirTy::Named(ty) => ty.name.to_string(),
            HirTy::Uninitialized(_) => "uninitialized".to_string(),
            HirTy::Nullable(ty) => format!("nullable_{}", ty.inner.get_valid_c_string()),
            HirTy::Generic(ty) => {
                if ty.inner.is_empty() {
                    ty.name.to_string()
                } else {
                    let params = ty
                        .inner
                        .iter()
                        .map(|p| p.get_valid_c_string())
                        .collect::<Vec<_>>()
                        .join("_");
                    format!("{}_{}", ty.name, params)
                }
            }
            HirTy::MutableReference(ty) => format!("{}_mutptr", ty.inner.get_valid_c_string()),
            HirTy::ReadOnlyReference(ty) => format!("{}_ptr", ty.inner.get_valid_c_string()),
            HirTy::PtrTy(ptr_ty) => format!("ptr_{}", ptr_ty.inner.get_valid_c_string()),
            HirTy::Function(func) => {
                let params = func
                    .params
                    .iter()
                    .map(|p| p.get_valid_c_string())
                    .collect::<Vec<_>>()
                    .join("_");
                format!("fn_{}_ret_{}", params, func.ret_ty.get_valid_c_string())
            }
            HirTy::Reference(r) => match r.kind {
                HirReferenceKind::Mutable => format!("{}_mutrf", r.inner.get_valid_c_string()),
                HirReferenceKind::ReadOnly => format!("{}_cstrf", r.inner.get_valid_c_string()),
                HirReferenceKind::Moveable => format!("{}_movrf", r.inner.get_valid_c_string()),
            },
        }
    }
}

impl fmt::Display for HirTy<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            HirTy::Integer(i) => write!(f, "int{}", i.size_in_bits),
            HirTy::Float(flt) => write!(f, "float{}", flt.size_in_bits),
            HirTy::UnsignedInteger(ui) => write!(f, "uint{}", ui.size_in_bits),
            HirTy::Char(_) => write!(f, "char"),
            HirTy::Unit(_) => write!(f, "unit"),
            HirTy::Boolean(_) => write!(f, "bool"),
            HirTy::String(_) => write!(f, "string"),
            HirTy::Slice(ty) => write!(f, "[{}]", ty.inner),
            HirTy::InlineArray(ty) => write!(f, "[{}; {}]", ty.inner, ty.size),
            HirTy::Named(ty) => write!(f, "{}", ty.name),
            HirTy::Uninitialized(_) => write!(f, "uninitialized"),
            HirTy::Nullable(ty) => write!(f, "{}?", ty.inner),
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
            HirTy::MutableReference(ty) => write!(f, "&{}", ty.inner),
            HirTy::ReadOnlyReference(ty) => write!(f, "&const {}", ty.inner),
            HirTy::PtrTy(ptr_ty) => write!(f, "ptr<{}>", ptr_ty.inner),
            HirTy::Function(func) => {
                let params = func
                    .params
                    .iter()
                    .map(|p| format!("{}", p))
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "({}) -> {}", params, func.ret_ty)
            }
            HirTy::Reference(r) => match r.kind {
                HirReferenceKind::Mutable => write!(f, "{}&", r.inner),
                HirReferenceKind::ReadOnly => write!(f, "const {}&", r.inner),
                HirReferenceKind::Moveable => write!(f, "{}&&", r.inner),
            },
        }
    }
}

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub struct HirPtrTy<'hir> {
    pub inner: &'hir HirTy<'hir>,
}

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
#[deprecated(note = "Use Reference types instead")]
pub struct HirMutableReferenceTy<'hir> {
    pub inner: &'hir HirTy<'hir>,
}

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
#[deprecated(note = "Use Reference types instead")]
pub struct HirReadOnlyReferenceTy<'hir> {
    pub inner: &'hir HirTy<'hir>,
}

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub struct HirReferenceTy<'hir> {
    pub span: Span,
    pub inner: &'hir HirTy<'hir>,
    pub kind: HirReferenceKind,
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, Eq, Hash, PartialEq)]
pub enum HirReferenceKind {
    Mutable,
    ReadOnly,
    Moveable,
}

impl From<AstReferenceKind> for HirReferenceKind {
    fn from(value: AstReferenceKind) -> Self {
        match value {
            AstReferenceKind::Mutable => HirReferenceKind::Mutable,
            AstReferenceKind::ReadOnly => HirReferenceKind::ReadOnly,
            AstReferenceKind::Moveable => HirReferenceKind::Moveable,
        }
    }
}

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
//TODO: remove HirNullableTy as this will be replaced by option types
//e.g.: T? -> Option<T>
#[deprecated(note = "Use Option types instead of Nullable types")]
pub struct HirNullableTy<'hir> {
    pub inner: &'hir HirTy<'hir>,
}

/// The char type is a 32-bit Unicode code point.
///
/// It can be considered as a 4-byte integer.
#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub struct HirCharTy {}

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub struct HirSliceTy<'hir> {
    pub inner: &'hir HirTy<'hir>,
}
impl fmt::Display for HirSliceTy<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.inner)
    }
}

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub struct HirInlineArrayTy<'hir> {
    pub inner: &'hir HirTy<'hir>,
    pub size: usize,
}
impl fmt::Display for HirInlineArrayTy<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}; {}", self.inner, self.size)
    }
}

// all the types should hold a span
#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub struct HirUninitializedTy {}

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub struct HirIntegerTy {
    pub size_in_bits: u8,
}

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub struct HirFloatTy {
    pub size_in_bits: u8,
}

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub struct HirUnsignedIntTy {
    pub size_in_bits: u8,
}

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub struct HirUnitTy {}

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub struct HirBooleanTy {}

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub struct HirStringTy {}

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub struct HirFunctionTy<'hir> {
    pub ret_ty: &'hir HirTy<'hir>,
    pub params: Vec<HirTy<'hir>>,
    pub span: Span,
}

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub struct HirGenericTy<'hir> {
    pub name: &'hir str,
    pub inner: Vec<HirTy<'hir>>,
    pub span: Span,
}

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub struct HirNamedTy<'hir> {
    pub name: &'hir str,
    /// Span of the name declaration.
    pub span: Span,
}
