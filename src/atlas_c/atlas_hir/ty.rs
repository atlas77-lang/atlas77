use crate::atlas_c::atlas_hir::signature::HirModuleSignature;
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

    pub fn compute_pointer_ty_id(inner: &HirTyId, is_const: bool) -> Self {
        let mut hasher = DefaultHasher::new();
        (POINTER_TY_ID, is_const, inner).hash(&mut hasher);
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
            HirTy::Generic(g) => {
                let params = g.inner.iter().map(HirTyId::from).collect::<Vec<_>>();
                HirTyId::compute_generic_ty_id(g.name, &params)
            }
            HirTy::PtrTy(ptr_ty) => {
                HirTyId::compute_pointer_ty_id(&HirTyId::from(ptr_ty.inner), ptr_ty.is_const)
            }
            HirTy::Function(f) => {
                let parameters = f.params.iter().map(HirTyId::from).collect::<Vec<_>>();
                let ret_ty = HirTyId::from(f.ret_ty);
                HirTyId::compute_function_ty_id(&ret_ty, &parameters)
            }
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
    Generic(HirGenericTy<'hir>),
    Function(HirFunctionTy<'hir>),
    PtrTy(HirPtrTy<'hir>),
}

impl HirTy<'_> {
    /// Returns true if this is a const pointer type (*const T)
    pub fn is_const_ptr(&self) -> bool {
        matches!(self, HirTy::PtrTy(p) if p.is_const)
    }

    /// Returns true if this is a mutable pointer type (*T)
    pub fn is_mutable_ptr(&self) -> bool {
        matches!(self, HirTy::PtrTy(p) if !p.is_const)
    }

    /// Returns the inner type of a pointer type, if this is a pointer
    pub fn get_inner_ptr_ty(&self) -> Option<&HirTy<'_>> {
        match self {
            HirTy::PtrTy(ptr_ty) => Some(ptr_ty.inner),
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

    pub fn is_copyable(&self, signatures: &HirModuleSignature<'_>) -> bool {
        if self.is_primitive() {
            return true;
        }
        match self {
            HirTy::Named(named_ty) => {
                if let Some(struct_sig) = signatures.structs.get(named_ty.name) {
                    struct_sig.copy_constructor.is_some()
                } else {
                    false
                }
            }
            HirTy::Generic(generic_ty) => {
                // No need to monomorphize the name. This is ready for the next rework of the passes.
                // The monomorphization pass will happen AFTER the type checking pass in the future.
                if let Some(struct_sig) = signatures.structs.get(generic_ty.name) {
                    struct_sig.copy_constructor.is_some()
                } else {
                    false
                }
            }
            // Pointers are trivially copyable (they're just addresses)
            HirTy::PtrTy(_) => true,
            _ => false,
        }
    }

    pub fn is_moveable(&self, signatures: &HirModuleSignature<'_>) -> bool {
        if self.is_primitive() {
            return true;
        }
        match self {
            HirTy::Named(named_ty) => {
                if let Some(struct_sig) = signatures.structs.get(named_ty.name) {
                    struct_sig.move_constructor.is_some()
                } else {
                    false
                }
            }
            HirTy::Generic(generic_ty) => {
                // No need to monomorphize the name. This is ready for the next rework of the passes.
                // The monomorphization pass will happen AFTER the type checking pass in the future.
                if let Some(struct_sig) = signatures.structs.get(generic_ty.name) {
                    struct_sig.move_constructor.is_some()
                } else {
                    false
                }
            }
            // Pointers are trivially moveable
            HirTy::PtrTy(_) => true,
            _ => false,
        }
    }

    pub fn is_ptr(&self) -> bool {
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
            HirTy::PtrTy(ptr_ty) => {
                if ptr_ty.is_const {
                    format!("{}_cstptr", ptr_ty.inner.get_valid_c_string())
                } else {
                    format!("{}_mutptr", ptr_ty.inner.get_valid_c_string())
                }
            }
            HirTy::Function(func) => {
                let params = func
                    .params
                    .iter()
                    .map(|p| p.get_valid_c_string())
                    .collect::<Vec<_>>()
                    .join("_");
                format!("fn_{}_ret_{}", params, func.ret_ty.get_valid_c_string())
            }
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
            HirTy::PtrTy(ptr_ty) => {
                if ptr_ty.is_const {
                    write!(f, "*const {}", ptr_ty.inner)
                } else {
                    write!(f, "*{}", ptr_ty.inner)
                }
            }
            HirTy::Function(func) => {
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

/// A raw pointer type: *T (mutable) or *const T (immutable)
///
/// TEMPORARY DESIGN (v0.8.0 MVP): References will be added back in v0.9+
/// with proper lifetime tracking. Current constructor signatures use pointers:
///   - Copy constructor: Foo(from: *const Foo)
///   - Move constructor: Foo(from: *Foo)
#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub struct HirPtrTy<'hir> {
    pub inner: &'hir HirTy<'hir>,
    /// Whether this is a const pointer (*const T) or mutable pointer (*T)
    pub is_const: bool,
    pub span: Span,
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
