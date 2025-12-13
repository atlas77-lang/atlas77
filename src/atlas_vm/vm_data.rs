use crate::atlas_vm::object::ObjectIndex;
use std::ffi::c_void;
use std::fmt::Formatter;
use std::{
    fmt,
    fmt::Display,
    ops::{Add, Div, Mul, Rem, Sub},
};

#[derive(Copy, Clone)]
pub union RawVMData {
    as_unit: (),
    as_i64: i64,
    as_i32: i32,
    as_u64: u64,
    as_u32: u32,
    as_f64: f64,
    as_f32: f32,
    as_boolean: bool,
    as_char: char,
    /// Pointer to a value in the stack
    ///
    /// The first element is the stack frame index and the second is the index in the frame
    as_stack_ptr: [u32; 2],
    /// Pointer to a function
    as_fn_ptr: usize,
    _as_extern_ptr: *mut c_void,
    /// Pointer to an object in the object map
    as_object: ObjectIndex,
}

#[derive(Copy, Clone)]
pub struct VMData {
    pub tag: VMTag,
    data: RawVMData,
}

#[repr(u8)]
#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Debug)]
pub enum VMTag {
    Unit,

    Int64,
    Int32,
    UInt64,
    UInt32,
    Float64,
    Float32,

    Boolean,
    String,
    Char,
    StackPtr,
    FnPtr,
    List,
    Object,
}

impl From<VMTag> for u8 {
    fn from(tag: VMTag) -> u8 {
        tag as u8
    }
}
impl From<u8> for VMTag {
    fn from(tag: u8) -> VMTag {
        unsafe { std::mem::transmute(tag) }
    }
}

impl Display for VMTag {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                VMTag::Unit => "unit",
                VMTag::Int64 => "int64",
                VMTag::Int32 => "int32",
                VMTag::UInt64 => "uint64",
                VMTag::UInt32 => "uint32",
                VMTag::Float64 => "float64",
                VMTag::Float32 => "float32",
                VMTag::Boolean => "bool",
                VMTag::String => "str",
                VMTag::Char => "char",
                VMTag::StackPtr => "stack_ptr",
                VMTag::FnPtr => "fn_ptr",
                VMTag::List => "list",
                VMTag::Object => "object",
            }
        )
    }
}
impl VMTag {
    pub fn is_equivalent(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Int64, Self::Int32) | (Self::Int32, Self::Int64) => true,
            (Self::UInt64, Self::UInt32) | (Self::UInt32, Self::UInt64) => true,
            (Self::Float64, Self::Float32) | (Self::Float32, Self::Float64) => true,
            _ => self == other,
        }
    }
}

macro_rules! def_new_vm_data_func {
    ($ident: ident, $field: ident, $ty: ty, $const: ident) => {
        #[inline(always)]
        pub fn $ident(val: $ty) -> Self {
            Self::new(VMTag::$const, RawVMData { $field: val })
        }
    };
}

impl VMData {
    pub fn new(tag: VMTag, data: RawVMData) -> Self {
        Self { tag, data }
    }

    pub fn new_unit() -> Self {
        Self {
            tag: VMTag::Unit,
            data: RawVMData { as_unit: () },
        }
    }

    pub fn new_object(val: ObjectIndex) -> Self {
        Self {
            tag: VMTag::Object,
            data: RawVMData { as_object: val },
        }
    }

    pub fn new_string(val: ObjectIndex) -> Self {
        Self {
            tag: VMTag::String,
            data: RawVMData { as_object: val },
        }
    }

    pub fn new_list(val: ObjectIndex) -> Self {
        Self {
            tag: VMTag::List,
            data: RawVMData { as_object: val },
        }
    }

    pub fn is_zero(&self) -> bool {
        match self.tag {
            VMTag::Int64 => self.as_i64() == 0,
            VMTag::Int32 => self.as_i32() == 0,
            VMTag::UInt64 => self.as_u64() == 0,
            VMTag::UInt32 => self.as_u32() == 0,
            VMTag::Float64 => self.as_f64() == 0.0,
            VMTag::Float32 => self.as_f32() == 0.0,
            _ => false,
        }
    }
    def_new_vm_data_func!(new_i64, as_i64, i64, Int64);
    def_new_vm_data_func!(new_i32, as_i32, i32, Int32);
    def_new_vm_data_func!(new_u64, as_u64, u64, UInt64);
    def_new_vm_data_func!(new_u32, as_u32, u32, UInt32);
    def_new_vm_data_func!(new_f64, as_f64, f64, Float64);
    def_new_vm_data_func!(new_f32, as_f32, f32, Float32);
    def_new_vm_data_func!(new_boolean, as_boolean, bool, Boolean);
    def_new_vm_data_func!(new_char, as_char, char, Char);
    def_new_vm_data_func!(new_stack_ptr, as_stack_ptr, [u32; 2], StackPtr);
    def_new_vm_data_func!(new_fn_ptr, as_fn_ptr, usize, FnPtr);
}

impl PartialEq for VMData {
    fn eq(&self, other: &Self) -> bool {
        if self.tag.is_equivalent(&other.tag) {
            return false;
        }
        match self.tag {
            VMTag::Boolean => self.as_boolean() == other.as_boolean(),
            VMTag::Float64 => self.as_f64() == other.as_f64(),
            VMTag::Int64 => self.as_i64() == other.as_i64(),
            VMTag::UInt64 => self.as_u64() == other.as_u64(),
            VMTag::Char => self.as_char() == other.as_char(),
            // comparison based on pointer and not inner data
            _ if self.is_object() => self.as_object() == other.as_object(),
            _ => panic!("Illegal comparison between {:?} and {:?}", self, other),
        }
    }
}

impl PartialOrd for VMData {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        if self.tag != other.tag {
            return None;
        }
        match self.tag {
            VMTag::Float64 => self.as_f64().partial_cmp(&other.as_f64()),
            VMTag::UInt64 => self.as_u64().partial_cmp(&other.as_u64()),
            VMTag::Int64 => self.as_i64().partial_cmp(&other.as_i64()),
            VMTag::Char => self.as_char().partial_cmp(&other.as_char()),
            _ => panic!("Illegal comparison between {:?} and {:?}", self, other),
        }
    }
}

impl Add for VMData {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        match (self.tag, other.tag) {
            (VMTag::Int64, VMTag::Int64) => Self::new_i64(self.as_i64() + other.as_i64()),
            (VMTag::UInt64, VMTag::UInt64) => Self::new_u64(self.as_u64() + other.as_u64()),
            (VMTag::Float64, VMTag::Float64) => Self::new_f64(self.as_f64() + other.as_f64()),
            _ => panic!("Illegal addition between {:?} and {:?}", self, other),
        }
    }
}

impl Sub for VMData {
    type Output = Self;

    fn sub(self, other: Self) -> Self {
        match (self.tag, other.tag) {
            (VMTag::Int64, VMTag::Int64) => Self::new_i64(self.as_i64() - other.as_i64()),
            (VMTag::UInt64, VMTag::UInt64) => Self::new_u64(self.as_u64() - other.as_u64()),
            (VMTag::Float64, VMTag::Float64) => Self::new_f64(self.as_f64() - other.as_f64()),
            _ => panic!("Illegal addition between {:?} and {:?}", self, other),
        }
    }
}

impl Mul for VMData {
    type Output = Self;

    fn mul(self, other: Self) -> Self {
        match (self.tag, other.tag) {
            (VMTag::Int64, VMTag::Int64) => Self::new_i64(self.as_i64() * other.as_i64()),
            (VMTag::UInt64, VMTag::UInt64) => Self::new_u64(self.as_u64() * other.as_u64()),
            (VMTag::Float64, VMTag::Float64) => Self::new_f64(self.as_f64() * other.as_f64()),
            _ => panic!("Illegal addition between {:?} and {:?}", self, other),
        }
    }
}

impl Div for VMData {
    type Output = Self;

    fn div(self, other: Self) -> Self {
        match (self.tag, other.tag) {
            (VMTag::Int64, VMTag::Int64) => Self::new_i64(self.as_i64() / other.as_i64()),
            (VMTag::UInt64, VMTag::UInt64) => Self::new_u64(self.as_u64() / other.as_u64()),
            (VMTag::Float64, VMTag::Float64) => Self::new_f64(self.as_f64() / other.as_f64()),
            _ => panic!("Illegal addition between {:?} and {:?}", self, other),
        }
    }
}

impl Rem for VMData {
    type Output = Self;

    fn rem(self, other: Self) -> Self {
        match (self.tag, other.tag) {
            (VMTag::Int64, VMTag::Int64) => Self::new_i64(self.as_i64() % other.as_i64()),
            (VMTag::UInt64, VMTag::UInt64) => Self::new_u64(self.as_u64() % other.as_u64()),
            (VMTag::Float64, VMTag::Float64) => Self::new_f64(self.as_f64() % other.as_f64()),
            _ => panic!("Illegal addition between {:?} and {:?}", self, other),
        }
    }
}

impl Display for VMData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self.tag {
                VMTag::Unit => "()".to_string(),
                VMTag::Int64 => self.as_i64().to_string(),
                VMTag::UInt64 => self.as_u64().to_string(),
                VMTag::Float64 => self.as_f64().to_string(),
                VMTag::Boolean => self.as_boolean().to_string(),
                VMTag::Char => format!("'{}'", self.as_char()),
                VMTag::StackPtr =>
                    format!("&[{}, {}]", self.as_stack_ptr()[0], self.as_stack_ptr()[1]),
                VMTag::FnPtr => self.as_fn_ptr().to_string(),
                _ if self.is_object() => self.as_object().to_string(),
                _ => "reserved".to_string(),
            }
        )
    }
}

impl fmt::Debug for VMData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "VMData {{ tag: {}, data: {}}}",
            self.tag,
            format_args!("{}", self)
        )
    }
}

macro_rules! enum_variant_function {
    ($getter: ident, $is: ident, $variant: ident, $ty: ty) => {
        #[inline(always)]
        #[must_use]
        pub fn $getter(self) -> $ty {
            unsafe { self.data.$getter }
        }

        #[inline(always)]
        #[must_use]
        pub fn $is(self) -> bool {
            self.tag == VMTag::$variant
        }
    };
}

impl VMData {
    enum_variant_function!(as_i64, is_i64, Int64, i64);
    enum_variant_function!(as_i32, is_i32, Int32, i32);
    enum_variant_function!(as_f64, is_f64, Float64, f64);
    enum_variant_function!(as_f32, is_f32, Float32, f32);
    enum_variant_function!(as_u64, is_u64, UInt64, u64);
    enum_variant_function!(as_u32, is_u32, UInt32, u32);
    enum_variant_function!(as_boolean, is_boolean, Boolean, bool);
    enum_variant_function!(as_char, is_char, Char, char);
    enum_variant_function!(as_stack_ptr, is_stack_ptr, StackPtr, [u32; 2]);
    enum_variant_function!(as_fn_ptr, is_fn_ptr, FnPtr, usize);
    //Clippy doesn't like #[must_use] on () return types
    #[inline(always)]
    pub fn as_unit(self) {
        unsafe { self.data.as_unit }
    }
    #[inline(always)]
    #[must_use]
    pub fn is_unit(self) -> bool {
        self.tag == VMTag::Unit
    }

    #[inline(always)]
    #[must_use]
    pub fn is_object(self) -> bool {
        self.tag == VMTag::Object || self.tag == VMTag::List || self.tag == VMTag::String
    }

    #[inline(always)]
    #[must_use]
    pub fn as_object(self) -> ObjectIndex {
        if !self.is_object() {
            unreachable!(
                "Attempted to get an object from a non-object VMData {:?}",
                self
            );
        }

        unsafe { self.data.as_object }
    }
}
