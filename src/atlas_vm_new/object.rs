pub type ObjectDescriptorId = u64;
pub type TypeId = u64;

pub const I8_TYPE_ID: TypeId = 0;
pub const I16_TYPE_ID: TypeId = 1;
pub const I32_TYPE_ID: TypeId = 2;
pub const I64_TYPE_ID: TypeId = 3;
pub const F32_TYPE_ID: TypeId = 4;
pub const F64_TYPE_ID: TypeId = 5;
pub const U8_TYPE_ID: TypeId = 6;
pub const U16_TYPE_ID: TypeId = 7;
pub const U32_TYPE_ID: TypeId = 8;
pub const U64_TYPE_ID: TypeId = 9;
pub const BOOL_TYPE_ID: TypeId = 10;
pub const CHAR_TYPE_ID: TypeId = 11;

pub struct Object {
    pub ref_count: u16,
    pub descriptor: ObjectDescriptorId,
    pub data: Vec<u8>,
}

pub struct ObjectDescriptor {
    pub type_id: u64,
    pub size: u16,
    pub fields: Vec<ObjectField>,
    pub functions: Vec<ObjectFunction>,
}

pub struct ObjectField {
    pub name: &'static str,
    pub type_id: TypeId,
    pub size: u16,
    pub offset: u16,
}

pub struct ObjectFunction {
    pub name: &'static str,
    pub ret_type_id: TypeId,
    pub args: Vec<ObjectField>,
}
