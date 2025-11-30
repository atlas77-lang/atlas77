use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, PartialOrd, Serialize, Deserialize)]
pub struct Program {
    pub instructions: Vec<u32>,
    /// A constant can be an (unsigned) integer, float, boolean, extern_ptr, string, list or object
    pub constant_pool: Vec<ConstantValue>,
    pub function_pool: Vec<usize>,
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Serialize, Deserialize)]
pub enum ConstantValue {
    Integer(i64),
    UnsignedInteger(u64),
    ExternPtr(usize),
    Float(f64),
    Boolean(bool),
    String(String),
    List(Vec<ConstantValue>),
    Object(Vec<(String, ConstantValue)>),
}
