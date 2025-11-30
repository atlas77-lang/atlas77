use crate::atlas_vm::instruction::Instruction;
use std::collections::HashMap;

pub struct CodeGenProgram {
    pub functions: HashMap<String, CodeGenFunction>,
    pub structs: HashMap<String, CodeGenStruct>,
    pub concepts: HashMap<String, CodeGenConcept>,
    pub constant_pool: CodeGenConstantPool,
}

pub struct CodeGenFunction {
    pub name: String,
    pub instructions: Vec<Instruction>,
}

pub struct CodeGenStruct {
    pub name: String,
    pub fields: Vec<String>,
    pub methods: HashMap<String, CodeGenFunction>,
}

/// A concept is similar to an interface in other languages. It defines a set of methods that a type must implement to conform to the concept.
pub struct CodeGenConcept {
    pub name: String,
    pub methods: Vec<String>,
}

pub struct CodeGenConstantPool {
    pub pool: Vec<ConstantValue>,
}

pub enum ConstantValue {
    String(String),
    Integer(i64),
    UnsignedInteger(u64),
    ExternPointer(usize),
    Char(char),
    Float(f64),
    Boolean(bool),
    List(Vec<ConstantValue>),
}
