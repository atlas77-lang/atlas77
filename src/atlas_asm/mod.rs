//The codegen outputs a `Vec<Instruction>` and this crate will try to take it and output a `Vec<u8>` or a `Vec<u16>`

use serde::{Deserialize, Serialize};

/// # The opcodes for the VM
///
/// ## Instruction format
/// An instruction in the atlas VM is a 1 byte opcode followed by the immediate data

pub const NOP: u8 = 0x00;

pub enum OpCode {
    /// No operation
    Nop = 0x00,

    // Constants
    I8Load,
    I16Load,
    I32Load,
    I64Load,

    F32Load,
    F64Load,

    U8Load,
    U16Load,
    U32Load,
    U64Load,

    BoolLoad,

    CharLoad,

    StrLoad,

    FnPtrLoad,
    PushList,
    PushUnit,
    PushNull,

    CastTo,

    // Stack operations
    Pop,
    Swap,
    Dup,
    Rot,

    //Variables
    Get,
    Store,

    // Arithmetics
    Mul,
    Div,
    Sub,
    Mod,
    Add,

    // Comparisons
    Eq,
    Neq,
    Gt,
    Gte,
    Lt,
    Lte,

    // Jumps
    DJmp,
    RJmp,
    JmpZ,
    Call,
    DirectCall,
    ExternCall,
    Return,

    NewList,
    ListCopy,
    ListLoad,
    ListStore,

    Halt,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Program {
    pub instructions: Vec<u8>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ConstantPool {
    pub strings: Vec<String>,
    pub functions: Vec<FunctionDescriptor>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct FunctionPool {
    pub functions: Vec<FunctionDescriptor>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct FunctionDescriptor {
    pub name: String,
    pub nb_args: u8,
}
