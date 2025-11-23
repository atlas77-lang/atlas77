/// Push a constant #index from a constant pool 
/// (``(u)int``, ``float``, ``bool``, ``extern_ptr``, `char`, ``string``, `struct`, ``list``) 
/// onto the stack
/// NB: if the constant is an object (defined as ``struct``, ``list``, ``string``),
/// a new heap object will be created and a reference to it will be pushed onto the stack
pub const LOAD_CONST: u8 = 0x01;

pub const POP: u8 = 0x10;
pub const DUP: u8 = 0x11;
pub const SWAP: u8 = 0x12;

pub const STORE_VAR: u8 = 0x20;
pub const LOAD_VAR: u8 = 0x21;

pub const INDEX_LOAD: u8 = 0x30;
pub const INDEX_STORE: u8 = 0x31;
pub const NEW_LIST: u8 = 0x32;

pub const ADD: u8 = 0x40;
pub const SUB: u8 = 0x41;
pub const MUL: u8 = 0x42;
pub const DIV: u8 = 0x43;
pub const MOD: u8 = 0x44;
///Not in use yet
pub const NEGATE: u8 = 0x45;
pub const EQUAL: u8 = 0x46;
pub const NOT_EQUAL: u8 = 0x47;
pub const GREATER_THAN: u8 = 0x48;
pub const LESS_THAN: u8 = 0x49;
pub const GREATER_THAN_OR_EQUAL: u8 = 0x4A;
pub const LESS_THAN_OR_EQUAL: u8 = 0x4B;

pub const JUMP: u8 = 0x50;
pub const JUMP_IF_FALSE: u8 = 0x51;

pub const RESERVE_LOCAL_SPACE: u8 = 0x60;
pub const CALL_FUNCTION: u8 = 0x61;
pub const CALL_EXTERNAL_FUNCTION: u8 = 0x62;
pub const LOAD_ARGUMENT: u8 = 0x63;
pub const RETURN: u8 = 0x64;

pub const NEW_OBJECT: u8 = 0x70;
pub const LOAD_FIELD: u8 = 0x71;
pub const STORE_FIELD: u8 = 0x72;

pub const CAST_TO: u8 = 0x80;

pub const HALT: u8 = 0xFF;