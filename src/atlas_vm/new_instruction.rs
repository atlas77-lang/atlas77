/* 
    This is an in progress instruction set for a new Atlas77 VM
    The new atlas77 will be typed, so instructions will be more specific
    And it will be register based rather than stack based
*/

// === Load Constants ===
/// Opcode for loading an 8-bit immediate value onto the stack
/// Args: ``[value: 8bits]``
pub const LOAD_IMM8: u8 = 0x01;
/// Opcode for loading a 16-bit immediate value onto the stack
/// Args: ``[value: 16bits]``
pub const LOAD_IMM16: u8 = 0x02;
/// Opcode for loading a 32-bit immediate value onto the stack
/// Args: ``[value: 32bits]``
pub const LOAD_IMM32: u8 = 0x03;
/// Opcode for loading a 64-bit immediate value onto the stack
/// Args: ``[value: 64bits]``
pub const LOAD_IMM64: u8 = 0x04;
/// Opcode for loading a string literal into the heap and pushing its reference onto the stack
/// 
/// Args: ``[string_idx: 16bits]``
pub const LOAD_STRING: u8 = 0x05;
/// Opcode for loading a list literal into the heap and pushing its reference onto the stack
/// 
/// Args: ``[list_idx: 16bits]``
pub const LOAD_LIST: u8 = 0x06;

