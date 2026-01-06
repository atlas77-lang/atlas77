//TODO: Rename this file to program.rs later

pub const _SIZE_CHECK: [(); 0] = [(); 2 * 4 - size_of::<Instr>() - 4];

#[repr(u8)]
#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
//Todo: ensure variants to always be the same value e.g. LoadConst = 1
#[allow(non_camel_case_types)]
pub enum OpCode {
    // === Literals & constants ===
    /// Args: [constant_pool_idx: 24bits]
    LOAD_CONST,

    // === Stack manipulation ===
    ///No Args
    POP,
    ///No Args
    DUP,
    ///No Args
    SWAP,
    // === Variables ===
    /// Args: [local_slot_idx: 24bits]
    STORE_VAR,
    /// Args: [local_slot_idx: 24bits]
    LOAD_VAR,

    // === Collections & indexing ===
    /// No Args
    INDEX_LOAD,
    /// No Args
    INDEX_STORE,
    /// No Args
    STRING_LOAD,
    /// No Args
    STRING_STORE,

    /// No Args
    NEW_ARRAY,

    // === Arithmetic & comparisons ===
    /// No Args
    INT_ADD,
    FLOAT_ADD,
    UINT_ADD,

    INT_SUB,
    FLOAT_SUB,
    UINT_SUB,
    /// Args: [type: 24bits]
    INT_MUL,
    FLOAT_MUL,
    UINT_MUL,
    /// Args: [type: 24bits]
    INT_DIV,
    FLOAT_DIV,
    UINT_DIV,
    /// Args: [type: 24bits]
    INT_MOD,
    UINT_MOD,
    FLOAT_MOD,
    /// Args: [type: 24bits]
    INT_EQUAL,
    UINT_EQUAL,
    FLOAT_EQUAL,
    BOOL_EQUAL,
    CHAR_EQUAL,
    /// Args: [type: 24bits]
    INT_NOT_EQUAL,
    UINT_NOT_EQUAL,
    FLOAT_NOT_EQUAL,
    BOOL_NOT_EQUAL,
    CHAR_NOT_EQUAL,
    /// Args: [type: 24bits]
    INT_GREATER_THAN,
    UINT_GREATER_THAN,
    FLOAT_GREATER_THAN,
    /// Args: [type: 24bits]
    INT_GREATER_THAN_EQUAL,
    UINT_GREATER_THAN_EQUAL,
    FLOAT_GREATER_THAN_EQUAL,
    /// Args: [type: 24bits]
    INT_LESS_THAN,
    UINT_LESS_THAN,
    FLOAT_LESS_THAN,
    /// Args: [type: 24bits]
    INT_LESS_THAN_EQUAL,
    UINT_LESS_THAN_EQUAL,
    FLOAT_LESS_THAN_EQUAL,

    BOOL_AND,
    BOOL_OR,

    // === Control flow ===
    /// Args: [where_to: 24bits]
    JMP,
    /// Args: [where_to: 24bits]
    JMP_Z,

    // === Functions ===
    /// Args: [local_space_size: 24bits]
    LOCAL_SPACE,
    /// Args: [func_id: 24bits]
    CALL,
    /// Args: [func_name_idx: 24bits]
    EXTERN_CALL,
    /// No Args
    RETURN,

    // === Objects ===
    /// Args: [obj_descriptor_id: 24bits]
    NEW_OBJ,
    /// Args: [obj_field_idx: 24bits]
    GET_FIELD,
    /// Args: [obj_field_idx: 24bits]
    SET_FIELD,
    /// No Args
    DELETE_OBJ,

    // === Reference operations ===
    /// Load the address of a local variable onto the stack
    /// Args: [local_slot_idx: 24bits]
    LOAD_VAR_ADDR,
    /// Load the value at the address on top of stack (dereference)
    /// No Args: [Ref] -> [Value]
    LOAD_INDIRECT,
    /// Store a value to the address on top of stack
    /// No Args: [Ref, Value] -> []
    STORE_INDIRECT,
    /// Get the address of a field in an object
    /// Args: [obj_field_idx: 24bits]: [ObjPtr] -> [Ref]
    GET_FIELD_ADDR,
    /// Get the address of an indexed element in a collection
    /// No Args: [Index, Ptr] -> [Ref]
    INDEX_GET_ADDR,

    // === Type ops ===
    //Similar than the arithmetic operation,
    // this is reserved for the primitive types
    /// Args: [type: 24bits]
    CAST_TO,

    // === Misc ===
    /// Deep clone the string on top of stack
    /// No Args: [String] -> [ClonedString]
    CLONE_STRING,
    /// No Args
    NoOp = 254,
    /// No Args
    HALT = 255,
}

#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct Instr {
    pub opcode: OpCode,
    pub arg: Arg,
}

#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct Arg {
    bytes: [u8; 3],
}

impl Default for Arg {
    fn default() -> Self {
        Arg::from_u24(0)
    }
}

impl Arg {
    #[inline]
    pub fn from_i24(v: i32) -> Self {
        // mask ensures only the low 24 bits are used
        let v = (v as u32) & 0x00FF_FFFF;
        Self {
            bytes: [
                (v & 0x0000_FF) as u8,
                ((v >> 8) & 0x0000_FF) as u8,
                ((v >> 16) & 0x0000_FF) as u8,
            ],
        }
    }
    #[inline]
    pub fn from_u24(v: u32) -> Self {
        // mask ensures only the low 24 bits are used
        let v = v & 0x00FF_FFFF;
        Self {
            bytes: [
                (v & 0x0000_FF) as u8,
                ((v >> 8) & 0x0000_FF) as u8,
                ((v >> 16) & 0x0000_FF) as u8,
            ],
        }
    }

    #[inline]
    pub fn as_u24(&self) -> u32 {
        (self.bytes[0] as u32) | ((self.bytes[1] as u32) << 8) | ((self.bytes[2] as u32) << 16)
    }

    pub fn as_i24(&self) -> i32 {
        let unsigned = self.as_u24();
        // If the sign bit (bit 23) is set, extend the sign to the upper bits
        if (unsigned & 0x0080_0000) != 0 {
            (unsigned | 0xFF00_0000) as i32
        } else {
            unsigned as i32
        }
    }

    #[inline]
    pub fn first_16(&self) -> u16 {
        (self.bytes[0] as u16) | ((self.bytes[1] as u16) << 8)
    }

    #[inline]
    pub fn last_16(&self) -> u16 {
        (self.bytes[1] as u16) | ((self.bytes[2] as u16) << 8)
    }

    #[inline]
    pub fn first_8(&self) -> u8 {
        self.bytes[0]
    }

    #[inline]
    pub fn middle_8(&self) -> u8 {
        self.bytes[1]
    }

    #[inline]
    pub fn last_8(&self) -> u8 {
        self.bytes[2]
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructDescriptor {
    pub name: String,
    pub nb_fields: usize,
    pub fields: Vec<String>,
    pub is_union: bool,
}
