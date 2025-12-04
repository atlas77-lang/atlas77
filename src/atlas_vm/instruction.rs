#[repr(u8)]
#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
//Todo: ensure variants to always be the same value e.g. LoadConst = 1
pub enum OpCode {
    // === Literals & constants ===
    /// Args: [constant_pool_idx: 24bits]
    LoadConst,

    // === Stack manipulation ===
    ///No Args
    Pop,
    ///No Args
    Dup,
    ///No Args
    Swap,

    // === Variables ===
    /// Args: [local_slot_idx: 24bits]
    StoreVar,
    /// Args: [local_slot_idx: 24bits]
    LoadVar,

    // === Collections & indexing ===
    /// No Args
    IndexLoad,
    /// No Args
    IndexStore,
    /// No Args
    NewList,

    // === Arithmetic & comparisons ===
    //NB: It is not possible for obj to use one of these OpCode
    //Even if they have implemented one of the operator.
    //Why? Because at compile time, those `obj1+obj2` becomes `add(obj1, obj2)`
    //The type in those instructions can only be one of those:
    //int8, int16, int32, int64, uint8, uint16, uint32, uint64, float32, float64, char
    /// Args: [type: 24bits]
    Add,
    /// Args: [type: 24bits]
    Sub,
    /// Args: [type: 24bits]
    Mul,
    /// Args: [type: 24bits]
    Div,
    /// Args: [type: 24bits]
    Mod,
    /// Args: [type: 24bits]
    Eq,
    /// Args: [type: 24bits]
    Neq,
    /// Args: [type: 24bits]
    Gt,
    /// Args: [type: 24bits]
    Gte,
    /// Args: [type: 24bits]
    Lt,
    /// Args: [type: 24bits]
    Lte,

    // === Control flow ===
    /// Args: [where_to: 24bits]
    Jmp,
    /// Args: [where_to: 24bits]
    JmpZ,

    // === Functions ===
    /// Args: [local_space_size: 24bits]
    LocalSpace,
    /// Args: [func_id: 24bits]
    Call,
    /// Args: [func_name_idx: 24bits]
    ExternCall,
    /// No Args
    Return,

    // === Objects ===
    /// Args: [obj_descriptor_id: 24bits]
    NewObj,
    /// Args: [obj_field_idx: 24bits]
    GetField,
    /// Args: [obj_field_idx: 24bits]
    SetField,

    // === Type ops ===
    //Similar than the arithmetic operation,
    // this is reserved for the primitive types
    /// Args: [type: 24bits]
    CastTo,

    // === Misc ===
    /// No Args
    Halt,
    /// No Args
    NoOp,
}

pub const SIZE_CHECK: [(); 0] = [(); 2 * 4 - size_of::<Instr>() - 4];
#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct Instr {
    pub opcode: OpCode,
    pub arg: Arg,
}

#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct Arg {
    bytes: [u8; 3]
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
        (self.bytes[0] as u32)
            | ((self.bytes[1] as u32) << 8)
            | ((self.bytes[2] as u32) << 16)
    }

    pub fn get_all(&self) -> u32 {
        (self.bytes[0] as u32) | ((self.bytes[1] as u32) << 8 | ((self.bytes[2] as u32) << 16))
    }
    #[inline]
    pub fn first_16(&self) -> u16 {
        (self.bytes[0] as u16) | ((self.bytes[1] as u16) << 8)
    }

    #[inline]
    pub fn last_16(&self) -> u16 {
        // bytes[1] is low, bytes[2] is high
        (self.bytes[1] as u16) | ((self.bytes[2] as u16) << 8)
    }

    #[inline]
    pub fn first_8(&self) -> u8 { self.bytes[0] }

    #[inline]
    pub fn middle_8(&self) -> u8 { self.bytes[1] }

    #[inline]
    pub fn last_8(&self) -> u8 { self.bytes[2] }
}
