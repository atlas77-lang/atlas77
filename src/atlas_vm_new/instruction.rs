use crate::atlas_c::atlas_hir::signature::ConstantValue;
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, HashMap};
use std::ops::Index;


#[derive(Debug, Clone, PartialEq, PartialOrd, Serialize)]
///**TODO**: Those instructions should be lowered to an asm-ish set later on.
///
/// Something akin to a ``[u32; N]`` representation
pub enum Instruction {
    // === Literals & constants ===
    LoadConst(u32),     // Load constant from constant pool
    PushInt(i64),       // Push integer (signed, covers chars + unsigned at type-level)
    PushFloat(f64),     // Push float
    PushBool(bool),     // Push boolean
    PushStr(usize),     // Push string from constant pool (returns pointer)
    PushList(usize),    // Push list from constant pool (returns pointer)
    PushUnit,           // Push unit value ()

    // === Stack manipulation ===
    Pop,                // Discard top of stack
    Dup,                // Duplicate top value
    Swap,               // Swap top two values

    // === Variables ===
    StoreVar(usize),    // Store TOS in local slot
    LoadVar(usize),     // Load local slot onto TOS

    // === Collections & indexing ===
    IndexLoad,          // [ContainerPtr, Index] -> [Value]
    IndexStore,         // [ContainerPtr, Index, Value] -> []

    NewList,            // [Size] -> [ListPtr]

    // === Arithmetic & comparisons ===
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Neq,
    Gt,
    Gte,
    Lt,
    Lte,

    // === Control flow ===
    Jmp { pos: isize },   // Relative unconditional jump
    JmpZ { pos: isize },  // Jump if TOS == false/0

    // === Functions ===
    LocalSpace { nb_vars: u8 },   // Reserve local slots
    Call {
        func_id: String,
        nb_args: u8,
    },
    ///TODO: Extern calls should be handled differently.
    ///This is completely temporary, the VM is still under heavy overhaul
    ExternCall {
        func_id: String,
        nb_args: u8,
    },
    // Call function:
    // - `func_id` is an index into the constant pool
    // - The **first bit** of `func_id` defines the call type:
    //    * 0 → Local function call (resolved to a function defined in this module)
    //    * 1 → Extern function call (resolved via FFI / host environment)
    // - Remaining bits encode the actual function index
    // - `nb_args` is the number of arguments to pop from the stack and pass to the function
    // Stack effect: [arg1, arg2, ..., argN] -> [return_value]
    LoadArg { index: u8 },        // Load function argument
    Return,                       // Return from function

    // === Objects ===
    NewObj { obj_descriptor: usize }, // Create object
    GetField { field: usize },          // [ObjPtr] -> [Value]
    SetField { field: usize },          // [ObjPtr, Value] -> []

    // === Type ops ===
    CastTo(Type),    // Explicit type coercion (if kept)

    // === Misc ===
    Halt,            // Stop execution
}

#[repr(u8)]
#[derive(Debug, Clone, PartialEq, PartialOrd, Serialize, Deserialize)]
pub enum Type {
    Integer,
    Float,
    UnsignedInteger,
    Boolean,
    String,
    Char,
}

/// Read by the VM before execution to import the related functions
#[derive(Debug, Clone, PartialEq, PartialOrd, Serialize)]
pub struct ImportedLibrary {
    pub name: String,
    pub is_std: bool,
}

///todo: Make the program serializable and deserializable
/// This will allow the program to be saved and loaded from a file
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct ProgramDescriptor<'run> {
    pub labels: Vec<Label<'run>>,
    pub entry_point: String,
    pub libraries: Vec<ImportedLibrary>,
    pub global: ConstantPool<'run>,
    pub structs: &'run [StructDescriptor<'run>],
    //todo: Change `usize` to a `FunctionDescriptor`
    pub functions: HashMap<&'run str, usize>,
}

impl<'run> Index<usize> for ProgramDescriptor<'run> {
    type Output = Instruction;

    /// Inefficient implementation
    /// THIS SHOULD REALLY BE REPLACED BY A `[u32; N]` REPRESENTATION
    ///
    /// Right now we are iterating through all labels and their bodies to find the instruction at the given index.
    fn index(&self, index: usize) -> &Self::Output {
        let mut current_index = 0;
        for label in &self.labels {
            if current_index + label.body.len() > index {
                return &label.body[index - current_index];
            }
            current_index += label.body.len();
        }
        panic!("Index out of bounds");
    }
}
impl Default for ProgramDescriptor<'_> {
    fn default() -> Self {
        Self::new()
    }
}
impl ProgramDescriptor<'_> {
    pub fn len(&self) -> usize {
        self.labels.iter().map(|label| label.body.len()).sum()
    }
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
    pub fn new() -> Self {
        Self {
            labels: vec![],
            entry_point: String::new(),
            structs: &[],
            functions: HashMap::new(),
            global: ConstantPool {
                string_pool: &[],
                list_pool: &[],
                function_pool: &[],
            },
            libraries: vec![],
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Serialize)]
pub struct ConstantPool<'run> {
    //todo: Vec<T> -> &'run [T]
    pub string_pool: &'run [&'run str],
    pub list_pool: &'run [ConstantValue],
    pub function_pool: &'run [usize],
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Serialize)]
pub struct StructDescriptor<'run> {
    pub name: &'run str,
    pub fields: Vec<&'run str>,
    pub constants: BTreeMap<&'run str, ConstantValue>,
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Serialize)]
//todo: This should be dropped cuz it's slow
pub struct Label<'run> {
    pub name: &'run str,
    pub position: usize,
    pub body: &'run [Instruction],
}
