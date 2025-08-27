//NB: This is a dumb down version of the instruction set.
//A more powerful version will be done for the v0.5.2 & v0.5.3

use std::collections::{BTreeMap, HashMap};
use std::ops::Index;

use crate::atlas_c::atlas_hir::signature::ConstantValue;
use crate::atlas_vm::memory;
use serde::{Deserialize, Serialize};

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

/// The instruction set for the VM
#[derive(Debug, Clone, PartialEq, PartialOrd, Serialize, Deserialize)]
pub enum Instruction<'run> {
    PushInt(i64),
    PushFloat(f64),
    PushUnsignedInt(u64),
    PushBool(bool),
    PushChar(char),
    /// Push a string from the constant pool
    /// The string is directly put in the memory and its pointer is pushed to the stack
    PushStr(usize),
    /// Push a list from the constant pool
    /// The list is directly put in the memory and its pointer is pushed to the stack
    PushList(usize),
    PushNull,
    PushUnit,

    /// Store the value at the top of the stack to its corresponding slot in the stack variable
    ///
    /// cf. [memory::stack::Stack] for more information on variable slots
    StoreVar(usize),
    /// Load the value from the stack variable to the top of the stack
    ///
    /// cf. [memory::stack::Stack] for more information on variable slots
    LoadVar(usize),

    Pop,
    Swap,
    Dup,

    StoreVarMap {
        var_name: &'run str,
    },

    LoadVarMap {
        var_name: &'run str,
    },

    /// Stack state:
    ///
    /// - **Bottom** `[ListPointer, Index,]` **Top**
    ///
    /// Load a value from a list to the top of the stack
    ListLoad,
    /// Stack state:
    ///
    /// - **Bottom** `[Index, ListPointer, Value]` **Top**
    ///
    /// Store a value in a given list
    ListStore,
    /// Create a new list, the size is the top of the stack
    NewList,
    ListIndex(usize),
    /// Stack state:
    ///
    /// - **Bottom** `[StrPointer, Index,]` **Top**
    ///
    /// Load a value from a str to the top of the stack
    StringLoad,
    /// Stack state:
    ///
    /// - **Bottom** `[StrPointer, Index, Value,]` **Top**
    ///
    /// Store a value in a given str
    StringStore,

    CastTo(Type),
    //Math
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

    /// Relative unconditional jump
    Jmp {
        pos: isize,
    },
    /// Relative Jump if the top of the stack value is eq to 0
    JmpZ {
        pos: isize,
    },
    /// Make space for the local variables of a function
    /// todo: Rename the instruction
    LocalSpace {
        nb_vars: u8,
    },
    Call {
        nb_args: u8,
    },
    FunctionCall {
        function_name: &'run str,
        nb_args: u8,
    },
    LoadArg {
        index: u8,
    },

    ExternCall {
        function_name: &'run str,
        nb_args: u8,
    },
    Return,

    /// Delete the object from memory (the object pointer is at the top of the stack)
    DeleteObj,
    /// Stack:
    /// - **[ClassPtr,] -> [FieldValue,]**
    GetField {
        field: usize,
    },
    /// Stack:
    /// - [ClassPtr, Value] -> []
    SetField {
        field: usize,
    },
    /// Create a new object
    /// The information about the object is in the constant pool
    NewObj {
        class_descriptor: usize,
    },
    Halt,
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
    type Output = Instruction<'run>;

    /// Inefficient implementation
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
    pub body: &'run [Instruction<'run>],
}
