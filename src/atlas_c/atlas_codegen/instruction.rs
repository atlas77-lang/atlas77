use crate::atlas_c::atlas_hir::signature::ConstantValue;
use crate::atlas_c::atlas_hir::ty::HirTy;
use crate::atlas_vm::vm_data::VMTag;
use std::collections::{BTreeMap, HashMap};
use std::fmt::{Display, Formatter};
use std::ops::Index;

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Instruction {
    // === Literals & constants ===
    LoadConst(ConstantValue), // Load constant from constant pool

    // === Stack manipulation ===
    Pop,  // Discard top of stack
    Dup,  // Duplicate top value
    Swap, // Swap top two values

    // === Variables ===
    StoreVar(usize), // Store TOS in local slot
    LoadVar(usize),  // Load local slot onto TOS

    // === Collections & indexing ===
    IndexLoad,   // [Index, Ptr] -> [Value]
    IndexStore,  // [Index, Ptr, Value] -> []
    StringLoad,  // [Index, Ptr] -> [Value]
    StringStore, // [Index, Ptr, Value] -> []

    NewArray, // [Size] -> [ListPtr]

    // === Arithmetic & comparisons ===
    Add(Type),
    Sub(Type),
    Mul(Type),
    Div(Type),
    Mod(Type),
    Eq(Type),
    Neq(Type),
    Gt(Type),
    Gte(Type),
    Lt(Type),
    Lte(Type),
    //NB: These instructions should be short-circuited and not exist in the VM
    //They are only temporary
    And,
    Or,

    // === Control flow ===
    Jmp {
        pos: isize,
    }, // Relative unconditional jump
    JmpZ {
        pos: isize,
    }, // Jump if TOS == false/0

    // === Functions ===
    LocalSpace {
        nb_vars: u8,
    }, // Reserve local slots
    Call {
        func_name: String,
        nb_args: u8,
    },
    ///TODO: Extern calls should be handled differently.
    ///This is completely temporary, the VM is still under heavy overhaul
    ExternCall {
        func_name: String,
    },
    Return, // Return from function

    // === Objects ===
    NewObj {
        obj_descriptor: usize,
    }, // Create empty zeroed object
    GetField {
        field: usize,
    }, // [ObjPtr] -> [Value]
    SetField {
        field: usize,
    }, // [ObjPtr, Value] -> []
    //Works for List/String/Object
    DeleteObj, // [ObjPtr] -> []

    // === Type ops ===
    CastTo(Type), // Explicit type coercion (if kept)

    // === Misc ===
    Halt, // Stop execution
}

impl Display for Instruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::LoadConst(c) => write!(f, "LoadConst {}", c),
            Instruction::Pop => write!(f, "Pop"),
            Instruction::Dup => write!(f, "Dup"),
            Instruction::Swap => write!(f, "Swap"),
            Instruction::StoreVar(i) => write!(f, "StoreVar {}", i),
            Instruction::LoadVar(i) => write!(f, "LoadVar {}", i),
            Instruction::IndexLoad => write!(f, "IndexLoad"),
            Instruction::IndexStore => write!(f, "IndexStore"),
            Instruction::StringLoad => write!(f, "StringLoad"),
            Instruction::StringStore => write!(f, "StringStore"),
            Instruction::NewArray => write!(f, "NewList"),
            Instruction::Add(t) => write!(f, "Add {:?}", t),
            Instruction::Sub(t) => write!(f, "Sub {:?}", t),
            Instruction::Mul(t) => write!(f, "Mul {:?}", t),
            Instruction::Div(t) => write!(f, "Div {:?}", t),
            Instruction::Mod(t) => write!(f, "Mod {:?}", t),
            Instruction::Eq(t) => write!(f, "Eq {:?}", t),
            Instruction::Neq(t) => write!(f, "Neq {:?}", t),
            Instruction::Gt(t) => write!(f, "Gt {:?}", t),
            Instruction::Gte(t) => write!(f, "Gte {:?}", t),
            Instruction::Lt(t) => write!(f, "Lt {:?}", t),
            Instruction::Lte(t) => write!(f, "Lte {:?}", t),
            Instruction::And => write!(f, "And"),
            Instruction::Or => write!(f, "Or"),
            Instruction::Jmp { pos } => write!(f, "Jmp {}", pos),
            Instruction::JmpZ { pos } => write!(f, "JmpZ {}", pos),
            Instruction::LocalSpace { nb_vars } => write!(f, "LocalSpace {}", nb_vars),
            Instruction::Call {
                func_name: func_id,
                nb_args,
            } => {
                write!(f, "Call {} {}", func_id, nb_args)
            }
            Instruction::ExternCall { func_name: func_id } => {
                write!(f, "ExternCall {}", func_id)
            }
            Instruction::Return => write!(f, "Return"),
            Instruction::NewObj { obj_descriptor } => {
                write!(f, "NewObj {}", obj_descriptor)
            }
            Instruction::GetField { field } => write!(f, "GetField {}", field),
            Instruction::SetField { field } => write!(f, "SetField {}", field),
            Instruction::DeleteObj => write!(f, "DeleteObj"),
            Instruction::CastTo(t) => write!(f, "CastTo {:?}", t),
            Instruction::Halt => write!(f, "Halt"),
        }
    }
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum Type {
    Integer,
    Float,
    UnsignedInteger,
    Boolean,
    String,
    List,
    Char,
    Unit,
    Object,
}

impl Into<VMTag> for Type {
    fn into(self) -> VMTag {
        match self {
            Type::Integer => VMTag::Int64,
            Type::Float => VMTag::Float64,
            Type::UnsignedInteger => VMTag::UInt64,
            Type::Boolean => VMTag::Boolean,
            Type::String => VMTag::String,
            Type::Char => VMTag::Char,
            Type::Unit => VMTag::Unit,
            Type::List => VMTag::List,
            Type::Object => VMTag::Object,
        }
    }
}

impl From<&HirTy<'_>> for Type {
    fn from(ty: &HirTy) -> Self {
        match ty {
            HirTy::Int64(_) => Type::Integer,
            HirTy::Float64(_) => Type::Float,
            HirTy::UInt64(_) => Type::UnsignedInteger,
            HirTy::Boolean(_) => Type::Boolean,
            HirTy::String(_) => Type::String,
            HirTy::Char(_) => Type::Char,
            HirTy::Unit(_) => Type::Unit,
            HirTy::List(_) => Type::List,
            HirTy::Named(_) | HirTy::Generic(_) => Type::Object,
            _ => {
                panic!(
                    "Unsupported type for conversion to Instruction::Type {:?}",
                    ty
                )
            }
        }
    }
}

/// Read by the VM before execution to import the related functions
#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct ImportedLibrary {
    pub name: String,
    pub is_std: bool,
}

///todo: Make the program serializable and deserializable
/// This will allow the program to be saved and loaded from a file
#[derive(Debug, Clone, PartialEq)]
pub struct ProgramDescriptor<'run> {
    pub labels: Vec<Label<'run>>,
    pub entry_point: String,
    pub libraries: Vec<ImportedLibrary>,
    //pub global: ConstantPool<'run>,
    pub structs: &'run [StructDescriptor<'run>],
    //todo: Change `usize` to a `FunctionDescriptor`
    pub functions: HashMap<&'run str, usize>,
}

impl<'run> Index<usize> for ProgramDescriptor<'run> {
    type Output = Instruction;

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
            libraries: vec![],
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct ConstantPool<'run> {
    //todo: Vec<T> -> &'run [T]
    pub string_pool: &'run [&'run str],
    pub list_pool: &'run [ConstantValue],
    pub function_pool: &'run [usize],
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct StructDescriptor<'run> {
    pub name: &'run str,
    pub fields: Vec<&'run str>,
    pub constants: BTreeMap<&'run str, ConstantValue>,
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
//todo: This should be dropped cuz it's slow
pub struct Label<'run> {
    pub name: &'run str,
    pub position: usize,
    pub body: &'run [Instruction],
}

impl Display for Label<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}:", self.name)?;
        for (i, instr) in self.body.iter().enumerate() {
            writeln!(f, "\t{:04} {}", i + self.position, instr)?;
        }
        Ok(())
    }
}
