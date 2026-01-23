use std::collections::HashSet;

use crate::atlas_c::atlas_hir::signature::ConstantValue;
// TODO: Add Span info to Lir structures for better error reporting
pub type Label = String;

#[derive(Debug, Clone)]
pub struct LirProgram {
    pub functions: Vec<LirFunction>,
    pub extern_functions: Vec<LirExternFunction>,
}

#[derive(Debug, Clone)]
pub struct LirExternFunction {
    pub name: String,
    pub args: Vec<LirTy>,
    pub return_type: Option<LirTy>,
}

#[derive(Debug, Clone)]
pub struct LirFunction {
    pub name: String,
    pub args: Vec<LirTy>,
    pub return_type: Option<LirTy>,
    pub blocks: Vec<LirBlock>,
}

impl LirFunction {
    /// Remove blocks that are empty (no instructions, no real terminator)
    /// and not referenced by any branch.
    pub fn remove_dead_blocks(&mut self) {
        // Collect all labels that are targets of branches (as owned strings)
        let mut referenced_labels: HashSet<String> = HashSet::new();

        // Entry block is always referenced
        if let Some(first) = self.blocks.first() {
            referenced_labels.insert(first.label.clone());
        }

        // Collect all branch targets
        for block in &self.blocks {
            match &block.terminator {
                LirTerminator::Branch { target } => {
                    referenced_labels.insert(target.clone());
                }
                LirTerminator::BranchIf {
                    then_label,
                    else_label,
                    ..
                } => {
                    referenced_labels.insert(then_label.clone());
                    referenced_labels.insert(else_label.clone());
                }
                _ => {}
            }
        }

        // Remove blocks that are:
        // 1. Not referenced by any branch AND
        // 2. Empty (no instructions) AND
        // 3. Have no real terminator (None or fallthrough)
        self.blocks.retain(|block| {
            let is_referenced = referenced_labels.contains(&block.label);
            let is_empty =
                block.instructions.is_empty() && matches!(block.terminator, LirTerminator::None);

            // Keep if referenced OR not empty
            is_referenced || !is_empty
        });
    }
}

#[derive(Debug, Clone)]
pub struct LirBlock {
    pub label: String,
    pub instructions: Vec<LirInstr>,
    pub terminator: LirTerminator,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LirTy {
    // Signed Integers
    Int8,
    Int16,
    Int32,
    Int64,
    // Unsigned Integers
    UInt8,
    UInt16,
    UInt32,
    UInt64,
    // Floating Point
    Float32,
    Float64,
    // Other Types
    Boolean,
    Str,
    // Unicode Character
    Char,
    Unit,
    Ref(Box<LirTy>),
}

impl LirTy {
    pub fn size_of(&self) -> usize {
        match self {
            LirTy::Int8 | LirTy::UInt8 | LirTy::Boolean => 1,
            LirTy::Int16 | LirTy::UInt16 => 2,
            LirTy::Int32 | LirTy::UInt32 | LirTy::Float32 => 4,
            LirTy::Int64 | LirTy::UInt64 | LirTy::Float64 => 8,
            LirTy::Char => 4, // Unicode scalar value (4 bytes)
            LirTy::Str | LirTy::Unit | LirTy::Ref(_) => 8, // Pointer size
        }
    }
}

#[derive(Debug, Clone)]
pub enum LirInstr {
    Add {
        ty: LirTy,
        dest: LirOperand,
        a: LirOperand,
        b: LirOperand,
    },
    Sub {
        ty: LirTy,
        dest: LirOperand,
        a: LirOperand,
        b: LirOperand,
    },
    Mul {
        ty: LirTy,
        dest: LirOperand,
        a: LirOperand,
        b: LirOperand,
    },
    Div {
        ty: LirTy,
        dest: LirOperand,
        a: LirOperand,
        b: LirOperand,
    },
    Mod {
        ty: LirTy,
        dest: LirOperand,
        a: LirOperand,
        b: LirOperand,
    },
    LessThan {
        ty: LirTy,
        dest: LirOperand,
        a: LirOperand,
        b: LirOperand,
    },
    LessThanOrEqual {
        ty: LirTy,
        dest: LirOperand,
        a: LirOperand,
        b: LirOperand,
    },
    GreaterThan {
        ty: LirTy,
        dest: LirOperand,
        a: LirOperand,
        b: LirOperand,
    },
    GreaterThanOrEqual {
        ty: LirTy,
        dest: LirOperand,
        a: LirOperand,
        b: LirOperand,
    },
    Equal {
        ty: LirTy,
        dest: LirOperand,
        a: LirOperand,
        b: LirOperand,
    },
    NotEqual {
        ty: LirTy,
        dest: LirOperand,
        a: LirOperand,
        b: LirOperand,
    },
    // Load immediate value into a temporary
    LoadImm {
        ty: LirTy,
        dst: LirOperand,
        value: LirOperand,
    },
    // Load constant (from constant pool) into a temporary
    LoadConst {
        dst: LirOperand,
        value: LirOperand,
    },
    Call {
        ty: LirTy,
        dst: Option<LirOperand>,
        func_name: String,
        args: Vec<LirOperand>,
    },
    ExternCall {
        ty: LirTy,
        dst: Option<LirOperand>,
        func_name: String,
        args: Vec<LirOperand>,
    },
    /// Allocate a new value of the given type
    New {
        ty: LirTy,
        dst: LirOperand,
    },
    /// Free a value of the given type
    Delete {
        ty: LirTy,
        src: LirOperand,
    },
    Assign {
        ty: LirTy,
        dst: LirOperand,
        src: LirOperand,
    },
}

#[derive(Debug, Clone)]
pub enum LirOperand {
    /// A temporary variable
    ///
    /// e.g., t1, t2, etc.
    Temp(u32),
    Arg(u8),
    Const(ConstantValue),
    Deref(Box<LirOperand>),
    AsRef(Box<LirOperand>),
    /// Immediate values
    ImmInt(i64),
    ImmUInt(u64),
    ImmFloat(f64),
    ImmBool(bool),
    ImmChar(char),
    ImmUnit,
}

impl LirOperand {
    pub fn is_temp(&self) -> bool {
        matches!(self, LirOperand::Temp(_))
    }
    pub fn is_arg(&self) -> bool {
        matches!(self, LirOperand::Arg(_))
    }
    pub fn get_temp_id(&self) -> Option<u32> {
        if let LirOperand::Temp(id) = self {
            Some(*id)
        } else {
            None
        }
    }
}

#[derive(Debug, Clone)]
pub enum LirTerminator {
    Return {
        value: Option<LirOperand>,
    },
    Branch {
        target: Label,
    },
    BranchIf {
        condition: LirOperand,
        then_label: Label,
        else_label: Label,
    },
    /// Program halt
    Halt,
    /// No terminator (used for blocks that are not yet terminated)
    None,
}
