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
    pub args: Vec<LirPrimitiveType>,
    pub return_type: Option<LirPrimitiveType>,
}

#[derive(Debug, Clone)]
pub struct LirFunction {
    pub name: String,
    pub args: Vec<LirPrimitiveType>,
    pub return_type: Option<LirPrimitiveType>,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LirPrimitiveType {
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
    Char,
    Unit,
}

#[derive(Debug, Clone)]
pub enum LirInstr {
    Add {
        ty: LirPrimitiveType,
        dest: LirOperand,
        a: LirOperand,
        b: LirOperand,
    },
    Sub {
        ty: LirPrimitiveType,
        dest: LirOperand,
        a: LirOperand,
        b: LirOperand,
    },
    Mul {
        ty: LirPrimitiveType,
        dest: LirOperand,
        a: LirOperand,
        b: LirOperand,
    },
    Div {
        ty: LirPrimitiveType,
        dest: LirOperand,
        a: LirOperand,
        b: LirOperand,
    },
    Mod {
        ty: LirPrimitiveType,
        dest: LirOperand,
        a: LirOperand,
        b: LirOperand,
    },
    LessThan {
        ty: LirPrimitiveType,
        dest: LirOperand,
        a: LirOperand,
        b: LirOperand,
    },
    LessThanOrEqual {
        ty: LirPrimitiveType,
        dest: LirOperand,
        a: LirOperand,
        b: LirOperand,
    },
    GreaterThan {
        ty: LirPrimitiveType,
        dest: LirOperand,
        a: LirOperand,
        b: LirOperand,
    },
    GreaterThanOrEqual {
        ty: LirPrimitiveType,
        dest: LirOperand,
        a: LirOperand,
        b: LirOperand,
    },
    Equal {
        ty: LirPrimitiveType,
        dest: LirOperand,
        a: LirOperand,
        b: LirOperand,
    },
    NotEqual {
        ty: LirPrimitiveType,
        dest: LirOperand,
        a: LirOperand,
        b: LirOperand,
    },
    // Load immediate value into a temporary
    LoadImm {
        dst: LirOperand,
        value: LirOperand,
    },
    // Load constant (from constant pool) into a temporary
    LoadConst {
        dst: LirOperand,
        value: LirOperand,
    },
    Call {
        dst: Option<LirOperand>,
        func_name: String,
        args: Vec<LirOperand>,
    },
    ExternCall {
        dst: Option<LirOperand>,
        func_name: String,
        args: Vec<LirOperand>,
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
