pub type Label = String;

#[derive(Debug, Clone)]
pub struct LIRProgram {
    pub functions: Vec<LIRFunction>,
}

use std::collections::HashSet;

#[derive(Debug, Clone)]
pub struct LIRFunction {
    pub name: String,
    pub args: Vec<LIRPrimitiveType>,
    pub return_type: Option<LIRPrimitiveType>,
    pub blocks: Vec<LIRBlock>,
}

impl LIRFunction {
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
                LIRTerminator::Branch { target } => {
                    referenced_labels.insert(target.clone());
                }
                LIRTerminator::BranchIf {
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
                block.instructions.is_empty() && matches!(block.terminator, LIRTerminator::None);

            // Keep if referenced OR not empty
            is_referenced || !is_empty
        });
    }
}

#[derive(Debug, Clone)]
pub struct LIRBlock {
    pub label: String,
    pub instructions: Vec<LIRInstr>,
    pub terminator: LIRTerminator,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LIRPrimitiveType {
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
pub enum LIRInstr {
    Add {
        ty: LIRPrimitiveType,
        dest: LIROperand,
        a: LIROperand,
        b: LIROperand,
    },
    Sub {
        ty: LIRPrimitiveType,
        dest: LIROperand,
        a: LIROperand,
        b: LIROperand,
    },
    Mul {
        ty: LIRPrimitiveType,
        dest: LIROperand,
        a: LIROperand,
        b: LIROperand,
    },
    Div {
        ty: LIRPrimitiveType,
        dest: LIROperand,
        a: LIROperand,
        b: LIROperand,
    },
    Mod {
        ty: LIRPrimitiveType,
        dest: LIROperand,
        a: LIROperand,
        b: LIROperand,
    },
    LessThan {
        ty: LIRPrimitiveType,
        dest: LIROperand,
        a: LIROperand,
        b: LIROperand,
    },
    LessThanOrEqual {
        ty: LIRPrimitiveType,
        dest: LIROperand,
        a: LIROperand,
        b: LIROperand,
    },
    GreaterThan {
        ty: LIRPrimitiveType,
        dest: LIROperand,
        a: LIROperand,
        b: LIROperand,
    },
    GreaterThanOrEqual {
        ty: LIRPrimitiveType,
        dest: LIROperand,
        a: LIROperand,
        b: LIROperand,
    },
    Equal {
        ty: LIRPrimitiveType,
        dest: LIROperand,
        a: LIROperand,
        b: LIROperand,
    },
    NotEqual {
        ty: LIRPrimitiveType,
        dest: LIROperand,
        a: LIROperand,
        b: LIROperand,
    },
    Copy {
        dst: LIROperand,
        src: LIROperand,
    },
    // Load immediate value into a temporary
    LoadImm {
        dst: LIROperand,
        value: LIROperand,
    },
    // Load constant (from constant pool) into a temporary
    LoadConst {
        dst: LIROperand,
        value: LIROperand,
    },
    Call {
        dst: Option<LIROperand>,
        func_name: String,
        args: Vec<LIROperand>,
    },
    ExternCall {
        dst: Option<LIROperand>,
        func_name: String,
        args: Vec<LIROperand>,
    },
}

#[derive(Debug, Clone)]
pub enum LIROperand {
    /// A temporary variable
    ///
    /// e.g., t1, t2, etc.
    Temp(u32),
    Arg(u8),
    /// Constant pool index
    Const(usize),
    /// Immediate values
    ImmInt(i64),
    ImmUInt(u64),
    ImmFloat(f64),
    ImmBool(bool),
    ImmChar(char),
}

#[derive(Debug, Clone)]
pub enum LIRTerminator {
    Return {
        value: Option<LIROperand>,
    },
    Branch {
        target: Label,
    },
    BranchIf {
        condition: LIROperand,
        then_label: Label,
        else_label: Label,
    },
    /// Program halt
    Halt,
    /// No terminator (used for blocks that are not yet terminated)
    None,
}
