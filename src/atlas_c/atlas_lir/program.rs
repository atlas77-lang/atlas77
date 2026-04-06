use std::collections::{HashMap, HashSet};

use crate::atlas_c::atlas_hir::signature::ConstantValue;
// TODO: Add Span info to Lir structures for better error reporting
pub type Label = String;

#[derive(Debug, Clone)]
pub struct LirProgram {
    pub functions: Vec<LirFunction>,
    pub extern_functions: Vec<LirExternFunction>,
    pub structs: Vec<LirStruct>,
    pub unions: Vec<LirUnion>,
}

impl LirProgram {
    /// Compute `sizeof(T)` following C layout rules for structs/unions/arrays.
    pub fn size_of_ty(&self, ty: &LirTy) -> usize {
        self.layout_of_ty(ty, &mut HashSet::new()).0
    }

    /// Compute `alignof(T)` following C layout rules for structs/unions/arrays.
    pub fn align_of_ty(&self, ty: &LirTy) -> usize {
        self.layout_of_ty(ty, &mut HashSet::new()).1
    }

    fn layout_of_ty(&self, ty: &LirTy, visiting: &mut HashSet<String>) -> (usize, usize) {
        match ty {
            LirTy::Int8 | LirTy::UInt8 | LirTy::Boolean => (1, 1),
            LirTy::Int16 | LirTy::UInt16 => (2, 2),
            LirTy::Int32 | LirTy::UInt32 | LirTy::Float32 => (4, 4),
            LirTy::Int64 | LirTy::UInt64 | LirTy::Float64 => (8, 8),
            LirTy::Char => (4, 4),
            LirTy::Str | LirTy::Ptr { .. } | LirTy::FnPtr { .. } | LirTy::Unit => (8, 8),
            LirTy::ArrayTy { inner, size } => {
                let (inner_size, inner_align) = self.layout_of_ty(inner, visiting);
                (inner_size.saturating_mul(*size), inner_align)
            }
            LirTy::StructType(name) => {
                if !visiting.insert(format!("S:{}", name)) {
                    return (8, 8);
                }

                let mut offset = 0usize;
                let mut max_align = 1usize;

                if let Some(strukt) = self.structs.iter().find(|s| s.name == *name) {
                    let mut fields: Vec<(&String, &LirTy)> = strukt.fields.iter().collect();
                    fields.sort_by(|a, b| a.0.cmp(b.0));

                    for (_, field_ty) in fields {
                        let (field_size, field_align) = self.layout_of_ty(field_ty, visiting);
                        let field_align = field_align.max(1);
                        offset = Self::align_to(offset, field_align);
                        offset = offset.saturating_add(field_size);
                        max_align = max_align.max(field_align);
                    }
                } else {
                    visiting.remove(&format!("S:{}", name));
                    return (8, 8);
                }

                let total_size = Self::align_to(offset, max_align);
                visiting.remove(&format!("S:{}", name));
                (total_size, max_align)
            }
            LirTy::UnionType(name) => {
                if !visiting.insert(format!("U:{}", name)) {
                    return (8, 8);
                }

                let mut max_size = 0usize;
                let mut max_align = 1usize;

                if let Some(union) = self.unions.iter().find(|u| u.name == *name) {
                    let mut variants: Vec<(&String, &LirTy)> = union.variants.iter().collect();
                    variants.sort_by(|a: &(&String, &LirTy), b| a.0.cmp(b.0));

                    for (_, variant_ty) in variants {
                        let (variant_size, variant_align) = self.layout_of_ty(variant_ty, visiting);
                        max_size = max_size.max(variant_size);
                        max_align = max_align.max(variant_align.max(1));
                    }
                } else {
                    visiting.remove(&format!("U:{}", name));
                    return (8, 8);
                }

                let total_size = Self::align_to(max_size, max_align);
                visiting.remove(&format!("U:{}", name));
                (total_size, max_align)
            }
        }
    }

    fn align_to(value: usize, align: usize) -> usize {
        if align <= 1 {
            value
        } else {
            value.div_ceil(align) * align
        }
    }
}

#[derive(Debug, Clone)]
pub struct LirExternFunction {
    pub name: String,
    pub c_name: Option<String>,
    pub args: Vec<LirTy>,
    pub return_type: Option<LirTy>,
}

#[derive(Debug, Clone)]
/// Represents a union definition in LIR
/// e.g., union Value { a: int32, b: float32 }
pub struct LirUnion {
    pub name: String,
    pub variants: HashMap<String, LirTy>,
}

#[derive(Debug, Clone)]
/// Represents a structure definition in LIR
/// e.g., struct Point { x: int32, y: int32 }
///
/// The methods of the struct are not included here; they are part of the functions in the program.
pub struct LirStruct {
    pub name: String,
    pub fields: HashMap<String, LirTy>,
    pub is_extern: bool,
    pub c_name: Option<String>,
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
    FnPtr { ret: Box<LirTy>, args: Vec<LirTy> },
    Ptr { is_const: bool, inner: Box<LirTy> },
    StructType(String),
    UnionType(String),
    ArrayTy { inner: Box<LirTy>, size: usize },
}

impl LirTy {
    /// Layout-aware size query. Prefer this for structs/unions.
    pub fn size_of_in(&self, program: &LirProgram) -> usize {
        program.size_of_ty(self)
    }

    /// Layout-aware alignment query. Prefer this for structs/unions.
    pub fn align_of_in(&self, program: &LirProgram) -> usize {
        program.align_of_ty(self)
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
    LogicalOr {
        ty: LirTy,
        dest: LirOperand,
        a: LirOperand,
        b: LirOperand,
    },
    LogicalAnd {
        ty: LirTy,
        dest: LirOperand,
        a: LirOperand,
        b: LirOperand,
    },
    Negate {
        ty: LirTy,
        dest: LirOperand,
        src: LirOperand,
    },
    Not {
        ty: LirTy,
        dest: LirOperand,
        src: LirOperand,
    },
    Index {
        ty: LirTy,
        dst: LirOperand,
        src: LirOperand,
        index: LirOperand,
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
    CallPtr {
        ty: LirTy,
        dst: Option<LirOperand>,
        callee: LirOperand,
        args: Vec<LirOperand>,
        param_tys: Vec<LirTy>,
    },
    HeapAllocCopy {
        ty: LirTy,
        dst: LirOperand,
        src: LirOperand,
    },
    ConstructArray {
        ty: LirTy,
        dst: LirOperand,
        size: usize,
    },
    ConstructObject {
        ty: LirTy,
        dst: LirOperand,
        field_values: HashMap<String, LirOperand>,
    },
    /// Delete semantics for a value of the given type.
    ///
    /// - `should_free = true`: delete pointer-like storage (may also run destructor)
    /// - `should_free = false`: run value destruction only (no heap free)
    Delete {
        ty: LirTy,
        src: LirOperand,
        should_free: bool,
    },
    FieldAccess {
        ty: LirTy,
        dst: LirOperand,
        src: LirOperand,
        field_name: String,
    },
    Assign {
        ty: LirTy,
        dst: LirOperand,
        src: LirOperand,
    },
    Cast {
        ty: LirTy,
        from: LirTy,
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
    GlobalFn(String),
    Const(ConstantValue),
    // Should those two be operands or instructions?
    Deref(Box<LirOperand>),
    AsRef(Box<LirOperand>),
    FieldAccess {
        src: Box<LirOperand>,
        field_name: String,
        ty: LirTy,
        is_arrow: bool,
    },
    Index {
        src: Box<LirOperand>,
        index: Box<LirOperand>,
    },
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
