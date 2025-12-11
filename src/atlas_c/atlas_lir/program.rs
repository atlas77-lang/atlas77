pub type Label = String;

#[derive(Debug, Clone)]
pub struct LIRProgram {
    pub functions: Vec<LIRFunction>,
}

#[derive(Debug, Clone)]
pub struct LIRFunction {
    pub name: String,
    pub blocks: Vec<LIRBlock>,
}

#[derive(Debug, Clone)]
pub struct LIRBlock {
    pub label: String,
    pub instructions: Vec<LIRInstr>,
}

#[derive(Debug, Clone)]
pub enum LIRType {
    Int8,
    Int16,
    Int32,
    Int64,
    UInt8,
    UInt16,
    UInt32,
    UInt64,
    Float32,
    Float64,
    Boolean,
    Str,
    Char,
    Unit,
}

/// Let's try to only have the necessary instructions for the recursive `fib` function
#[derive(Debug, Clone)]
pub enum LIRInstr {
    Add {
        ty: LIRType,
        dest: LIROperand,
        a: LIROperand,
        b: LIROperand,
    },
    Sub {
        ty: LIRType,
        dest: LIROperand,
        a: LIROperand,
        b: LIROperand,
    },
    LessThanOrEqual {
        ty: LIRType,
        dest: LIROperand,
        a: LIROperand,
        b: LIROperand,
    },
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
    Return {
        value: Option<LIROperand>,
    },
    BranchIf {
        condition: LIROperand,
        then_label: Label,
        else_label: Label,
    },
}

#[derive(Debug, Clone)]
pub enum LIROperand {
    /// A temporary variable
    ///
    /// e.g., t1, t2, etc.
    Temp(u32),
    Arg(u8),
    Const(usize),
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
}
