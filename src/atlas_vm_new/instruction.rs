use crate::atlas_vm::runtime::instruction::Type;


#[derive(Debug)]
pub enum Instruction {
    // === Literals & constants ===
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
    Add, Sub, Mul, Div, Mod,
    Eq, Neq, Gt, Gte, Lt, Lte,

    // === Control flow ===
    Jmp { pos: isize },   // Relative unconditional jump
    JmpZ { pos: isize },  // Jump if TOS == false/0

    // === Functions ===
    LocalSpace { nb_vars: u8 },   // Reserve local slots
    Call {
        func_id: usize,
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
    NewObj { class_descriptor: usize }, // Create object
    GetField { field: usize },          // [ObjPtr] -> [Value]
    SetField { field: usize },          // [ObjPtr, Value] -> []

    // === Type ops ===
    CastTo(Type),    // Explicit type coercion (if kept)

    // === Misc ===
    Halt,            // Stop execution
}