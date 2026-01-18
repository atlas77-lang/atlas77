// Will replace atlas_c/atlas_codegen & atlas_c/atlas_asm in the future
// Will also remove the need for atlas_vm/
// Will use Cranelift for code generation
// Takes in Lir and outputs machine code
// Let's try to ONLY support recursive fibonacci for now

/*
 * All the todos:
 * - Add extern functions to the LIR and codegen them properly
 *      (see if it's possible to just link to libc directly)
 * - Currently, calling a function will add its FuncRef every time,
 *      we should cache those to avoid duplicates (HashMap<String, FuncRef>)
 * - Support more LIR instructions (We already have everything needed for fibonacci, but still)
 * - Potentially review the LIR design to make sure it's optimal for codegen 
 *      (I encountered a lot of issues while coding this)
 * - IDK, test it? I hope to be able to run fib(40) & "Hello, Atlas!" soon™️
*/

use crate::atlas_c::atlas_lir::program::{
    LirBlock, LirFunction, LirInstr, LirOperand, LirPrimitiveType, LirProgram, LirTerminator,
};
use cranelift::codegen::ir::condcodes::IntCC;
use cranelift::codegen::ir::types::*;
use cranelift::codegen::ir::{AbiParam, Function, InstBuilder, Signature, UserFuncName};
use cranelift::prelude::{ExtFuncData, ExternalName};
use cranelift::codegen::isa::CallConv;
use cranelift::codegen::settings;
use cranelift::codegen::verifier::verify_function;
use cranelift::frontend::{FunctionBuilder, FunctionBuilderContext};
use cranelift::prelude::{Block, Variable};
use std::collections::HashMap;

const ARG_BASE: u32 = 100_000;

// Everything lives in the same namespace for now
pub fn codegen_program(lir_program: &LirProgram) -> Vec<Function> {
    let mut functions = Vec::new();

    let mut func_map: HashMap<String, u32> = HashMap::new();

    for (index, lir_function) in lir_program.functions.iter().enumerate() {
        func_map.insert(lir_function.name.clone(), index as u32);
    }

    for (index, lir_function) in lir_program.functions.iter().enumerate() {
        let func = codegen_function(lir_function, &mut func_map, index as u32);
        functions.push(func);
    }

    functions
}

pub fn codegen_function(lir_function: &LirFunction, func_map: &mut HashMap<String, u32>, index: u32) -> Function {
    let mut sig = Signature::new(CallConv::SystemV);
    // Build signature from LIR function args and return type.
    for arg in &lir_function.args {
        match arg {
            &LirPrimitiveType::Int64 => sig.params.push(AbiParam::new(I64)),
            _ => unimplemented!("Only Int64 args supported for this codegen"),
        }
    }
    if let Some(ret) = &lir_function.return_type {
        match ret {
            &LirPrimitiveType::Int64 => sig.returns.push(AbiParam::new(I64)),
            _ => unimplemented!("Only Int64 return supported for this codegen"),
        }
    }

    let mut func = Function::with_name_signature(UserFuncName::user(0, index), sig);

    let mut ctx = FunctionBuilderContext::new();
    let mut builder = FunctionBuilder::new(&mut func, &mut ctx);

    // Create cranelift blocks for each LIR block
    let mut block_map: HashMap<String, Block> = HashMap::new();
    for lir_block in &lir_function.blocks {
        let b = builder.create_block();
        block_map.insert(lir_block.label.clone(), b);
    }

    // Append params to entry block for function parameters
    let entry_label = lir_function
        .blocks
        .first()
        .expect("function must have at least one block")
        .label
        .clone();
    let entry_block = *block_map.get(&entry_label).unwrap();
    builder.append_block_params_for_function_params(entry_block);

    // Declare variables for temps and args
    // ARG_BASE is offset to avoid collisions with temp ids
    const ARG_BASE: u32 = 100_000;
    let mut max_temp: u32 = 0;
    for block in &lir_function.blocks {
        for instr in &block.instructions {
            // very shady way to find max temp id used, but works for now
            match instr {
                LirInstr::Add { dest, a, b, .. }
                | LirInstr::Sub { dest, a, b, .. }
                | LirInstr::Mul { dest, a, b, .. }
                | LirInstr::Div { dest, a, b, .. }
                | LirInstr::Mod { dest, a, b, .. }
                | LirInstr::LessThan { dest, a, b, .. }
                | LirInstr::LessThanOrEqual { dest, a, b, .. }
                | LirInstr::GreaterThan { dest, a, b, .. }
                | LirInstr::GreaterThanOrEqual { dest, a, b, .. }
                | LirInstr::Equal { dest, a, b, .. }
                | LirInstr::NotEqual { dest, a, b, .. } => {
                    if let LirOperand::Temp(t) = dest {
                        max_temp = max_temp.max(*t);
                    }
                    if let LirOperand::Temp(t) = a {
                        max_temp = max_temp.max(*t);
                    }
                    if let LirOperand::Temp(t) = b {
                        max_temp = max_temp.max(*t);
                    }
                }
                LirInstr::LoadImm { dst, value } | LirInstr::LoadConst { dst, value } => {
                    if let LirOperand::Temp(t) = dst {
                        max_temp = max_temp.max(*t);
                    }
                    if let LirOperand::Temp(t) = value {
                        max_temp = max_temp.max(*t);
                    }
                }
                LirInstr::Call { dst, args, .. } | LirInstr::ExternCall { dst, args, .. } => {
                    if let Some(LirOperand::Temp(t)) = dst {
                        max_temp = max_temp.max(*t);
                    }
                    for a in args {
                        if let LirOperand::Temp(t) = a {
                            max_temp = max_temp.max(*t);
                        }
                    }
                }
                _ => {}
            }
        }
    }

    // Create a mapping from LIR temp IDs (and arg IDs) to Cranelift Variables
    let mut var_map: HashMap<u32, Variable> = HashMap::new();
    for t in 0..=max_temp {
        let v = builder.declare_var(I64);
        var_map.insert(t, v);
    }
    for (i, _) in lir_function.args.iter().enumerate() {
        let v = builder.declare_var(I64);
        var_map.insert(ARG_BASE + i as u32, v);
    }

    // Switch to entry block and move function params into arg variables
    builder.switch_to_block(entry_block);
    let params = builder.block_params(entry_block).to_vec();
    for (i, param_val) in params.iter().enumerate() {
        let var = *var_map.get(&(ARG_BASE + i as u32)).unwrap();
        builder.def_var(var, *param_val);
    }

    // Generate code for each LIR block
    for lir_block in &lir_function.blocks {
        let b = *block_map.get(&lir_block.label).unwrap();
        builder.switch_to_block(b);
        codegen_block(&mut builder, lir_block, &block_map, &var_map, func_map);
    }

    builder.finalize();

    verify_function(&func, &settings::Flags::new(settings::builder())).unwrap();

    func
}

fn codegen_block(
    builder: &mut FunctionBuilder,
    lir_block: &LirBlock,
    block_map: &HashMap<String, Block>,
    var_map: &HashMap<u32, Variable>,
    func_map: &HashMap<String, u32>,
) {
    use LirInstr::*;
    for instr in &lir_block.instructions {
        match instr {
            Add { .. } => codegen_add(builder, instr, var_map),
            Sub { .. } => codegen_sub(builder, instr, var_map),
            LoadImm { dst, value } => {
                if let LirOperand::Temp(t) = dst {
                    let val = match value {
                        LirOperand::ImmInt(n) => builder.ins().iconst(I64, *n),
                        LirOperand::ImmUInt(u) => builder.ins().iconst(I64, *u as i64),
                        LirOperand::Temp(t2) => {
                            let v = *var_map.get(t2).unwrap();
                            builder.use_var(v)
                        }
                        LirOperand::Arg(a) => {
                            let v = *var_map.get(&(ARG_BASE + *a as u32)).unwrap();
                            builder.use_var(v)
                        }
                        _ => unreachable!("Unsupported LoadImm value"),
                    };
                    let dest_var = *var_map.get(t).unwrap();
                    builder.def_var(dest_var, val);
                } else {
                    unreachable!("LoadImm dst must be Temp");
                }
            }
            LessThanOrEqual { ty: _, dest, a, b } => {
                let va = load_operand(builder, a, var_map);
                let vb = load_operand(builder, b, var_map);
                let cmp = builder.ins().icmp(IntCC::SignedLessThanOrEqual, va, vb);
                // convert boolean cmp into i64 0/1 using select
                let one = builder.ins().iconst(I64, 1);
                let zero = builder.ins().iconst(I64, 0);
                let as_i64 = builder.ins().select(cmp, one, zero);
                if let LirOperand::Temp(t) = dest {
                    let dv = *var_map.get(t).unwrap();
                    builder.def_var(dv, as_i64);
                }
            }
            Call { func_name, dst, args } => {
                // Build arg values
                let mut arg_vals = Vec::new();
                for a in args {
                    let v = load_operand(builder, a, var_map);
                    arg_vals.push(v);
                }
                // Create a signature for the callee (assume i64 params/return for this LIR)
                let mut callee_sig = Signature::new(CallConv::SystemV);
                for _ in args.iter() {
                    callee_sig.params.push(AbiParam::new(I64));
                }
                if dst.is_some() {
                    callee_sig.returns.push(AbiParam::new(I64));
                }
                let sigref = builder.import_signature(callee_sig);
                // Import the function as an external (symbol) so we get a FuncRef
                let name = ExternalName::testcase(func_name.clone());
                let ext = ExtFuncData {
                    name,
                    signature: sigref,
                    colocated: true,
                };
                let callee = builder.import_function(ext);
                let call_inst = builder.ins().call(callee, &arg_vals);
                if let Some(LirOperand::Temp(t)) = dst {
                    let results = builder.inst_results(call_inst);
                    let ret_val = results[0];
                    let dv = *var_map.get(t).unwrap();
                    builder.def_var(dv, ret_val);
                }
            }
            ExternCall { dst, func_name, args } => {
                // Lower extern calls to imported symbol `func_name` with i64 args
                let mut arg_vals = Vec::new();
                for a in args {
                    let v = load_operand(builder, a, var_map);
                    arg_vals.push(v);
                }
                let mut callee_sig = Signature::new(CallConv::SystemV);
                for _ in args.iter() {
                    callee_sig.params.push(AbiParam::new(I64));
                }
                // assume no return for externs unless dst present
                if dst.is_some() {
                    callee_sig.returns.push(AbiParam::new(I64));
                }
                let sigref = builder.import_signature(callee_sig);
                let name = ExternalName::testcase(func_name.clone());
                let ext = ExtFuncData {
                    name,
                    signature: sigref,
                    colocated: false,
                };
                let callee = builder.import_function(ext);
                let call_inst = builder.ins().call(callee, &arg_vals);
                if let Some(LirOperand::Temp(t)) = dst {
                    let results = builder.inst_results(call_inst);
                    let ret_val = results[0];
                    let dv = *var_map.get(t).unwrap();
                    builder.def_var(dv, ret_val);
                }
            }
            _ => {}
        }
    }

    // Handle terminator
    match &lir_block.terminator {
        LirTerminator::Return { value } => {
            if let Some(op) = value {
                let v = load_operand(builder, op, var_map);
                builder.ins().return_(&[v]);
            } else {
                builder.ins().return_(&[]);
            }
        }
        LirTerminator::Branch { target } => {
            let t = *block_map.get(target).expect("unknown branch target");
            builder.ins().jump(t, &[]);
        }
        LirTerminator::BranchIf {
            condition,
            then_label,
            else_label,
        } => {
            // Lower conditional branch using Cranelift's `brif`:
            // take `then_label` when `condition != 0`, else `else_label`.
            let cond = load_operand(builder, condition, var_map);
            let then_block = *block_map.get(then_label).expect("unknown then target");
            let else_block = *block_map.get(else_label).expect("unknown else target");
            builder.ins().brif(cond, then_block, &[], else_block, &[]);
        }
        LirTerminator::Halt => {
            builder.ins().return_(&[]);
        }
        LirTerminator::None => {}
    }
    builder.seal_block(builder.current_block().unwrap());
}

fn load_operand(
    builder: &mut FunctionBuilder,
    op: &LirOperand,
    var_map: &HashMap<u32, Variable>,
) -> cranelift::prelude::Value {
    match op {
        LirOperand::Temp(t) => builder.use_var(*var_map.get(t).unwrap()),
        LirOperand::Arg(a) => builder.use_var(*var_map.get(&(ARG_BASE + *a as u32)).unwrap()),
        LirOperand::ImmInt(n) => builder.ins().iconst(I64, *n),
        LirOperand::ImmUInt(u) => builder.ins().iconst(I64, *u as i64),
        _ => unreachable!("unsupported operand in this codegen"),
    }
}

// Fibonacci function only use temp = add.int64 temp, temp
fn codegen_add(
    builder: &mut FunctionBuilder,
    lir_add: &LirInstr,
    var_map: &HashMap<u32, Variable>,
) {
    match lir_add {
        LirInstr::Add { ty, dest, a, b } => {
            let val_a = load_operand(builder, a, var_map);
            let val_b = load_operand(builder, b, var_map);
            let sum = match ty {
                &LirPrimitiveType::Int64 => builder.ins().iadd(val_a, val_b),
                &LirPrimitiveType::Int32 => {
                    let x = builder.ins().ireduce(I32, val_a);
                    let y = builder.ins().ireduce(I32, val_b);
                    builder.ins().iadd(x, y)
                }
                _ => unreachable!("Unsupported type for Add instruction"),
            };
            if let LirOperand::Temp(t) = dest {
                let dv = *var_map.get(t).unwrap();
                builder.def_var(dv, sum);
            }
        }
        _ => unreachable!("Not a supported Add instruction"),
    }
}

// Sub only use temp = sub.int64 arg, temp
fn codegen_sub(build: &mut FunctionBuilder, lir_sub: &LirInstr, var_map: &HashMap<u32, Variable>) {
    match lir_sub {
        LirInstr::Sub { ty, dest, a, b } => {
            let val_a = load_operand(build, a, var_map);
            let val_b = load_operand(build, b, var_map);
            let diff = match ty {
                &LirPrimitiveType::Int64 => build.ins().isub(val_a, val_b),
                &LirPrimitiveType::Int32 => {
                    let x = build.ins().ireduce(I32, val_a);
                    let y = build.ins().ireduce(I32, val_b);
                    build.ins().isub(x, y)
                }
                _ => unreachable!("Unsupported type for Sub instruction"),
            };
            if let LirOperand::Temp(t) = dest {
                let dv = *var_map.get(t).unwrap();
                build.def_var(dv, diff);
            }
        }
        _ => unreachable!("Not a supported Sub instruction"),
    }
}
