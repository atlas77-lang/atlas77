/*
 * This file will contain the C codegen.
 * We will codegen to C from our LIR here.
 * Why C? It's easier to target than LLVM/Cranelift/etc.
 *
 * In the future, I'll potentially target actual backends, but for now, C is good enough.
 */

use crate::atlas_c::atlas_lir::program::{
    LirBlock, LirFunction, LirInstr, LirOperand, LirPrimitiveType, LirProgram, LirTerminator,
};

pub const HEADER_NAME: &str = "__atlas77_header.h";

pub struct CCodeGen {
    pub c_file: String,
    /// Will contain the prototype declarations for functions.
    /// And all struct definitions
    pub c_header: String,
    indent_level: usize,
}

impl CCodeGen {
    pub fn new() -> Self {
        Self {
            c_file: String::new(),
            c_header: String::new(),
            indent_level: 0,
        }
    }

    pub fn emit_c(&mut self, program: &LirProgram) -> Result<(), String> {
        Self::write_to_file(
            &mut self.c_file,
            "#include <stdint.h>\n#include <stdbool.h>\n#include <stdio.h>\n",
            self.indent_level,
        );
        //Include the generated header
        Self::write_to_file(
            &mut self.c_file,
            &format!("#include \"{}\"\n\n", HEADER_NAME),
            self.indent_level,
        );
        for func in program.functions.iter() {
            self.codegen_function(func);
        }
        eprintln!("Generated C Header:\n{}", self.c_header);
        eprintln!("Generated C File:\n{}", self.c_file);
        Ok(())
    }

    fn codegen_function(&mut self, func: &LirFunction) {
        let signature = self.codegen_signature(
            &func.name,
            &func.args,
            &func.return_type.unwrap_or(LirPrimitiveType::Unit),
        );
        Self::write_to_file(
            &mut self.c_header,
            &format!("{};", signature),
            self.indent_level,
        );
        Self::write_to_file(
            &mut self.c_file,
            &format!("{} {{", signature),
            self.indent_level,
        );
        self.indent_level += 1;
        for block in func.blocks.iter() {
            self.codegen_block(block);
        }
        self.indent_level -= 1;
        Self::write_to_file(&mut self.c_file, "}\n", self.indent_level);
    }

    fn codegen_signature(
        &mut self,
        name: &str,
        args: &Vec<LirPrimitiveType>,
        ret: &LirPrimitiveType,
    ) -> String {
        let mut prototype = format!("{} {}(", self.codegen_type(ret), name);
        for arg in args.iter() {
            let arg_type = self.codegen_type(arg);
            // For now, just name args arg0, arg1, etc.
            let arg_name = format!("arg_{}", args.iter().position(|x| x == arg).unwrap());
            prototype.push_str(&format!("{} {}", arg_type, arg_name));
            if args.iter().position(|x| x == arg).unwrap() != args.len() - 1 {
                prototype.push_str(", ");
            }
        }
        prototype.push_str(")");
        prototype
    }

    fn codegen_type(&mut self, ty: &LirPrimitiveType) -> String {
        match ty {
            LirPrimitiveType::Unit => "void".to_string(),
            LirPrimitiveType::Int32 => "int32_t".to_string(),
            LirPrimitiveType::Int64 => "int64_t".to_string(),
            LirPrimitiveType::Float32 => "float".to_string(),
            LirPrimitiveType::Float64 => "double".to_string(),
            LirPrimitiveType::Boolean => "bool".to_string(),
            _ => unimplemented!("Type codegen not implemented for {:?}", ty),
        }
    }

    fn codegen_block(&mut self, block: &LirBlock) {
        // Let's write the label
        Self::write_to_file(
            &mut self.c_file,
            &format!("{}:", block.label),
            self.indent_level - 1,
        );
        for instr in block.instructions.iter() {
            self.codegen_instruction(instr);
        }
        self.codegen_terminator(&block.terminator);
    }

    fn codegen_terminator(&mut self, terminator: &LirTerminator) {
        match terminator {
            LirTerminator::Return { value } => {
                if let Some(val) = value {
                    let value_str = self.codegen_operand(val);
                    let line = format!("return {};", value_str);
                    Self::write_to_file(&mut self.c_file, &line, self.indent_level);
                } else {
                    let line = "return;".to_string();
                    Self::write_to_file(&mut self.c_file, &line, self.indent_level);
                }
            }
            LirTerminator::BranchIf {
                condition,
                then_label,
                else_label,
            } => {
                let condition_str = self.codegen_operand(condition);
                let line = format!("if ({}) {{", condition_str);
                Self::write_to_file(&mut self.c_file, &line, self.indent_level);
                self.indent_level += 1;
                let then_line = format!("goto {};", then_label);
                Self::write_to_file(&mut self.c_file, &then_line, self.indent_level);
                self.indent_level -= 1;
                Self::write_to_file(&mut self.c_file, "}", self.indent_level);
                Self::write_to_file(&mut self.c_file, "else {", self.indent_level);
                self.indent_level += 1;
                let else_line = format!("goto {};", else_label);
                Self::write_to_file(&mut self.c_file, &else_line, self.indent_level);
                self.indent_level -= 1;
                Self::write_to_file(&mut self.c_file, "}", self.indent_level);
            }
            LirTerminator::Branch { target } => {
                let line = format!("goto {};", target);
                Self::write_to_file(&mut self.c_file, &line, self.indent_level);
            }
            LirTerminator::Halt => {
                // An Halt terminator just means we exit the program gracefully
                let line = "exit(0);".to_string();
                Self::write_to_file(&mut self.c_file, &line, self.indent_level);
            }
            _ => {
                eprintln!("Terminator codegen not implemented for {:?}", terminator)
            }
        }
    }

    fn codegen_instruction(&mut self, instr: &LirInstr) {
        match instr {
            LirInstr::Add { ty, dest, a, b } => {
                let dest_str = self.codegen_operand(dest);
                let a_str = self.codegen_operand(a);
                let b_str = self.codegen_operand(b);
                let line = format!(
                    "{} {} = {} + {};",
                    self.codegen_type(ty),
                    dest_str,
                    a_str,
                    b_str
                );
                Self::write_to_file(&mut self.c_file, &line, self.indent_level);
            }
            LirInstr::Sub { ty, dest, a, b } => {
                let dest_str = self.codegen_operand(dest);
                let a_str = self.codegen_operand(a);
                let b_str = self.codegen_operand(b);
                let line = format!(
                    "{} {} = {} - {};",
                    self.codegen_type(ty),
                    dest_str,
                    a_str,
                    b_str
                );
                Self::write_to_file(&mut self.c_file, &line, self.indent_level);
            }
            LirInstr::LessThan { ty, dest, a, b } => {
                let dest_str = self.codegen_operand(dest);
                let a_str = self.codegen_operand(a);
                let b_str = self.codegen_operand(b);
                let line = format!(
                    "{} {} = {} < {};",
                    self.codegen_type(ty),
                    dest_str,
                    a_str,
                    b_str
                );
                Self::write_to_file(&mut self.c_file, &line, self.indent_level);
            }
            LirInstr::LessThanOrEqual { ty, dest, a, b } => {
                let dest_str = self.codegen_operand(dest);
                let a_str = self.codegen_operand(a);
                let b_str = self.codegen_operand(b);
                let line = format!(
                    "{} {} = {} <= {};",
                    self.codegen_type(ty),
                    dest_str,
                    a_str,
                    b_str
                );
                Self::write_to_file(&mut self.c_file, &line, self.indent_level);
            }
            LirInstr::LoadImm { dst, value } => {
                let dest_str = self.codegen_operand(dst);
                let value = self.codegen_operand(value);
                let line = format!("{} = {};", dest_str, value);
                Self::write_to_file(&mut self.c_file, &line, self.indent_level);
            }
            LirInstr::Call {
                dst,
                func_name,
                args,
                ty,
            } => {
                let args_str: Vec<String> =
                    args.iter().map(|arg| self.codegen_operand(arg)).collect();
                let args_joined = args_str.join(", ");
                if let Some(dest_op) = dst {
                    let dest_str = self.codegen_operand(dest_op);
                    let line = format!(
                        "{} {} = {}({});",
                        self.codegen_type(ty),
                        dest_str,
                        func_name,
                        args_joined
                    );
                    Self::write_to_file(&mut self.c_file, &line, self.indent_level);
                } else {
                    let line = format!("{}({});", func_name, args_joined);
                    Self::write_to_file(&mut self.c_file, &line, self.indent_level);
                }
            }
            _ => {
                eprintln!("Instruction codegen not implemented for {:?}", instr)
            }
        }
    }

    fn codegen_operand(&mut self, operand: &LirOperand) -> String {
        match operand {
            LirOperand::Arg(a) => format!("arg_{}", a),
            LirOperand::Temp(t) => format!("temp_{}", t),
            LirOperand::Const(c) => unimplemented!("Constant codegen not implemented for {:?}", c),
            LirOperand::ImmBool(b) => format!("{}", b),
            LirOperand::ImmInt(i) => format!("{}", i),
            LirOperand::ImmUInt(u) => format!("{}", u),
            LirOperand::ImmFloat(f) => format!("{}", f),
            LirOperand::ImmChar(c) => format!("'{}'", c),
            LirOperand::ImmUnit => "void".to_string(),
        }
    }

    fn write_to_file(file: &mut String, content: &str, indent_level: usize) {
        for _ in 0..indent_level {
            file.push_str("\t");
        }
        file.push_str(content);
        file.push('\n');
    }
}
