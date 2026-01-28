/*
 * This file will contain the C codegen.
 * We will codegen to C from our LIR here.
 * Why C? It's easier to target than LLVM/Cranelift/etc.
 *
 * In the future, I'll potentially target actual backends, but for now, C is good enough.
 */

use crate::atlas_c::{
    atlas_hir::signature::ConstantValue,
    atlas_lir::program::{
        LirBlock, LirFunction, LirInstr, LirOperand, LirProgram, LirStruct, LirTerminator, LirTy,
        LirUnion,
    },
};

pub const HEADER_NAME: &str = "__atlas77_header.h";
pub const PORTABLE_TIMER_HEADER: &str = include_str!("../../.././libraries/std/useful_header.h");

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
            "#include <stdint.h>\n#include <stdbool.h>\n#include <stdio.h>\n#include <stdlib.h>\n#include <string.h>\n#include <math.h>\n#include <time.h>\n",
            self.indent_level,
        );
        //Include the generated header
        Self::write_to_file(
            &mut self.c_file,
            &format!("#include \"{}\"\n\n", HEADER_NAME),
            self.indent_level,
        );
        // Include the portable timer header
        Self::write_to_file(&mut self.c_header, PORTABLE_TIMER_HEADER, self.indent_level);
        for union in program.unions.iter() {
            self.codegen_union(union);
        }
        for strukt in program.structs.iter() {
            self.codegen_struct(strukt);
        }
        for func in program.functions.iter() {
            self.codegen_function(func);
        }
        Ok(())
    }

    fn codegen_union(&mut self, union: &LirUnion) {
        let mut union_def = format!("typedef union {} {{\n", union.name);
        for (variant_name, variant_type) in union.variants.iter() {
            let variant_type_str = self.codegen_type(variant_type);
            union_def.push_str(&format!("\t{} {};\n", variant_type_str, variant_name));
        }
        union_def.push_str(&format!("}} {};\n\n", union.name));
        Self::write_to_file(&mut self.c_header, &union_def, self.indent_level);
    }

    fn codegen_struct(&mut self, strukt: &LirStruct) {
        let mut struct_def = format!("typedef struct {} {{\n", strukt.name);
        for (field_name, field_type) in strukt.fields.iter() {
            let field_sig = match field_type {
                LirTy::ArrayTy { inner, size } => {
                    format!("\t{} {}[{}];\n", self.codegen_type(inner), field_name, size)
                }
                _ => format!("\t{} {};\n", self.codegen_type(field_type), field_name),
            };
            struct_def.push_str(&field_sig);
        }
        struct_def.push_str(&format!("}} {};\n\n", strukt.name));
        Self::write_to_file(&mut self.c_header, &struct_def, self.indent_level);
    }

    fn codegen_function(&mut self, func: &LirFunction) {
        let signature = self.codegen_signature(
            &func.name,
            &func.args,
            &func.return_type.clone().unwrap_or(LirTy::Unit),
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

    fn codegen_signature(&mut self, name: &str, args: &[LirTy], ret: &LirTy) -> String {
        let mut prototype = format!("{} {}(", self.codegen_type(ret), name);
        for (i, arg) in args.iter().enumerate() {
            let arg_sig = match arg {
                LirTy::ArrayTy { inner, size } => {
                    format!("{} arg_{}[{}]", self.codegen_type(inner), i, size)
                }
                _ => format!("{} arg_{}", self.codegen_type(arg), i),
            };
            self.codegen_type(arg);
            // For now, just name args arg0, arg1, etc.
            prototype.push_str(&arg_sig);
            if i != args.len() - 1 {
                prototype.push_str(", ");
            }
        }
        prototype.push(')');
        prototype
    }

    fn codegen_type(&mut self, ty: &LirTy) -> String {
        match ty {
            LirTy::Unit => "void".to_string(),
            LirTy::Int64 => "int64_t".to_string(),
            LirTy::Int32 => "int32_t".to_string(),
            LirTy::Int16 => "int16_t".to_string(),
            LirTy::Int8 => "int8_t".to_string(),
            LirTy::Float32 => "float".to_string(),
            LirTy::Float64 => "double".to_string(),
            LirTy::UInt64 => "uint64_t".to_string(),
            LirTy::UInt32 => "uint32_t".to_string(),
            LirTy::UInt16 => "uint16_t".to_string(),
            LirTy::UInt8 => "uint8_t".to_string(),
            LirTy::Boolean => "bool".to_string(),
            LirTy::Char => "uint32_t".to_string(),
            LirTy::Str => "char*".to_string(),
            LirTy::Ptr(inner) => {
                let inner_type = self.codegen_type(inner);
                if inner_type.ends_with('*') {
                    // Avoid double pointers for now
                    inner_type
                } else {
                    format!("{}*", inner_type)
                }
            }
            // For struct types, we use pointers
            LirTy::StructType(name) => format!("{}*", name),
            // For union types, we don't use pointers for now
            LirTy::UnionType(name) => name.to_string(),
            LirTy::ArrayTy { inner, size } => format!("{}[{}]", self.codegen_type(inner), size),
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
            LirTerminator::None => {
                // No terminator, do nothing
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
            LirInstr::Mul { ty, dest, a, b } => {
                let dest_str = self.codegen_operand(dest);
                let a_str = self.codegen_operand(a);
                let b_str = self.codegen_operand(b);
                let line = format!(
                    "{} {} = {} * {};",
                    self.codegen_type(ty),
                    dest_str,
                    a_str,
                    b_str
                );
                Self::write_to_file(&mut self.c_file, &line, self.indent_level);
            }
            LirInstr::Div { ty, dest, a, b } => {
                let dest_str = self.codegen_operand(dest);
                let a_str = self.codegen_operand(a);
                let b_str = self.codegen_operand(b);
                let line = format!(
                    "{} {} = {} / {};",
                    self.codegen_type(ty),
                    dest_str,
                    a_str,
                    b_str
                );
                Self::write_to_file(&mut self.c_file, &line, self.indent_level);
            }
            LirInstr::Mod { ty, dest, a, b } => {
                let dest_str = self.codegen_operand(dest);
                let a_str = self.codegen_operand(a);
                let b_str = self.codegen_operand(b);
                let line = format!(
                    "{} {} = {} % {};",
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
            LirInstr::GreaterThan { ty, dest, a, b } => {
                let dest_str = self.codegen_operand(dest);
                let a_str = self.codegen_operand(a);
                let b_str = self.codegen_operand(b);
                let line = format!(
                    "{} {} = {} > {};",
                    self.codegen_type(ty),
                    dest_str,
                    a_str,
                    b_str
                );
                Self::write_to_file(&mut self.c_file, &line, self.indent_level);
            }
            LirInstr::GreaterThanOrEqual { ty, dest, a, b } => {
                let dest_str = self.codegen_operand(dest);
                let a_str = self.codegen_operand(a);
                let b_str = self.codegen_operand(b);
                let line = format!(
                    "{} {} = {} >= {};",
                    self.codegen_type(ty),
                    dest_str,
                    a_str,
                    b_str
                );
                Self::write_to_file(&mut self.c_file, &line, self.indent_level);
            }
            LirInstr::Equal { ty, dest, a, b } => {
                let dest_str = self.codegen_operand(dest);
                let a_str = self.codegen_operand(a);
                let b_str = self.codegen_operand(b);
                let line = format!(
                    "{} {} = {} == {};",
                    self.codegen_type(ty),
                    dest_str,
                    a_str,
                    b_str
                );
                Self::write_to_file(&mut self.c_file, &line, self.indent_level);
            }
            LirInstr::NotEqual { ty, dest, a, b } => {
                let dest_str = self.codegen_operand(dest);
                let a_str = self.codegen_operand(a);
                let b_str = self.codegen_operand(b);
                let line = format!(
                    "{} {} = {} != {};",
                    self.codegen_type(ty),
                    dest_str,
                    a_str,
                    b_str
                );
                Self::write_to_file(&mut self.c_file, &line, self.indent_level);
            }
            LirInstr::Negate { ty, dest, src } => {
                let dest_str = self.codegen_operand(dest);
                let src_str = self.codegen_operand(src);
                let line = format!("{} {} = -{};", self.codegen_type(ty), dest_str, src_str);
                Self::write_to_file(&mut self.c_file, &line, self.indent_level);
            }
            LirInstr::Not { ty, dest, src } => {
                let dest_str = self.codegen_operand(dest);
                let src_str = self.codegen_operand(src);
                let line = format!("{} {} = !{};", self.codegen_type(ty), dest_str, src_str);
                Self::write_to_file(&mut self.c_file, &line, self.indent_level);
            }
            LirInstr::LoadImm { ty, dst, value } => {
                let dest_str = self.codegen_operand(dst);
                let value = self.codegen_operand(value);
                let line = format!("{} {} = {};", self.codegen_type(ty), dest_str, value);
                Self::write_to_file(&mut self.c_file, &line, self.indent_level);
            }
            LirInstr::LoadConst { dst, value } => {
                let value = self.codegen_operand(value);
                // We assume the type is always `string` so `char*` in C for now
                // THIS OBVIOUSLY NEEDS TO BE FIXED LATER
                let dest_str = self.codegen_operand(dst);
                let line = format!("char* {} = {};", dest_str, value);
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
            LirInstr::ExternCall {
                dst,
                func_name,
                args,
                ty,
            } => {
                let args_str: Vec<String> =
                    args.iter().map(|arg| self.codegen_operand(arg)).collect();
                let args_joined = args_str.join(", ");
                if ty == &LirTy::Unit {
                    // For extern calls that return void, we don't need to declare a variable
                    let line = format!("{}({});", func_name, args_joined);
                    Self::write_to_file(&mut self.c_file, &line, self.indent_level);
                    return;
                }
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
            LirInstr::Assign { ty, dst, src } => match ty {
                LirTy::ArrayTy { inner, size } => {
                    let dest_str = self.codegen_operand(dst);
                    let src_str = self.codegen_operand(src);
                    let type_str = self.codegen_type(inner);
                    let line = format!(
                        "memcpy({}, {}, sizeof({}) * {});",
                        dest_str, src_str, type_str, size
                    );
                    Self::write_to_file(&mut self.c_file, &line, self.indent_level);
                }
                _ => {
                    let dest_str = self.codegen_operand(dst);
                    let src_str = self.codegen_operand(src);
                    let line = format!("{} = {};", dest_str, src_str);
                    Self::write_to_file(&mut self.c_file, &line, self.indent_level);
                }
            },
            LirInstr::Delete { ty: _, src: _ } => {
                // Delete instructions are ignored for now. We don't properly handle move semantics in C yet.
                /* let src_str = self.codegen_operand(src);
                let line = format!("free({});", src_str);
                Self::write_to_file(&mut self.c_file, &line, self.indent_level); */
            }
            LirInstr::Construct { ty, dst, args } => {
                let dest_str = self.codegen_operand(dst);
                let type_str = self.codegen_type(ty);
                let type_name_str = type_str.trim_end_matches('*').to_string();
                let mut args_str: Vec<String> =
                    args.iter().map(|arg| self.codegen_operand(arg)).collect();
                args_str.insert(0, dest_str.clone());
                let ctor_call = format!("{}_new({})", type_name_str, args_str.join(", "));
                let line = format!(
                    "{} {} = ({})malloc(sizeof({}));\n\
                    \tif ({} == NULL) {{\n\
                    \t\tprintf(\"Failed to allocate memory for {}\\n\", {});\n\
                    \t\texit(1);\n\
                    \t}}\n\
                    \t{};\n",
                    type_str,
                    dest_str,
                    type_str,
                    type_name_str,
                    dest_str,
                    type_str,
                    dest_str,
                    ctor_call
                );
                Self::write_to_file(&mut self.c_file, &line, self.indent_level);
            }
            // Creates an array on the stack.
            // C equivalent to T dest[size] = {0};
            LirInstr::ConstructArray { ty, dst, size } => {
                let dest_str = self.codegen_operand(dst);
                // In C, arrays are defined as T name[size];
                // This will probably not work for multi-dimensional arrays yet
                let type_str = match ty {
                    LirTy::ArrayTy { inner, .. } => self.codegen_type(inner),
                    _ => panic!("ConstructArray expected ArrayTy"),
                };
                let type_name_str = type_str.trim_end_matches('*').to_string();
                let line = format!("{} {}[{}] = {{0}};", type_name_str, dest_str, size);
                Self::write_to_file(&mut self.c_file, &line, self.indent_level);
            }
            // Similar to {foo: bar, baz: qux}
            // It DOES NOT allocate memory, just creates a raw object on the stack
            LirInstr::ConstructUnion {
                ty,
                dst,
                field_values,
            } => {
                let dest_str = self.codegen_operand(dst);
                let type_str = self.codegen_type(ty);
                let type_name_str = type_str.trim_end_matches('*').to_string();
                let mut field_inits: Vec<String> = Vec::new();
                for (field_name, field_value) in field_values.iter() {
                    let value_str = self.codegen_operand(field_value);
                    field_inits.push(format!(".{} = {}", field_name, value_str));
                }
                let field_inits_str = field_inits.join(", ");
                let line = format!(
                    "{} {} = {{ {} }};",
                    type_name_str, dest_str, field_inits_str
                );
                Self::write_to_file(&mut self.c_file, &line, self.indent_level);
            }
            LirInstr::Cast { ty, from, dst, src } => {
                if !(ty == from) {
                    let dest_str = self.codegen_operand(dst);
                    let src_str = self.codegen_operand(src);
                    let line = format!(
                        "{} {} = ({}){};",
                        self.codegen_type(ty),
                        dest_str,
                        self.codegen_type(ty),
                        src_str
                    );
                    Self::write_to_file(&mut self.c_file, &line, self.indent_level);
                } else {
                    // No-op cast
                    let dest_str = self.codegen_operand(dst);
                    let src_str = self.codegen_operand(src);
                    let line_comment = "//No-op cast, needs to be removed".to_string();
                    let line = format!("{} {} = {};", self.codegen_type(ty), dest_str, src_str);
                    Self::write_to_file(&mut self.c_file, &line_comment, self.indent_level);
                    Self::write_to_file(&mut self.c_file, &line, self.indent_level);
                }
            }
            LirInstr::BuiltInOperator { ty, op, dst } => {
                let dest_str = self.codegen_operand(dst);
                eprintln!("sizeof({})", self.codegen_type(ty));
                let line = format!(
                    "{} {} = {}({});",
                    self.codegen_type(&LirTy::UInt64),
                    dest_str,
                    op,
                    self.codegen_type(ty)
                );
                Self::write_to_file(&mut self.c_file, &line, self.indent_level);
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
            LirOperand::Const(c) => match c {
                ConstantValue::Int(i) => format!("{}", i),
                ConstantValue::UInt(u) => format!("{}", u),
                ConstantValue::Float(f) => format!("{}", f),
                ConstantValue::Bool(b) => format!("{}", b),
                ConstantValue::Char(c) => format!("'{}'", c),
                // We need to keep all the special characters in strings escaped
                // e.g.: \n, \t, etc.
                ConstantValue::String(s) => format!("\"{}\"", s.escape_default()),
                ConstantValue::Unit => "void".to_string(),
                _ => unimplemented!("Constant codegen not implemented for {:?}", c),
            },
            LirOperand::ImmBool(b) => format!("{}", b),
            LirOperand::ImmInt(i) => format!("{}", i),
            LirOperand::ImmUInt(u) => format!("{}", u),
            LirOperand::ImmFloat(f) => format!("{}", f),
            LirOperand::ImmChar(c) => format!("'{}'", c),
            LirOperand::ImmUnit => "void".to_string(),
            LirOperand::Deref(d) => format!("(*{})", self.codegen_operand(d)),
            LirOperand::AsRef(a) => format!("(&{})", self.codegen_operand(a)),
            LirOperand::FieldAccess {
                src,
                field_name,
                ty: field_ty,
            } => {
                let src_str = self.codegen_operand(src);
                // We assume src is a pointer for now
                if matches!(field_ty, LirTy::StructType(_)) {
                    if let LirOperand::Deref(_) = **src {
                        format!("{}.{}", src_str, field_name)
                    } else {
                        format!("{}->{}", src_str, field_name)
                    }
                } else {
                    // For union types, we use dot operator
                    format!("({}).{}", src_str, field_name)
                }
            }
            LirOperand::Index { src, index } => {
                let src_str = self.codegen_operand(src);
                let index_str = self.codegen_operand(index);
                format!("{}[{}]", src_str, index_str)
            }
        }
    }

    fn write_to_file(file: &mut String, content: &str, indent_level: usize) {
        for _ in 0..indent_level {
            file.push('\t');
        }
        file.push_str(content);
        file.push('\n');
    }
}
