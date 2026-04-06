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
use std::collections::HashSet;

pub const HEADER_NAME: &str = "__atlas77_header.h";
pub const PORTABLE_ATLAS77_HEADER: &str = include_str!("../../.././libraries/std/useful_header.h");

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum TypeDependency {
    Struct(String),
    Union(String),
}

pub struct CCodeGen {
    pub c_file: String,
    /// Will contain the prototype declarations for functions.
    /// And all struct definitions
    pub c_header: String,
    pub struct_names: Vec<String>,
    pub union_names: Vec<String>,
    indent_level: usize,
}

impl CCodeGen {
    fn c_ident(name: &str) -> String {
        let mut out = String::with_capacity(name.len());
        let mut chars = name.chars().peekable();
        while let Some(ch) = chars.next() {
            if ch == ':' && chars.peek() == Some(&':') {
                let _ = chars.next();
                out.push('_');
                continue;
            }
            if ch.is_ascii_alphanumeric() || ch == '_' {
                out.push(ch);
            } else {
                out.push('_');
            }
        }
        if out.is_empty() { "_".to_string() } else { out }
    }

    pub fn new() -> Self {
        Self {
            c_file: String::new(),
            c_header: String::new(),
            struct_names: vec![],
            union_names: vec![],
            indent_level: 0,
        }
    }

    pub fn emit_c(&mut self, program: &LirProgram, extra_headers: &[String]) -> Result<(), String> {
        self.emit_type_forward_declarations(program);
        self.codegen_type_definitions_dependency_order(program);
        for func in program.functions.iter() {
            self.codegen_function(func);
        }
        //Include the generated header
        Self::write_to_top(
            &mut self.c_file,
            &format!("#include \"{}\"\n\n", HEADER_NAME),
        );
        // Include the portable atlas77 header
        Self::write_to_top(&mut self.c_header, PORTABLE_ATLAS77_HEADER);
        Self::write_to_top(
            &mut self.c_file,
            "#include <stdint.h>\n#include <stdbool.h>\n#include <stdio.h>\n#include <stdlib.h>\n#include <string.h>\n#include <math.h>\n#include <time.h>\n",
        );
        for header in extra_headers.iter().rev() {
            let include = if header.starts_with('<') || header.starts_with('"') {
                format!("#include {}\n", header)
            } else {
                format!("#include <{}>\n", header)
            };
            Self::write_to_top(&mut self.c_header, &include);
        }
        Ok(())
    }

    fn emit_type_forward_declarations(&mut self, program: &LirProgram) {
        for union in program.unions.iter() {
            let union_name = Self::c_ident(&union.name);
            Self::write_to_top(
                &mut self.c_header,
                &format!("typedef union {} {};", union_name, union_name),
            );
        }
        for strukt in program.structs.iter() {
            if strukt.is_extern {
                continue;
            }
            let struct_name = Self::c_ident(&strukt.name);
            Self::write_to_top(
                &mut self.c_header,
                &format!("typedef struct {} {};", struct_name, struct_name),
            );
        }
    }

    fn type_dependencies_for_ty(ty: &LirTy, deps: &mut HashSet<TypeDependency>) {
        match ty {
            LirTy::StructType(name) => {
                deps.insert(TypeDependency::Struct(name.clone()));
            }
            LirTy::UnionType(name) => {
                deps.insert(TypeDependency::Union(name.clone()));
            }
            // Pointers can reference incomplete types in C.
            LirTy::Ptr { .. } => {}
            LirTy::ArrayTy { inner, .. } => Self::type_dependencies_for_ty(inner, deps),
            _ => {}
        }
    }

    fn type_dependencies_for_struct(strukt: &LirStruct) -> HashSet<TypeDependency> {
        let mut deps = HashSet::new();
        for ty in strukt.fields.values() {
            Self::type_dependencies_for_ty(ty, &mut deps);
        }
        deps.remove(&TypeDependency::Struct(strukt.name.clone()));
        deps
    }

    fn type_dependencies_for_union(union: &LirUnion) -> HashSet<TypeDependency> {
        let mut deps = HashSet::new();
        for ty in union.variants.values() {
            Self::type_dependencies_for_ty(ty, &mut deps);
        }
        deps.remove(&TypeDependency::Union(union.name.clone()));
        deps
    }

    fn can_emit_with_defined(
        deps: &HashSet<TypeDependency>,
        defined_structs: &HashSet<String>,
        defined_unions: &HashSet<String>,
    ) -> bool {
        deps.iter().all(|dep| match dep {
            TypeDependency::Struct(name) => defined_structs.contains(name),
            TypeDependency::Union(name) => defined_unions.contains(name),
        })
    }

    fn codegen_type_definitions_dependency_order(&mut self, program: &LirProgram) {
        let mut remaining_structs: Vec<&LirStruct> =
            program.structs.iter().filter(|s| !s.is_extern).collect();
        let mut remaining_unions: Vec<&LirUnion> = program.unions.iter().collect();

        let mut defined_structs: HashSet<String> = HashSet::new();
        let mut defined_unions: HashSet<String> = HashSet::new();

        loop {
            let mut progress = false;

            let mut i = 0;
            while i < remaining_structs.len() {
                let strukt = remaining_structs[i];
                let deps = Self::type_dependencies_for_struct(strukt);
                if Self::can_emit_with_defined(&deps, &defined_structs, &defined_unions) {
                    self.codegen_struct(strukt);
                    defined_structs.insert(strukt.name.clone());
                    remaining_structs.remove(i);
                    progress = true;
                } else {
                    i += 1;
                }
            }

            let mut j = 0;
            while j < remaining_unions.len() {
                let union = remaining_unions[j];
                let deps = Self::type_dependencies_for_union(union);
                if Self::can_emit_with_defined(&deps, &defined_structs, &defined_unions) {
                    self.codegen_union(union);
                    defined_unions.insert(union.name.clone());
                    remaining_unions.remove(j);
                    progress = true;
                } else {
                    j += 1;
                }
            }

            let mut k = 0;
            while k < remaining_structs.len() {
                let strukt = remaining_structs[k];
                let deps = Self::type_dependencies_for_struct(strukt);
                if Self::can_emit_with_defined(&deps, &defined_structs, &defined_unions) {
                    self.codegen_struct(strukt);
                    defined_structs.insert(strukt.name.clone());
                    remaining_structs.remove(k);
                    progress = true;
                } else {
                    k += 1;
                }
            }

            if !progress {
                break;
            }
        }

        for strukt in remaining_structs {
            self.codegen_struct(strukt);
        }
        for union in remaining_unions {
            self.codegen_union(union);
        }
    }

    fn codegen_union(&mut self, union: &LirUnion) {
        let union_name = Self::c_ident(&union.name);
        let mut union_def = format!("typedef union {} {{\n", union_name);
        for (variant_name, variant_type) in union.variants.iter() {
            let variant_type_str = self.codegen_type(variant_type);
            union_def.push_str(&format!("\t{} {};\n", variant_type_str, variant_name));
        }
        union_def.push_str(&format!("}} {};\n\n", union_name));
        self.union_names.push(union.name.clone());
        Self::write_to_file(&mut self.c_header, &union_def, self.indent_level);
    }

    fn codegen_struct(&mut self, strukt: &LirStruct) {
        let struct_name = Self::c_ident(&strukt.name);
        let mut struct_def = format!("typedef struct {} {{\n", struct_name);
        for (field_name, field_type) in strukt.fields.iter() {
            let field_sig = match field_type {
                LirTy::ArrayTy { .. } => {
                    format!("\t{};\n", self.codegen_array_decl(field_type, field_name))
                }
                _ => format!("\t{};\n", self.codegen_declaration(field_type, field_name)),
            };
            struct_def.push_str(&field_sig);
        }
        struct_def.push_str(&format!("}} {};\n\n", struct_name));
        self.struct_names.push(strukt.name.clone());
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
        let mangled_name = Self::c_ident(name);
        let mut prototype = format!("{}(", self.codegen_return_type(ret, &mangled_name));
        for (i, arg) in args.iter().enumerate() {
            let arg_sig = self.codegen_declaration(arg, &format!("arg_{}", i));
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

    fn codegen_array_decl(&mut self, ty: &LirTy, name: &str) -> String {
        let mut dims: Vec<usize> = Vec::new();
        let mut current = ty;
        while let LirTy::ArrayTy { inner, size } = current {
            dims.push(*size);
            current = inner;
        }
        let base = self.codegen_type(current);
        let mut decl = format!("{} {}", base, name);
        for dim in dims {
            decl.push_str(&format!("[{}]", dim));
        }
        decl
    }

    fn codegen_declaration(&mut self, ty: &LirTy, name: &str) -> String {
        match ty {
            LirTy::FnPtr { ret, args } => {
                let ret_str = self.codegen_type(ret);
                let args_str = if args.is_empty() {
                    "void".to_string()
                } else {
                    args.iter()
                        .map(|arg| self.codegen_type(arg))
                        .collect::<Vec<_>>()
                        .join(", ")
                };
                format!("{} (*{})({})", ret_str, name, args_str)
            }
            LirTy::ArrayTy { .. } => self.codegen_array_decl(ty, name),
            _ => format!("{} {}", self.codegen_type(ty), name),
        }
    }

    fn codegen_return_type(&mut self, ty: &LirTy, name: &str) -> String {
        match ty {
            LirTy::FnPtr { ret, args } => {
                let ret_str = self.codegen_type(ret);
                let args_str = if args.is_empty() {
                    "void".to_string()
                } else {
                    args.iter()
                        .map(|arg| self.codegen_type(arg))
                        .collect::<Vec<_>>()
                        .join(", ")
                };
                format!("{} (*{} )({})", ret_str, name, args_str).replace("* ", "*")
            }
            _ => format!("{} {}", self.codegen_type(ty), name),
        }
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
            LirTy::Str => "uint8_t*".to_string(),
            LirTy::FnPtr { ret, args } => {
                let ret_str = self.codegen_type(ret);
                let args_str = if args.is_empty() {
                    "void".to_string()
                } else {
                    args.iter()
                        .map(|arg| self.codegen_type(arg))
                        .collect::<Vec<_>>()
                        .join(", ")
                };
                format!("{} (*)({})", ret_str, args_str)
            }
            LirTy::Ptr { is_const, inner } => {
                let inner_type = self.codegen_type(inner);
                format!("{}{}*", if *is_const { "const " } else { "" }, inner_type)
            }
            // Struct type is a value type in LIR. Pointer semantics are represented by LirTy::Ptr.
            LirTy::StructType(name) => Self::c_ident(name),
            // For union types, we don't use pointers for now
            LirTy::UnionType(name) => Self::c_ident(name),
            LirTy::ArrayTy { inner, size } => format!("{}[{}]", self.codegen_type(inner), size),
            /* _ => unimplemented!("Type codegen not implemented for {:?}", ty), */
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
                    "{} {} = ({} + {});",
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
                    "{} {} = ({} - {});",
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
                    "{} {} = ({} * {});",
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
                    "{} {} = ({} / {});",
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
                    "{} {} = ({} % {});",
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
                    "{} {} = ({} < {});",
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
                    "{} {} = ({} <= {});",
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
                    "{} {} = ({} > {});",
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
                    "{} {} = ({} >= {});",
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
                    "{} {} = ({} == {});",
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
                    "{} {} = ({} != {});",
                    self.codegen_type(ty),
                    dest_str,
                    a_str,
                    b_str
                );
                Self::write_to_file(&mut self.c_file, &line, self.indent_level);
            }
            LirInstr::LogicalAnd { ty, dest, a, b } => {
                let dest_str = self.codegen_operand(dest);
                let a_str = self.codegen_operand(a);
                let b_str = self.codegen_operand(b);
                let line = format!(
                    "{} {} = ({} && {});",
                    self.codegen_type(ty),
                    dest_str,
                    a_str,
                    b_str
                );
                Self::write_to_file(&mut self.c_file, &line, self.indent_level);
            }
            LirInstr::LogicalOr { ty, dest, a, b } => {
                let dest_str = self.codegen_operand(dest);
                let a_str = self.codegen_operand(a);
                let b_str = self.codegen_operand(b);
                let line = format!(
                    "{} {} = ({} || {});",
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
                let line = format!("{} = {};", self.codegen_declaration(ty, &dest_str), value);
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
                let callee_name = Self::c_ident(func_name);
                let args_str: Vec<String> =
                    args.iter().map(|arg| self.codegen_operand(arg)).collect();
                let args_joined = args_str.join(", ");
                if let Some(dest_op) = dst {
                    let dest_str = self.codegen_operand(dest_op);
                    let line = format!(
                        "{} {} = {}({});",
                        self.codegen_type(ty),
                        dest_str,
                        callee_name,
                        args_joined
                    );
                    Self::write_to_file(&mut self.c_file, &line, self.indent_level);
                } else {
                    let line = format!("{}({});", callee_name, args_joined);
                    Self::write_to_file(&mut self.c_file, &line, self.indent_level);
                }
            }
            LirInstr::ExternCall {
                dst,
                func_name,
                args,
                ty,
            } => {
                let callee_name = Self::c_ident(func_name);
                let args_str: Vec<String> =
                    args.iter().map(|arg| self.codegen_operand(arg)).collect();
                let args_joined = args_str.join(", ");
                if ty == &LirTy::Unit {
                    // For extern calls that return void, we don't need to declare a variable
                    let line = format!("{}({});", callee_name, args_joined);
                    Self::write_to_file(&mut self.c_file, &line, self.indent_level);
                    return;
                }
                if let Some(dest_op) = dst {
                    let dest_str = self.codegen_operand(dest_op);
                    let line = format!(
                        "{} {} = {}({});",
                        self.codegen_type(ty),
                        dest_str,
                        callee_name,
                        args_joined
                    );
                    Self::write_to_file(&mut self.c_file, &line, self.indent_level);
                } else {
                    let line = format!("{}({});", callee_name, args_joined);
                    Self::write_to_file(&mut self.c_file, &line, self.indent_level);
                }
            }
            LirInstr::CallPtr {
                dst,
                callee,
                args,
                ty,
                param_tys,
            } => {
                let args_str: Vec<String> =
                    args.iter().map(|arg| self.codegen_operand(arg)).collect();
                let args_joined = args_str.join(", ");
                let callee_expr = self.codegen_operand(callee);
                let param_list = if param_tys.is_empty() {
                    String::from("void")
                } else {
                    param_tys
                        .iter()
                        .map(|p| self.codegen_type(p))
                        .collect::<Vec<_>>()
                        .join(", ")
                };
                let call_expr = format!(
                    "(({} (*)({})){})({})",
                    self.codegen_type(ty),
                    param_list,
                    callee_expr,
                    args_joined
                );

                if *ty == LirTy::Unit {
                    let line = format!("{};", call_expr);
                    Self::write_to_file(&mut self.c_file, &line, self.indent_level);
                } else if let Some(dest_op) = dst {
                    let dest_str = self.codegen_operand(dest_op);
                    let line = format!("{} {} = {};", self.codegen_type(ty), dest_str, call_expr);
                    Self::write_to_file(&mut self.c_file, &line, self.indent_level);
                } else {
                    let line = format!("{};", call_expr);
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
            LirInstr::Delete {
                ty,
                src,
                should_free,
            } => {
                let src_str = self.codegen_operand(src);

                // Value delete: run destructor only (no free).
                // Pointer delete: run destructor when applicable, then free.
                match ty {
                    LirTy::StructType(name) => {
                        let dtor_line = format!("{}___dtor(&{});", Self::c_ident(name), src_str);
                        Self::write_to_file(&mut self.c_file, &dtor_line, self.indent_level);
                    }
                    LirTy::Ptr { inner, .. } => {
                        if let LirTy::StructType(name) = inner.as_ref() {
                            let dtor_line = format!("{}___dtor({});", Self::c_ident(name), src_str);
                            Self::write_to_file(&mut self.c_file, &dtor_line, self.indent_level);
                        }
                    }
                    _ => {}
                }

                if *should_free {
                    let free_line = format!("free({});", src_str);
                    Self::write_to_file(&mut self.c_file, &free_line, self.indent_level);
                }
            }
            LirInstr::HeapAllocCopy { ty, dst, src } => {
                let dest_str = self.codegen_operand(dst);
                let src_str = self.codegen_operand(src);
                let type_str = self.codegen_type(ty);
                let line = format!(
                    "{}* {} = ({}*)malloc(sizeof({}));\n\
                    	if ({} == NULL) {{\n\
                    		printf(\"Failed to allocate memory for {}*\\n\");\n\
                    		exit(1);\n\
                    	}}\n\
                    	memcpy({}, &{}, sizeof({}));",
                    type_str,
                    dest_str,
                    type_str,
                    type_str,
                    dest_str,
                    type_str,
                    dest_str,
                    src_str,
                    type_str
                );
                Self::write_to_file(&mut self.c_file, &line, self.indent_level);
            }
            // Creates an array on the stack.
            // C equivalent to T dest[size] = {0};
            LirInstr::ConstructArray { ty, dst, size: _ } => {
                let dest_str = self.codegen_operand(dst);
                // In C, arrays are defined as T name[size];
                // This will probably not work for multi-dimensional arrays yet
                let line = match ty {
                    LirTy::ArrayTy { .. } => {
                        format!("{} = {{0}};", self.codegen_array_decl(ty, &dest_str))
                    }
                    _ => panic!("ConstructArray expected ArrayTy"),
                };
                Self::write_to_file(&mut self.c_file, &line, self.indent_level);
            }
            // Similar to {foo: bar, baz: qux}
            // It DOES NOT allocate memory, just creates a raw object on the stack
            LirInstr::ConstructObject {
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
            _ => {
                eprintln!("Instruction codegen not implemented for {:?}", instr)
            }
        }
    }

    fn codegen_operand(&mut self, operand: &LirOperand) -> String {
        match operand {
            LirOperand::Arg(a) => format!("arg_{}", a),
            LirOperand::Temp(t) => format!("temp_{}", t),
            LirOperand::GlobalFn(name) => Self::c_ident(name),
            LirOperand::Const(c) => match c {
                ConstantValue::Int(i) => format!("{}", i),
                ConstantValue::UInt(u) => format!("{}", u),
                ConstantValue::Float(f) => format!("{}", f),
                ConstantValue::Bool(b) => format!("{}", b),
                ConstantValue::Char(c) => {
                    let escaped = match c {
                        '\0' => "\\0".to_string(),
                        '\n' => "\\n".to_string(),
                        '\r' => "\\r".to_string(),
                        '\t' => "\\t".to_string(),
                        '\\' => "\\\\".to_string(),
                        '\'' => "\\'".to_string(),
                        _ if c.is_ascii_graphic() || (*c) == ' ' => c.to_string(),
                        _ => format!("\\x{:02x}", (*c) as u32),
                    };
                    format!("'{}'", escaped)
                }
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
            LirOperand::ImmChar(c) => {
                let escaped = match c {
                    '\0' => "\\0".to_string(),
                    '\n' => "\\n".to_string(),
                    '\r' => "\\r".to_string(),
                    '\t' => "\\t".to_string(),
                    '\\' => "\\\\".to_string(),
                    '\'' => "\\'".to_string(),
                    _ if c.is_ascii_graphic() || (*c) == ' ' => c.to_string(),
                    _ => format!("\\x{:02x}", (*c) as u32),
                };
                format!("'{}'", escaped)
            }
            LirOperand::ImmUnit => "NULL".to_string(),
            LirOperand::Deref(d) => format!("(*{})", self.codegen_operand(d)),
            LirOperand::AsRef(a) => {
                if let LirOperand::ImmUnit = **a {
                    format!("({})", self.codegen_operand(a))
                } else {
                    format!("(&{})", self.codegen_operand(a))
                }
            }
            LirOperand::FieldAccess {
                src,
                field_name,
                is_arrow,
                ..
            } => {
                let src_str = self.codegen_operand(src);
                if *is_arrow {
                    if let LirOperand::Deref(_) = **src {
                        format!("({}).{}", src_str, field_name)
                    } else {
                        format!("{}->{}", src_str, field_name)
                    }
                } else {
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

    fn write_to_top(file: &mut String, content: &str) {
        let entry = format!("{}\n", content);
        file.insert_str(0, &entry);
    }
}
