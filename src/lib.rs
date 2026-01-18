#![allow(clippy::result_large_err)]
#![allow(clippy::single_match)]
#![allow(clippy::new_without_default)]
#![allow(clippy::unusual_byte_groupings)]

//! Atlas77 — an experimental statically-typed wannabe systems programming language.
//!
//! This crate provides the Atlas77 compiler and runtime: lexer/parser, HIR passes,
//! code generation, assembler, and a VM. It exposes small helper functions such
//! as `build`, `run`, and `init` (see `DEFAULT_INIT_CODE`) for working with
//! Atlas projects programmatically.
//!
//! Current focus: the v0.7.x "Covenant" series (ownership & move/copy semantics).
//! Upcoming work includes a VM redesign and LIR-based pipeline in the v0.8.x
//! series (typed/register VM, async/await, improved performance).
//!
//! See the repository README and ROADMAP for details and the online docs:
//! https://atlas77-lang.github.io/atlas77-docs/docs/latest/index.html

pub mod atlas_c;
pub mod atlas_docs;
pub mod atlas_lib;
pub mod atlas_vm;

use crate::{
    atlas_c::{
        atlas_asm::AsmProgram,
        atlas_hir::{
            dead_code_elimination_pass::DeadCodeEliminationPass, pretty_print::HirPrettyPrinter,
        },
        atlas_lir::hir_lowering_pass::HirLoweringPass,
    },
    atlas_vm::runtime::AtlasRuntime,
};
use atlas_c::{
    atlas_asm,
    atlas_codegen::{CodeGenUnit, arena::CodeGenArena},
    atlas_frontend::{parse, parser::arena::AstArena},
    atlas_hir::{
        arena::HirArena, monomorphization_pass::MonomorphizationPass,
        ownership_pass::OwnershipPass, syntax_lowering_pass::AstSyntaxLoweringPass,
        type_check_pass::TypeChecker,
    },
};
use bumpalo::Bump;
use cranelift::codegen::ir::Function;
use std::{collections::BTreeMap, io::Write, path::PathBuf, time::Instant};

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum CompilationFlag {
    Release,
    Debug,
}

fn get_path(path: &str) -> PathBuf {
    let mut path_buf = PathBuf::from(path.to_owned());
    if let Ok(current_dir) = std::env::current_dir() {
        if !path_buf.is_absolute() {
            path_buf = current_dir.join(path_buf);
        }
    } else {
        eprintln!("Failed to get current directory");
    }
    path_buf
}

pub fn build(
    path: String,
    flag: CompilationFlag,
    using_std: bool,
    produces_output: bool,
) -> miette::Result<AsmProgram> {
    if produces_output {
        std::fs::create_dir_all("./build").unwrap();
    }
    let start = Instant::now();
    println!("Building project at path: {}", path);
    let path_buf = get_path(&path);

    let source = std::fs::read_to_string(&path).unwrap_or_else(|_| {
        eprintln!("Failed to read source file at path: {}", path);
        std::process::exit(1);
    }); //parse
    let bump = Bump::new();
    let ast_arena = AstArena::new(&bump);
    let file_path = atlas_c::utils::string_to_static_str(path_buf.to_str().unwrap().to_owned());
    let program = match parse(file_path, &ast_arena, source) {
        Ok(prog) => prog,
        Err(e) => {
            return Err((*e).into());
        }
    };

    //hir
    let hir_arena = HirArena::new();
    let mut lower = AstSyntaxLoweringPass::new(&hir_arena, &program, &ast_arena, using_std);
    let hir = lower.lower()?;

    //monomorphize
    let mut monomorphizer = MonomorphizationPass::new(&hir_arena, lower.generic_pool);
    let hir = monomorphizer.monomorphize(hir)?;
    //type-check
    let mut type_checker = TypeChecker::new(&hir_arena);
    let hir = type_checker.check(hir)?;

    // Ownership analysis pass (MOVE/COPY semantics and destructor insertion)
    let mut ownership_pass = OwnershipPass::new(hir.signature.clone(), &hir_arena);
    let mut hir = match ownership_pass.run(hir) {
        Ok(hir) => hir,
        Err((hir, err)) => {
            // Write HIR output (even if there are ownership errors)
            use crate::atlas_c::atlas_hir::pretty_print::HirPrettyPrinter;
            let mut hir_printer = HirPrettyPrinter::new();
            let hir_output = hir_printer.print_module(hir);
            let mut file_hir = std::fs::File::create("./build/output.atlas").unwrap();
            file_hir.write_all(hir_output.as_bytes()).unwrap();
            return Err((err).into());
        }
    };

    //Dead code elimination (only in release mode)
    if flag == CompilationFlag::Release {
        let mut dce_pass = DeadCodeEliminationPass::new(&hir_arena);
        hir = dce_pass.eliminate_dead_code(hir)?;
    }

    // Write HIR output
    if produces_output {
        let mut hir_printer = HirPrettyPrinter::new();
        let hir_output = hir_printer.print_module(hir);
        let mut file_hir = std::fs::File::create("./build/output.atlas").unwrap();
        file_hir.write_all(hir_output.as_bytes()).unwrap();
    }

    let mut lir_lower = HirLoweringPass::new(hir);
    match lir_lower.lower() {
        Ok(lir) => {
            if produces_output {
                let mut file_lir = std::fs::File::create("./build/output.atlas_lir").unwrap();
                let lir_output = format!("{}", lir);
                file_lir.write_all(lir_output.as_bytes()).unwrap();
            }
        }
        Err(e) => {
            eprintln!("{:?}", Into::<miette::Report>::into(*e));
        }
    }

    //codegen
    let bump = Bump::new();
    let arena = CodeGenArena::new(&bump);
    let mut codegen = CodeGenUnit::new(hir, arena, &hir_arena);
    let program = codegen.compile()?;
    if produces_output {
        let mut file = std::fs::File::create("./build/output.atlasc").unwrap();
        let mut content = String::new();
        for label in program.labels.iter() {
            content.push_str(&format!("{}", label));
        }
        file.write_all(content.as_bytes()).unwrap();
    }

    let mut assembler = atlas_asm::Assembler::new();
    let asm = assembler.asm_from_instruction(!using_std, program)?;
    if produces_output {
        let mut file2 = std::fs::File::create("./build/output.atlas_asm").unwrap();
        let content2 = format!("{}", asm);
        file2.write_all(content2.as_bytes()).unwrap();
    }

    let end = Instant::now();
    println!("Build completed in {}µs", (end - start).as_micros());
    Ok(asm)
}

//The "run" function needs a bit of refactoring
pub fn run(path: String, _flag: CompilationFlag, using_std: bool) -> miette::Result<()> {
    let res = build(path.clone(), _flag, using_std, false)?;

    let extern_fn = BTreeMap::new();
    let mut vm = AtlasRuntime::new(res, extern_fn);
    let start = Instant::now();
    let res = vm.run();
    let end = Instant::now();
    match res {
        Ok(_) => {
            println!(
                "Program ran successfully (time: {}µs)",
                (end - start).as_micros()
            );
        }
        Err(e) => {
            eprintln!("{}", e);
        }
    }

    // Print memory report if ATLAS_MEMORY_REPORT env var is set
    if std::env::var("ATLAS_MEMORY_REPORT").is_ok() {
        vm.heap
            .print_memory_report(&vm.asm_program.struct_descriptors);
    }

    Ok(())
}

pub const DEFAULT_INIT_CODE: &str = r#"import "std/io";

fun main() {
    print("Hello, Atlas!");
}
"#;

/// Initializes a new Atlas project by creating a src/ directory and a main.atlas file with default code.
pub fn init(name: String) {
    let path_buf = get_path(&name);
    let project_dir = path_buf.join("src");
    if !project_dir.exists() {
        std::fs::create_dir_all(&project_dir).unwrap();
    }
    let main_file_path = project_dir.join("main.atlas");
    if !main_file_path.exists() {
        let mut file = std::fs::File::create(&main_file_path).unwrap();
        file.write_all(DEFAULT_INIT_CODE.as_bytes()).unwrap();
    }
}

/// Compile up to the AST, then generate documentation in the specified output directory.
pub fn generate_docs(output_dir: String, path: Option<&str>) {
    // Ensure output directory exists
    let output_path = get_path(&output_dir);
    std::fs::create_dir_all(&output_path).unwrap();

    // This should find and do it for every .atlas file in the project, but for now we just do src/main.atlas
    let source_path = get_path(path.unwrap_or("src/main.atlas"));
    let source = std::fs::read_to_string(&source_path).unwrap_or_else(|_| {
        eprintln!(
            "Failed to read source file at path: {}",
            source_path.display()
        );
        std::process::exit(1);
    });
    let ast_arena = Bump::new();
    let ast_arena = AstArena::new(&ast_arena);
    let file_path = atlas_c::utils::string_to_static_str(source_path.to_str().unwrap().to_owned());
    let program = match parse(file_path, &ast_arena, source) {
        Ok(prog) => prog,
        Err(e) => {
            let report: miette::Report = (*e).into();
            eprintln!("{:?}", report);
            std::process::exit(1);
        }
    };

    let hir_arena = HirArena::new();
    let mut lower = AstSyntaxLoweringPass::new(&hir_arena, &program, &ast_arena, true);
    let hir = match lower.lower() {
        Ok(hir) => hir,
        Err(e) => {
            let report: miette::Report = e.into();
            eprintln!("{:?}", report);
            std::process::exit(1);
        }
    };
    // Generate documentation using the AST
    let out_path = output_path.clone();
    #[allow(clippy::unit_arg)]
    {
        if let Err(e) = crate::atlas_docs::generate_docs(&hir.signature, &out_path) {
            eprintln!("atlas_docs error: {}", e);
        }
    }
}

pub fn test(path: String) -> miette::Result<Vec<Function>> {
    std::fs::create_dir_all("./build").unwrap();
    let start = Instant::now();
    println!("Building project at path: {}", path);
    let path_buf = get_path(&path);

    let source = std::fs::read_to_string(&path).unwrap_or_else(|_| {
        eprintln!("Failed to read source file at path: {}", path);
        std::process::exit(1);
    }); //parse
    let bump = Bump::new();
    let ast_arena = AstArena::new(&bump);
    let file_path = atlas_c::utils::string_to_static_str(path_buf.to_str().unwrap().to_owned());
    let program = match parse(file_path, &ast_arena, source) {
        Ok(prog) => prog,
        Err(e) => {
            return Err((*e).into());
        }
    };

    //hir
    let hir_arena = HirArena::new();
    let mut lower = AstSyntaxLoweringPass::new(&hir_arena, &program, &ast_arena, true);
    let hir = lower.lower()?;

    //monomorphize
    let mut monomorphizer = MonomorphizationPass::new(&hir_arena, lower.generic_pool);
    let hir = monomorphizer.monomorphize(hir)?;
    //type-check
    let mut type_checker = TypeChecker::new(&hir_arena);
    let hir = type_checker.check(hir)?;

    // Ownership analysis pass (MOVE/COPY semantics and destructor insertion)
    let mut ownership_pass = OwnershipPass::new(hir.signature.clone(), &hir_arena);
    let mut hir = match ownership_pass.run(hir) {
        Ok(hir) => hir,
        Err((hir, err)) => {
            // Write HIR output (even if there are ownership errors)
            use crate::atlas_c::atlas_hir::pretty_print::HirPrettyPrinter;
            let mut hir_printer = HirPrettyPrinter::new();
            let hir_output = hir_printer.print_module(hir);
            let mut file_hir = std::fs::File::create("./build/output.atlas").unwrap();
            file_hir.write_all(hir_output.as_bytes()).unwrap();
            return Err((err).into());
        }
    };

    //Dead code elimination (only in release mode)
    let mut dce_pass = DeadCodeEliminationPass::new(&hir_arena);
    hir = dce_pass.eliminate_dead_code(hir)?;

    // Write HIR output
    let mut hir_printer = HirPrettyPrinter::new();
    let hir_output = hir_printer.print_module(hir);
    let mut file_hir = std::fs::File::create("./build/output.atlas").unwrap();
    file_hir.write_all(hir_output.as_bytes()).unwrap();

    let mut lir_lower = HirLoweringPass::new(hir);
    let lir = match lir_lower.lower() {
        Ok(lir) => {
            let mut file_lir = std::fs::File::create("./build/output.atlas_lir").unwrap();
            let lir_output = format!("{}", &lir);
            file_lir.write_all(lir_output.as_bytes()).unwrap();
            lir
        }
        Err(e) => {
            eprintln!("{:?}", Into::<miette::Report>::into(*e));
            std::process::exit(1);
        }
    };
    // codegen
    use atlas_c::atlas_new_codegen::codegen_program;
    let functions = codegen_program(&lir);
    let end = Instant::now();
    println!("Build completed in {}µs", (end - start).as_micros());

    let mut file_clif = std::fs::File::create("./build/output.clif").unwrap();
    for func in functions.iter() {
        let content = format!("{}", func);
        file_clif.write_all(content.as_bytes()).unwrap();
    }

    // Let's try to actually produce a binary for testing purposes
    use cranelift::prelude::*;

    Ok(functions)
}
