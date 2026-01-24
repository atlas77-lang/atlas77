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
pub(crate) mod tcc;

use crate::{atlas_c::{
    atlas_codegen::{CCodeGen, HEADER_NAME},
    atlas_hir::{
        dead_code_elimination_pass::DeadCodeEliminationPass, pretty_print::HirPrettyPrinter,
    },
    atlas_lir::hir_lowering_pass::HirLoweringPass,
}, tcc::{OutputType, tcc_add_include_path, tcc_add_library_path, tcc_compile_string, tcc_new, tcc_output_file, tcc_set_output_type}};
use atlas_c::{
    atlas_frontend::{parse, parser::arena::AstArena},
    atlas_hir::{
        arena::HirArena, monomorphization_pass::MonomorphizationPass,
        ownership_pass::OwnershipPass, syntax_lowering_pass::AstSyntaxLoweringPass,
        type_check_pass::TypeChecker,
    },
};
use bumpalo::Bump;
use std::{io::Write, path::PathBuf, time::Instant};

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

//The "run" function needs a bit of refactoring
pub fn run(path: String, flag: CompilationFlag, using_std: bool) -> miette::Result<()> {
    build(path.clone(), flag, using_std, false)?;
    let start = Instant::now();

    let tcc = unsafe { tcc_new() };
    unsafe {
        tcc_set_output_type(tcc, OutputType::Exe.into());
        // Add include paths for TinyCC and generated header
        let path_to_tcc_include = std::env::current_dir().unwrap().join("vendor/tinycc/include");
        tcc_add_include_path(tcc, path_to_tcc_include.to_str().unwrap().as_ptr() as *const i8);
        let header_path = std::env::current_dir().unwrap().join("build");
        tcc_add_include_path(tcc, header_path.to_str().unwrap().as_ptr() as *const i8);

        // Add library path for libtcc1.a
        let path_to_tcc_lib = std::env::current_dir().unwrap().join("vendor/tinycc");
        tcc_add_library_path(tcc, path_to_tcc_lib.to_str().unwrap().as_ptr() as *const i8);

        // Compile the generated C code
        let res = tcc_compile_string(tcc, std::fs::read_to_string("./build/output.atlas_c.c").unwrap().as_ptr() as *const i8);
        use std::ffi::CString;
        let out_name = CString::new("./build/a.out").unwrap();
        let out_res = tcc_output_file(tcc, out_name.as_ptr());

        let end = Instant::now();
        if res == 0 && out_res == 0 {
            println!(
                "Program compiled and output to ./build/a.out (time: {}µs)",
                (end - start).as_micros()
            );
        } else if res != 0 {
            eprintln!("TCC Compilation Error");
        } else {
            eprintln!("TCC failed to output executable");
        }
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

pub fn build(
    path: String,
    _flag: CompilationFlag,
    using_std: bool,
    produce_output: bool,
) -> miette::Result<()> {
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
    let mut c_codegen = CCodeGen::new();
    c_codegen.emit_c(&lir).unwrap();

    let mut c_file = std::fs::File::create("./build/output.atlas_c.c").unwrap();
    c_file.write_all(c_codegen.c_file.as_bytes()).unwrap();
    let mut c_header = std::fs::File::create(format!("./build/{}", HEADER_NAME)).unwrap();
    c_header.write_all(c_codegen.c_header.as_bytes()).unwrap();

    let end = Instant::now();
    println!("Build completed in {}µs", (end - start).as_micros());
    Ok(())
}
