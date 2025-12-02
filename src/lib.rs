pub mod atlas_c;
pub mod atlas_lib;
mod atlas_vm;

use atlas_c::{
    atlas_asm,
    atlas_codegen::{CodeGenUnit, arena::CodeGenArena},
    atlas_frontend::{parse, parser::arena::AstArena},
    atlas_hir::{
        arena::HirArena, syntax_lowering_pass::AstSyntaxLoweringPass, type_check_pass::TypeChecker,
    },
};
use bumpalo::Bump;

use crate::atlas_c::atlas_hir::monomorphization_pass::MonomorphizationPass;
use std::{io::Write, path::PathBuf};
//todo: The pipeline of the compiler should be more straightforward and should include the "debug" and "release" modes
//todo: There should also be a function for each stage of the pipeline

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

pub fn build(path: String, _flag: CompilationFlag) -> miette::Result<()> {
    let path_buf = get_path(&path);

    let source = std::fs::read_to_string(&path).unwrap_or_else(|_| {
        eprintln!("Failed to read source file at path: {}", path);
        std::process::exit(1);
    }); //parse
    let bump = Bump::new();
    let ast_arena = AstArena::new(&bump);
    let file_path = atlas_c::utils::string_to_static_str(path_buf.to_str().unwrap().to_owned());
    let program = parse(file_path.into(), &ast_arena, source.clone())?;

    //hir
    let hir_arena = HirArena::new();
    let mut lower = AstSyntaxLoweringPass::new(&hir_arena, &program, &ast_arena);
    let hir = lower.lower()?;

    //monomorphize
    let mut monomorphizer =
        MonomorphizationPass::new(&hir_arena, lower.generic_pool, source.clone());
    monomorphizer.monomorphize(hir)?;

    //type-check
    let mut type_checker = TypeChecker::new(&hir_arena);
    type_checker.check(hir)?;

    //codegen
    let bump = Bump::new();
    let arena = CodeGenArena::new(&bump);
    let mut codegen = CodeGenUnit::new(hir, arena, source);
    let program = codegen.compile()?;

    Ok(())
}

//The "run" function needs a bit of refactoring
pub fn run(path: String, _flag: CompilationFlag) -> miette::Result<()> {
    let path_buf = get_path(&path);

    let source = std::fs::read_to_string(&path).unwrap_or_else(|_| {
        eprintln!("Failed to read source file at path: {}", path);
        std::process::exit(1);
    });
    //parse
    let bump = Bump::new();
    let ast_arena = AstArena::new(&bump);
    let file_path = atlas_c::utils::string_to_static_str(path_buf.to_str().unwrap().to_owned());
    let program = parse(file_path.into(), &ast_arena, source.clone())?;

    //hir
    let hir_arena = HirArena::new();
    let mut lower = AstSyntaxLoweringPass::new(&hir_arena, &program, &ast_arena);
    let hir = lower.lower()?;

    //monomorphize
    let mut monomorphizer =
        MonomorphizationPass::new(&hir_arena, lower.generic_pool, source.clone());
    monomorphizer.monomorphize(hir)?;

    //type-check
    let mut type_checker = TypeChecker::new(&hir_arena);
    type_checker.check(hir)?;

    //codegen
    let bump = Bump::new();
    let arena = CodeGenArena::new(&bump);
    let mut codegen = CodeGenUnit::new(hir, arena, source);
    let program = codegen.compile()?;

    //asm
    let assembler = atlas_asm::Assembler::new();
    let asm = assembler.asm_from_instruction(program)?;
    let mut file = std::fs::File::create("output.atlas_asm").unwrap();
    //file.write_all(assembler.display_asm(&asm).as_bytes()).unwrap();
    //WARNING: The VM is currently disabled
    /*
    //run
    let bump = Bump::new();
    let runtime_arena = RuntimeArena::new(&bump);
    let mut vm = atlas_vm::Atlas77VM::new(program, runtime_arena);
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
    */

    Ok(())
}

pub const DEFAULT_INIT_CODE: &str = r#"
import "std/io";

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
