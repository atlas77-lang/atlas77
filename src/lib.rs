#![allow(clippy::result_large_err)]

pub mod atlas_c;
pub mod atlas_lib;
pub mod atlas_vm;

use crate::{
    atlas_c::{
        atlas_asm::AsmProgram,
        atlas_hir::{
            dead_code_elimination_pass::DeadCodeEliminationPass, lifetime_pass::LifeTimePass,
        },
    },
    atlas_vm::runtime::AtlasRuntime,
};
use atlas_c::{
    atlas_asm,
    atlas_codegen::{CodeGenUnit, arena::CodeGenArena},
    atlas_frontend::{parse, parser::arena::AstArena},
    atlas_hir::{
        arena::HirArena, monomorphization_pass::MonomorphizationPass,
        syntax_lowering_pass::AstSyntaxLoweringPass, type_check_pass::TypeChecker,
    },
};
use bumpalo::Bump;
use std::{collections::BTreeMap, io::Write, path::PathBuf, time::Instant};
//todo: The pipeline of the compiler should be more straightforward and should include the "debug" and "release" modes
//todo: There should also be a function for each stage of the pipeline

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

pub fn build(path: String, flag: CompilationFlag, using_std: bool) -> miette::Result<AsmProgram> {
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
    let mut hir = monomorphizer.monomorphize(hir)?;
    //type-check
    let mut type_checker = TypeChecker::new(&hir_arena);
    hir = type_checker.check(hir)?;

    //Lifetime analysis pass
    let mut lifetime = LifeTimePass::new();
    //hir = lifetime.run(hir)?;

    //Dead code elimination (only in release mode)
    if flag == CompilationFlag::Release {
        let mut dce_pass = DeadCodeEliminationPass::new(&hir_arena);
        hir = dce_pass.eliminate_dead_code(hir)?;
    }

    //codegen
    let bump = Bump::new();
    let arena = CodeGenArena::new(&bump);
    let mut codegen = CodeGenUnit::new(hir, arena, &hir_arena);
    let program = codegen.compile()?;
    let mut file = std::fs::File::create("output.atlasc").unwrap();
    let mut content = String::new();
    for label in program.labels.iter() {
        content.push_str(&format!("{}", label));
    }
    file.write_all(content.as_bytes()).unwrap();

    let mut file2 = std::fs::File::create("output.atlas_asm").unwrap();
    let mut assembler = atlas_asm::Assembler::new();
    let asm = assembler.asm_from_instruction(!using_std, program)?;
    let content2 = format!("{}", asm);
    file2.write_all(content2.as_bytes()).unwrap();

    let end = Instant::now();
    println!("Build completed in {}µs", (end - start).as_micros());
    Ok(asm)
}

//The "run" function needs a bit of refactoring
pub fn run(path: String, _flag: CompilationFlag, using_std: bool) -> miette::Result<()> {
    let res = build(path.clone(), _flag, using_std)?;

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
