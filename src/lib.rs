#![allow(clippy::result_large_err)]
#![allow(clippy::single_match)]
#![allow(clippy::new_without_default)]
#![allow(clippy::unusual_byte_groupings)]

//! Atlas77 — an experimental statically-typed wannabe systems programming language.
//!
//! This crate provides the Atlas77 compiler and runtime: lexer/parser, HIR passes,
//! code generation, assembler, and a VM. It exposes small helper functions such
//! as `build` and `init` (see `DEFAULT_INIT_CODE`) for working with
//! Atlas projects programmatically.
//! 
//! See the repository README and ROADMAP for details and the online docs:
//! https://atlas77-lang.github.io/atlas77-docs/docs/latest/index.html

pub mod atlas_c;
pub mod atlas_docs;
pub mod atlas_lib;
#[cfg(all(feature = "embedded-tinycc", not(tinycc_unavailable)))]
pub(crate) mod tcc;

use crate::atlas_c::{
    atlas_codegen::{CCodeGen, HEADER_NAME},
    atlas_hir::{
        dead_code_elimination_pass::DeadCodeEliminationPass, pretty_print::HirPrettyPrinter,
    },
    atlas_lir::hir_lowering_pass::HirLoweringPass,
};
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
#[cfg(all(feature = "embedded-tinycc", not(tinycc_unavailable)))]
pub mod with_tcc {
    use std::{ffi::CString, path::PathBuf, time::Instant};

    use crate::tcc::{
        self, OutputType, tcc_add_include_path, tcc_add_library_path, tcc_compile_string, tcc_new,
        tcc_output_file, tcc_set_output_type,
    };
    // output_dir only tells where to put the output binary
    // The input C file is always ./build/output.atlas_c.c
    // The input C header is always ./build/__atlas77_header.h
    pub fn emit_binary(output_dir: String) -> miette::Result<()> {
        let start = Instant::now();

        unsafe {
            let tcc = tcc_new();
            tcc_set_output_type(tcc, OutputType::Exe.into());
            // Add include paths for TinyCC and generated header

            // tinycc include path
            include_path(tcc).expect("Failed to find TinyCC include path for current platform");

            // generated header (keep CString around)
            let header_path = std::env::current_dir().unwrap().join("build");
            let header_c = CString::new(header_path.to_string_lossy().as_ref()).unwrap();
            eprintln!(
                "Adding generated header include path: {}",
                header_path.display()
            );
            tcc_add_include_path(tcc, header_c.as_ptr() as *const i8);

            // library path (keep CString)
            let path_to_tcc_lib = get_prebuilt_path()
                .expect("Failed to find prebuilt TinyCC binaries for current platform");
            let lib_c = CString::new(path_to_tcc_lib.to_string_lossy().as_ref()).unwrap();
            tcc_add_library_path(tcc, lib_c.as_ptr() as *const i8);

            // read C file and pass as C string
            let code = std::fs::read_to_string("./build/output.atlas_c.c").unwrap();
            let code_c = CString::new(code).unwrap();
            let res = tcc_compile_string(tcc, code_c.as_ptr() as *const i8);

            // out name already uses CString; keep it around until after tcc_output_file
            let target = get_current_platform();
            let out_name = if target.contains("windows") {
                CString::new(format!("{}/a.exe", output_dir)).unwrap()
            } else {
                CString::new(format!("{}/a.out", output_dir)).unwrap()
            };
            let out_res = tcc_output_file(tcc, out_name.as_ptr());

            let end = Instant::now();
            if res == 0 && out_res == 0 {
                println!(
                    "Program compiled and output to {} (time: {}µs)",
                    out_name.to_str().unwrap(),
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

    fn get_current_platform() -> String {
        use target_lexicon::Triple;
        let target = Triple::host();
        target.to_string()
    }

    fn get_prebuilt_path() -> Option<PathBuf> {
        let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        let target = get_current_platform();
        let prebuilt_dir = manifest_dir.join("tinycc/prebuilt");

        eprintln!(
            "Looking for prebuilt TinyCC binaries for target: {}",
            target
        );
        // Map Rust target triples to TinyCC platform directories
        let platform_dir = match target.as_str() {
            t if t.contains("x86_64") && t.contains("linux") => "linux-x64",
            t if t.contains("aarch64") && t.contains("linux") => "linux-arm64",
            t if t.contains("x86_64") && t.contains("windows") => {
                // Can be "windows-x64/gnu" or "windows-x64/msvc" based on toolchain.
                if cfg!(target_env = "msvc") {
                    "windows-x64/msvc"
                } else {
                    "windows-x64/gnu"
                }
            }
            t if t.contains("x86_64") && t.contains("apple") => "macos-x64",
            t if t.contains("aarch64") && t.contains("apple") => "macos-arm64",
            //t if t.contains("aarch64") && t.contains("windows") => "windows-aarch64",
            _ => return None,
        };

        let full_path = prebuilt_dir.join(platform_dir);
        eprintln!(
            "Checking for prebuilt TinyCC binaries at path: {}",
            full_path.display()
        );
        if full_path.exists() {
            Some(full_path)
        } else {
            None
        }
    }

    /// Find and add every include path needed for TinyCC compilation
    ///
    /// It's in a separate function, because the logic differs per platform.
    fn include_path(tcc: *mut tcc::TCCState) -> Result<(), ()> {
        let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        let target = get_current_platform();

        eprintln!("Looking for TinyCC include path for target: {}", target);
        // Needs to include vendor/tinycc/win32/include/ + vendor/tinycc/win32/include/winapi/ +
        // vendor/tinycc/win32/include/sys/ + vendor/tinycc/win32/include/tcc/ + vendor/tinycc/win32/include/sec_api +
        // vendor/tinycc/win32/include/sec_api/sys/
        if target.contains("windows") {
            let base_include = manifest_dir.join("vendor/tinycc/win32/include");
            let include_c = CString::new(base_include.to_string_lossy().as_ref()).unwrap();
            eprintln!("Adding TinyCC include path: {}", base_include.display());
            unsafe {
                tcc_add_include_path(tcc, include_c.as_ptr() as *const i8);
            }

            let winapi_include = base_include.join("winapi");
            let winapi_c = CString::new(winapi_include.to_string_lossy().as_ref()).unwrap();
            eprintln!("Adding TinyCC include path: {}", winapi_include.display());
            unsafe {
                tcc_add_include_path(tcc, winapi_c.as_ptr() as *const i8);
            }

            let sys_include = base_include.join("sys");
            let sys_c = CString::new(sys_include.to_string_lossy().as_ref()).unwrap();
            eprintln!("Adding TinyCC include path: {}", sys_include.display());
            unsafe {
                tcc_add_include_path(tcc, sys_c.as_ptr() as *const i8);
            }

            let tcc_include = base_include.join("tcc");
            let tcc_c = CString::new(tcc_include.to_string_lossy().as_ref()).unwrap();
            eprintln!("Adding TinyCC include path: {}", tcc_include.display());
            unsafe {
                tcc_add_include_path(tcc, tcc_c.as_ptr() as *const i8);
            }

            let sec_api_include = base_include.join("sec_api");
            let sec_api_c = CString::new(sec_api_include.to_string_lossy().as_ref()).unwrap();
            eprintln!("Adding TinyCC include path: {}", sec_api_include.display());
            unsafe {
                tcc_add_include_path(tcc, sec_api_c.as_ptr() as *const i8);
            }

            let sec_api_sys_include = sec_api_include.join("sys");
            let sec_api_sys_c =
                CString::new(sec_api_sys_include.to_string_lossy().as_ref()).unwrap();
            eprintln!(
                "Adding TinyCC include path: {}",
                sec_api_sys_include.display()
            );
            unsafe {
                tcc_add_include_path(tcc, sec_api_sys_c.as_ptr() as *const i8);
            }
        } else {
            // For Linux and macOS, just use the standard include path
            let base_include = manifest_dir.join("vendor/tinycc/include");
            let include_c = CString::new(base_include.to_string_lossy().as_ref()).unwrap();
            eprintln!("Adding TinyCC include path: {}", base_include.display());
            unsafe {
                tcc_add_include_path(tcc, include_c.as_ptr() as *const i8);
            }
        }

        Ok(())
    }
}

#[cfg(all(feature = "embedded-tinycc", not(tinycc_unavailable)))]
pub use with_tcc::emit_binary;

#[cfg(any(not(feature = "embedded-tinycc"), tinycc_unavailable))]
pub fn emit_binary(path: String) -> miette::Result<()> {
    eprintln!(
        "Embedded TinyCC feature is not enabled or TinyCC is unavailable on this platform. Cannot run compiled programs."
    );
    std::process::exit(1);
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
    //TODO: `using_std` is currently unused
    _using_std: bool,
    compiler: SupportedCompiler,
    output_dir: String,
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

    match compiler {
        SupportedCompiler::TinyCC => {
            #[cfg(all(feature = "embedded-tinycc", not(tinycc_unavailable)))]
            {
                emit_binary(output_dir)?;
            }
            #[cfg(all(not(feature = "embedded-tinycc"), tinycc_unavailable))]
            {
                // Let's invoke it with `tcc ./build/output.atlas_c.c -o {output_dir}` 
                eprintln!("Embedded TinyCC feature is not enabled, trying to invoke system TCC compiler.");
                let mut command = std::process::Command::new("tcc");
                command.arg("./build/output.atlas_c.c");
                command.arg("-o");
                let target = if cfg!(target_os = "windows") {
                    format!("{}/a.exe", output_dir)
                } else {
                    format!("{}/a.out", output_dir)
                };
                command.arg(target);
                eprintln!("Invoking TCC with command: {:?}", command);
                let status = command.status().expect("Failed to invoke TCC");
                if status.success() {
                    println!("Program compiled successfully with TCC.");
                } else {
                    eprintln!("TCC compilation failed.");
                }                
            }
        }
        SupportedCompiler::GCC => {
            // Let's invoke it with `gcc ./build/output.atlas_c.c -o {output_dir}` (and `-O2` for release)
            let mut command = std::process::Command::new("gcc");
            command.arg("./build/output.atlas_c.c");
            command.arg("-o");
            let target = if cfg!(target_os = "windows") {
                format!("{}/a.exe", output_dir)
            } else {
                format!("{}/a.out", output_dir)
            };
            command.arg(target);
            if _flag == CompilationFlag::Release {
                command.arg("-O2");
            }
            // TODO: Make it pretty print
            eprintln!("Invoking GCC with command: {:?}", command);
            let status = command.status().expect("Failed to invoke GCC");
            if status.success() {
                println!("Program compiled successfully with GCC.");
            } else {
                eprintln!("GCC compilation failed.");
            }
        }
        SupportedCompiler::MSVC => {
            // Let's invoke it with `cl ./build/output.atlas_c.c /Fe:{output_dir}` (and `/O2` for release)
            let mut command = std::process::Command::new("cl");
            command.arg("./build/output.atlas_c.c");
            let target = if cfg!(target_os = "windows") {
                format!("{}/a.exe", output_dir)
            } else {
                format!("{}/a.out", output_dir)
            };
            command.arg(format!("/Fe:{}", target));
            if _flag == CompilationFlag::Release {
                command.arg("/O2");
            }
            // TODO: Make it pretty print
            eprintln!("Invoking MSVC with command: {:?}", command);
            let status = command.status().expect("Failed to invoke MSVC cl.exe");
            if status.success() {
                println!("Program compiled successfully with MSVC.");
            } else {
                eprintln!("MSVC compilation failed.");
            }
        }
        SupportedCompiler::Clang => {
            // Let's invoke it with `clang ./build/output.atlas_c.c -o {output_dir}` (and `-O2` for release)
            let mut command = std::process::Command::new("clang");
            command.arg("./build/output.atlas_c.c");
            command.arg("-o");
            let target = if cfg!(target_os = "windows") {
                format!("{}/a.exe", output_dir)
            } else {
                format!("{}/a.out", output_dir)
            };
            command.arg(target);
            if _flag == CompilationFlag::Release {
                command.arg("-O2");
            }
            // TODO: Make it pretty print
            eprintln!("Invoking Clang with command: {:?}", command);
            let status = command.status().expect("Failed to invoke Clang");
            if status.success() {
                println!("Program compiled successfully with Clang.");
            } else {
                eprintln!("Clang compilation failed.");
            }
        }
        SupportedCompiler::Intel => {
            // Let's invoke it with `icc ./build/output.atlas_c.c -o {output_dir}` (and `-O2` for release)
            let mut command = std::process::Command::new("icc");
            command.arg("./build/output.atlas_c.c");
            command.arg("-o");
            let target = if cfg!(target_os = "windows") {
                format!("{}/a.exe", output_dir)
            } else {
                format!("{}/a.out", output_dir)
            };
            command.arg(target);
            if _flag == CompilationFlag::Release {
                command.arg("-O2");
            }
            // TODO: Make it pretty print
            eprintln!("Invoking Intel ICC with command: {:?}", command);
            let status = command.status().expect("Failed to invoke Intel ICC");
            if status.success() {
                println!("Program compiled successfully with Intel ICC.");
            } else {
                eprintln!("Intel ICC compilation failed.");
            }
        }
        SupportedCompiler::None => {
            println!("Skipping compilation step as per user request.");
        }
    }

    let end = Instant::now();
    println!("Build completed in {}µs", (end - start).as_micros());
    Ok(())
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum SupportedCompiler {
    TinyCC,
    GCC,
    MSVC,
    Clang,
    Intel,
    None,
}

impl SupportedCompiler {
    pub fn from_str(s: &str) -> Self {
        match s.to_lowercase().as_str() {
            "tinycc" | "tcc" => SupportedCompiler::TinyCC,
            "gcc" => SupportedCompiler::GCC,
            "msvc" | "cl" => SupportedCompiler::MSVC,
            "clang" => SupportedCompiler::Clang,
            "intel" | "icc" => SupportedCompiler::Intel,
            "none" => SupportedCompiler::None,
            _ => SupportedCompiler::TinyCC, // default to TinyCC
        }
    }
}
