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
pub mod tcc;

use crate::atlas_c::{
    atlas_codegen::{CCodeGen, HEADER_NAME},
    atlas_hir::{
        dead_code_elimination_pass::DeadCodeEliminationPass,
        error::{HirError, HirErrorGravity, HirPass, SemanticAnalysisFailedError},
        ownership_pass::HirOwnershipPass,
        pretty_print::HirPrettyPrinter,
    },
    atlas_lir::hir_lowering_pass::HirLoweringPass,
};
use atlas_c::{
    atlas_frontend::{parse, parser::arena::AstArena},
    atlas_hir::{
        arena::HirArena, monomorphization_pass::MonomorphizationPass,
        syntax_lowering_pass::AstSyntaxLoweringPass, type_check_pass::TypeChecker,
    },
};
use bumpalo::Bump;
use std::{
    io::Write,
    path::{Path, PathBuf},
    process::Command,
    str::FromStr,
    time::Instant,
};

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
        self, OutputType, tcc_add_include_path, tcc_add_library, tcc_add_library_path,
        tcc_compile_string, tcc_new, tcc_output_file, tcc_set_output_type,
    };
    // output_dir only tells where to put the output binary
    // The input C file is always ./build/output.atlas_c.c
    // The input C header is always ./build/__atlas77_header.h
    pub fn emit_binary(
        output_dir: String,
        extra_include_dirs: &[String],
        extra_library_dirs: &[String],
        extra_libraries: &[String],
    ) -> miette::Result<()> {
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
            tcc_add_include_path(tcc, header_c.as_ptr());

            for include_dir in extra_include_dirs {
                let include_c = CString::new(include_dir.as_str()).unwrap();
                eprintln!("Adding atlas.toml include path: {}", include_dir);
                tcc_add_include_path(tcc, include_c.as_ptr());
            }

            // library path (keep CString)
            let path_to_tcc_lib = get_prebuilt_path()
                .expect("Failed to find prebuilt TinyCC binaries for current platform");
            let lib_c = CString::new(path_to_tcc_lib.to_string_lossy().as_ref()).unwrap();
            tcc_add_library_path(tcc, lib_c.as_ptr());

            for library_dir in extra_library_dirs {
                let library_dir_c = CString::new(library_dir.as_str()).unwrap();
                eprintln!("Adding atlas.toml library path: {}", library_dir);
                tcc_add_library_path(tcc, library_dir_c.as_ptr());
            }

            for library in extra_libraries {
                let library_name = library
                    .strip_prefix("-l")
                    .unwrap_or(library)
                    .trim_end_matches(".lib")
                    .trim_end_matches(".a")
                    .trim_end_matches(".so")
                    .trim_end_matches(".dylib")
                    .to_owned();
                let library_c = CString::new(library_name.as_str()).unwrap();
                eprintln!("Linking atlas.toml library with TinyCC: {}", library_name);
                tcc_add_library(tcc, library_c.as_ptr());
            }

            // read C file and pass as C string
            let code = std::fs::read_to_string("./build/output.atlas_c.c").unwrap();
            let code_c = CString::new(code).unwrap();
            let res = tcc_compile_string(tcc, code_c.as_ptr());

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
                tcc_add_include_path(tcc, include_c.as_ptr());
            }

            let winapi_include = base_include.join("winapi");
            let winapi_c = CString::new(winapi_include.to_string_lossy().as_ref()).unwrap();
            eprintln!("Adding TinyCC include path: {}", winapi_include.display());
            unsafe {
                tcc_add_include_path(tcc, winapi_c.as_ptr());
            }

            let sys_include = base_include.join("sys");
            let sys_c = CString::new(sys_include.to_string_lossy().as_ref()).unwrap();
            eprintln!("Adding TinyCC include path: {}", sys_include.display());
            unsafe {
                tcc_add_include_path(tcc, sys_c.as_ptr());
            }

            let tcc_include = base_include.join("tcc");
            let tcc_c = CString::new(tcc_include.to_string_lossy().as_ref()).unwrap();
            eprintln!("Adding TinyCC include path: {}", tcc_include.display());
            unsafe {
                tcc_add_include_path(tcc, tcc_c.as_ptr());
            }

            let sec_api_include = base_include.join("sec_api");
            let sec_api_c = CString::new(sec_api_include.to_string_lossy().as_ref()).unwrap();
            eprintln!("Adding TinyCC include path: {}", sec_api_include.display());
            unsafe {
                tcc_add_include_path(tcc, sec_api_c.as_ptr());
            }

            let sec_api_sys_include = sec_api_include.join("sys");
            let sec_api_sys_c =
                CString::new(sec_api_sys_include.to_string_lossy().as_ref()).unwrap();
            eprintln!(
                "Adding TinyCC include path: {}",
                sec_api_sys_include.display()
            );
            unsafe {
                tcc_add_include_path(tcc, sec_api_sys_c.as_ptr());
            }
        } else {
            // For Linux and macOS, just use the standard include path
            let base_include = manifest_dir.join("vendor/tinycc/include");
            let include_c = CString::new(base_include.to_string_lossy().as_ref()).unwrap();
            eprintln!("Adding TinyCC include path: {}", base_include.display());
            unsafe {
                tcc_add_include_path(tcc, include_c.as_ptr());
            }
        }

        Ok(())
    }
}

#[cfg(all(feature = "embedded-tinycc", not(tinycc_unavailable)))]
pub use with_tcc::emit_binary;

#[cfg(any(not(feature = "embedded-tinycc"), tinycc_unavailable))]
pub fn emit_binary(
    _path: String,
    _extra_include_dirs: &[String],
    _extra_library_dirs: &[String],
    _extra_libraries: &[String],
) -> miette::Result<()> {
    eprintln!(
        "Embedded TinyCC feature is not enabled or TinyCC is unavailable on this platform. Cannot run compiled programs."
    );
    std::process::exit(1);
}

#[derive(Debug, Default, Clone)]
struct AtlasBuildConfig {
    preferred_compiler: Option<SupportedCompiler>,
    headers: Vec<String>,
    include_dirs: Vec<String>,
    library_dirs: Vec<String>,
    libraries: Vec<String>,
    compiler_args: Vec<String>,
}

fn parse_supported_compiler(name: &str) -> Option<SupportedCompiler> {
    match name.to_lowercase().as_str() {
        "tinycc" | "tcc" => Some(SupportedCompiler::TinyCC),
        "gcc" => Some(SupportedCompiler::GCC),
        "msvc" | "cl" => Some(SupportedCompiler::MSVC),
        "clang" => Some(SupportedCompiler::Clang),
        "intel" | "icc" => Some(SupportedCompiler::Intel),
        "none" => Some(SupportedCompiler::None),
        _ => None,
    }
}

fn normalize_dir_path(project_dir: &Path, value: &str) -> String {
    let candidate = PathBuf::from(value);
    if candidate.is_absolute() {
        candidate.to_string_lossy().to_string()
    } else {
        project_dir.join(candidate).to_string_lossy().to_string()
    }
}

fn dedup_preserve_order(values: &mut Vec<String>) {
    let mut seen = std::collections::HashSet::new();
    values.retain(|item| seen.insert(item.clone()));
}

fn collect_string_array(table: &toml::value::Table, key: &str) -> Vec<String> {
    table
        .get(key)
        .and_then(|value| value.as_array())
        .map(|values| {
            values
                .iter()
                .filter_map(|entry| entry.as_str().map(ToOwned::to_owned))
                .collect::<Vec<_>>()
        })
        .unwrap_or_default()
}

fn merge_link_table_into_config(config: &mut AtlasBuildConfig, table: &toml::value::Table) {
    config
        .compiler_args
        .extend(collect_string_array(table, "args"));
    config
        .compiler_args
        .extend(collect_string_array(table, "c_args"));
    config
        .library_dirs
        .extend(collect_string_array(table, "lib_dirs"));
    config
        .library_dirs
        .extend(collect_string_array(table, "library_dirs"));
    config
        .include_dirs
        .extend(collect_string_array(table, "include_dirs"));

    for key in ["libs", "shared", "static"] {
        config.libraries.extend(collect_string_array(table, key));
    }
}

fn current_platform_config_key() -> &'static str {
    if cfg!(target_os = "windows") {
        "windows"
    } else if cfg!(target_os = "linux") {
        "linux"
    } else if cfg!(target_os = "macos") {
        "macos"
    } else {
        "unknown"
    }
}

fn normalize_link_arg(compiler: SupportedCompiler, lib: &str) -> String {
    if lib.is_empty()
        || lib.starts_with('-')
        || lib.ends_with(".lib")
        || lib.ends_with(".a")
        || lib.ends_with(".so")
        || lib.ends_with(".dylib")
        || lib.contains('/')
        || lib.contains('\\')
    {
        return lib.to_owned();
    }

    match compiler {
        SupportedCompiler::MSVC => format!("{}.lib", lib),
        _ => format!("-l{}", lib),
    }
}

fn render_include_arg(compiler: SupportedCompiler, include_dir: &str) -> String {
    match compiler {
        SupportedCompiler::MSVC => format!("/I{}", include_dir),
        _ => format!("-I{}", include_dir),
    }
}

fn render_library_dir_arg(compiler: SupportedCompiler, library_dir: &str) -> String {
    match compiler {
        SupportedCompiler::MSVC => format!("/LIBPATH:{}", library_dir),
        _ => format!("-L{}", library_dir),
    }
}

fn load_build_config(project_dir: &PathBuf) -> miette::Result<AtlasBuildConfig> {
    let config_path = project_dir.join("atlas.toml");
    if !config_path.exists() {
        return Ok(AtlasBuildConfig::default());
    }

    let content = std::fs::read_to_string(&config_path)
        .map_err(|err| miette::miette!("Failed to read {}: {}", config_path.display(), err))?;
    let root: toml::value::Table = toml::from_str(&content)
        .map_err(|err| miette::miette!("Failed to parse {}: {}", config_path.display(), err))?;

    let mut config = AtlasBuildConfig::default();

    if let Some(dependencies) = root.get("dependencies").and_then(|v| v.as_table()) {
        config
            .headers
            .extend(collect_string_array(dependencies, "headers"));
        config
            .include_dirs
            .extend(collect_string_array(dependencies, "include_dirs"));
    }

    if let Some(build_table) = root.get("build").and_then(|v| v.as_table()) {
        config.preferred_compiler = build_table
            .get("compiler")
            .and_then(|v| v.as_str())
            .and_then(parse_supported_compiler);
        config
            .compiler_args
            .extend(collect_string_array(build_table, "c_args"));
        config
            .compiler_args
            .extend(collect_string_array(build_table, "args"));
    }

    if let Some(c_table) = root.get("c").and_then(|v| v.as_table()) {
        if config.preferred_compiler.is_none() {
            config.preferred_compiler = c_table
                .get("compiler")
                .and_then(|v| v.as_str())
                .and_then(parse_supported_compiler);
        }
        config
            .compiler_args
            .extend(collect_string_array(c_table, "args"));
        config
            .compiler_args
            .extend(collect_string_array(c_table, "c_args"));
        config
            .include_dirs
            .extend(collect_string_array(c_table, "include_dirs"));
        config
            .library_dirs
            .extend(collect_string_array(c_table, "lib_dirs"));
        config
            .library_dirs
            .extend(collect_string_array(c_table, "library_dirs"));
    }

    if let Some(link_table) = root.get("link").and_then(|v| v.as_table()) {
        merge_link_table_into_config(&mut config, link_table);
        if let Some(platform_table) = link_table
            .get(current_platform_config_key())
            .and_then(|v| v.as_table())
        {
            merge_link_table_into_config(&mut config, platform_table);
        }
    }

    for include_dir in &mut config.include_dirs {
        *include_dir = normalize_dir_path(project_dir, include_dir);
    }
    for library_dir in &mut config.library_dirs {
        *library_dir = normalize_dir_path(project_dir, library_dir);
    }

    dedup_preserve_order(&mut config.headers);
    dedup_preserve_order(&mut config.include_dirs);
    dedup_preserve_order(&mut config.library_dirs);
    dedup_preserve_order(&mut config.libraries);
    dedup_preserve_order(&mut config.compiler_args);

    Ok(config)
}

fn apply_default_native_layout(
    config: &mut AtlasBuildConfig,
    project_dir: &Path,
    compiler: SupportedCompiler,
) {
    // Keep this lenient: we only add defaults if folders exist.
    if compiler != SupportedCompiler::TinyCC {
        return;
    }

    let include_dir = project_dir.join("include");
    if include_dir.is_dir() {
        config
            .include_dirs
            .push(include_dir.to_string_lossy().to_string());
    }

    let library_dir = project_dir.join("lib");
    if library_dir.is_dir() {
        config
            .library_dirs
            .push(library_dir.to_string_lossy().to_string());
    }

    dedup_preserve_order(&mut config.include_dirs);
    dedup_preserve_order(&mut config.library_dirs);
}

fn build_compiler_args(config: &AtlasBuildConfig, compiler: SupportedCompiler) -> Vec<String> {
    let mut compiler_args = Vec::new();
    for include_dir in &config.include_dirs {
        compiler_args.push(render_include_arg(compiler, include_dir));
    }
    for library_dir in &config.library_dirs {
        compiler_args.push(render_library_dir_arg(compiler, library_dir));
    }
    for lib in &config.libraries {
        compiler_args.push(normalize_link_arg(compiler, lib));
    }
    compiler_args.extend(config.compiler_args.clone());
    compiler_args
}

fn create_probe_source(headers: &[String]) -> String {
    let mut source = String::new();
    for header in headers {
        let include_line = if header.starts_with('<') || header.starts_with('"') {
            format!("#include {}\n", header)
        } else {
            format!("#include <{}>\n", header)
        };
        source.push_str(&include_line);
    }
    source.push_str("int main(void) { return 0; }\n");
    source
}

fn preflight_external_compile(
    compiler: SupportedCompiler,
    headers: &[String],
    compiler_args: &[String],
) -> miette::Result<()> {
    if headers.is_empty() && compiler_args.is_empty() {
        return Ok(());
    }

    let (cmd_name, output_arg_mode) = match compiler {
        SupportedCompiler::GCC => ("gcc", "gnu"),
        SupportedCompiler::Clang => ("clang", "gnu"),
        SupportedCompiler::Intel => ("icc", "gnu"),
        SupportedCompiler::MSVC => ("cl", "msvc"),
        SupportedCompiler::TinyCC => ("tcc", "gnu"),
        SupportedCompiler::None => return Ok(()),
    };

    std::fs::create_dir_all("./build")
        .map_err(|err| miette::miette!("Failed to create build directory: {}", err))?;

    let probe_source = "./build/.atlas77_probe.c";
    let probe_binary = if cfg!(target_os = "windows") {
        "./build/.atlas77_probe.exe"
    } else {
        "./build/.atlas77_probe.out"
    };

    std::fs::write(probe_source, create_probe_source(headers)).map_err(|err| {
        miette::miette!(
            "Failed to write Atlas C probe source file {}: {}",
            probe_source,
            err
        )
    })?;

    let mut command = Command::new(cmd_name);
    command.arg(probe_source);
    if output_arg_mode == "msvc" {
        command.arg(format!("/Fe:{}", probe_binary));
        command.arg("/nologo");
    } else {
        command.arg("-o");
        command.arg(probe_binary);
    }
    command.args(compiler_args);

    let output = command.output().map_err(|err| {
        miette::miette!(
            "Failed to execute compiler preflight command {:?}: {}",
            command,
            err
        )
    })?;

    let _ = std::fs::remove_file(probe_source);
    let _ = std::fs::remove_file(probe_binary);

    if output.status.success() {
        return Ok(());
    }

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    Err(miette::miette!(
        "Atlas preflight failed before final C compilation.\nCompiler: {}\nArgs: {:?}\nstdout:\n{}\nstderr:\n{}",
        cmd_name,
        compiler_args,
        stdout,
        stderr
    ))
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
    compiler: Option<SupportedCompiler>,
    output_dir: String,
    extra_c_args: Vec<String>,
) -> miette::Result<()> {
    std::fs::create_dir_all("./build").unwrap();
    let start = Instant::now();
    println!("Building project at path: {}", path);
    let path_buf = get_path(&path);
    let project_dir = std::env::current_dir()
        .map_err(|err| miette::miette!("Failed to get current directory: {}", err))?;
    let mut atlas_build_config = load_build_config(&project_dir)?;
    let compiler = compiler
        .or(atlas_build_config.preferred_compiler)
        .unwrap_or(SupportedCompiler::TinyCC);

    apply_default_native_layout(&mut atlas_build_config, &project_dir, compiler);

    let mut merged_c_args = build_compiler_args(&atlas_build_config, compiler);
    merged_c_args.extend(extra_c_args);

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

    let mut hir_printer = HirPrettyPrinter::new();
    let hir_output = hir_printer.print_module(hir, "AST Syntax Lowering Pass");
    let mut file_hir = std::fs::File::create("./build/unfinished_output.atlas").unwrap();
    file_hir.write_all(hir_output.as_bytes()).unwrap();

    //monomorphize
    let mut monomorphizer = MonomorphizationPass::new(&hir_arena, lower.generic_pool);
    let hir = monomorphizer.monomorphize(hir)?;
    hir_printer.clear();
    let mut file_monomorphization =
        std::fs::File::create("./build/m_unfinished_output.atlas").unwrap();
    file_monomorphization
        .write_all(
            hir_printer
                .print_module(hir, "Monomorphization pass")
                .as_bytes(),
        )
        .unwrap();

    //type-check (collect errors and continue when gravity allows it)
    let mut hir = hir;
    let mut semantic_errors: Vec<HirError> = Vec::new();

    let mut type_checker = TypeChecker::new(&hir_arena);
    if let Err(err) = type_checker.check(&mut *hir) {
        let can_continue_to_ownership = match err.gravity() {
            HirErrorGravity::CanGoUpTo(pass) => (pass as u8) >= (HirPass::OwnershipPass as u8),
            HirErrorGravity::CanFinishCurrentPassButNotContinue => false,
            HirErrorGravity::Critical => false,
        };

        match err {
            HirError::TypeCheckFailed(aggregate) => {
                semantic_errors.extend(aggregate.errors);
            }
            other => semantic_errors.push(other),
        }

        if !can_continue_to_ownership {
            return Err(
                HirError::SemanticAnalysisFailed(SemanticAnalysisFailedError {
                    error_count: semantic_errors.len(),
                    errors: semantic_errors,
                })
                .into(),
            );
        }
    }

    //ownership analysis + RAII delete insertion (run even after recoverable type-check errors)
    let mut ownership_pass = HirOwnershipPass::new(&hir_arena, &hir.signature);
    if let Err(err) = ownership_pass.run(&mut *hir) {
        match err {
            HirError::OwnershipAnalysisFailed(aggregate) => {
                semantic_errors.extend(aggregate.errors);
            }
            other => semantic_errors.push(other),
        }
    }

    if !semantic_errors.is_empty() {
        return Err(
            HirError::SemanticAnalysisFailed(SemanticAnalysisFailedError {
                error_count: semantic_errors.len(),
                errors: semantic_errors,
            })
            .into(),
        );
    }

    //Dead code elimination (only in release mode)
    let mut dce_pass = DeadCodeEliminationPass::new(&hir_arena);
    hir = dce_pass.eliminate_dead_code(hir)?;

    // Write HIR output
    let mut hir_printer = HirPrettyPrinter::new();
    let hir_output = hir_printer.print_module(hir, "Dead Code Elimination Pass");
    let mut file_hir = std::fs::File::create("./build/output.atlas").unwrap();
    file_hir.write_all(hir_output.as_bytes()).unwrap();

    let mut lir_lower = HirLoweringPass::new(hir, &hir_arena);
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
    c_codegen.emit_c(&lir, &atlas_build_config.headers).unwrap();

    let mut c_file = std::fs::File::create("./build/output.atlas_c.c").unwrap();
    c_file.write_all(c_codegen.c_file.as_bytes()).unwrap();
    let mut c_header = std::fs::File::create(format!("./build/{}", HEADER_NAME)).unwrap();
    c_header.write_all(c_codegen.c_header.as_bytes()).unwrap();

    let has_embedded_tinycc = cfg!(all(feature = "embedded-tinycc", not(tinycc_unavailable)));
    let should_preflight = compiler != SupportedCompiler::None
        && !(compiler == SupportedCompiler::TinyCC && has_embedded_tinycc);
    if should_preflight {
        preflight_external_compile(compiler, &atlas_build_config.headers, &merged_c_args)?;
    }

    // TODO: put that in its own function, e.g.: "emit_binary(output_dir, compiler)"
    match compiler {
        SupportedCompiler::TinyCC => {
            #[cfg(all(feature = "embedded-tinycc", not(tinycc_unavailable)))]
            {
                emit_binary(
                    output_dir,
                    &atlas_build_config.include_dirs,
                    &atlas_build_config.library_dirs,
                    &atlas_build_config.libraries,
                )?;
            }
            #[cfg(all(not(feature = "embedded-tinycc"), tinycc_unavailable))]
            {
                // Let's invoke it with `tcc ./build/output.atlas_c.c -o {output_dir}`
                eprintln!(
                    "Embedded TinyCC feature is not enabled, trying to invoke system TCC compiler."
                );
                let mut command = std::process::Command::new("tcc");
                command.arg("./build/output.atlas_c.c");
                command.arg("-o");
                let target = if cfg!(target_os = "windows") {
                    format!("{}/a.exe", output_dir)
                } else {
                    format!("{}/a.out", output_dir)
                };
                command.arg(target);
                command.args(&merged_c_args);
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
            command.args(&merged_c_args);
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
            command.args(&merged_c_args);
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
            command.args(&merged_c_args);
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
            command.args(&merged_c_args);
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

impl FromStr for SupportedCompiler {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        parse_supported_compiler(s).ok_or(())
    }
}
