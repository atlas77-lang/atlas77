use std::{
    env,
    fs,
    path::{Path, PathBuf},
};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let target = env::var("TARGET").expect("TARGET not set");
    let out_dir = PathBuf::from(env::var("OUT_DIR").unwrap());
    let manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());
    
    println!("cargo:rerun-if-changed=vendor/tinycc");
    println!("cargo:rerun-if-env-changed=TARGET");

    // Try to get TinyCC libraries in order of preference:
    // 1. Pre-built binaries shipped with the repo
    // 2. Build from source using cc crate
    
    let lib_dir = match try_get_prebuilt_tinycc(&manifest_dir, &target) {
        Some(dir) => {
            println!("cargo:warning=Using pre-built TinyCC for {}", target);
            dir
        }
        None => {
            println!("cargo:warning=Building TinyCC from source for {}", target);
            build_tinycc_from_source(&manifest_dir, &out_dir, &target)?;
            out_dir
        }
    };

    // Emit link instructions
    println!("cargo:rustc-link-search=native={}", lib_dir.display());
    println!("cargo:rustc-link-lib=static=tcc");
    println!("cargo:rustc-link-lib=static=tcc1");
    
    // Link platform-specific libraries
    if target.contains("windows") {
        // Windows doesn't need pthread/dl/m
    } else {
        // Unix-like systems
        println!("cargo:rustc-link-lib=dl");
        println!("cargo:rustc-link-lib=pthread");
        println!("cargo:rustc-link-lib=m");
    }
    Ok(())
}

/// Try to find pre-built TinyCC binaries for the target platform
fn try_get_prebuilt_tinycc(
    manifest_dir: &Path,
    target: &str,
) -> Option<PathBuf> {
    let prebuilt_dir = manifest_dir.join("tinycc/prebuilt");
    
    // Map Rust target triples to TinyCC platform directories
    let platform_dir = match target {
        "x86_64-pc-windows-msvc" | "x86_64-pc-windows-gnu" => "windows-x64",
        "x86_64-unknown-linux-gnu" | "x86_64-unknown-linux-musl" => "linux-x64",
        "aarch64-unknown-linux-gnu" | "aarch64-unknown-linux-musl" => "linux-arm64",
        "x86_64-apple-darwin" => "macos-x64",
        "aarch64-apple-darwin" => "macos-arm64",
        _ => return None,
    };
    
    let platform_path = prebuilt_dir.join(platform_dir);
    let libtcc = platform_path.join("libtcc.a");
    let libtcc1 = platform_path.join("libtcc1.a");
    
    if libtcc.exists() && libtcc1.exists() {
        Some(platform_path)
    } else {
        None
    }
}

/// Build TinyCC from source using the cc crate
fn build_tinycc_from_source(
    manifest_dir: &Path,
    out_dir: &Path,
    target: &str,
) -> Result<(), Box<dyn std::error::Error>> {
    let tcc_src = manifest_dir.join("vendor/tinycc");
    
    // Determine target configuration
    let extra_defines = match target {
        t if t.contains("x86_64") && t.contains("linux") => {
            vec![("TCC_TARGET_X86_64", "1")]
        }
        t if t.contains("x86_64") && t.contains("windows") => {
            vec![("TCC_TARGET_X86_64", "1"), ("TCC_TARGET_PE", "1")]
        }
        t if t.contains("aarch64") && t.contains("linux") => {
            vec![("TCC_TARGET_ARM64", "1")]
        }
        _ => {
            return Err(format!("Unsupported target for building TinyCC: {}", target).into());
        }
    };
    
    // Build libtcc.a
    let mut build = cc::Build::new();
    build
        .warnings(false)
        .opt_level(2)
        .flag_if_supported("-std=gnu99")
        .flag_if_supported("-fno-strict-aliasing")
        .define("ONE_SOURCE", "1")
        .define("CONFIG_TCC_STATIC", "1")
        .define("TCC_VERSION", "\"0.9.27\"")
        .define("CONFIG_TCCDIR", format!("\"{}\"", tcc_src.join("lib").display()).as_str());
    
    for (key, val) in extra_defines {
        build.define(key, val);
    }
    
    build.include(&tcc_src);
    build.file(tcc_src.join("libtcc.c"));
    build.compile("tcc");
    
    // Build libtcc1.a (runtime)
    build_tinycc_runtime(&tcc_src, out_dir, target)?;
    
    Ok(())
}

/// Build TinyCC runtime library (libtcc1.a)
fn build_tinycc_runtime(
    tcc_src: &Path,
    out_dir: &Path,
    target: &str,
) -> Result<(), Box<dyn std::error::Error>> {
    let lib_dir = tcc_src.join("lib");
    
    // Determine which runtime sources to compile based on target
    let c_sources: Vec<&str> = vec!["libtcc1.c"];
    
    let asm_sources: Vec<&str> = if target.contains("x86_64") {
        vec!["alloca86_64.S", "alloca86_64-bt.S"]
    } else if target.contains("aarch64") || target.contains("arm64") {
        vec![] // ARM uses different runtime
    } else {
        vec![]
    };
    
    // Build all sources together into libtcc1
    let mut build = cc::Build::new();
    build
        .warnings(false)
        .opt_level(2)
        .flag_if_supported("-std=gnu99")
        .flag_if_supported("-fno-strict-aliasing")
        .include(&lib_dir);
    
    // Add C sources
    for source in c_sources {
        let src_path = lib_dir.join(source);
        if src_path.exists() {
            build.file(&src_path);
        }
    }
    
    // Add assembly sources
    for source in asm_sources {
        let src_path = lib_dir.join(source);
        if src_path.exists() {
            build.flag_if_supported("-x");
            build.flag_if_supported("assembler-with-cpp");
            build.file(&src_path);
        }
    }
    
    // Compile everything into libtcc1.a
    build.compile("tcc1");
    
    Ok(())
}