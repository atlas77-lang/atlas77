use std::{
    env,
    path::{Path, PathBuf},
};

fn main() {
    println!("cargo::rustc-check-cfg=cfg(tinycc_unavailable)");
    // Only build TinyCC if the feature is enabled
    let build_tinycc = env::var("CARGO_FEATURE_EMBEDDED_TINYCC").is_ok();

    if !build_tinycc {
        println!("cargo:warning=TinyCC support disabled - using external C compiler only");
        return;
    }
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
            // if the target is windows, let's just error out for now
            if target.contains("windows") {
                // Tell Rust code that TinyCC is not available
                println!("cargo:rustc-cfg=tinycc_unavailable");
                return;
            }
            match build_tinycc_from_source(&manifest_dir, &out_dir, &target) {
                Ok(_) => (),
                Err(e) => {
                    // Shouldn't we just disable TinyCC support instead of panicking?
                    println!("cargo:warning=Failed to build TinyCC from source: {}", e);
                    println!("cargo:rustc-cfg=tinycc_unavailable");
                    return;
                }
            }
            out_dir
        }
    };

    // Emit link instructions
    println!(
        "cargo:warning=Linking TinyCC libraries from {}",
        lib_dir.display()
    );
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
}

/// Try to find pre-built TinyCC binaries for the target platform
fn try_get_prebuilt_tinycc(manifest_dir: &Path, target: &str) -> Option<PathBuf> {
    let prebuilt_dir = manifest_dir.join("tinycc/prebuilt");

    // Prefer Cargo-provided target config vars for architecture/os detection.
    let target_arch = env::var("CARGO_CFG_TARGET_ARCH").unwrap_or_else(|_| "unknown".into());
    let target_os = env::var("CARGO_CFG_TARGET_OS").unwrap_or_else(|_| "unknown".into());

    // Map architecture + OS to platform dir names used in tinycc/prebuilt
    let platform_dir = match (target_arch.as_str(), target_os.as_str()) {
        ("x86_64", "windows") => "windows-x64",
        ("x86_64", "linux") => "linux-x64",
        ("aarch64", "linux") | ("arm", "linux") => "linux-arm64",
        ("x86_64", "macos") | ("x86_64", "darwin") => "macos-x64",
        ("aarch64", "macos") | ("aarch64", "darwin") => "macos-arm64",
        _ => {
            // fallback: try to parse the triple (best-effort)
            if target.contains("x86_64") && target.contains("windows") {
                "windows-x64"
            } else if target.contains("x86_64") && target.contains("linux") {
                "linux-x64"
            } else if (target.contains("aarch64") || target.contains("arm64"))
                && target.contains("linux")
            {
                "linux-arm64"
            } else if target.contains("x86_64")
                && (target.contains("apple") || target.contains("darwin"))
            {
                "macos-x64"
            } else if (target.contains("aarch64") || target.contains("arm64"))
                && (target.contains("apple") || target.contains("darwin"))
            {
                "macos-arm64"
            } else {
                return None;
            }
        }
    };

    let platform_path = prebuilt_dir.join(platform_dir);

    // Prefer platform-specific naming: MSVC expects `tcc.lib`, GNU/Unix expect `libtcc.a`.
    let target_env = env::var("CARGO_CFG_TARGET_ENV").unwrap_or_else(|_| String::new());

    if target_env == "msvc" {
        let lib = platform_path.join("msvc/tcc.lib");
        let lib1 = platform_path.join("msvc/tcc1.lib");
        if lib.exists() && lib1.exists() {
            return Some(platform_path.join("msvc"));
        }
        // Fallback: if GNU-style static libs are present, still return the dir but warn.
        let lib_a = platform_path.join("gnu/libtcc.a");
        let lib1_a = platform_path.join("gnu/libtcc1.a");
        if lib_a.exists() && lib1_a.exists() {
            println!(
                "cargo:warning=Found GNU-style TinyCC static libs in {} but target env is MSVC; consider building MSVC .lib import/static libs",
                platform_path.display()
            );
            return Some(platform_path.join("gnu"));
        }
    } else {
        // Default: look for Unix/GNU style static libs
        if platform_dir == "windows-x64" {
            let libtcc = platform_path.join("gnu/libtcc.a");
            let libtcc1 = platform_path.join("gnu/libtcc1.a");
            if libtcc.exists() && libtcc1.exists() {
                return Some(platform_path.join("gnu"));
            }
        }
        let libtcc = platform_path.join("libtcc.a");
        let libtcc1 = platform_path.join("libtcc1.a");
        if libtcc.exists() && libtcc1.exists() {
            return Some(platform_path);
        }
        // If on Windows GNU toolchain (mingw) the above should be fine; if a MSVC-style .lib exists, accept it too.
        let lib = platform_path.join("msvc/tcc.lib");
        let lib1 = platform_path.join("msvc/tcc1.lib");
        if lib.exists() && lib1.exists() {
            println!(
                "cargo:warning=Found MSVC-style .lib files in {} but target env is not MSVC; rustc may not be able to use them",
                platform_path.display()
            );
            return Some(platform_path.join("msvc"));
        }
    }

    None
}

/// Build TinyCC from source using the cc crate
fn build_tinycc_from_source(
    manifest_dir: &Path,
    out_dir: &Path,
    target: &str,
) -> Result<(), Box<dyn std::error::Error>> {
    let tcc_src = manifest_dir.join("vendor/tinycc");
    // Determine target configuration using Cargo cfg variables when available
    let target_arch = env::var("CARGO_CFG_TARGET_ARCH").unwrap_or_else(|_| "unknown".into());
    let target_os = env::var("CARGO_CFG_TARGET_OS").unwrap_or_else(|_| "unknown".into());

    let extra_defines = if target_arch == "x86_64" && target_os == "linux" {
        vec![("TCC_TARGET_X86_64", "1")]
    } else if target_arch == "x86_64" && target_os == "windows" {
        vec![("TCC_TARGET_X86_64", "1"), ("TCC_TARGET_PE", "1")]
    } else if (target_arch == "aarch64" || target_arch == "arm") && target_os == "linux" {
        vec![("TCC_TARGET_ARM64", "1")]
    } else if target.contains("x86_64") && target.contains("windows") {
        // best-effort fallback to triple parsing
        vec![("TCC_TARGET_X86_64", "1"), ("TCC_TARGET_PE", "1")]
    } else if (target.contains("aarch64") || target.contains("arm64")) && target.contains("linux") {
        vec![("TCC_TARGET_ARM64", "1")]
    } else {
        return Err(format!(
            "Unsupported target for building TinyCC: {} (arch='{}' os='{}')",
            target, target_arch, target_os
        )
        .into());
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
        .define(
            "CONFIG_TCCDIR",
            format!("\"{}\"", tcc_src.join("lib").display()).as_str(),
        );

    for (key, val) in extra_defines {
        build.define(key, val);
    }

    // Ensure common architecture macros are defined for compilers (MSVC/clang may not define the GCC-style macros)
    let _target_env = env::var("CARGO_CFG_TARGET_ENV").unwrap_or_else(|_| String::new());
    let target_arch = env::var("CARGO_CFG_TARGET_ARCH").unwrap_or_else(|_| String::new());
    if target_arch == "x86_64" {
        build.define("__x86_64__", "1");
    } else if target_arch == "x86" || target_arch == "i686" || target_arch == "i386" {
        build.define("__i386__", "1");
    } else if target_arch == "aarch64" || target_arch == "arm" {
        build.define("__arm__", "1");
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
    _out_dir: &Path,
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

    // Mirror architecture defines for runtime build too
    let target_arch_rt = env::var("CARGO_CFG_TARGET_ARCH").unwrap_or_else(|_| "".into());
    if target_arch_rt == "x86_64" {
        build.define("__x86_64__", "1");
    } else if target_arch_rt == "x86" || target_arch_rt == "i686" || target_arch_rt == "i386" {
        build.define("__i386__", "1");
    } else if target_arch_rt == "aarch64" || target_arch_rt == "arm" {
        build.define("__arm__", "1");
    }

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
