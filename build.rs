use std::{
    env,
    fs,
    path::{Path, PathBuf},
    process::{Command, Stdio},
};

fn main() {
    // 1. Detect Cargo target and host
    let target = env::var("TARGET").expect("TARGET not set");
    let host = env::var("HOST").expect("HOST not set");
    let out_dir = PathBuf::from(env::var("OUT_DIR").unwrap());
    let manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());
    let tcc_dir = manifest_dir.join("vendor/tinycc");
    let build_dir = out_dir.join("tcc-build");

    // 2. Map Cargo target to TCC target triple
    let (tcc_arch, tcc_cpu) = match target.split_once('-').map(|(arch, _)| arch) {
        Some("x86_64") => ("x86_64", None::<&str>),
        Some("i686") | Some("i386") => ("i386", None),
        Some("arm") => ("arm", None),
        Some("aarch64") => ("aarch64", None),
        Some("riscv64") => ("riscv64", None),
        Some("riscv32") => ("riscv32", None),
        Some(arch) => panic!("Unsupported arch for TCC: {}", arch),
        None => panic!("Malformed target triple: {}", target),
    };

    // 3. Only rebuild if TCC sources or config changed
    println!("cargo:rerun-if-changed=vendor/tinycc/");
    println!("cargo:rerun-if-env-changed=TARGET");
    println!("cargo:rerun-if-env-changed=HOST");

    // 4. Prepare build dir
    if !build_dir.exists() {
        fs::create_dir_all(&build_dir).unwrap();
    }

    // 5. Configure TinyCC
    let mut configure = Command::new(tcc_dir.join("configure"));
    configure
        .current_dir(&build_dir)
        .arg(format!("--cc={}", env::var("CC").unwrap_or_else(|_| "cc".to_string())))
        .arg("--disable-shared")
        .arg("--enable-static")
        .arg(format!("--cpu={}", tcc_arch));
    // Add cross flags if needed
    if target != host {
        configure.arg(format!("--cross-prefix={}-", tcc_arch));
    }
    // Add OS-specific flags
    if target.contains("linux") {
        configure.arg("--config-linux");
    } else if target.contains("windows") {
        configure.arg("--config-win32");
    } else if target.contains("darwin") {
        configure.arg("--config-darwin");
    }

    // 6. Run configure if needed
    let config_stamp = build_dir.join("config.stamp");
    if !config_stamp.exists() {
        let status = configure.status().expect("Failed to run TCC configure");
        if !status.success() {
            panic!("TCC configure failed");
        }
        fs::write(&config_stamp, b"configured").unwrap();
    }

    // 7. Build TinyCC
    let make_status = Command::new("make")
        .current_dir(&build_dir)
        .arg("-j")
        .status()
        .expect("Failed to run make");
    if !make_status.success() {
        panic!("TCC make failed");
    }

    // 8. Find libtcc.a
    let libtcc = build_dir.join("libtcc.a");
    if !libtcc.exists() {
        panic!("libtcc.a not found after build");
    }

    // 9. Emit cargo link instructions
    println!("cargo:rustc-link-search=native={}", build_dir.display());
    println!("cargo:rustc-link-lib=static=tcc");
    // Link system libs required by TCC
    println!("cargo:rustc-link-lib=dl");
    println!("cargo:rustc-link-lib=pthread");
    println!("cargo:rustc-link-lib=m");
    println!("cargo:rustc-link-lib=c");

    // 10. Optionally, emit more rerun-if-changed for TCC sources
    for entry in walkdir::WalkDir::new(&tcc_dir) {
        let entry = entry.unwrap();
        if entry.file_type().is_file() {
            println!("cargo:rerun-if-changed={}", entry.path().display());
        }
    }
}