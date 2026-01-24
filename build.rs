use std::{
    env,
    fs,
    path::{Path, PathBuf},
    process::{Command, Stdio},
};
use walkdir::WalkDir;

fn main() {
    // Build `vendor/tinycc` in-place and emit link instructions so the Rust
    // build links statically to the vendored TinyCC libraries.
    let target = env::var("TARGET").expect("TARGET not set");
    let host = env::var("HOST").expect("HOST not set");
    let manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());
    let tcc_dir = manifest_dir.join("vendor/tinycc");

    // Only rebuild when vendor sources or relevant env vars change
    println!("cargo:rerun-if-changed={}", tcc_dir.display());
    println!("cargo:rerun-if-env-changed=TARGET");
    println!("cargo:rerun-if-env-changed=HOST");

    // Run configure in-place if needed
    let config_mak = tcc_dir.join("config.mak");
    if !config_mak.exists() {
        let status = Command::new("./configure")
            .current_dir(&tcc_dir)
            .arg(format!("--cc={}", env::var("CC").unwrap_or_else(|_| "cc".to_string())))
            .arg("--disable-shared")
            .arg("--enable-static")
            .status()
            .expect("Failed to run ./configure for tinycc");
        if !status.success() {
            panic!("tinycc configure failed");
        }
    }

    // Run make if libtcc.a is missing
    let libtcc = tcc_dir.join("libtcc.a");
    if !libtcc.exists() {
        let status = Command::new("make")
            .current_dir(&tcc_dir)
            .arg("-j")
            .status()
            .expect("Failed to run make for tinycc");
        if !status.success() {
            panic!("tinycc make failed");
        }
    }

    // Ensure tinycc runtime is present
    let libtcc1 = tcc_dir.join("libtcc1.a");
    if !libtcc1.exists() {
        panic!("libtcc1.a not found after building tinycc");
    }

    // Emit cargo link instructions pointing at the vendor/tinycc directory.
    println!("cargo:rustc-link-search=native={}", tcc_dir.display());
    println!("cargo:rustc-link-lib=static=tcc");
    println!("cargo:rustc-link-lib=static=tcc1");
    println!("cargo:rustc-link-lib=dl");
    println!("cargo:rustc-link-lib=pthread");
    println!("cargo:rustc-link-lib=m");
    println!("cargo:rustc-link-lib=c");

    // Emit rerun-if-changed for every file under vendor/tinycc
    for entry in WalkDir::new(&tcc_dir) {
        let entry = entry.expect("walkdir entry");
        if entry.file_type().is_file() {
            println!("cargo:rerun-if-changed={}", entry.path().display());
        }
    }
}