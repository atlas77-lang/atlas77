// TODO: We should remove those allow clippy directives one day
#![deny(warnings)]
#![deny(clippy::unwrap_used)]
#![allow(clippy::result_large_err)]
#![allow(clippy::single_match)]
#![allow(clippy::new_without_default)]
#![allow(clippy::unusual_byte_groupings)]

use atlas_77::{CompilationFlag, SupportedCompiler, build, generate_docs};
use clap::Parser;

#[derive(Parser)] // requires `derive` feature
#[command(name = "Atlas77")]
#[command(
    bin_name = "atlas_77",
    author = "atlas77-lang",
    version("v0.8.0 No Codename yet"),
    about = "Programming language made in Rust, a goofy cousin to C++. \nNB: The language is still in early development and is not stable yet, BEWARE.",
    long_about = "Atlas77 is a programming language made in Rust. It is a statically typed language with a focus on being a goofy cousin to C++ and useful for me (Gipson62) at least. \n\nNB: The language is still in early development and is not stable yet, BEWARE."
)]
enum AtlasRuntimeCLI {
    #[command(
        about = "Compile a local package and all of its dependencies",
        long_about = "Compile a local package and all of its dependencies. The output will be written to the current directory as `output.atlas_asm`. NB: That output file is not executable."
    )]
    Build {
        file_path: Option<String>,
        #[arg(short = 'r', long)]
        /// Build in release mode
        release: bool,
        #[arg(short = 'd', long)]
        /// Build in debug mode
        debug: bool,
        #[arg(long)]
        /// Do not include the standard library
        /// As of now, it just means the Runtime won't load all the extern functions from the standard library
        ///
        /// BEWARE: It is not stable yet, so using this flag may lead to unexpected behavior
        no_std: bool,
        #[arg(short = 'c', long, default_value = "tinycc")]
        /// Specifies which compiler to use. By default, it uses TinyCC if available.
        /// You can set it to "none" to skip the compilation step and only emit C
        compiler: String,
        #[arg(short = 'o', long, default_value = "build")]
        output_dir: String,
    },
    #[command(
        arg_required_else_help = true,
        about = "Initialize a new Atlas77 project",
        long_about = "Initialize a new Atlas77 project in the current directory"
    )]
    Init { name: Option<String> },
    #[command(
        about = "Check a local package for errors without producing output",
        long_about = "Check a local package for errors without producing output. This is similar to 'build' but does not produce any output files."
    )]
    Check {
        file_path: Option<String>,
        #[arg(short = 'r', long)]
        /// Check in release mode
        release: bool,
    },
    //#[cfg(feature = "docs")]
    Docs {
        #[arg(short = 'o', long, default_value = "docs")]
        /// Output directory for the generated documentation
        output: String,
        file_path: Option<String>,
    },
}

fn main() -> miette::Result<()> {
    match AtlasRuntimeCLI::parse() {
        AtlasRuntimeCLI::Build {
            file_path,
            release,
            debug,
            no_std: no_standard_lib,
            compiler,
            output_dir,
        } => {
            if release && debug {
                eprintln!("Cannot build in both release and debug mode");
                std::process::exit(1);
            }
            let path = file_path.unwrap_or("src/main.atlas".to_string());
            build(
                path,
                if release {
                    CompilationFlag::Release
                } else {
                    CompilationFlag::Debug
                },
                no_standard_lib,
                SupportedCompiler::from_str(&compiler.to_lowercase()),
                output_dir,
            )
            .map(|_| ())
        }
        AtlasRuntimeCLI::Init { name } => {
            match name {
                Some(name) => {
                    atlas_77::init(name);
                }
                None => {
                    atlas_77::init("my_awesome_atlas77_project".to_owned());
                }
            }
            Ok(())
        }
        AtlasRuntimeCLI::Check { file_path, release } => {
            let path = file_path.unwrap_or("src/main.atlas".to_string());
            build(
                path,
                if release {
                    CompilationFlag::Release
                } else {
                    CompilationFlag::Debug
                },
                true,
                // We don't care about the compiler here, as we won't compile
                SupportedCompiler::from_str("none"),
                "build".to_string(),
            )
            .map(|_| ())
        }
        AtlasRuntimeCLI::Docs { output, file_path } => {
            generate_docs(output, file_path.as_deref());
            Ok(())
        }
    }
}
