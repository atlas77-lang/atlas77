#![deny(warnings)]
#![deny(clippy::redundant_clone)]
#![deny(clippy::unwrap_used)]

use atlas_77::{CompilationFlag, build, run};
use clap::{Parser, command};

#[derive(Parser)] // requires `derive` feature
#[command(name = "Atlas77")]
#[command(
    bin_name = "atlas_77",
    author = "atlas77-lang",
    version("v0.6.3 Bastion"),
    about = "Programming language made in Rust, a goofy cousin to C++. \nNB: The language is still in early development and is not stable yet, BEWARE.",
    long_about = "Atlas77 is a programming language made in Rust. It is a statically typed language with a focus on being a goofy cousin to C++ and useful for me (Gipson62) at least. \n\nNB: The language is still in early development and is not stable yet, BEWARE."
)]
enum AtlasRuntimeCLI {
    #[command(
        about = "Compile then run a local package",
        long_about = "Compile then run a local package. The output will be written to the current directory."
    )]
    Run {
        file_path: Option<String>,
        #[arg(short = 'r', long)]
        /// Build then run in release mode
        release: bool,
        #[arg(short = 'd', long)]
        /// Build then run in debug mode
        debug: bool,
        #[arg(long)]
        /// Do not include the standard library
        /// As of now, it just means the Runtime won't load all the extern functions from the standard library
        ///
        /// BEWARE: It is not stable yet, so using this flag may lead to unexpected behavior
        no_std: bool,
    },
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
    },
    #[command(
        arg_required_else_help = true,
        about = "Initialize a new Atlas77 project",
        long_about = "Initialize a new Atlas77 project in the current directory"
    )]
    Init { name: Option<String> },
}

fn main() -> miette::Result<()> {
    match AtlasRuntimeCLI::parse() {
        AtlasRuntimeCLI::Run {
            file_path,
            release,
            debug,
            no_std: no_standard_lib,
        } => {
            if release && debug {
                eprintln!("Cannot run in both release and debug mode");
                std::process::exit(1);
            }
            let path = file_path.unwrap_or_else(|| "src/main.atlas".to_string());
            run(
                path,
                if release {
                    CompilationFlag::Release
                } else {
                    CompilationFlag::Debug
                },
                no_standard_lib,
            )
        }
        AtlasRuntimeCLI::Build {
            file_path,
            release,
            debug,
            no_std: no_standard_lib,
        } => {
            if release && debug {
                eprintln!("Cannot build in both release and debug mode");
                std::process::exit(1);
            }
            let path = file_path.unwrap_or_else(|| "src/main.atlas".to_string());
            build(
                path,
                if release {
                    CompilationFlag::Release
                } else {
                    CompilationFlag::Debug
                },
                no_standard_lib,
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
    }
}
