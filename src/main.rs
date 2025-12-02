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
    version("v0.6.0-dev Bastion"),
    about = "Programming language made in Rust",
    long_about = "Atlas77 is a programming language made in Rust. It is a statically typed language with a focus on being a goofy cousin to C++ and useful for me (Gipson62) at least."
)]
enum AtlasRuntimeCLI {
    #[command(
        about = "Compile then run a local package",
        long_about = "Compile then run a local package. The output will be written to the current directory."
    )]
    Run {
        file_path: Option<String>,
        #[arg(short = 'r', long)]
        release: bool,
        #[arg(short = 'd', long)]
        debug: bool,
    },
    #[command(
        about = "Compile a local package and all of its dependencies",
        long_about = "Compile a local package and all of its dependencies. The output will be written to the current directory as `output.atlas_c`. NB: That output file is not executable."
    )]
    Build {
        file_path: Option<String>,
        #[arg(short = 'r', long)]
        release: bool,
        #[arg(short = 'd', long)]
        debug: bool,
    },
    #[command(
        arg_required_else_help = true,
        about = "Initialize a new Atlas77 project",
        long_about = "Initialize a new Atlas77 project in the current directory"
    )]
    Init {
        //Always required
        name: Option<String>,
    },
}

fn main() -> miette::Result<()> {
    match AtlasRuntimeCLI::parse() {
        AtlasRuntimeCLI::Run {
            file_path,
            release,
            debug,
        } => {
            if release && debug {
                eprintln!("Cannot run in both release and debug mode");
                std::process::exit(1);
            }
            match file_path {
                Some(file_path) => run(
                    file_path,
                    if release {
                        CompilationFlag::Release
                    } else {
                        CompilationFlag::Debug
                    },
                ),
                None => {
                    //We need to find the src/main.atlas or return an error
                    run(
                        "src/main.atlas".to_string(),
                        if release {
                            CompilationFlag::Release
                        } else {
                            CompilationFlag::Debug
                        },
                    )
                }
            }
        }
        AtlasRuntimeCLI::Build {
            file_path,
            release,
            debug,
        } => {
            if release && debug {
                eprintln!("Cannot build in both release and debug mode");
                std::process::exit(1);
            }
            match file_path {
                Some(file_path) => build(
                    file_path,
                    if release {
                        CompilationFlag::Release
                    } else {
                        CompilationFlag::Debug
                    },
                ),
                None => {
                    //We need to find the src/main.atlas or return an error
                    build(
                        "src/main.atlas".to_string(),
                        if release {
                            CompilationFlag::Release
                        } else {
                            CompilationFlag::Debug
                        },
                    )
                }
            }
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
