use atlas_77::{build, run, CompilationFlag};
use clap::{command, Parser};

#[derive(Parser)] // requires `derive` feature
#[command(name = "Atlas77")]
#[command(
    bin_name = "atlas_77",
    author = "atlas77-lang",
    version("v0.5.1 Phoenix"),
    about = "Programming language made in Rust",
    long_about = "Atlas77 is a programming language made in Rust. It is a statically typed language with a focus on being cool and useful for me (Gipson62) at least."
)]
enum AtlasRuntimeCLI {
    #[command(
        arg_required_else_help = true,
        about = "Compile then run a local package",
        long_about = "Compile then run a local package. The output will be written to the current directory."
    )]
    Run {
        file_path: String,
        #[arg(short = 'r', long)]
        release: bool,
        #[arg(short = 'd', long)]
        debug: bool,
    },
    #[command(
        arg_required_else_help = true,
        about = "Compile a local package and all of its dependencies",
        long_about = "Compile a local package and all of its dependencies. The output will be written to the current directory as `output.atlas_c`. NB: That output file is not executable."
    )]
    Build {
        file_path: String,
        #[arg(short = 'r', long)]
        release: bool,
        #[arg(short = 'd', long)]
        debug: bool,
    },
}


fn main() -> miette::Result<()> {
    match AtlasRuntimeCLI::parse() {
        AtlasRuntimeCLI::Run { file_path, release, debug } => {
            if release && debug {
                eprintln!("Cannot run in both release and debug mode");
                std::process::exit(1);
            }
            run(file_path, if release { CompilationFlag::Release } else { CompilationFlag::Debug })
        }
        AtlasRuntimeCLI::Build { file_path, release, debug } => {
            if release && debug {
                eprintln!("Cannot run in both release and debug mode");
                std::process::exit(1);
            }
            build(file_path, if release { CompilationFlag::Release } else { CompilationFlag::Debug })
        }
    }
}
