// This crate should hold a HashMap or something similar to store all the functions and types of the standard/core library.
use include_dir::{include_dir, Dir};

pub static STD_LIB_DIR: Dir<'_> = include_dir!("$CARGO_MANIFEST_DIR/stdlib");