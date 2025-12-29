use crate::atlas_lib::STD_LIB_DIR;
use miette::SourceSpan;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    pub start: usize,
    pub end: usize,
    pub path: &'static str,
}

impl Default for Span {
    fn default() -> Self {
        Self {
            start: 0,
            end: 0,
            path: "<stdin>",
        }
    }
}

impl From<Span> for SourceSpan {
    fn from(span: Span) -> Self {
        SourceSpan::new(span.start.into(), span.end - span.start)
    }
}

/// Reads the content of a file given its path. If the path starts with "std/", it
/// attempts to read the file from the embedded standard library directory.
/// Otherwise, it reads the file from the filesystem.
///
/// TODO: At one point the standard library will have subdirectories, so this function
/// will need to be updated to handle that.
pub fn get_file_content(path: &str) -> Result<String, std::io::Error> {
    let path = if path.ends_with(".atlas") {
        path.to_string()
    } else {
        format!("{}.atlas", path)
    };
    if path.starts_with("std/") {
        let file_name = path.trim_start_matches("std/");
        return match STD_LIB_DIR.get_file(file_name) {
            Some(file) => match file.contents_utf8() {
                Some(content) => Ok(content.to_string()),
                None => Err(std::io::Error::new(
                    std::io::ErrorKind::InvalidData,
                    format!("Standard library file '{}' is not valid UTF-8", file_name),
                )),
            },
            None => Err(std::io::Error::new(
                std::io::ErrorKind::NotFound,
                format!("Standard library file '{}' not found", file_name),
            )),
        };
    }
    std::fs::read_to_string(path)
}

/// Yeah, we shouldn't be doing this but oh well
/// But I guess it's okay since I only leak strings for file paths which are few and far between
/// Later, I'll try to implement that with some kind of map, so we only have one static str per file path
pub fn string_to_static_str(s: String) -> &'static str {
    Box::leak(s.into_boxed_str())
}
