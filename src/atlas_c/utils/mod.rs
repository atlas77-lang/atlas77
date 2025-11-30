use crate::atlas_lib::STD_LIB_DIR;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Span {
    pub start: usize,
    pub end: usize,
    pub path: String,
}

impl Default for Span {
    fn default() -> Self {
        Self {
            start: 0,
            end: 0,
            path: "<stdin>".into(),
        }
    }
}

/// Reads the content of a file given its path. If the path starts with "std/", it
/// attempts to read the file from the embedded standard library directory.
/// Otherwise, it reads the file from the filesystem.
///
/// TODO: At one point the standard library will have subdirectories, so this function
/// will need to be updated to handle that.
///
pub fn get_file_content(path: &str) -> Result<String, std::io::Error> {
    let path = path.to_owned() + ".atlas";
    if path.starts_with("std/") {
        let file_name = path.split("/").last().unwrap();
        match STD_LIB_DIR.get_file(file_name) {
            Some(file) => {
                match file.contents_utf8() {
                    Some(content) => {
                        return Ok(content.to_string())
                    }
                    None => {
                        return Err(std::io::Error::new(
                            std::io::ErrorKind::InvalidData,
                            format!(
                                "Standard library file '{}' is not valid UTF-8",
                                file_name
                            ),
                        ));
                    }
                };
            }
            None => {
                return Err(std::io::Error::new(
                    std::io::ErrorKind::NotFound,
                    format!("Standard library file '{}' not found", file_name),
                ));
            }
        }
    }
    std::fs::read_to_string(path)
}
