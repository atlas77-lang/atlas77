use crate::atlas_lib::{CORE_LIB_DIR, STD_LIB_DIR};
use miette::{LabeledSpan, SourceSpan};
use serde::Serialize;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize)]
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

impl From<LabeledSpan> for Span {
    fn from(val: LabeledSpan) -> Self {
        Span {
            start: val.offset(),
            end: val.offset() + val.len(),
            // Sadly we don't know the file...
            path: "<stdin>",
        }
    }
}

pub fn resolve_import_path(path: &str) -> String {
    let normalized = if path.ends_with(".atlas") {
        path.to_string()
    } else {
        format!("{}.atlas", path)
    };

    if normalized.starts_with("std/") || normalized.starts_with("core/") {
        return normalized;
    }

    if let Some(first_segment) = normalized.split('/').next()
        && std::path::Path::new("build/libs")
            .join(first_segment)
            .is_dir()
    {
        return normalized;
    }

    let direct = std::path::Path::new(&normalized);
    let candidate = if direct.exists() {
        direct.to_path_buf()
    } else {
        std::path::Path::new(&format!("src/{}", normalized)).to_path_buf()
    };

    let canonical = std::fs::canonicalize(&candidate)
        .map(|p| p.to_string_lossy().replace('\\', "/"))
        .unwrap_or_else(|_| normalized.replace('\\', "/"));

    logical_lib_path(&canonical, "libraries/std", "std")
        .or_else(|| logical_lib_path(&canonical, "libraries/core", "core"))
        .unwrap_or(canonical)
}

fn logical_lib_path(canonical: &str, lib_dir: &str, prefix: &str) -> Option<String> {
    let marker = format!("/{}/", lib_dir);
    let idx = canonical.find(&marker)?;
    let rest = &canonical[idx + marker.len()..];
    let rest = rest.strip_suffix(".atlas").unwrap_or(rest);
    Some(format!("{}/{}", prefix, rest))
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
    if path.starts_with("core/") {
        let file_name = path.trim_start_matches("core/");
        return match CORE_LIB_DIR.get_file(file_name) {
            Some(file) => match file.contents_utf8() {
                Some(content) => Ok(content.to_string()),
                None => Err(std::io::Error::new(
                    std::io::ErrorKind::InvalidData,
                    format!("Core library file '{}' is not valid UTF-8", file_name),
                )),
            },
            None => Err(std::io::Error::new(
                std::io::ErrorKind::NotFound,
                format!("Core library file '{}' not found", file_name),
            )),
        };
    }
    if let Some(slash_pos) = path.find('/') {
        let first_segment = &path[..slash_pos];
        let rest = &path[slash_pos + 1..];
        let dependency_root = std::path::Path::new("build/libs").join(first_segment);
        if dependency_root.is_dir() {
            if let Ok(content) = std::fs::read_to_string(dependency_root.join(rest)) {
                return Ok(content);
            }
            if let Ok(content) = std::fs::read_to_string(dependency_root.join("src").join(rest)) {
                return Ok(content);
            }
        }
    } else if let Some(dependency_name) = path.strip_suffix(".atlas") {
        let dependency_root = std::path::Path::new("build/libs").join(dependency_name);
        if dependency_root.is_dir()
            && let Ok(content) = std::fs::read_to_string(dependency_root.join("src/lib.atlas"))
        {
            return Ok(content);
        }
    }

    match std::fs::read_to_string(&path) {
        Ok(s) => Ok(s),
        Err(err) => {
            // Try workspace-style `src/` fallback when the direct path didn't resolve.
            let fallback = format!("src/{}", &path);
            match std::fs::read_to_string(&fallback) {
                Ok(s) => Ok(s),
                Err(_) => Err(err),
            }
        }
    }
}

pub fn missing_dependency_file(path: &str) -> Option<(String, String)> {
    let normalized = if path.ends_with(".atlas") {
        path.to_string()
    } else {
        format!("{}.atlas", path)
    };

    if normalized.starts_with("std/") || normalized.starts_with("core/") {
        return None;
    }

    if let Some(slash_pos) = normalized.find('/') {
        let first_segment = &normalized[..slash_pos];
        let rest = &normalized[slash_pos + 1..];
        if std::path::Path::new("build/libs")
            .join(first_segment)
            .is_dir()
        {
            return Some((first_segment.to_string(), rest.to_string()));
        }
    } else if let Some(dependency_name) = normalized.strip_suffix(".atlas")
        && std::path::Path::new("build/libs")
            .join(dependency_name)
            .is_dir()
    {
        return Some((dependency_name.to_string(), "src/lib.atlas".to_string()));
    }

    None
}

/// Yeah, we shouldn't be doing this but oh well
/// I guess it's okay since I only leak strings for file paths which are few and far between
/// Later, I'll try to implement that with some kind of map, so we only have one static str per file path
pub fn string_to_static_str(s: String) -> &'static str {
    Box::leak(s.into_boxed_str())
}
