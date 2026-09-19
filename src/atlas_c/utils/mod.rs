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

static NO_STD: std::sync::atomic::AtomicBool = std::sync::atomic::AtomicBool::new(false);

pub fn set_no_std(value: bool) {
    NO_STD.store(value, std::sync::atomic::Ordering::Relaxed);
}

fn dependency_dir_usable(name: &str) -> bool {
    if name == "std" && NO_STD.load(std::sync::atomic::Ordering::Relaxed) {
        return false;
    }
    std::path::Path::new("build/libs").join(name).is_dir()
}

fn dependency_root_of_file(importer: &str) -> Option<std::path::PathBuf> {
    let first_segment = importer.split('/').next()?;
    if !dependency_dir_usable(first_segment) {
        return None;
    }
    Some(std::path::Path::new("build/libs").join(first_segment))
}

pub fn resolve_import_path(path: &str, importer: Option<&str>) -> String {
    let normalized = if path.ends_with(".atlas") {
        path.to_string()
    } else {
        format!("{}.atlas", path)
    };

    if let Some(first_segment) = normalized.split('/').next()
        && dependency_dir_usable(first_segment)
    {
        return normalized;
    }

    if let Some(importer) = importer
        && let Some(dependency_root) = dependency_root_of_file(importer)
        && (dependency_root.join(&normalized).is_file()
            || dependency_root.join("src").join(&normalized).is_file())
        && let Some(dependency_name) = dependency_root.file_name().and_then(|n| n.to_str())
    {
        return format!("{dependency_name}/{normalized}");
    }

    let direct = std::path::Path::new(&normalized);
    let candidate = if direct.exists() {
        direct.to_path_buf()
    } else {
        std::path::Path::new(&format!("src/{}", normalized)).to_path_buf()
    };

    std::fs::canonicalize(&candidate)
        .map(|p| p.to_string_lossy().replace('\\', "/"))
        .unwrap_or_else(|_| normalized.replace('\\', "/"))
}

pub fn get_file_content(path: &str) -> Result<String, std::io::Error> {
    get_file_content_impl(path, None)
}

pub fn get_file_content_for_import(
    import_path: &str,
    importer: &str,
) -> Result<String, std::io::Error> {
    get_file_content_impl(import_path, Some(importer))
}

fn get_file_content_impl(path: &str, importer: Option<&str>) -> Result<String, std::io::Error> {
    let path = if path.ends_with(".atlas") {
        path.to_string()
    } else {
        format!("{}.atlas", path)
    };

    if let Some(slash_pos) = path.find('/') {
        let first_segment = &path[..slash_pos];
        let rest = &path[slash_pos + 1..];
        if dependency_dir_usable(first_segment) {
            let dependency_root = std::path::Path::new("build/libs").join(first_segment);
            if let Ok(content) = std::fs::read_to_string(dependency_root.join(rest)) {
                return Ok(content);
            }
            if let Ok(content) = std::fs::read_to_string(dependency_root.join("src").join(rest)) {
                return Ok(content);
            }
        }
    } else if let Some(dependency_name) = path.strip_suffix(".atlas")
        && dependency_dir_usable(dependency_name)
    {
        let dependency_root = std::path::Path::new("build/libs").join(dependency_name);
        if let Ok(content) = std::fs::read_to_string(dependency_root.join("src/lib.atlas")) {
            return Ok(content);
        }
    }

    if let Some(importer) = importer
        && let Some(dependency_root) = dependency_root_of_file(importer)
    {
        if let Ok(content) = std::fs::read_to_string(dependency_root.join(&path)) {
            return Ok(content);
        }
        if let Ok(content) = std::fs::read_to_string(dependency_root.join("src").join(&path)) {
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

pub fn missing_dependency_file(path: &str, importer: Option<&str>) -> Option<(String, String)> {
    let normalized = if path.ends_with(".atlas") {
        path.to_string()
    } else {
        format!("{}.atlas", path)
    };

    if let Some(slash_pos) = normalized.find('/') {
        let first_segment = &normalized[..slash_pos];
        let rest = &normalized[slash_pos + 1..];
        if dependency_dir_usable(first_segment) {
            return Some((first_segment.to_string(), rest.to_string()));
        }
    } else if let Some(dependency_name) = normalized.strip_suffix(".atlas")
        && dependency_dir_usable(dependency_name)
    {
        return Some((dependency_name.to_string(), "src/lib.atlas".to_string()));
    }

    let importer = importer?;
    let dependency_root = dependency_root_of_file(importer)?;
    let dependency_name = dependency_root.file_name()?.to_str()?.to_string();
    Some((dependency_name, normalized))
}

pub fn string_to_static_str(s: String) -> &'static str {
    Box::leak(s.into_boxed_str())
}
