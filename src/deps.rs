//! Git-based package dependencies declared in `atlas.toml`'s `[dependencies]` table.
//!
//! Each entry is `name = { git = "...", version = "..." }` or `{ git = "...", commit = "..." }`.
//! Resolved dependencies are cloned into `build/libs/<name>/`, and what was actually
//! resolved is recorded in `atlas-lock.toml` (sibling to `atlas.toml`) so repeat builds
//! can tell "nothing changed" from "the request changed" without re-touching the network.

use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::path::Path;
use std::process::Command;

const LOCKFILE_PATH: &str = "atlas-lock.toml";

#[derive(Debug, Clone)]
pub struct AtlasPackageDependency {
    pub name: String,
    pub git: String,
    pub version: Option<String>,
    pub commit: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct LockEntry {
    git: String,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    version: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    commit: Option<String>,
    resolved_commit: String,
}

impl LockEntry {
    fn matches_request(&self, dependency: &AtlasPackageDependency) -> bool {
        self.git == dependency.git
            && self.version == dependency.version
            && self.commit == dependency.commit
    }
}

type Lockfile = BTreeMap<String, LockEntry>;

fn load_lockfile() -> Lockfile {
    std::fs::read_to_string(LOCKFILE_PATH)
        .ok()
        .and_then(|content| toml::from_str(&content).ok())
        .unwrap_or_default()
}

fn save_lockfile(lockfile: &Lockfile) -> miette::Result<()> {
    let content = toml::to_string_pretty(lockfile)
        .map_err(|err| miette::miette!("Failed to serialize {LOCKFILE_PATH}: {err}"))?;
    std::fs::write(LOCKFILE_PATH, content)
        .map_err(|err| miette::miette!("Failed to write {LOCKFILE_PATH}: {err}"))
}

pub fn parse_dependencies_table(
    table: &toml::value::Table,
) -> miette::Result<Vec<AtlasPackageDependency>> {
    let mut dependencies = Vec::new();

    for (name, value) in table {
        let Some(entry) = value.as_table() else {
            continue;
        };
        let Some(git) = entry.get("git").and_then(|v| v.as_str()) else {
            continue;
        };

        let version = entry
            .get("version")
            .and_then(|v| v.as_str())
            .map(str::to_owned);
        let commit = entry
            .get("commit")
            .and_then(|v| v.as_str())
            .map(str::to_owned);

        if version.is_some() && commit.is_some() {
            return Err(miette::miette!(
                "Dependency '{name}' specifies both 'version' and 'commit'. Pick exactly one"
            ));
        }

        dependencies.push(AtlasPackageDependency {
            name: name.clone(),
            git: git.to_owned(),
            version,
            commit,
        });
    }

    Ok(dependencies)
}

/// Fetches a single dependency into `build/libs/<name>/` (relative to the current
/// working directory, matching the existing `./build` convention), consulting and
/// updating `atlas-lock.toml` (see the module docs above for the skip/reclone
/// policy). Returns the directory it was fetched into, so the caller can look for
/// that dependency's own `atlas.toml` (recursive dependencies, its own `[c]`/
/// `[link]` config) without having to reconstruct the path itself.
pub fn fetch_dependency(dependency: &AtlasPackageDependency) -> miette::Result<std::path::PathBuf> {
    let libs_dir = Path::new("build/libs");
    std::fs::create_dir_all(libs_dir)
        .map_err(|err| miette::miette!("Failed to create {}: {}", libs_dir.display(), err))?;

    let target = libs_dir.join(&dependency.name);

    let mut lockfile = load_lockfile();
    let locked = lockfile.get(&dependency.name).cloned();
    let request_matches_lock = locked
        .as_ref()
        .is_some_and(|entry| entry.matches_request(dependency));

    if target.is_dir() && request_matches_lock {
        // Nothing about this dependency's request has changed since it was last
        // successfully fetched. Skip it entirely, no network access at all.
        return Ok(target);
    }

    if target.exists() {
        // Delete-then-reclone rather than trying to reconcile an existing clone in
        // place: a changed `git` URL, a moved tag, or a manually-edited checkout
        // all need different in-place fixes, and this handles every one of them
        // the same simple way.
        std::fs::remove_dir_all(&target).map_err(|err| {
            miette::miette!(
                "Failed to remove stale dependency directory {}: {err}",
                target.display()
            )
        })?;
    }

    let pin_to = if request_matches_lock {
        locked.as_ref().map(|entry| entry.resolved_commit.as_str())
    } else {
        None
    };

    let resolved_commit = fetch_one(dependency, &target, pin_to)?;

    lockfile.insert(
        dependency.name.clone(),
        LockEntry {
            git: dependency.git.clone(),
            version: dependency.version.clone(),
            commit: dependency.commit.clone(),
            resolved_commit,
        },
    );
    save_lockfile(&lockfile)?;

    Ok(target)
}

fn fetch_one(
    dependency: &AtlasPackageDependency,
    target: &Path,
    pin_to: Option<&str>,
) -> miette::Result<String> {
    println!(
        "Fetching dependency '{}' from {}...",
        dependency.name, dependency.git
    );

    let clone_status = Command::new("git")
        .arg("clone")
        .arg("--quiet")
        .arg(&dependency.git)
        .arg(target)
        .status()
        .map_err(|err| {
            miette::miette!(
                "Failed to run 'git clone' for dependency '{}': {err} (is git installed and on PATH?)",
                dependency.name
            )
        })?;

    if !clone_status.success() {
        return Err(miette::miette!(
            "'git clone {}' failed for dependency '{}'",
            dependency.git,
            dependency.name
        ));
    }

    let reference: Option<String> = if let Some(commit) = pin_to {
        Some(commit.to_owned())
    } else if let Some(commit) = &dependency.commit {
        Some(commit.clone())
    } else if let Some(version) = &dependency.version {
        Some(resolve_version_ref(target, version, &dependency.name)?)
    } else {
        None // no version/commit requested. Stay on the clone's default branch
    };

    if let Some(reference) = reference {
        checkout(target, &reference, &dependency.name)?;
    }

    current_commit(target, &dependency.name)
}

/// Tries tag `v{version}` then tag `{version}`, in that order. No semver-range
/// resolution, an exact tag match only.
fn resolve_version_ref(repo: &Path, version: &str, name: &str) -> miette::Result<String> {
    for candidate in [format!("v{version}"), version.to_owned()] {
        let status = Command::new("git")
            .arg("-C")
            .arg(repo)
            .arg("rev-parse")
            .arg("--verify")
            .arg("--quiet")
            .arg(format!("refs/tags/{candidate}"))
            .status()
            .map_err(|err| {
                miette::miette!("Failed to run 'git rev-parse' for dependency '{name}': {err}")
            })?;
        if status.success() {
            return Ok(candidate);
        }
    }

    Err(miette::miette!(
        "Dependency '{name}' requested version '{version}', but no tag 'v{version}' or '{version}' exists in {}",
        repo.display()
    ))
}

fn checkout(repo: &Path, reference: &str, name: &str) -> miette::Result<()> {
    let status = Command::new("git")
        .arg("-C")
        .arg(repo)
        .arg("checkout")
        .arg("--quiet")
        .arg(reference)
        .status()
        .map_err(|err| {
            miette::miette!("Failed to run 'git checkout' for dependency '{name}': {err}")
        })?;

    if !status.success() {
        return Err(miette::miette!(
            "'git checkout {reference}' failed for dependency '{name}'"
        ));
    }

    Ok(())
}

fn current_commit(repo: &Path, name: &str) -> miette::Result<String> {
    let output = Command::new("git")
        .arg("-C")
        .arg(repo)
        .arg("rev-parse")
        .arg("HEAD")
        .output()
        .map_err(|err| {
            miette::miette!("Failed to run 'git rev-parse HEAD' for dependency '{name}': {err}")
        })?;

    if !output.status.success() {
        return Err(miette::miette!(
            "'git rev-parse HEAD' failed for dependency '{name}'"
        ));
    }

    Ok(String::from_utf8_lossy(&output.stdout).trim().to_owned())
}
