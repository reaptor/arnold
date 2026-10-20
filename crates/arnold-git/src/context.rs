//! Whole files for handing to an AI model next to a patch: the new version of
//! the changed files, and unchanged files from the same folders, which show
//! the conventions the repository follows.

use std::collections::HashSet;
use std::path::Path;

use serde::Serialize;

use crate::patch::PatchSource;
use crate::Result;

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
pub struct ContextFile {
    pub path: String,
    pub text: String,
    /// One of the changed files (its new version); else an unchanged neighbour.
    pub changed: bool,
}

/// How much text `context_files` gathers, in bytes.
#[derive(Debug, Clone, Copy)]
pub struct ContextLimits {
    /// A file larger than this is left out.
    pub file: usize,
    /// All changed files together.
    pub changed: usize,
    /// All neighbours together.
    pub neighbours: usize,
    /// Neighbours per folder of a changed file.
    pub per_folder: usize,
}

impl Default for ContextLimits {
    fn default() -> Self {
        ContextLimits {
            file: 40_000,
            changed: 80_000,
            neighbours: 32_000,
            per_folder: 2,
        }
    }
}

/// The new version of `changed` (as `source` has them, in `cwd`), then
/// unchanged files from their folders with the same extension, within `limits`.
/// Deleted, binary and too large files are left out.
pub async fn context_files(cwd: &Path, source: PatchSource<'_>, changed: &[String], limits: ContextLimits) -> Result<Vec<ContextFile>> {
    let mut files = Vec::new();
    let mut used = 0;
    for path in changed {
        let Some(text) = read(cwd, source, path).await? else { continue };
        if text.len() > limits.file || used + text.len() > limits.changed {
            continue;
        }
        used += text.len();
        files.push(ContextFile { path: path.clone(), text, changed: true });
    }

    let all = match source {
        PatchSource::Commits { to, .. } => crate::files::list_files(cwd, to).await?,
        _ => crate::working::worktree_files(cwd).await?,
    };
    let mut used = 0;
    for path in neighbours(changed, &all, limits.per_folder) {
        let Some(text) = read(cwd, source, &path).await? else { continue };
        if text.len() > limits.file / 2 || used + text.len() > limits.neighbours {
            continue;
        }
        used += text.len();
        files.push(ContextFile { path, text, changed: false });
    }
    Ok(files)
}

/// `path` as `source` has it on its new side, if it is text.
async fn read(cwd: &Path, source: PatchSource<'_>, path: &str) -> Result<Option<String>> {
    let bytes = match source {
        PatchSource::Commits { to, .. } => crate::compare::read_blob(cwd, to, path).await?,
        PatchSource::Staged => crate::working::read_index(cwd, path).await?,
        PatchSource::OnDisk { .. } | PatchSource::Unstaged => crate::working::read_worktree(cwd, path).await?,
    };
    Ok(bytes.and_then(|bytes| String::from_utf8(bytes).ok()).filter(|text| !text.contains('\0')))
}

/// Unchanged files of `all` beside the changed ones: in the same folder, with
/// the same extension, at most `per_folder` per folder and extension, in the
/// order of `changed`.
fn neighbours(changed: &[String], all: &[String], per_folder: usize) -> Vec<String> {
    let changed_set: HashSet<&str> = changed.iter().map(String::as_str).collect();
    let mut seen = HashSet::new();
    let mut out = Vec::new();
    for path in changed {
        let (folder, extension) = (folder_of(path), extension_of(path));
        if extension.is_empty() || !seen.insert((folder, extension)) {
            continue;
        }
        let picked = all
            .iter()
            .filter(|other| {
                folder_of(other) == folder
                    && extension_of(other) == extension
                    && !changed_set.contains(other.as_str())
                    && !crate::patch::is_lock_file(other)
            })
            .take(per_folder);
        out.extend(picked.cloned());
    }
    out
}

fn folder_of(path: &str) -> &str {
    path.rfind('/').map_or("", |at| &path[..at])
}

fn extension_of(path: &str) -> &str {
    let name = &path[path.rfind('/').map_or(0, |at| at + 1)..];
    name.rfind('.').filter(|&at| at > 0).map_or("", |at| &name[at + 1..])
}

#[cfg(test)]
mod tests {
    use super::*;

    fn strings(paths: &[&str]) -> Vec<String> {
        paths.iter().map(|path| path.to_string()).collect()
    }

    #[test]
    fn picks_unchanged_files_of_the_same_folder_and_extension() {
        let changed = strings(&["src/Orders.fs", "src/Api/Routes.fs", "README.md"]);
        let all = strings(&[
            "README.md",
            "docs/guide.md",
            "src/Api/Handlers.fs",
            "src/Api/Routes.fs",
            "src/Customers.fs",
            "src/Invoices.fs",
            "src/Orders.fs",
            "src/Products.fs",
            "src/app.json",
            "src/Nested/Deep.fs",
        ]);
        assert_eq!(
            neighbours(&changed, &all, 2),
            strings(&["src/Customers.fs", "src/Invoices.fs", "src/Api/Handlers.fs"])
        );
    }

    #[test]
    fn looks_at_each_folder_once_and_skips_files_without_an_extension() {
        let changed = strings(&["src/A.fs", "src/B.fs", "Makefile"]);
        let all = strings(&["Dockerfile", "Makefile", "src/A.fs", "src/B.fs", "src/C.fs"]);
        assert_eq!(neighbours(&changed, &all, 2), strings(&["src/C.fs"]));
    }

    #[test]
    fn leaves_out_lock_files() {
        let changed = strings(&["package.json"]);
        let all = strings(&["package-lock.json", "package.json", "tsconfig.json"]);
        assert_eq!(neighbours(&changed, &all, 2), strings(&["tsconfig.json"]));
    }

    #[test]
    fn tells_extensions_apart_from_dotfiles() {
        assert_eq!(extension_of("src/.gitignore"), "");
        assert_eq!(extension_of("src/a.test.ts"), "ts");
        assert_eq!(folder_of("a.fs"), "");
    }
}
