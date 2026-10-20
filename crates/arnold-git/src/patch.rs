//! Changes as unified-diff text, for handing to an AI model as context.

use std::path::Path;

use serde::Serialize;

use crate::cli::run;
use crate::Result;

/// Which two states of the files to diff.
#[derive(Debug, Clone, Copy)]
pub enum PatchSource<'a> {
    /// `from` → `to`, both commits.
    Commits { from: &'a str, to: &'a str },
    /// `from` → the files on disk in the checkout the patch is made in, untracked ones included.
    OnDisk { from: &'a str },
    /// The index → the files on disk (unstaged changes), untracked ones included.
    Unstaged,
    /// `HEAD` → the index (staged changes).
    Staged,
}

/// A file that is not in the patch text, and why.
#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
pub struct Omitted {
    pub path: String,
    pub reason: OmitReason,
}

#[derive(Debug, Clone, Copy, Serialize, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
pub enum OmitReason {
    Binary,
    /// A generated lock file (package-lock.json, Cargo.lock, …), left out unless asked for by name.
    LockFile,
    /// Did not fit in the size limit.
    TooLarge,
}

#[derive(Debug, Clone, Serialize, Default)]
#[serde(rename_all = "camelCase")]
pub struct Patch {
    pub text: String,
    /// Files whose changes are in `text`.
    pub files: Vec<String>,
    pub omitted: Vec<Omitted>,
}

const LOCK_FILES: &[&str] = &[
    "package-lock.json",
    "npm-shrinkwrap.json",
    "yarn.lock",
    "pnpm-lock.yaml",
    "bun.lockb",
    "Cargo.lock",
    "packages.lock.json",
    "paket.lock",
    "poetry.lock",
    "Pipfile.lock",
    "uv.lock",
    "Gemfile.lock",
    "composer.lock",
    "go.sum",
    "flake.lock",
];

/// The changes of `source` in `cwd` as a unified diff (5 lines of context),
/// limited to `paths` when any are given, and to about `limit` bytes: files
/// that do not fit are listed in `omitted` instead, as are binary files and
/// lock files (unless named in `paths`).
pub async fn patch(cwd: &Path, source: PatchSource<'_>, paths: &[String], limit: usize) -> Result<Patch> {
    let mut args = vec!["diff", "--no-color", "--no-ext-diff", "--no-textconv", "-U5", "-M"];
    match source {
        PatchSource::Commits { from, to } => args.extend([from, to]),
        PatchSource::OnDisk { from } => args.push(from),
        PatchSource::Unstaged => {}
        PatchSource::Staged => args.push("--cached"),
    }
    args.push("--");
    args.extend(paths.iter().map(String::as_str));
    let out = run(cwd, &args).await?;
    let mut chunks = split_files(&String::from_utf8_lossy(&out));

    if matches!(source, PatchSource::OnDisk { .. } | PatchSource::Unstaged) {
        for file in crate::working::untracked_files(cwd).await? {
            if !paths.is_empty() && !paths.iter().any(|path| covers(path, &file.path)) {
                continue;
            }
            let chunk = match tokio::fs::read(cwd.join(&file.path)).await {
                Ok(bytes) => match String::from_utf8(bytes) {
                    Ok(text) if !text.contains('\0') => new_file(&file.path, &text),
                    _ => binary_file(&file.path),
                },
                Err(_) => continue,
            };
            chunks.push(chunk);
        }
        chunks.sort_by(|a, b| a.path.cmp(&b.path));
    }
    Ok(assemble(chunks, paths, limit))
}

/// `path` names `file` or a folder holding it.
fn covers(path: &str, file: &str) -> bool {
    let path = path.trim_end_matches('/');
    file == path || file.strip_prefix(path).is_some_and(|rest| rest.starts_with('/'))
}

#[derive(Debug, Clone, PartialEq)]
struct FileChunk {
    path: String,
    text: String,
    binary: bool,
}

/// Splits `git diff` output into one chunk per file.
fn split_files(diff: &str) -> Vec<FileChunk> {
    let mut chunks: Vec<FileChunk> = Vec::new();
    for line in diff.split_inclusive('\n') {
        if line.starts_with("diff --git ") || chunks.is_empty() {
            chunks.push(FileChunk {
                path: String::new(),
                text: String::new(),
                binary: false,
            });
        }
        let chunk = chunks.last_mut().expect("pushed above");
        chunk.text.push_str(line);
    }
    for chunk in &mut chunks {
        chunk.path = chunk_path(&chunk.text);
        chunk.binary = chunk
            .text
            .lines()
            .take_while(|line| !line.starts_with("@@"))
            .any(|line| line.starts_with("Binary files ") || line == "GIT binary patch");
    }
    chunks.retain(|chunk| !chunk.path.is_empty());
    chunks
}

/// The file a chunk is about: the new path, or the old one for a deleted file.
fn chunk_path(text: &str) -> String {
    let mut old = None;
    for line in text.lines() {
        if line.starts_with("@@") {
            break;
        }
        if let Some(path) = line.strip_prefix("+++ b/") {
            return path.to_string();
        }
        if let Some(path) = line.strip_prefix("--- a/") {
            old = Some(path.to_string());
        }
        if let Some(path) = line.strip_prefix("rename to ") {
            return path.to_string();
        }
    }
    if let Some(old) = old {
        return old;
    }
    // Binary or mode-only changes have no ---/+++ lines: `diff --git a/x b/x`.
    let header = text.lines().next().unwrap_or("");
    header
        .strip_prefix("diff --git a/")
        .and_then(|rest| rest.rsplit_once(" b/"))
        .map(|(_, new)| new.to_string())
        .unwrap_or_default()
}

fn new_file(path: &str, text: &str) -> FileChunk {
    let lines: Vec<&str> = text.split_inclusive('\n').collect();
    let mut out = format!("diff --git a/{path} b/{path}\nnew file mode 100644\n--- /dev/null\n+++ b/{path}\n");
    if !lines.is_empty() {
        out.push_str(&format!("@@ -0,0 +1,{} @@\n", lines.len()));
        for line in &lines {
            out.push('+');
            out.push_str(line);
        }
        if !text.ends_with('\n') {
            out.push_str("\n\\ No newline at end of file\n");
        }
    }
    FileChunk {
        path: path.to_string(),
        text: out,
        binary: false,
    }
}

fn binary_file(path: &str) -> FileChunk {
    FileChunk {
        path: path.to_string(),
        text: String::new(),
        binary: true,
    }
}

fn is_lock_file(path: &str) -> bool {
    let name = path.rsplit('/').next().unwrap_or(path);
    LOCK_FILES.contains(&name)
}

fn assemble(chunks: Vec<FileChunk>, paths: &[String], limit: usize) -> Patch {
    let mut patch = Patch::default();
    let named = |path: &str| paths.iter().any(|named| named == path);
    let mut too_large = Vec::new();
    for chunk in chunks {
        let reason = if chunk.binary {
            Some(OmitReason::Binary)
        } else if is_lock_file(&chunk.path) && !named(&chunk.path) {
            Some(OmitReason::LockFile)
        } else {
            None
        };
        if let Some(reason) = reason {
            patch.omitted.push(Omitted { path: chunk.path, reason });
        } else if patch.text.len() + chunk.text.len() > limit {
            too_large.push(chunk);
        } else {
            patch.text.push_str(&chunk.text);
            patch.files.push(chunk.path);
        }
    }
    // Smaller files that still fit are better than none: fill the remaining room.
    too_large.sort_by_key(|chunk| chunk.text.len());
    for chunk in too_large {
        if patch.text.len() + chunk.text.len() <= limit {
            patch.text.push_str(&chunk.text);
            patch.files.push(chunk.path);
        } else {
            patch.omitted.push(Omitted {
                path: chunk.path,
                reason: OmitReason::TooLarge,
            });
        }
    }
    patch
}

#[cfg(test)]
mod tests {
    use super::*;

    const DIFF: &str = "diff --git a/src/a.rs b/src/a.rs
index 1..2 100644
--- a/src/a.rs
+++ b/src/a.rs
@@ -1 +1 @@
-old
+new
diff --git a/gone.txt b/gone.txt
deleted file mode 100644
--- a/gone.txt
+++ /dev/null
@@ -1 +0,0 @@
-bye
diff --git a/logo.png b/logo.png
index 1..2 100644
Binary files a/logo.png and b/logo.png differ
diff --git a/old name.txt b/new name.txt
similarity index 100%
rename from old name.txt
rename to new name.txt
diff --git a/web/package-lock.json b/web/package-lock.json
--- a/web/package-lock.json
+++ b/web/package-lock.json
@@ -1 +1 @@
-1
+2
";

    #[test]
    fn splits_a_diff_per_file() {
        let chunks = split_files(DIFF);
        let summary: Vec<(&str, bool)> = chunks.iter().map(|c| (c.path.as_str(), c.binary)).collect();
        assert_eq!(
            summary,
            [
                ("src/a.rs", false),
                ("gone.txt", false),
                ("logo.png", true),
                ("new name.txt", false),
                ("web/package-lock.json", false),
            ]
        );
        assert!(chunks[0].text.starts_with("diff --git a/src/a.rs"));
        assert!(chunks[0].text.ends_with("+new\n"));
    }

    #[test]
    fn leaves_out_binary_and_lock_files() {
        let patch = assemble(split_files(DIFF), &[], 10_000);
        assert_eq!(patch.files, ["src/a.rs", "gone.txt", "new name.txt"]);
        assert_eq!(
            patch.omitted,
            [
                Omitted { path: "logo.png".into(), reason: OmitReason::Binary },
                Omitted { path: "web/package-lock.json".into(), reason: OmitReason::LockFile },
            ]
        );
        assert!(!patch.text.contains("package-lock"));
    }

    #[test]
    fn keeps_a_lock_file_asked_for_by_name() {
        let patch = assemble(split_files(DIFF), &["web/package-lock.json".to_string()], 10_000);
        assert!(patch.files.contains(&"web/package-lock.json".to_string()));
    }

    #[test]
    fn fills_the_limit_with_the_files_that_fit() {
        let big = FileChunk { path: "big".into(), text: "x".repeat(80), binary: false };
        let small = FileChunk { path: "small".into(), text: "y".repeat(30), binary: false };
        let tiny = FileChunk { path: "tiny".into(), text: "z".repeat(10), binary: false };
        let patch = assemble(vec![small.clone(), big, tiny], &[], 50);
        assert_eq!(patch.files, ["small", "tiny"]);
        assert_eq!(patch.omitted, [Omitted { path: "big".into(), reason: OmitReason::TooLarge }]);
    }

    #[test]
    fn writes_untracked_files_as_added() {
        let chunk = new_file("notes.md", "one\ntwo");
        assert_eq!(
            chunk.text,
            "diff --git a/notes.md b/notes.md\nnew file mode 100644\n--- /dev/null\n+++ b/notes.md\n@@ -0,0 +1,2 @@\n+one\n+two\n\\ No newline at end of file\n"
        );
    }

    #[test]
    fn folder_paths_cover_their_files() {
        assert!(covers("src", "src/a.rs"));
        assert!(covers("src/", "src/a.rs"));
        assert!(covers("src/a.rs", "src/a.rs"));
        assert!(!covers("src", "srcs/a.rs"));
    }
}
