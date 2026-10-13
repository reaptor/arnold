//! Reading and writing working-tree files for the editor.
//!
//! The editor always works with `\n` line endings and no byte-order mark. Both are
//! recorded when a file is read and put back when it is saved, so that saving
//! never adds line-ending or BOM noise to the diff.

use std::path::{Component, Path, PathBuf};

use serde::{Deserialize, Serialize};

const BOM: &[u8] = &[0xEF, 0xBB, 0xBF];
/// Files above this size are not loaded into the editor.
pub const MAX_TEXT_BYTES: usize = 5 * 1024 * 1024;

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, Default)]
#[serde(rename_all = "lowercase")]
pub enum Eol {
    #[default]
    Lf,
    Crlf,
}

#[derive(Debug, PartialEq)]
pub enum Decoded {
    Text { text: String, eol: Eol, bom: bool },
    Binary,
    TooLarge,
}

pub fn decode(bytes: &[u8]) -> Decoded {
    if bytes.len() > MAX_TEXT_BYTES {
        return Decoded::TooLarge;
    }
    let (bom, body) = match bytes.strip_prefix(BOM) {
        Some(rest) => (true, rest),
        None => (false, bytes),
    };
    if body.contains(&0) {
        return Decoded::Binary;
    }
    let Ok(text) = std::str::from_utf8(body) else {
        return Decoded::Binary;
    };
    let crlf = text.matches("\r\n").count();
    let lf = text.matches('\n').count() - crlf;
    let eol = if crlf > lf { Eol::Crlf } else { Eol::Lf };
    Decoded::Text {
        text: text.replace("\r\n", "\n"),
        eol,
        bom,
    }
}

pub fn encode(text: &str, eol: Eol, bom: bool) -> Vec<u8> {
    let body = match eol {
        Eol::Lf => text.to_string(),
        Eol::Crlf => text.replace('\n', "\r\n"),
    };
    let mut bytes = Vec::with_capacity(body.len() + BOM.len());
    if bom {
        bytes.extend_from_slice(BOM);
    }
    bytes.extend_from_slice(body.as_bytes());
    bytes
}

/// Resolves a repository-relative path inside `root`, refusing anything that
/// would land outside it (absolute paths, `..`, symlinked parents).
pub fn resolve(root: &Path, relative: &str) -> Result<PathBuf, String> {
    let relative_path = Path::new(relative);
    let only_normal = relative_path
        .components()
        .all(|c| matches!(c, Component::Normal(_) | Component::CurDir));
    if relative.is_empty() || !only_normal {
        return Err(format!("not a path inside the repository: {relative}"));
    }
    let full = root.join(relative_path);

    // Check the nearest existing ancestor after resolving symlinks.
    let canonical_root = root
        .canonicalize()
        .map_err(|e| format!("{}: {e}", root.display()))?;
    let mut existing = full.as_path();
    while !existing.exists() {
        existing = existing
            .parent()
            .ok_or_else(|| format!("not a path inside the repository: {relative}"))?;
    }
    let canonical_existing = existing
        .canonicalize()
        .map_err(|e| format!("{}: {e}", existing.display()))?;
    if !canonical_existing.starts_with(&canonical_root) {
        return Err(format!("path leaves the repository: {relative}"));
    }
    Ok(full)
}

/// Saves editor text, restoring the file's line endings and BOM.
pub async fn write_text(root: &Path, relative: &str, text: &str, eol: Eol, bom: bool) -> Result<(), String> {
    write_bytes(root, relative, &encode(text, eol, bom)).await
}

async fn write_bytes(root: &Path, relative: &str, bytes: &[u8]) -> Result<(), String> {
    let target = resolve(root, relative)?;
    if let Some(parent) = target.parent() {
        tokio::fs::create_dir_all(parent)
            .await
            .map_err(|e| format!("{relative}: {e}"))?;
    }
    tokio::fs::write(&target, bytes)
        .await
        .map_err(|e| format!("{relative}: {e}"))
}

/// Puts a file back to its base version. `base` is its content there (`None`
/// if it did not exist) and `base_path` its path there, which differs from
/// `path` for a renamed file.
pub async fn reset_to_base(
    root: &Path,
    path: &str,
    base_path: &str,
    base: Option<Vec<u8>>,
) -> Result<(), String> {
    if base.is_none() || base_path != path {
        match tokio::fs::remove_file(resolve(root, path)?).await {
            Ok(()) => {}
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => {}
            Err(error) => return Err(format!("{path}: {error}")),
        }
    }
    match base {
        Some(bytes) => write_bytes(root, base_path, &bytes).await,
        None => Ok(()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn saves_with_original_line_endings_into_new_directories() {
        let dir = tempfile::tempdir().unwrap();
        write_text(dir.path(), "src/New/File.cs", "a\nb\n", Eol::Crlf, true)
            .await
            .unwrap();
        let written = std::fs::read(dir.path().join("src/New/File.cs")).unwrap();
        assert_eq!(written, b"\xEF\xBB\xBFa\r\nb\r\n");
    }

    #[tokio::test]
    async fn reset_restores_modified_and_deleted_files() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::write(dir.path().join("a.fs"), "changed").unwrap();
        reset_to_base(dir.path(), "a.fs", "a.fs", Some(b"original".to_vec()))
            .await
            .unwrap();
        reset_to_base(dir.path(), "gone/b.fs", "gone/b.fs", Some(b"back".to_vec()))
            .await
            .unwrap();
        assert_eq!(std::fs::read(dir.path().join("a.fs")).unwrap(), b"original");
        assert_eq!(std::fs::read(dir.path().join("gone/b.fs")).unwrap(), b"back");
    }

    #[tokio::test]
    async fn reset_removes_added_files_and_undoes_renames() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::write(dir.path().join("added.fs"), "new").unwrap();
        std::fs::write(dir.path().join("new-name.fs"), "moved and edited").unwrap();

        reset_to_base(dir.path(), "added.fs", "added.fs", None).await.unwrap();
        reset_to_base(dir.path(), "new-name.fs", "old-name.fs", Some(b"moved".to_vec()))
            .await
            .unwrap();

        assert!(!dir.path().join("added.fs").exists());
        assert!(!dir.path().join("new-name.fs").exists());
        assert_eq!(std::fs::read(dir.path().join("old-name.fs")).unwrap(), b"moved");
    }

    #[test]
    fn round_trips_crlf_and_bom() {
        let original = b"\xEF\xBB\xBFnamespace A\r\n{\r\n}\r\n";
        let Decoded::Text { text, eol, bom } = decode(original) else {
            panic!("expected text");
        };
        assert_eq!(text, "namespace A\n{\n}\n");
        assert_eq!((eol, bom), (Eol::Crlf, true));
        assert_eq!(encode(&text, eol, bom), original);
    }

    #[test]
    fn round_trips_plain_lf() {
        let original = b"let x = 1\nlet y = 2";
        let Decoded::Text { text, eol, bom } = decode(original) else {
            panic!("expected text");
        };
        assert_eq!((eol, bom), (Eol::Lf, false));
        assert_eq!(encode(&text, eol, bom), original);
    }

    #[test]
    fn detects_binary() {
        assert_eq!(decode(b"PNG\0\x01\x02"), Decoded::Binary);
        assert_eq!(decode(&[0xFF, 0xFE, 0x41]), Decoded::Binary);
    }

    #[test]
    fn refuses_paths_outside_the_root() {
        let dir = tempfile::tempdir().unwrap();
        let root = dir.path();
        std::fs::create_dir(root.join("src")).unwrap();

        assert!(resolve(root, "src/new/File.fs").is_ok());
        assert!(resolve(root, "../outside.txt").is_err());
        assert!(resolve(root, "src/../../outside.txt").is_err());
        assert!(resolve(root, "/etc/passwd").is_err());
        assert!(resolve(root, "").is_err());
    }

    #[cfg(unix)]
    #[test]
    fn refuses_symlinked_directories_that_leave_the_root() {
        let dir = tempfile::tempdir().unwrap();
        let outside = tempfile::tempdir().unwrap();
        std::os::unix::fs::symlink(outside.path(), dir.path().join("link")).unwrap();
        assert!(resolve(dir.path(), "link/file.txt").is_err());
    }
}
