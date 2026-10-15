//! Reading working-tree files for the editor.
//!
//! The editor works with `\n` line endings and no byte-order mark, so both are
//! taken off when a file is read.

use std::path::{Component, Path, PathBuf};

const BOM: &[u8] = &[0xEF, 0xBB, 0xBF];
/// Files above this size are not loaded into the editor.
pub const MAX_TEXT_BYTES: usize = 5 * 1024 * 1024;

#[derive(Debug, PartialEq)]
pub enum Decoded {
    Text(String),
    Binary,
    TooLarge,
}

pub fn decode(bytes: &[u8]) -> Decoded {
    if bytes.len() > MAX_TEXT_BYTES {
        return Decoded::TooLarge;
    }
    let body = bytes.strip_prefix(BOM).unwrap_or(bytes);
    if body.contains(&0) {
        return Decoded::Binary;
    }
    match std::str::from_utf8(body) {
        Ok(text) => Decoded::Text(text.replace("\r\n", "\n")),
        Err(_) => Decoded::Binary,
    }
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn strips_crlf_and_bom() {
        let decoded = decode(b"\xEF\xBB\xBFnamespace A\r\n{\r\n}\r\n");
        assert_eq!(decoded, Decoded::Text("namespace A\n{\n}\n".to_string()));
    }

    #[test]
    fn keeps_plain_lf() {
        let decoded = decode(b"let x = 1\nlet y = 2");
        assert_eq!(decoded, Decoded::Text("let x = 1\nlet y = 2".to_string()));
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
