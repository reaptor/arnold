use std::path::Path;

use crate::cli::run;
use crate::Result;

/// Mode git gives a submodule: a folder of another repository, not a file.
const GITLINK_MODE: &str = "160000";

/// A file of a commit.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TreeFile {
    /// Path from the top of the repository, forward slashes.
    pub path: String,
    /// Id of the file's contents: equal ids mean equal contents.
    pub oid: String,
}

/// Every file of `rev`, sorted by path.
pub async fn list_files(root: &Path, rev: &str) -> Result<Vec<String>> {
    Ok(tree_files(root, rev).await?.into_iter().map(|file| file.path).collect())
}

/// Every file of `rev` with the id of its contents, sorted by path.
pub async fn tree_files(root: &Path, rev: &str) -> Result<Vec<TreeFile>> {
    let out = run(root, &["ls-tree", "-r", "-z", "--full-tree", rev]).await?;
    let mut files: Vec<TreeFile> = entries(&String::from_utf8_lossy(&out))
        .filter(|(mode, _, _)| *mode != GITLINK_MODE)
        .map(|(_, oid, path)| TreeFile {
            path: path.to_string(),
            oid: oid.to_string(),
        })
        .collect();
    files.sort_by(|a, b| a.path.cmp(&b.path));
    Ok(files)
}

/// `(mode, object id, path)` of each "mode type id\tpath" record.
fn entries(listing: &str) -> impl Iterator<Item = (&str, &str, &str)> {
    listing.split('\0').filter_map(|record| {
        let (info, path) = record.split_once('\t')?;
        let mut fields = info.split(' ');
        let mode = fields.next()?;
        let oid = fields.nth(1)?;
        Some((mode, oid, path))
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_records_with_tabs_and_spaces_in_paths() {
        let listing = "100644 blob 1f2a\tsrc/a b.fs\x00160000 commit 9c3d\tvendor/lib\x00100644 blob 3e4f\tweird\tname\x00";
        let parsed: Vec<_> = entries(listing).collect();
        assert_eq!(
            parsed,
            vec![
                ("100644", "1f2a", "src/a b.fs"),
                ("160000", "9c3d", "vendor/lib"),
                ("100644", "3e4f", "weird\tname"),
            ]
        );
    }
}
