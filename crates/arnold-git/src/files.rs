use std::collections::{BTreeSet, HashSet};
use std::path::Path;

use crate::cli::run;
use crate::Result;

/// Mode git gives a submodule: a folder of another repository, not a file.
const GITLINK_MODE: &str = "160000";

/// Every file of `head`, sorted: in its checkout when it has one (tracked
/// files still on disk plus untracked files that are not ignored), otherwise
/// the files of its tip commit.
pub async fn list_files(root: &Path, head: &str, worktree: Option<&Path>) -> Result<Vec<String>> {
    match worktree {
        Some(worktree) => checkout_files(worktree).await,
        None => commit_files(root, head).await,
    }
}

async fn checkout_files(worktree: &Path) -> Result<Vec<String>> {
    let (staged, deleted, untracked) = tokio::try_join!(
        run(worktree, &["ls-files", "-z", "--stage"]),
        run(worktree, &["ls-files", "-z", "--deleted"]),
        run(worktree, &["ls-files", "-z", "--others", "--exclude-standard"]),
    )?;
    let deleted = String::from_utf8_lossy(&deleted);
    let deleted: HashSet<&str> = paths(&deleted).collect();
    let staged = String::from_utf8_lossy(&staged);
    let untracked = String::from_utf8_lossy(&untracked);

    let mut files = BTreeSet::new();
    // A file with a merge conflict is listed once per stage; the set keeps one.
    files.extend(
        entries(&staged)
            .filter(|(mode, path)| *mode != GITLINK_MODE && !deleted.contains(path))
            .map(|(_, path)| path.to_string()),
    );
    files.extend(paths(&untracked).map(str::to_string));
    Ok(files.into_iter().collect())
}

async fn commit_files(root: &Path, head: &str) -> Result<Vec<String>> {
    let out = run(root, &["ls-tree", "-r", "-z", "--full-tree", head]).await?;
    let listing = String::from_utf8_lossy(&out);
    let mut files: Vec<String> = entries(&listing)
        .filter(|(mode, _)| *mode != GITLINK_MODE)
        .map(|(_, path)| path.to_string())
        .collect();
    files.sort();
    Ok(files)
}

fn paths(listing: &str) -> impl Iterator<Item = &str> {
    listing.split('\0').filter(|path| !path.is_empty())
}

/// `(mode, path)` of each "mode object-or-type ...\tpath" record.
fn entries(listing: &str) -> impl Iterator<Item = (&str, &str)> {
    paths(listing).filter_map(|record| {
        let (info, path) = record.split_once('\t')?;
        Some((info.split(' ').next()?, path))
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_records_with_tabs_and_spaces_in_paths() {
        let listing = "100644 1f2a 0\tsrc/a b.fs\x00160000 9c3d 0\tvendor/lib\x00100644 3e4f 0\tweird\tname\x00";
        let parsed: Vec<_> = entries(listing).collect();
        assert_eq!(
            parsed,
            vec![
                ("100644", "src/a b.fs"),
                ("160000", "vendor/lib"),
                ("100644", "weird\tname"),
            ]
        );
    }
}
