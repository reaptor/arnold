use std::collections::HashMap;
use std::path::{Path, PathBuf};

use serde::Serialize;

use crate::cli::{run, run_raw, run_text, GitError};
use crate::Result;

#[derive(Debug, Clone, Copy, Serialize, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
pub enum FileStatus {
    Added,
    Modified,
    Deleted,
    Renamed,
    Untracked,
}

#[derive(Debug, Clone, Serialize, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct ChangedFile {
    /// Path relative to the checkout, forward slashes.
    pub path: String,
    /// Path on the base side when the file was renamed.
    pub old_path: Option<String>,
    pub status: FileStatus,
    pub additions: Option<u32>,
    pub deletions: Option<u32>,
    pub binary: bool,
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Comparison {
    pub base: String,
    pub head: String,
    /// Commit the diff starts from: what a pull request would compare against.
    pub merge_base: String,
    /// Checkout holding `head`. When set, the diff runs to its working tree
    /// (uncommitted and untracked files included) and files can be edited.
    pub worktree_path: Option<PathBuf>,
    pub files: Vec<ChangedFile>,
}

/// Untracked files larger than this are listed without a line count.
const MAX_COUNTED_BYTES: u64 = 2 * 1024 * 1024;

/// PR-style comparison: merge-base of `base` and `head` → `head`, in the
/// checkout holding `head` if there is one, else at its tip commit.
pub async fn compare(root: &Path, base: &str, head: &str) -> Result<Comparison> {
    let worktree = worktree_of(root, head).await?;
    compare_in(root, base, head, worktree).await
}

/// Like [`compare`], with the checkout of `head` given (see `ensure_checkout`).
pub async fn compare_in(
    root: &Path,
    base: &str,
    head: &str,
    worktree_path: Option<PathBuf>,
) -> Result<Comparison> {
    // Unrelated histories have no merge-base; compare against `base` itself then.
    let merge_base = match run_text(root, &["merge-base", base, head]).await {
        Ok(sha) if !sha.is_empty() => sha,
        _ => run_text(root, &["rev-parse", "--verify", &format!("{base}^{{commit}}")]).await?,
    };

    let files = match &worktree_path {
        Some(worktree) => {
            let mut files = diff_files(worktree, &[merge_base.as_str()]).await?;
            files.extend(untracked_files(worktree).await?);
            files
        }
        None => diff_files(root, &[merge_base.as_str(), head]).await?,
    };

    let mut files = files;
    files.sort_by(|a, b| a.path.cmp(&b.path));
    Ok(Comparison {
        base: base.to_string(),
        head: head.to_string(),
        merge_base,
        worktree_path,
        files,
    })
}

/// Contents of `path` at `rev`, or `None` if it does not exist there.
pub async fn read_blob(root: &Path, rev: &str, path: &str) -> Result<Option<Vec<u8>>> {
    let out = run_raw(root, &["cat-file", "blob", &format!("{rev}:{path}")]).await?;
    Ok(out.success.then_some(out.stdout))
}

pub(crate) async fn worktree_of(root: &Path, branch: &str) -> Result<Option<PathBuf>> {
    let out = run_text(
        root,
        &[
            "for-each-ref",
            "--format=%(worktreepath)",
            &format!("refs/heads/{branch}"),
        ],
    )
    .await?;
    Ok(out
        .lines()
        .find(|line| !line.is_empty())
        .map(PathBuf::from))
}

async fn diff_files(cwd: &Path, revs: &[&str]) -> Result<Vec<ChangedFile>> {
    let mut status_args = vec!["diff", "--name-status", "-z", "-M"];
    status_args.extend_from_slice(revs);
    let mut numstat_args = vec!["diff", "--numstat", "-z", "-M"];
    numstat_args.extend_from_slice(revs);

    let (status, numstat) = tokio::try_join!(run(cwd, &status_args), run(cwd, &numstat_args))?;
    let mut files = parse_name_status(&String::from_utf8_lossy(&status))?;
    let counts = parse_numstat(&String::from_utf8_lossy(&numstat));
    for file in &mut files {
        if let Some(count) = counts.get(&file.path) {
            match count {
                Some((additions, deletions)) => {
                    file.additions = Some(*additions);
                    file.deletions = Some(*deletions);
                }
                None => file.binary = true,
            }
        }
    }
    Ok(files)
}

fn parse_name_status(out: &str) -> Result<Vec<ChangedFile>> {
    let mut tokens = out.split('\0').filter(|t| !t.is_empty());
    let mut files = Vec::new();
    while let Some(code) = tokens.next() {
        let mut next_path = || {
            tokens
                .next()
                .map(str::to_string)
                .ok_or_else(|| GitError::Parse(format!("missing path after status {code}")))
        };
        let (status, old_path, path) = match code.chars().next() {
            Some('A') => (FileStatus::Added, None, next_path()?),
            Some('D') => (FileStatus::Deleted, None, next_path()?),
            Some('R') => {
                let old = next_path()?;
                (FileStatus::Renamed, Some(old), next_path()?)
            }
            // A copy is a new file as far as the working tree is concerned.
            Some('C') => {
                next_path()?;
                (FileStatus::Added, None, next_path()?)
            }
            Some(_) => (FileStatus::Modified, None, next_path()?),
            None => continue,
        };
        files.push(ChangedFile {
            path,
            old_path,
            status,
            additions: None,
            deletions: None,
            binary: false,
        });
    }
    Ok(files)
}

/// Path → `Some((additions, deletions))`, or `None` for a binary file.
fn parse_numstat(out: &str) -> HashMap<String, Option<(u32, u32)>> {
    let mut tokens = out.split('\0');
    let mut counts = HashMap::new();
    while let Some(record) = tokens.next() {
        let mut fields = record.splitn(3, '\t');
        let (Some(added), Some(deleted), Some(path)) =
            (fields.next(), fields.next(), fields.next())
        else {
            continue;
        };
        // A rename is written as "added\tdeleted\t" followed by the old and new paths.
        let path = if path.is_empty() {
            tokens.next();
            match tokens.next() {
                Some(new_path) => new_path,
                None => break,
            }
        } else {
            path
        };
        let count = match (added.parse(), deleted.parse()) {
            (Ok(added), Ok(deleted)) => Some((added, deleted)),
            _ => None,
        };
        counts.insert(path.to_string(), count);
    }
    counts
}

async fn untracked_files(worktree: &Path) -> Result<Vec<ChangedFile>> {
    let out = run(
        worktree,
        &["ls-files", "--others", "--exclude-standard", "-z"],
    )
    .await?;
    let listing = String::from_utf8_lossy(&out).into_owned();
    let mut files = Vec::new();
    for path in listing.split('\0').filter(|p| !p.is_empty()) {
        let (additions, binary) = count_lines(&worktree.join(path)).await;
        files.push(ChangedFile {
            path: path.to_string(),
            old_path: None,
            status: FileStatus::Untracked,
            additions,
            deletions: additions.map(|_| 0),
            binary,
        });
    }
    Ok(files)
}

/// Line count of a text file, or `(None, true)` when it looks binary.
async fn count_lines(path: &Path) -> (Option<u32>, bool) {
    let Ok(metadata) = tokio::fs::metadata(path).await else {
        return (None, false);
    };
    if metadata.len() > MAX_COUNTED_BYTES {
        return (None, false);
    }
    let Ok(bytes) = tokio::fs::read(path).await else {
        return (None, false);
    };
    if bytes.contains(&0) {
        return (None, true);
    }
    let newlines = bytes.iter().filter(|b| **b == b'\n').count();
    let unterminated_last_line = bytes.last().is_some_and(|b| *b != b'\n');
    (Some((newlines + usize::from(unterminated_last_line)) as u32), false)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_name_status_with_rename() {
        let files = parse_name_status("M\0src/a.fs\0R087\0old.fs\0new.fs\0A\0b.fs\0D\0c.fs\0").unwrap();
        let summary: Vec<_> = files
            .iter()
            .map(|f| (f.status, f.path.as_str(), f.old_path.as_deref()))
            .collect();
        assert_eq!(
            summary,
            vec![
                (FileStatus::Modified, "src/a.fs", None),
                (FileStatus::Renamed, "new.fs", Some("old.fs")),
                (FileStatus::Added, "b.fs", None),
                (FileStatus::Deleted, "c.fs", None),
            ]
        );
    }

    #[test]
    fn parses_numstat_with_rename_and_binary() {
        let counts = parse_numstat("3\t1\tsrc/a.fs\0-\t-\tlogo.png\x002\t0\t\0old.fs\0new.fs\0");
        assert_eq!(counts["src/a.fs"], Some((3, 1)));
        assert_eq!(counts["logo.png"], None);
        assert_eq!(counts["new.fs"], Some((2, 0)));
    }
}
