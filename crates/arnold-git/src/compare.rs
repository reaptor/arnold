use std::collections::HashMap;
use std::path::Path;

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
    /// The commit `head` pointed at: everything shown comes from it, even if
    /// the branch moves on meanwhile.
    pub head_commit: String,
    pub files: Vec<ChangedFile>,
}

/// PR-style comparison: merge-base of `base` and `head` → the tip commit of
/// `head`. Uncommitted and untracked files are never part of it.
pub async fn compare(root: &Path, base: &str, head: &str) -> Result<Comparison> {
    // Unrelated histories have no merge-base; compare against `base` itself then.
    let merge_base = match run_text(root, &["merge-base", base, head]).await {
        Ok(sha) if !sha.is_empty() => sha,
        _ => run_text(root, &["rev-parse", "--verify", &format!("{base}^{{commit}}")]).await?,
    };
    let head_commit = run_text(root, &["rev-parse", "--verify", &format!("{head}^{{commit}}")]).await?;
    let mut files = diff_files(root, &[merge_base.as_str(), head_commit.as_str()]).await?;
    files.sort_by(|a, b| a.path.cmp(&b.path));
    Ok(Comparison {
        base: base.to_string(),
        head: head.to_string(),
        merge_base,
        head_commit,
        files,
    })
}

/// Contents of `path` at `rev`, or `None` if it does not exist there.
pub async fn read_blob(root: &Path, rev: &str, path: &str) -> Result<Option<Vec<u8>>> {
    let out = run_raw(root, &["cat-file", "blob", &format!("{rev}:{path}")]).await?;
    Ok(out.success.then_some(out.stdout))
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
            // A copy is a new file as far as the review is concerned.
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
