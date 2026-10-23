use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};

use serde::{Deserialize, Serialize};

use crate::cli::{run, run_raw, run_text, GitError};
use crate::Result;

#[derive(Debug, Clone, Copy, Serialize, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
pub enum FileStatus {
    Added,
    Modified,
    Deleted,
    Renamed,
    /// Git could not merge it: the working tree has the conflicts (only
    /// listed by `working_status`).
    Conflicted,
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
    /// Differs on disk from the tip of the branch: edits not committed yet.
    /// Only set when the comparison reads the checkout's files.
    pub uncommitted: bool,
}

/// One commit of the branch under review.
#[derive(Debug, Clone, Serialize, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct Commit {
    pub sha: String,
    pub short_sha: String,
    /// First line of the message.
    pub subject: String,
    pub author: String,
    /// Author date, ISO 8601.
    pub date: String,
}

/// A run of consecutive commits of the branch under review, by sha.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
pub struct CommitRange {
    pub oldest: String,
    pub newest: String,
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Comparison {
    pub base: String,
    pub head: String,
    /// Commit the diff starts from: what a pull request would compare
    /// against, or the parent of the oldest commit of `range`.
    pub merge_base: String,
    /// Commit the diff ends at, which everything shown comes from: the tip of
    /// `head` (even if the branch moves on meanwhile), or the newest of `range`.
    pub head_commit: String,
    /// The tip of `head`.
    pub tip_commit: String,
    /// The commits shown, if not all of the changes.
    pub range: Option<CommitRange>,
    /// The commits of `head` since the merge-base, newest first.
    pub commits: Vec<Commit>,
    pub files: Vec<ChangedFile>,
    /// The checkout whose files the diff ends at instead of `head_commit`:
    /// `head` is checked out there and all of its changes are shown, so the
    /// files may be edited. `None` when the diff is between commits.
    pub checkout: Option<PathBuf>,
    /// Files with uncommitted changes in `checkout`.
    pub uncommitted: usize,
}

/// What git compares a parentless commit against.
const EMPTY_TREE: &str = "4b825dc642cb6eb9a060e54bf8d69288fbee4904";

/// PR-style comparison: merge-base of `base` and `head` → the tip commit of
/// `head`. With `range`, only what those commits (of the ones since the
/// merge-base) changed: the first parent of the oldest → the newest. A range
/// whose ends are no longer on the branch is ignored.
///
/// `checkout`: where `head` is checked out. Without a range the diff then
/// ends at that checkout's files, uncommitted edits and untracked files
/// included, and the files that differ from the tip are marked `uncommitted`.
pub async fn compare(
    root: &Path,
    base: &str,
    head: &str,
    range: Option<&CommitRange>,
    checkout: Option<&Path>,
) -> Result<Comparison> {
    // Unrelated histories have no merge-base; compare against `base` itself then.
    let merge_base = match run_text(root, &["merge-base", base, head]).await {
        Ok(sha) if !sha.is_empty() => sha,
        _ => run_text(root, &["rev-parse", "--verify", &format!("{base}^{{commit}}")]).await?,
    };
    let tip_commit = run_text(root, &["rev-parse", "--verify", &format!("{head}^{{commit}}")]).await?;
    let commits = commits_between(root, &merge_base, &tip_commit).await?;

    let position = |sha: &str| commits.iter().position(|c| c.sha == sha);
    let range = range.and_then(|range| {
        // Newest first: the oldest has the larger position.
        let (a, b) = (position(&range.oldest)?, position(&range.newest)?);
        Some(CommitRange {
            oldest: commits[a.max(b)].sha.clone(),
            newest: commits[a.min(b)].sha.clone(),
        })
    });
    let (from, to) = match &range {
        Some(range) => {
            let parent = run_raw(root, &["rev-parse", "--verify", "--quiet", &format!("{}^1", range.oldest)]).await?;
            let parent = if parent.success {
                String::from_utf8_lossy(&parent.stdout).trim().to_string()
            } else {
                EMPTY_TREE.to_string()
            };
            (parent, range.newest.clone())
        }
        None => (merge_base, tip_commit.clone()),
    };
    let checkout = checkout.filter(|_| range.is_none());
    let (mut files, uncommitted) = match checkout {
        Some(checkout) => on_disk(checkout, &from, &to).await?,
        None => (diff_files(root, &[from.as_str(), to.as_str()]).await?, 0),
    };
    files.sort_by(|a, b| a.path.cmp(&b.path));
    Ok(Comparison {
        base: base.to_string(),
        head: head.to_string(),
        merge_base: from,
        head_commit: to,
        tip_commit,
        range,
        commits,
        files,
        checkout: checkout.map(Path::to_path_buf),
        uncommitted,
    })
}

/// `from` → the files of `checkout`, whose branch is at `tip`, and how many
/// files differ from `tip` there.
async fn on_disk(checkout: &Path, from: &str, tip: &str) -> Result<(Vec<ChangedFile>, usize)> {
    let from_args = [from];
    let edited_args = ["diff", "--name-only", "-z", "--no-renames", tip];
    let (mut files, edited, untracked) = tokio::try_join!(
        diff_files(checkout, &from_args),
        run(checkout, &edited_args),
        crate::working::untracked_files(checkout),
    )?;
    let mut edited: HashSet<String> = String::from_utf8_lossy(&edited)
        .split('\0')
        .filter(|path| !path.is_empty())
        .map(str::to_string)
        .collect();
    edited.extend(untracked.iter().map(|file| file.path.clone()));
    files.extend(untracked);
    // Edits that took a file back to how `from` has it leave nothing to
    // diff, yet are edits (a branch's change undone, a file it added deleted
    // again): listed with no lines changed, so they can be seen and undone.
    let listed: HashSet<String> = files.iter().map(|file| file.path.clone()).collect();
    for path in edited.iter().filter(|path| !listed.contains(*path)) {
        let on_disk = tokio::fs::try_exists(checkout.join(path)).await.unwrap_or(false);
        files.push(ChangedFile {
            path: path.clone(),
            old_path: None,
            status: if on_disk { FileStatus::Modified } else { FileStatus::Deleted },
            additions: Some(0),
            deletions: Some(0),
            binary: false,
            uncommitted: false,
        });
    }
    for file in &mut files {
        file.uncommitted = edited.contains(&file.path);
    }
    Ok((files, edited.len()))
}

/// Commits reachable from `tip` but not from `merge_base`, newest first.
async fn commits_between(root: &Path, merge_base: &str, tip: &str) -> Result<Vec<Commit>> {
    let range = format!("{merge_base}..{tip}");
    let out = run(root, &["log", "-z", "--format=%H%x1f%h%x1f%an%x1f%aI%x1f%s", &range]).await?;
    Ok(parse_log(&String::from_utf8_lossy(&out)))
}

fn parse_log(out: &str) -> Vec<Commit> {
    out.split('\0')
        .filter_map(|record| {
            let mut fields = record.trim_start_matches('\n').splitn(5, '\x1f');
            Some(Commit {
                sha: fields.next()?.to_string(),
                short_sha: fields.next()?.to_string(),
                author: fields.next()?.to_string(),
                date: fields.next()?.to_string(),
                subject: fields.next()?.to_string(),
            })
        })
        .collect()
}

/// Contents of `path` at `rev`, or `None` if it does not exist there.
pub async fn read_blob(root: &Path, rev: &str, path: &str) -> Result<Option<Vec<u8>>> {
    let out = run_raw(root, &["cat-file", "blob", &format!("{rev}:{path}")]).await?;
    Ok(out.success.then_some(out.stdout))
}

pub(crate) async fn diff_files(cwd: &Path, revs: &[&str]) -> Result<Vec<ChangedFile>> {
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
            uncommitted: false,
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
    fn parses_log_records() {
        let commits = parse_log("abc123\x1fabc\x1fAda\x1f2026-10-15T10:00:00+02:00\x1fFix\x1fthings\0def456\x1fdef\x1fBo\x1f2026-10-14T09:00:00+02:00\x1f\0");
        assert_eq!(commits.len(), 2);
        assert_eq!(commits[0].sha, "abc123");
        assert_eq!(commits[0].subject, "Fix\x1fthings");
        assert_eq!(commits[1].author, "Bo");
        assert_eq!(commits[1].subject, "");
    }

    #[test]
    fn parses_numstat_with_rename_and_binary() {
        let counts = parse_numstat("3\t1\tsrc/a.fs\0-\t-\tlogo.png\x002\t0\t\0old.fs\0new.fs\0");
        assert_eq!(counts["src/a.fs"], Some((3, 1)));
        assert_eq!(counts["logo.png"], None);
        assert_eq!(counts["new.fs"], Some((2, 0)));
    }
}
