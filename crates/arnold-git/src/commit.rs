//! Committing and pushing the checkout under review.

use std::path::Path;

use serde::Serialize;

use crate::cli::{run, run_raw, run_text, run_with_input, GitError};
use crate::Result;

#[derive(Debug, Clone, Serialize, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct Uncommitted {
    /// Path relative to the checkout, forward slashes.
    pub path: String,
    /// Path before a rename that is already staged.
    pub old_path: Option<String>,
    /// `A`dded (untracked files too), `M`odified, `D`eleted, `R`enamed or `U`nmerged.
    pub status: char,
}

/// Where a push of the checkout goes.
#[derive(Debug, Clone, Serialize, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct PushTarget {
    pub remote: String,
    /// Branch name on the remote.
    pub branch: String,
    /// The remote does not have this branch yet (as far as the last fetch knows).
    pub unpublished: bool,
    /// Commits of the checkout the remote branch does not have.
    pub ahead: u32,
    /// Commits of the remote branch the checkout does not have; a push is refused then.
    pub behind: u32,
}

#[derive(Debug, Clone, Serialize, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct CheckoutStatus {
    /// Branch checked out; `None` when HEAD is detached (a remote branch under review).
    pub branch: Option<String>,
    /// `None` when there is nowhere to push to (no remote).
    pub push: Option<PushTarget>,
    pub changes: Vec<Uncommitted>,
}

/// Uncommitted changes of `worktree` and how it stands against its push target.
/// `head` is the branch under review, which names the push target of a
/// detached checkout of a remote branch.
pub async fn checkout_status(worktree: &Path, head: &str) -> Result<CheckoutStatus> {
    let (status, branch) = tokio::try_join!(
        run(worktree, &["status", "--porcelain=v1", "-z", "--untracked-files=all"]),
        current_branch(worktree),
    )?;
    let changes = parse_status(&String::from_utf8_lossy(&status))?;
    let push = match push_destination(worktree, branch.as_deref(), head).await? {
        Some((remote, remote_branch)) => Some(compare_with_remote(worktree, remote, remote_branch).await?),
        None => None,
    };
    Ok(CheckoutStatus { branch, push, changes })
}

/// Commits `paths` (every change when `None`) with `message`; returns the new commit's id.
pub async fn commit(worktree: &Path, message: &str, paths: Option<&[String]>) -> Result<String> {
    if message.trim().is_empty() {
        return Err(GitError::Parse("the commit message is empty".into()));
    }
    match paths {
        None => {
            run(worktree, &["add", "--all"]).await?;
            run(worktree, &["commit", "--quiet", "-m", message]).await?;
        }
        Some(paths) => {
            // Paths go through stdin: a long list could pass the command line's limit.
            let mut list = Vec::new();
            for path in paths {
                list.extend_from_slice(path.as_bytes());
                list.push(0);
            }
            let from_stdin = ["--pathspec-from-file=-", "--pathspec-file-nul"];
            let mut add = vec!["--literal-pathspecs", "add", "--all"];
            add.extend(from_stdin);
            run_with_input(worktree, &add, Some(&list)).await?;
            // Only these paths, even if other changes were staged before.
            let mut commit = vec!["--literal-pathspecs", "commit", "--quiet", "--only", "-m", message];
            commit.extend(from_stdin);
            run_with_input(worktree, &commit, Some(&list)).await?;
        }
    }
    run_text(worktree, &["rev-parse", "HEAD"]).await
}

/// Pushes the checkout's HEAD to its push target, setting the branch's
/// upstream when it has none yet.
pub async fn push(worktree: &Path, head: &str) -> Result<()> {
    let branch = current_branch(worktree).await?;
    let (remote, remote_branch) = push_destination(worktree, branch.as_deref(), head)
        .await?
        .ok_or_else(|| GitError::Parse("this repository has no remote to push to".into()))?;
    let refspec = format!("HEAD:refs/heads/{remote_branch}");
    let mut args = vec!["push", "--porcelain"];
    if let Some(branch) = &branch {
        if config(worktree, &format!("branch.{branch}.merge")).await.is_none() {
            args.push("--set-upstream");
        }
    }
    args.extend([remote.as_str(), refspec.as_str()]);
    run(worktree, &args).await?;
    Ok(())
}

async fn current_branch(worktree: &Path) -> Result<Option<String>> {
    let out = run_raw(worktree, &["symbolic-ref", "--short", "-q", "HEAD"]).await?;
    let name = String::from_utf8_lossy(&out.stdout).trim().to_string();
    Ok((out.success && !name.is_empty()).then_some(name))
}

/// Remote and remote branch a push goes to: the branch's upstream, else the
/// remote git would pick with a branch of the same name; for a detached
/// checkout, the remote branch under review.
async fn push_destination(
    worktree: &Path,
    branch: Option<&str>,
    head: &str,
) -> Result<Option<(String, String)>> {
    let remotes = run_text(worktree, &["remote"]).await?;
    let remotes: Vec<&str> = remotes.lines().filter(|line| !line.is_empty()).collect();
    let Some(branch) = branch else {
        // Longest match first: a remote may be named like a prefix of another.
        let mut matches: Vec<&str> = remotes
            .iter()
            .copied()
            .filter(|remote| head.starts_with(&format!("{remote}/")))
            .collect();
        matches.sort_by_key(|remote| std::cmp::Reverse(remote.len()));
        return Ok(matches
            .first()
            .map(|remote| (remote.to_string(), head[remote.len() + 1..].to_string())));
    };

    let mut remote = None;
    for key in [
        format!("branch.{branch}.pushRemote"),
        "remote.pushDefault".to_string(),
        format!("branch.{branch}.remote"),
    ] {
        remote = config(worktree, &key).await;
        if remote.is_some() {
            break;
        }
    }
    // `.` means the local repository itself: nothing to push to.
    let remote = remote.filter(|remote| remote != ".").or_else(|| match remotes.as_slice() {
        _ if remotes.contains(&"origin") => Some("origin".to_string()),
        [only] => Some(only.to_string()),
        _ => None,
    });
    let Some(remote) = remote else { return Ok(None) };
    let remote_branch = config(worktree, &format!("branch.{branch}.merge"))
        .await
        .and_then(|merge| merge.strip_prefix("refs/heads/").map(str::to_string))
        .unwrap_or_else(|| branch.to_string());
    Ok(Some((remote, remote_branch)))
}

async fn compare_with_remote(worktree: &Path, remote: String, branch: String) -> Result<PushTarget> {
    let tracking = format!("refs/remotes/{remote}/{branch}");
    let known = run_raw(worktree, &["rev-parse", "--verify", "--quiet", &tracking])
        .await?
        .success;
    let (ahead, behind) = if known {
        let counts = run_text(
            worktree,
            &["rev-list", "--left-right", "--count", &format!("{tracking}...HEAD")],
        )
        .await?;
        let mut counts = counts.split_whitespace().map(|n| n.parse().unwrap_or(0));
        let behind = counts.next().unwrap_or(0);
        (counts.next().unwrap_or(0), behind)
    } else {
        // A new branch: count what no remote has yet.
        let ahead = run_text(worktree, &["rev-list", "--count", "HEAD", "--not", "--remotes"]).await?;
        (ahead.parse().unwrap_or(0), 0)
    };
    Ok(PushTarget { remote, branch, unpublished: !known, ahead, behind })
}

async fn config(worktree: &Path, key: &str) -> Option<String> {
    let out = run_raw(worktree, &["config", "--get", key]).await.ok()?;
    let value = String::from_utf8_lossy(&out.stdout).trim().to_string();
    (out.success && !value.is_empty()).then_some(value)
}

fn parse_status(out: &str) -> Result<Vec<Uncommitted>> {
    let mut tokens = out.split('\0').filter(|t| !t.is_empty());
    let mut changes = Vec::new();
    while let Some(entry) = tokens.next() {
        let (code, path) = entry
            .split_at_checked(3)
            .ok_or_else(|| GitError::Parse(format!("short status entry {entry:?}")))?;
        let mut code = code.chars();
        let (x, y) = (code.next().unwrap_or(' '), code.next().unwrap_or(' '));
        let renamed = matches!(x, 'R' | 'C');
        let old_path = if renamed { tokens.next().map(str::to_string) } else { None };
        let status = match (x, y) {
            ('U', _) | (_, 'U') | ('A', 'A') | ('D', 'D') => 'U',
            ('?', _) | ('A', _) => 'A',
            ('D', _) | (_, 'D') => 'D',
            ('R', _) => 'R',
            ('C', _) => 'A',
            _ => 'M',
        };
        changes.push(Uncommitted { path: path.to_string(), old_path, status });
    }
    Ok(changes)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_status_with_renames_and_untracked_files() {
        let out = " M a.txt\0R  new name.txt\0old name.txt\0?? dir/x.txt\0 D gone.txt\0UU both.txt\0";
        let changes = parse_status(out).unwrap();
        let summary: Vec<_> = changes
            .iter()
            .map(|c| (c.status, c.path.as_str(), c.old_path.as_deref()))
            .collect();
        assert_eq!(
            summary,
            vec![
                ('M', "a.txt", None),
                ('R', "new name.txt", Some("old name.txt")),
                ('A', "dir/x.txt", None),
                ('D', "gone.txt", None),
                ('U', "both.txt", None),
            ]
        );
    }
}
