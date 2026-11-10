//! Checking a branch or a pull request out, so it can be edited and the
//! language server runs on its files. By default in the opened folder
//! (`Placement::Here`), which is refused while that has uncommitted changes:
//! then they are either stashed (`Placement::Stash`), thrown away
//! (`Placement::Discard`) or left alone by
//! checking out in a registered git worktree of the app's own, in
//! `.arnold/worktrees` (`Placement::Worktree`):
//! - `branch_checkout` / `branch_worktree`: a branch checked out nowhere;
//! - `pull_request_checkout`: a pull request, on its source branch (never
//!   detached).

use std::path::{Path, PathBuf};

use serde::{Deserialize, Serialize};

use crate::cli::{run, run_raw, run_text, GitError};
use crate::export::app_folder;
use crate::remotes::remotes;
use crate::Result;

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
pub struct Worktree {
    pub path: PathBuf,
    /// The commit checked out there.
    pub head: String,
    /// Whether it has uncommitted changes.
    pub edited: bool,
    /// The pull request's source branch, checked out there.
    pub branch: String,
    /// Whether it was checked out just now.
    pub created: bool,
}

async fn describe(path: &Path, branch: &str, created: bool) -> Result<Worktree> {
    let (head, status) = tokio::try_join!(
        run_text(path, &["rev-parse", "--verify", "HEAD^{commit}"]),
        run(path, &["status", "--porcelain", "-z", "--untracked-files=normal"]),
    )?;
    Ok(Worktree { path: path.to_path_buf(), head, edited: !status.is_empty(), branch: branch.to_string(), created })
}

/// The pull request's branch checked out at `path`, moved up to `commit`
/// when that is a fast-forward and it has no uncommitted changes, or with
/// `discard` after throwing them away (untracked files included). A branch
/// with commits of its own (not pushed yet) is never moved.
async fn follow(path: &Path, branch: &str, commit: &str, discard: bool, created: bool) -> Result<Worktree> {
    let found = describe(path, branch, created).await?;
    if found.head == commit || (found.edited && !discard) {
        return Ok(found);
    }
    let ancestor = run_raw(path, &["merge-base", "--is-ancestor", &found.head, commit]).await?.success;
    if !ancestor {
        return Ok(found);
    }
    if found.edited {
        run(path, &["reset", "--hard", "--quiet"]).await?;
        run(path, &["clean", "-d", "--force", "--quiet"]).await?;
    }
    run(path, &["merge", "--ff-only", "--quiet", commit]).await?;
    describe(path, branch, created).await
}

/// Where pull request `id` is checked out: on its source branch `branch`
/// (a local branch tracking `remote`'s, made at `commit` when there is
/// none), never detached. Wherever that is checked out already (the opened
/// folder, a linked worktree) it is used there, moved to `commit` as
/// `follow` says (`Placement::Discard` throws its edits away). Checked out
/// nowhere, it is checked out as `placement` says when `create`, else `None`:
/// in the opened folder, or in a worktree of its own in `.arnold/worktrees`.
pub async fn pull_request_checkout(
    root: &Path,
    id: u64,
    commit: &str,
    branch: &str,
    remote: Option<&str>,
    create: bool,
    placement: Placement,
) -> Result<Option<CheckedOut<Worktree>>> {
    if branch.starts_with('-') || !run_raw(root, &["check-ref-format", "--branch", branch]).await?.success {
        return Err(GitError::Parse(format!("not a branch name: {branch}")));
    }
    let remote = match remote {
        Some(name) if !remotes(root).await?.iter().any(|known| known.name == name) => {
            return Err(GitError::Parse(format!("not a remote: {name}")));
        }
        other => other,
    };
    let discard = placement == Placement::Discard;
    // Worktrees deleted by hand, whose branches git would still call checked out.
    run(root, &["worktree", "prune"]).await?;
    let exists = has_branch(root, branch).await?;
    if let Some(path) = checked_out_at(root, branch, exists).await? {
        return follow(&path, branch, commit, discard, false).await.map(|found| Some(CheckedOut::Done(found)));
    }
    if !create {
        return Ok(None);
    }
    let path = if placement == Placement::Worktree {
        add_worktree(root, branch, exists, commit).await?
    } else {
        if let Some(changes) = clear_folder(root, placement, &format!("pull request !{id}")).await? {
            return Ok(Some(changes));
        }
        if exists {
            run(root, &["switch", "--quiet", branch]).await?;
        } else {
            run(root, &["switch", "--quiet", "--no-track", "--create", branch, commit]).await?;
        }
        root.to_path_buf()
    };
    if let (false, Some(remote)) = (exists, remote) {
        track(root, branch, remote).await?;
    }
    follow(&path, branch, commit, false, true).await.map(|made| Some(CheckedOut::Done(made)))
}

/// Where a branch or pull request checked out nowhere yet is checked out.
#[derive(Debug, Clone, Copy, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
pub enum Placement {
    /// In the opened folder, unless it has uncommitted changes.
    Here,
    /// In the opened folder, stashing its uncommitted changes first
    /// (untracked files included), so `git stash pop` brings them back.
    Stash,
    /// In the opened folder, throwing its uncommitted changes away first
    /// (untracked files included).
    Discard,
    /// In a worktree of its own in `.arnold/worktrees`, leaving the opened
    /// folder as it is.
    Worktree,
}

/// What came of checking something out.
#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
#[serde(tag = "kind", rename_all = "camelCase")]
pub enum CheckedOut<T> {
    Done(T),
    /// Nothing was checked out: the opened folder has uncommitted changes
    /// (`Placement::Here`), `files` of them, `untracked` of those new files.
    Changes { files: usize, untracked: usize },
}

/// Readies the opened folder for checking `what` out as `placement` says:
/// `None` when it can go ahead (no uncommitted changes, or they were stashed
/// or thrown away), else the changes that stop it.
async fn clear_folder<T>(root: &Path, placement: Placement, what: &str) -> Result<Option<CheckedOut<T>>> {
    let status = run_text(root, &["status", "--porcelain", "--untracked-files=all"]).await?;
    if status.is_empty() {
        return Ok(None);
    }
    if placement == Placement::Stash {
        let message = format!("Arnold: before checking out {what}");
        run(root, &["stash", "push", "--include-untracked", "--quiet", "--message", &message]).await?;
        return Ok(None);
    }
    if placement == Placement::Discard {
        run(root, &["reset", "--hard", "--quiet"]).await?;
        run(root, &["clean", "-d", "--force", "--quiet"]).await?;
        return Ok(None);
    }
    let files = status.lines().count();
    let untracked = status.lines().filter(|line| line.starts_with("??")).count();
    Ok(Some(CheckedOut::Changes { files, untracked }))
}

/// Where `branch` is checked out (the opened folder, a linked worktree), or,
/// when it is checked out nowhere, checks it out as `placement` says. A
/// remote branch (`origin/x`) is checked out as the local branch `x`,
/// created to track it when there is none.
pub async fn branch_checkout(root: &Path, branch: &str, placement: Placement) -> Result<CheckedOut<BranchCheckout>> {
    if placement == Placement::Worktree {
        return branch_worktree(root, branch).await.map(CheckedOut::Done);
    }
    // Worktrees deleted by hand, whose branches git would still call checked out.
    run(root, &["worktree", "prune"]).await?;
    let (local, remote) = local_name(root, branch).await?;
    let exists = has_branch(root, &local).await?;
    if let Some(path) = checked_out_at(root, &local, exists).await? {
        return Ok(CheckedOut::Done(BranchCheckout { branch: local, path, created: false }));
    }
    if let Some(changes) = clear_folder(root, placement, &local).await? {
        return Ok(changes);
    }
    if exists {
        run(root, &["switch", "--quiet", &local]).await?;
    } else {
        run(root, &["switch", "--quiet", "--no-track", "--create", &local, branch]).await?;
        if let Some(remote) = remote {
            track(root, &local, &remote).await?;
        }
    }
    Ok(CheckedOut::Done(BranchCheckout { branch: local, path: root.to_path_buf(), created: true }))
}

/// Where the local branch `local` is checked out, if it is.
async fn checked_out_at(root: &Path, local: &str, exists: bool) -> Result<Option<PathBuf>> {
    if !exists {
        return Ok(None);
    }
    let listed = run_text(root, &["for-each-ref", "--format=%(worktreepath)", &format!("refs/heads/{local}")]).await?;
    Ok((!listed.is_empty()).then(|| PathBuf::from(listed)))
}

/// The upstream of a new branch, set by hand: `--track` refuses a remote
/// branch the remote's fetch setting does not cover (a single-branch clone,
/// whose other branches Arnold fetches all the same).
async fn track(root: &Path, local: &str, remote: &str) -> Result<()> {
    run(root, &["config", &format!("branch.{local}.remote"), remote]).await?;
    run(root, &["config", &format!("branch.{local}.merge"), &format!("refs/heads/{local}")]).await?;
    Ok(())
}

/// Where a branch is checked out.
#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
pub struct BranchCheckout {
    /// The local branch checked out: the one asked for, or for a remote
    /// branch (`origin/x`) the local branch `x`.
    pub branch: String,
    pub path: PathBuf,
    /// Whether it was checked out just now, in a new worktree.
    pub created: bool,
}

/// Where `branch` is checked out (the opened folder, a linked worktree),
/// or, when it is checked out nowhere, a new worktree of it in
/// `.arnold/worktrees/<branch>`. A remote branch (`origin/x`) is checked
/// out as the local branch `x`, created to track it when there is none.
pub async fn branch_worktree(root: &Path, branch: &str) -> Result<BranchCheckout> {
    // Worktrees deleted by hand, whose branches git would still call checked out.
    run(root, &["worktree", "prune"]).await?;
    let (local, remote) = local_name(root, branch).await?;
    let exists = has_branch(root, &local).await?;
    if let Some(path) = checked_out_at(root, &local, exists).await? {
        return Ok(BranchCheckout { branch: local, path, created: false });
    }
    let path = add_worktree(root, &local, exists, branch).await?;
    if let (false, Some(remote)) = (exists, remote) {
        track(root, &local, &remote).await?;
    }
    Ok(BranchCheckout { branch: local, path, created: true })
}

/// A new worktree of the local branch `local` in `.arnold/worktrees`, made
/// at `start` when the branch is new (its upstream is set by hand: `track`).
async fn add_worktree(root: &Path, local: &str, exists: bool, start: &str) -> Result<PathBuf> {
    let path = free_folder(root, &slug(local)).await?;
    let target = path.to_string_lossy().into_owned();
    let args: Vec<&str> = if exists {
        vec!["worktree", "add", "--quiet", &target, local]
    } else {
        vec!["worktree", "add", "--quiet", "--no-track", "-b", local, &target, start]
    };
    let out = run_raw(root, &args).await?;
    if !out.success {
        return Err(GitError::Failed { command: "worktree add".into(), stderr: out.stderr });
    }
    Ok(path)
}

async fn has_branch(root: &Path, name: &str) -> Result<bool> {
    Ok(run_raw(root, &["rev-parse", "--verify", "--quiet", &format!("refs/heads/{name}")]).await?.success)
}

/// The local branch for `branch`: itself unless it is only a remote branch,
/// then with the remote it is on.
async fn local_name(root: &Path, branch: &str) -> Result<(String, Option<String>)> {
    if has_branch(root, branch).await? {
        return Ok((branch.to_string(), None));
    }
    for remote in remotes(root).await? {
        if let Some(name) = branch.strip_prefix(&format!("{}/", remote.name)) {
            return Ok((name.to_string(), Some(remote.name)));
        }
    }
    Ok((branch.to_string(), None))
}

/// `<root>/.arnold/worktrees/<name>`, or `<name>-2`, `-3`… when that is taken
/// (by a pull request's worktree, or a branch whose name has the same slug).
async fn free_folder(root: &Path, name: &str) -> Result<PathBuf> {
    let folder = app_folder(root).await?.join("worktrees");
    for n in 1..100 {
        let candidate = if n == 1 { folder.join(name) } else { folder.join(format!("{name}-{n}")) };
        if !tokio::fs::try_exists(&candidate).await.unwrap_or(true) {
            return Ok(candidate);
        }
    }
    Err(GitError::Failed { command: "worktree add".into(), stderr: format!("no free folder for {name} in {}", folder.display()) })
}

/// A folder name safe on every platform: `feature/agent work` → `feature-agent-work`.
fn slug(name: &str) -> String {
    let slug: String = name
        .chars()
        .map(|c| if c.is_ascii_alphanumeric() || c == '.' || c == '-' || c == '_' { c } else { '-' })
        .take(60)
        .collect();
    // Not hidden, and never `.` or `..`.
    let slug = slug.trim_start_matches('.');
    if slug.is_empty() { "branch".into() } else { slug.into() }
}

#[cfg(test)]
mod tests {
    use super::slug;

    #[test]
    fn folder_names_are_safe() {
        assert_eq!(slug("feature/agent work"), "feature-agent-work");
        assert_eq!(slug("../x"), "-x");
        assert_eq!(slug(".."), "branch");
    }
}
