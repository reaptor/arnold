//! Checking a branch or a pull request out, so it can be edited and the
//! language server runs on its files. By default in the opened folder
//! (`Placement::Here`), which is refused while that has uncommitted changes:
//! then they are either stashed (`Placement::Stash`), thrown away
//! (`Placement::Discard`) or left alone by
//! checking out in a registered git worktree of the app's own, in
//! `.arnold/worktrees` (`Placement::Worktree`):
//! - `branch_checkout` / `branch_worktree`: a branch checked out nowhere;
//! - `pull_request_checkout` / `detached_worktree`: detached at a commit,
//!   where a pull request is checked out (its edits become suggestions on the
//!   pull request rather than commits); your own pull request is edited on
//!   its branch instead, where that is checked out.

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
    /// Whether it has uncommitted changes (edits not suggested yet).
    pub edited: bool,
    /// The local branch checked out there, for your own pull request edited
    /// on its branch; `None` when detached at the pull request's commit.
    pub branch: Option<String>,
}

/// `<root>/.arnold/worktrees/<name>`.
async fn folder(root: &Path, name: &str) -> Result<PathBuf> {
    if name.is_empty() || name.contains(['/', '\\']) || name.starts_with('.') {
        return Err(GitError::Parse(format!("not a worktree name: {name}")));
    }
    Ok(app_folder(root).await?.join("worktrees").join(name))
}

async fn registered(root: &Path, path: &Path) -> Result<bool> {
    let out = run_text(root, &["worktree", "list", "--porcelain"]).await?;
    let wanted = path.canonicalize().ok();
    Ok(out
        .lines()
        .filter_map(|line| line.strip_prefix("worktree "))
        .any(|listed| Path::new(listed).canonicalize().ok().is_some_and(|listed| Some(listed) == wanted)))
}

async fn describe(path: &Path) -> Result<Worktree> {
    let (head, status) = tokio::try_join!(
        run_text(path, &["rev-parse", "--verify", "HEAD^{commit}"]),
        run(path, &["status", "--porcelain", "-z", "--untracked-files=normal"]),
    )?;
    Ok(Worktree { path: path.to_path_buf(), head, edited: !status.is_empty(), branch: None })
}

/// The app's worktree `name`, if it has been made.
pub async fn find_worktree(root: &Path, name: &str) -> Result<Option<Worktree>> {
    let path = folder(root, name).await?;
    if !tokio::fs::try_exists(&path).await.unwrap_or(false) || !registered(root, &path).await? {
        return Ok(None);
    }
    describe(&path).await.map(Some)
}

/// The app's worktree `name` at `commit`: made if need be, and moved to
/// `commit` when it is elsewhere and has no uncommitted changes (with
/// changes it stays where it is, and says so by its `head`). With `discard`
/// its uncommitted changes are thrown away first, untracked files included.
pub async fn detached_worktree(root: &Path, name: &str, commit: &str, discard: bool) -> Result<Worktree> {
    let path = folder(root, name).await?;
    if let Some(found) = find_worktree(root, name).await? {
        return follow(found, commit, discard).await;
    }
    // A folder left from a worktree git has forgotten (or deleted by hand).
    run(root, &["worktree", "prune"]).await?;
    if tokio::fs::try_exists(&path).await.unwrap_or(false) {
        return Err(GitError::Failed {
            command: "worktree add".into(),
            stderr: format!("{} is in the way; delete it and try again.", path.display()),
        });
    }
    let target = path.to_string_lossy().into_owned();
    let out = run_raw(root, &["worktree", "add", "--quiet", "--detach", &target, commit]).await?;
    if !out.success {
        return Err(GitError::Failed { command: "worktree add".into(), stderr: out.stderr });
    }
    describe(&path).await
}

/// A detached checkout moved to `commit` when it is elsewhere and has no
/// uncommitted changes, or with `discard` after throwing them away.
async fn follow(found: Worktree, commit: &str, discard: bool) -> Result<Worktree> {
    let path = &found.path;
    if found.edited && discard {
        run(path, &["reset", "--hard", "--quiet"]).await?;
        run(path, &["clean", "-d", "--force", "--quiet"]).await?;
    }
    if found.head != commit && (!found.edited || discard) {
        run(path, &["checkout", "--quiet", "--detach", commit]).await?;
        return describe(path).await;
    }
    Ok(found)
}

/// Where pull request `id` is checked out: with `branch` (the source branch
/// of your own pull request), wherever that local branch is checked out,
/// as it is (never moved, nor its edits thrown away); else the opened
/// folder, when Arnold detached it there and it still is, else its worktree
/// `.arnold/worktrees/pr-<id>`. Moved to `commit` as `detached_worktree`
/// says (`Placement::Discard` throws edits away). Checked out nowhere, it
/// is checked out at `commit` as `placement` says when `create`, else `None`.
pub async fn pull_request_checkout(
    root: &Path,
    id: u64,
    commit: &str,
    branch: Option<&str>,
    create: bool,
    placement: Placement,
) -> Result<Option<CheckedOut<Worktree>>> {
    if let Some(branch) = branch {
        // Worktrees deleted by hand, whose branches git would still call checked out.
        run(root, &["worktree", "prune"]).await?;
        if let Some(path) = checked_out_at(root, branch, has_branch(root, branch).await?).await? {
            let found = describe(&path).await?;
            return Ok(Some(CheckedOut::Done(Worktree { branch: Some(branch.to_string()), ..found })));
        }
    }
    let discard = placement == Placement::Discard;
    let marker = app_folder(root).await?.join("pull-request");
    if let Some(found) = in_folder(root, &marker, id).await? {
        let moved = follow(found, commit, discard).await?;
        mark(&marker, id, &moved.head).await?;
        return Ok(Some(CheckedOut::Done(moved)));
    }
    let name = format!("pr-{id}");
    if find_worktree(root, &name).await?.is_some() {
        return detached_worktree(root, &name, commit, discard).await.map(|found| Some(CheckedOut::Done(found)));
    }
    if !create {
        return Ok(None);
    }
    if placement == Placement::Worktree {
        return detached_worktree(root, &name, commit, false).await.map(|made| Some(CheckedOut::Done(made)));
    }
    if let Some(changes) = clear_folder(root, placement, &format!("pull request !{id}")).await? {
        return Ok(Some(changes));
    }
    run(root, &["switch", "--quiet", "--detach", commit]).await?;
    mark(&marker, id, commit).await?;
    describe(root).await.map(|made| Some(CheckedOut::Done(made)))
}

/// The opened folder, when it has pull request `id` checked out: detached
/// at the commit Arnold last checked it out at (`mark`).
async fn in_folder(root: &Path, marker: &Path, id: u64) -> Result<Option<Worktree>> {
    let Ok(text) = tokio::fs::read_to_string(marker).await else { return Ok(None) };
    let Some((marked, at)) = text.trim().split_once(' ') else { return Ok(None) };
    if marked != id.to_string() || run_raw(root, &["symbolic-ref", "--quiet", "HEAD"]).await?.success {
        return Ok(None);
    }
    let found = describe(root).await?;
    Ok((found.head == at).then_some(found))
}

async fn mark(marker: &Path, id: u64, commit: &str) -> Result<()> {
    tokio::fs::write(marker, format!("{id} {commit}\n")).await.map_err(|e| GitError::Failed {
        command: "checkout".into(),
        stderr: format!("{}: {e}", marker.display()),
    })
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
    let path = free_folder(root, &slug(&local)).await?;
    let target = path.to_string_lossy().into_owned();
    // The upstream is set by hand (`track`).
    let args: Vec<&str> = if exists {
        vec!["worktree", "add", "--quiet", &target, &local]
    } else {
        vec!["worktree", "add", "--quiet", "--no-track", "-b", &local, &target, branch]
    };
    let out = run_raw(root, &args).await?;
    if !out.success {
        return Err(GitError::Failed { command: "worktree add".into(), stderr: out.stderr });
    }
    if let (false, Some(remote)) = (exists, remote) {
        track(root, &local, &remote).await?;
    }
    Ok(BranchCheckout { branch: local, path, created: true })
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
