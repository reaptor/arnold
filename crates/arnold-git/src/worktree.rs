//! Registered git worktrees of the app's own, in `.arnold/worktrees`, so the
//! opened folder, its branch and its uncommitted changes stay untouched:
//! - `branch_worktree`: a branch checked out nowhere else gets one, so it can
//!   be edited and the language server runs on its files;
//! - `detached_worktree`: one detached at a commit, where a pull request is
//!   checked out (its edits become suggestions on the pull request rather
//!   than commits).

use std::path::{Path, PathBuf};

use serde::Serialize;

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
    Ok(Worktree { path: path.to_path_buf(), head, edited: !status.is_empty() })
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
        if found.edited && discard {
            run(&path, &["reset", "--hard", "--quiet"]).await?;
            run(&path, &["clean", "-d", "--force", "--quiet"]).await?;
        }
        if found.head != commit && (!found.edited || discard) {
            run(&path, &["checkout", "--quiet", "--detach", commit]).await?;
            return describe(&path).await;
        }
        return Ok(found);
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
    if exists {
        let listed = run_text(root, &["for-each-ref", "--format=%(worktreepath)", &format!("refs/heads/{local}")]).await?;
        if !listed.is_empty() {
            return Ok(BranchCheckout { branch: local, path: PathBuf::from(listed), created: false });
        }
    }
    let path = free_folder(root, &slug(&local)).await?;
    let target = path.to_string_lossy().into_owned();
    // The upstream is set by hand: `--track` refuses a remote branch the
    // remote's fetch setting does not cover (a single-branch clone, whose
    // other branches Arnold fetches all the same).
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
        run(root, &["config", &format!("branch.{local}.remote"), &remote]).await?;
        run(root, &["config", &format!("branch.{local}.merge"), &format!("refs/heads/{local}")]).await?;
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
