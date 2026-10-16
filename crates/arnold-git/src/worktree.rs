//! A registered git worktree of the app's own, detached at a commit: where a
//! pull request is checked out to be edited (its edits become suggestions on
//! the pull request rather than commits). It lives in `.arnold/worktrees`, so
//! the opened folder, its branch and its uncommitted changes stay untouched.

use std::path::{Path, PathBuf};

use serde::Serialize;

use crate::cli::{run, run_raw, run_text, GitError};
use crate::export::app_folder;
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
