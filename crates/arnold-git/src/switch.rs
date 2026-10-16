//! Checking a branch out in the opened folder, so its files can be edited
//! while it is compared. Only done when asked, and only when the folder has
//! nothing uncommitted that the switch could carry along or lose.

use std::path::Path;

use crate::cli::{run_raw, run_text, GitError};
use crate::remotes::remotes;
use crate::Result;

/// Checks `branch` out in `root` and returns the local branch now checked
/// out there: `branch` itself, or for a remote branch (`origin/x`) the local
/// branch `x`, created to track it when there is none.
pub async fn switch_branch(root: &Path, branch: &str) -> Result<String> {
    let status = run_text(root, &["status", "--porcelain", "-z", "--untracked-files=normal"]).await?;
    if !status.is_empty() {
        return Err(GitError::Failed {
            command: "switch".into(),
            stderr: "the folder has uncommitted changes. Commit or put them back first.".into(),
        });
    }
    let local = local_name(root, branch).await?;
    let out = run_raw(root, &["switch", "--quiet", &local]).await?;
    if !out.success {
        return Err(GitError::Failed {
            command: format!("switch {local}"),
            stderr: out.stderr,
        });
    }
    Ok(local)
}

/// The local branch for `branch`: itself unless it is only a remote branch.
async fn local_name(root: &Path, branch: &str) -> Result<String> {
    let is_local = run_raw(root, &["rev-parse", "--verify", "--quiet", &format!("refs/heads/{branch}")])
        .await?
        .success;
    if is_local {
        return Ok(branch.to_string());
    }
    for remote in remotes(root).await? {
        if let Some(name) = branch.strip_prefix(&format!("{}/", remote.name)) {
            return Ok(name.to_string());
        }
    }
    Ok(branch.to_string())
}
