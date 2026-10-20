//! Getting what the remotes have: fetching every branch of a remote, so each
//! can be checked out, bringing local branches that only lag behind their
//! remote up to it, and pulling into the branch a checkout has checked out.
//! Never forced: a branch with commits of its own is only merged into when
//! its checkout is clean, and a merge that conflicts is undone.

use std::ffi::OsStr;
use std::path::Path;

use serde::Serialize;

use crate::cli::{run_raw, run_raw_with, run_text, GitError};
use crate::publish::BranchSync;
use crate::push::is_ancestor;
use crate::refs::list_branches;
use crate::Result;

/// Fetches every branch of `remote` into its remote-tracking branches
/// (`refs/remotes/<remote>/*`), whatever the remote's fetch setting says (a
/// single-branch clone fetches one), and drops those deleted there.
/// `extra_header` is sent with every HTTP request (credentials), as in
/// `fetch_objects`; without it git's own credentials are used.
pub async fn fetch_remote(root: &Path, remote: &str, extra_header: Option<&str>) -> Result<()> {
    if remote.is_empty() || remote.starts_with('-') || remote.contains(['/', '\\', '*', ':']) {
        return Err(GitError::Parse(format!("not a remote name: {remote}")));
    }
    let refspec = format!("+refs/heads/*:refs/remotes/{remote}/*");
    let args = ["fetch", "--quiet", "--prune", "--no-recurse-submodules", remote, &refspec];
    let mut environment: Vec<(&str, &OsStr)> = Vec::new();
    if let Some(header) = extra_header {
        environment.extend([
            ("GCM_INTERACTIVE", OsStr::new("never")),
            ("GIT_CONFIG_COUNT", OsStr::new("1")),
            ("GIT_CONFIG_KEY_0", OsStr::new("http.extraHeader")),
            ("GIT_CONFIG_VALUE_0", OsStr::new(header)),
        ]);
    }
    let output = run_raw_with(root, &args, &environment).await?;
    if output.success {
        Ok(())
    } else {
        Err(GitError::Failed { command: "fetch".into(), stderr: output.stderr })
    }
}

/// Moves each local branch that is checked out nowhere and only lags behind
/// its remote (as of the last fetch) up to the remote's commit. Names of the
/// branches moved.
pub async fn fast_forward_branches(root: &Path) -> Result<Vec<String>> {
    let mut moved = Vec::new();
    for branch in list_branches(root).await? {
        let Some(sync) = &branch.sync else { continue };
        if branch.is_remote || branch.worktree_path.is_some() || !sync.published || sync.ahead > 0 || sync.behind == 0 {
            continue;
        }
        let local = format!("refs/heads/{}", branch.name);
        let tracking = format!("refs/remotes/{}/{}", sync.remote, sync.remote_branch);
        let old = run_text(root, &["rev-parse", "--verify", &local]).await?;
        let new = run_text(root, &["rev-parse", "--verify", &tracking]).await?;
        if !is_ancestor(root, &old, &new).await {
            continue;
        }
        // With the old value, so a branch moved meanwhile is left alone.
        let message = format!("fast-forward to {tracking} by Arnold");
        if run_raw(root, &["update-ref", "-m", &message, &local, &new, &old]).await?.success {
            moved.push(branch.name);
        }
    }
    Ok(moved)
}

/// What pulling into a checked-out branch did.
#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
#[serde(rename_all = "camelCase", tag = "kind")]
pub enum Pulled {
    /// The remote's branch has nothing the local one lacks.
    UpToDate,
    /// The local branch had no commits of its own and now is where the remote's is.
    FastForwarded { commits: u32 },
    /// Both had commits of their own: the remote's were merged in.
    Merged { commits: u32 },
    /// The remote has no such branch yet: there is nothing to pull.
    NotPublished,
}

/// Brings `branch`, checked out in `checkout`, up to its remote branch as
/// `sync` (read after the fetch) says: a fast-forward when it has no commits
/// of its own, which keeps uncommitted changes that do not touch the same
/// files; else a merge, only in a clean checkout, undone when it conflicts.
pub async fn pull_branch(checkout: &Path, branch: &str, sync: &BranchSync) -> Result<Pulled> {
    if !sync.published {
        return Ok(Pulled::NotPublished);
    }
    if sync.behind == 0 {
        return Ok(Pulled::UpToDate);
    }
    let tracking = format!("refs/remotes/{}/{}", sync.remote, sync.remote_branch);
    let remote_name = format!("{}/{}", sync.remote, sync.remote_branch);
    if sync.ahead == 0 {
        let output = run_raw(checkout, &["merge", "--quiet", "--ff-only", &tracking]).await?;
        if !output.success {
            return Err(GitError::Failed {
                command: "merge".into(),
                stderr: format!("Could not bring {branch} up to {remote_name}.\n{}", output.stderr),
            });
        }
        return Ok(Pulled::FastForwarded { commits: sync.behind });
    }
    let status = run_raw(checkout, &["status", "--porcelain", "--untracked-files=no"]).await?;
    if !status.stdout.is_empty() {
        return Err(GitError::Failed {
            command: "merge".into(),
            stderr: format!(
                "{branch} and {remote_name} both have commits the other lacks, so pulling merges them. Commit or put back the uncommitted changes first."
            ),
        });
    }
    let message = format!("Merge {remote_name} into {branch}");
    let output = run_raw(checkout, &["merge", "--quiet", "--no-edit", "-m", &message, &tracking]).await?;
    if output.success {
        return Ok(Pulled::Merged { commits: sync.behind });
    }
    let conflicts = run_text(checkout, &["diff", "--name-only", "--diff-filter=U"]).await.unwrap_or_default();
    let merging = run_raw(checkout, &["rev-parse", "--verify", "--quiet", "MERGE_HEAD"]).await?.success;
    if merging {
        run_raw(checkout, &["merge", "--abort"]).await?;
    }
    let detail = if conflicts.is_empty() {
        output.stderr
    } else {
        format!("Merging {remote_name} into {branch} conflicts in:\n{conflicts}\nNothing was changed: merge it with git and resolve the conflicts there.")
    };
    Err(GitError::Failed { command: "merge".into(), stderr: detail })
}
