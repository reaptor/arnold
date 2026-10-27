//! Pushing the branch a checkout has checked out, from Commit mode: where it
//! goes, how far it is ahead of what the remote has, and the push itself.
//! Never forced.

use std::ffi::OsStr;
use std::path::Path;

use serde::Serialize;

use crate::cli::{run_raw, run_raw_with, GitError};
use crate::remotes::{remotes, ScopedHeader};
use crate::Result;

/// Where the checked-out branch is pushed, and how it stands against it.
#[derive(Debug, Clone, Serialize, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct BranchSync {
    /// The remote the branch is pushed to: its upstream's, else `origin`
    /// (or the only remote).
    pub remote: String,
    /// The branch's name there.
    pub remote_branch: String,
    /// The remote has the branch (as far as the last fetch or push knows).
    pub published: bool,
    /// Commits a push would send: those the remote's branch lacks, or, when
    /// it has none, those on no branch of the remote.
    pub ahead: u32,
    /// Commits of the remote's branch the local one lacks: the push would be refused.
    pub behind: u32,
}

/// How `branch`, checked out in `checkout`, stands against where it is
/// pushed; `None` when the repository has no remote.
pub async fn branch_sync(checkout: &Path, branch: &str) -> Result<Option<BranchSync>> {
    let Some((remote, remote_branch)) = push_target(checkout, branch).await? else {
        return Ok(None);
    };
    sync_against(checkout, "HEAD", remote, remote_branch).await.map(Some)
}

/// How `local` (a ref, or `HEAD`) stands against `remote_branch` on `remote`,
/// from the remote-tracking branches: as of the last fetch or push.
pub(crate) async fn sync_against(
    checkout: &Path,
    local: &str,
    remote: String,
    remote_branch: String,
) -> Result<BranchSync> {
    let tracking = format!("refs/remotes/{remote}/{remote_branch}");
    let published = run_raw(checkout, &["rev-parse", "--verify", "--quiet", &tracking]).await?.success;
    let (ahead, behind) = if published {
        let range = format!("{tracking}...{local}");
        let out = run_raw(checkout, &["rev-list", "--count", "--left-right", &range, "--"]).await?;
        parse_left_right(&String::from_utf8_lossy(&out.stdout)).map_or((0, 0), |(behind, ahead)| (ahead, behind))
    } else {
        let not_on_remote = format!("--remotes={remote}");
        let out = run_raw(checkout, &["rev-list", "--count", local, "--not", &not_on_remote, "--"]).await?;
        (String::from_utf8_lossy(&out.stdout).trim().parse().unwrap_or(0), 0)
    };
    Ok(BranchSync {
        remote,
        remote_branch,
        published,
        ahead,
        behind,
    })
}

/// The upstream of `branch` (remote and branch name there), else `origin`
/// or the only remote under the same name.
async fn push_target(checkout: &Path, branch: &str) -> Result<Option<(String, String)>> {
    let remote = config(checkout, &format!("branch.{branch}.remote")).await?;
    let merge = config(checkout, &format!("branch.{branch}.merge")).await?;
    if let (Some(remote), Some(merge)) = (remote.filter(|remote| remote != "."), merge) {
        let name = merge.strip_prefix("refs/heads/").unwrap_or(&merge).to_string();
        return Ok(Some((remote, name)));
    }
    let all = remotes(checkout).await?;
    let remote = all
        .iter()
        .find(|remote| remote.name == "origin")
        .or_else(|| (all.len() == 1).then(|| &all[0]))
        .map(|remote| remote.name.clone());
    Ok(remote.map(|remote| (remote, branch.to_string())))
}

async fn config(checkout: &Path, key: &str) -> Result<Option<String>> {
    let out = run_raw(checkout, &["config", "--get", key]).await?;
    let value = String::from_utf8_lossy(&out.stdout).trim().to_string();
    Ok((out.success && !value.is_empty()).then_some(value))
}

fn parse_left_right(out: &str) -> Option<(u32, u32)> {
    let mut counts = out.split_whitespace().map(str::parse::<u32>);
    Some((counts.next()?.ok()?, counts.next()?.ok()?))
}

/// Pushes `branch`, checked out in `checkout`, to where `sync` says, and
/// makes that its upstream when it has none. Never forced: a remote branch
/// with commits the local one lacks makes it fail. `extra_header` is sent
/// to the remote's own URL only (credentials); without it git's own
/// credentials are used.
pub async fn push_branch(checkout: &Path, branch: &str, sync: &BranchSync, extra_header: Option<&ScopedHeader>) -> Result<()> {
    let refspec = format!("refs/heads/{branch}:refs/heads/{}", sync.remote_branch);
    let has_upstream = config(checkout, &format!("branch.{branch}.merge")).await?.is_some();
    let mut args = vec!["push", "--quiet", "--no-recurse-submodules"];
    if !has_upstream {
        args.push("--set-upstream");
    }
    args.extend([sync.remote.as_str(), refspec.as_str()]);

    let environment = extra_header.map(ScopedHeader::environment);
    let environment: Vec<(&str, &OsStr)> = environment.iter().flatten().map(|(key, value)| (*key, value.as_os_str())).collect();
    let output = run_raw_with(checkout, &args, &environment).await?;
    if output.success {
        return Ok(());
    }
    let stderr = if output.stderr.contains("rejected") {
        format!(
            "{}/{} has commits that {branch} lacks. Pull them first, then push.\n{}",
            sync.remote, sync.remote_branch, output.stderr
        )
    } else {
        output.stderr
    };
    Err(GitError::Failed {
        command: "push".into(),
        stderr,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn reads_left_right_counts() {
        assert_eq!(parse_left_right("2\t5\n"), Some((2, 5)));
        assert_eq!(parse_left_right(""), None);
    }
}
