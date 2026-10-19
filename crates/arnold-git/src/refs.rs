use std::path::{Path, PathBuf};

use serde::Serialize;

use crate::cli::{run_raw, run_text};
use crate::publish::{sync_against, BranchSync};
use crate::remotes::remotes;
use crate::Result;

#[derive(Debug, Clone, Serialize, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct Branch {
    /// Short name: `main`, `feature/x`, `origin/main`.
    pub name: String,
    pub is_remote: bool,
    /// Where this branch is checked out, if anywhere (main checkout or a linked worktree).
    pub worktree_path: Option<PathBuf>,
    /// How a local branch stands against where it is pushed (its upstream,
    /// else `origin` or the only remote): commits a push would send, or that
    /// the remote has no such branch. `None` for remote-tracking branches and
    /// in a repository without remotes.
    pub sync: Option<BranchSync>,
}

/// A local branch's upstream as `for-each-ref` reports it.
#[derive(Debug, PartialEq)]
struct Upstream {
    /// `.` for a local upstream.
    remote: String,
    /// The branch's name on the remote.
    remote_branch: String,
    /// `None` when the remote-tracking branch is gone (or was never fetched).
    track: Option<(u32, u32)>,
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct RepoInfo {
    /// Top level of the checkout that was opened.
    pub root: PathBuf,
    pub branches: Vec<Branch>,
    /// Branch checked out in `root`; `None` when HEAD is detached.
    pub current_branch: Option<String>,
    /// Best guess at the branch PRs target.
    pub default_branch: Option<String>,
    /// Every checkout of this repository (main one first).
    pub worktrees: Vec<PathBuf>,
}

/// Opens the repository containing `path`.
pub async fn open(path: &Path) -> Result<RepoInfo> {
    let root = PathBuf::from(run_text(path, &["rev-parse", "--show-toplevel"]).await?);
    let branches = list_branches(&root).await?;
    let current_branch = symbolic_ref(&root, "HEAD").await;
    let default_branch = default_branch(&root, &branches).await;
    let worktrees = list_worktrees(&root).await?;
    Ok(RepoInfo {
        root,
        branches,
        current_branch,
        default_branch,
        worktrees,
    })
}

/// The `.git` folder shared by every checkout of the repository at `root`:
/// where branches live.
pub async fn common_dir(root: &Path) -> Result<PathBuf> {
    let out = run_text(root, &["rev-parse", "--path-format=absolute", "--git-common-dir"]).await?;
    Ok(PathBuf::from(out))
}

/// The `.git` folder of the checkout at `root` itself: for a linked worktree
/// the one in the common dir's `worktrees`, which holds its index.
pub async fn git_dir(root: &Path) -> Result<PathBuf> {
    let out = run_text(root, &["rev-parse", "--path-format=absolute", "--git-dir"]).await?;
    Ok(PathBuf::from(out))
}

/// Local and remote-tracking branches, with where each local one is checked
/// out and how it stands against its remote.
pub async fn list_branches(root: &Path) -> Result<Vec<Branch>> {
    let out = run_text(
        root,
        &[
            "for-each-ref",
            "--format=%(refname)%09%(worktreepath)%09%(upstream:remotename)%09%(upstream:remoteref)%09%(upstream:track,nobracket)",
            "refs/heads",
            "refs/remotes",
        ],
    )
    .await?;
    let (mut branches, upstreams): (Vec<Branch>, Vec<Option<Upstream>>) =
        out.lines().filter_map(parse_branch_line).unzip();
    // A worktree deleted by hand that git has not pruned yet has nothing to read.
    for branch in &mut branches {
        if branch.worktree_path.as_deref().is_some_and(|path| !path.exists()) {
            branch.worktree_path = None;
        }
    }
    add_syncs(root, &mut branches, upstreams).await?;
    Ok(branches)
}

/// Fills in `sync` of the local branches. One with a fetched upstream has its
/// counts from `for-each-ref`; the others are counted here, against the
/// remote they would be pushed to, all at once.
async fn add_syncs(root: &Path, branches: &mut [Branch], upstreams: Vec<Option<Upstream>>) -> Result<()> {
    let mut fallback: Option<Option<String>> = None;
    let mut counts = tokio::task::JoinSet::new();
    for (index, (branch, upstream)) in branches.iter_mut().zip(upstreams).enumerate() {
        if branch.is_remote {
            continue;
        }
        let upstream = upstream.filter(|upstream| upstream.remote != ".");
        if let Some(Upstream {
            remote,
            remote_branch,
            track: Some((ahead, behind)),
        }) = upstream
        {
            branch.sync = Some(BranchSync {
                remote,
                remote_branch,
                published: true,
                ahead,
                behind,
            });
            continue;
        }
        let (remote, remote_branch) = match upstream {
            Some(upstream) => (upstream.remote, upstream.remote_branch),
            None => {
                if fallback.is_none() {
                    let all = remotes(root).await?;
                    let remote = all
                        .iter()
                        .find(|remote| remote.name == "origin")
                        .or_else(|| (all.len() == 1).then(|| &all[0]))
                        .map(|remote| remote.name.clone());
                    fallback = Some(remote);
                }
                let Some(Some(remote)) = &fallback else { continue };
                (remote.clone(), branch.name.clone())
            }
        };
        let root = root.to_path_buf();
        let local = format!("refs/heads/{}", branch.name);
        counts.spawn(async move { (index, sync_against(&root, &local, remote, remote_branch).await) });
    }
    while let Some(done) = counts.join_next().await {
        let (index, sync) = done.expect("counting task does not panic");
        branches[index].sync = Some(sync?);
    }
    Ok(())
}

fn parse_branch_line(line: &str) -> Option<(Branch, Option<Upstream>)> {
    let mut fields = line.split('\t');
    let refname = fields.next()?;
    let worktree = fields.next().unwrap_or("");
    let (name, is_remote) = match refname.strip_prefix("refs/heads/") {
        Some(name) => (name, false),
        None => (refname.strip_prefix("refs/remotes/")?, true),
    };
    // `origin/HEAD` is a pointer, not a branch.
    if is_remote && name.ends_with("/HEAD") {
        return None;
    }
    let upstream = match (fields.next(), fields.next(), fields.next()) {
        (Some(remote), Some(remote_ref), track) if !remote.is_empty() && !remote_ref.is_empty() => Some(Upstream {
            remote: remote.to_string(),
            remote_branch: remote_ref.strip_prefix("refs/heads/").unwrap_or(remote_ref).to_string(),
            track: parse_track(track.unwrap_or("")),
        }),
        _ => None,
    };
    let branch = Branch {
        name: name.to_string(),
        is_remote,
        worktree_path: (!worktree.is_empty()).then(|| PathBuf::from(worktree)),
        sync: None,
    };
    Some((branch, upstream))
}

/// `%(upstream:track,nobracket)`: "ahead 2, behind 1", "ahead 2", "" when
/// level, "gone" without the remote-tracking branch. As (ahead, behind).
fn parse_track(track: &str) -> Option<(u32, u32)> {
    if track == "gone" {
        return None;
    }
    let (mut ahead, mut behind) = (0, 0);
    for part in track.split(", ").filter(|part| !part.is_empty()) {
        let (word, count) = part.split_once(' ')?;
        let count = count.parse().ok()?;
        match word {
            "ahead" => ahead = count,
            "behind" => behind = count,
            _ => return None,
        }
    }
    Some((ahead, behind))
}

async fn symbolic_ref(root: &Path, name: &str) -> Option<String> {
    let out = run_raw(root, &["symbolic-ref", "--short", "-q", name])
        .await
        .ok()?;
    let value = String::from_utf8_lossy(&out.stdout).trim().to_string();
    (out.success && !value.is_empty()).then_some(value)
}

async fn default_branch(root: &Path, branches: &[Branch]) -> Option<String> {
    let has = |name: &str| branches.iter().any(|b| b.name == name);
    // The remote's default branch, preferring a local branch of the same name.
    if let Some(remote_head) = symbolic_ref(root, "refs/remotes/origin/HEAD").await {
        let local = remote_head.strip_prefix("origin/").unwrap_or(&remote_head);
        if has(local) {
            return Some(local.to_string());
        }
        if has(&remote_head) {
            return Some(remote_head);
        }
    }
    ["main", "master", "develop", "origin/main", "origin/master"]
        .into_iter()
        .find(|name| has(name))
        .map(str::to_string)
}

async fn list_worktrees(root: &Path) -> Result<Vec<PathBuf>> {
    let out = run_text(root, &["worktree", "list", "--porcelain"]).await?;
    Ok(out
        .lines()
        .filter_map(|line| line.strip_prefix("worktree "))
        .map(PathBuf::from)
        .collect())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_local_remote_and_checked_out_branches() {
        assert_eq!(
            parse_branch_line("refs/heads/feature/x\t/work/tree\t\t\t"),
            Some((
                Branch {
                    name: "feature/x".into(),
                    is_remote: false,
                    worktree_path: Some("/work/tree".into()),
                    sync: None,
                },
                None
            ))
        );
        assert_eq!(
            parse_branch_line("refs/remotes/origin/main\t\t\t\t"),
            Some((
                Branch {
                    name: "origin/main".into(),
                    is_remote: true,
                    worktree_path: None,
                    sync: None,
                },
                None
            ))
        );
        assert_eq!(parse_branch_line("refs/remotes/origin/HEAD\t\t\t\t"), None);
    }

    #[test]
    fn parses_upstreams() {
        let upstream = |line: &str| parse_branch_line(line).and_then(|(_, upstream)| upstream);
        assert_eq!(
            upstream("refs/heads/x\t\torigin\trefs/heads/agent/x\tahead 2, behind 1"),
            Some(Upstream {
                remote: "origin".into(),
                remote_branch: "agent/x".into(),
                track: Some((2, 1)),
            })
        );
        assert_eq!(upstream("refs/heads/x\t\torigin\trefs/heads/x\t").unwrap().track, Some((0, 0)));
        assert_eq!(upstream("refs/heads/x\t\torigin\trefs/heads/x\tbehind 3").unwrap().track, Some((0, 3)));
        assert_eq!(upstream("refs/heads/x\t\torigin\trefs/heads/x\tgone").unwrap().track, None);
    }
}
