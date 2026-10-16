use std::path::{Path, PathBuf};

use serde::Serialize;

use crate::cli::{run_raw, run_text};
use crate::Result;

#[derive(Debug, Clone, Serialize, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct Branch {
    /// Short name: `main`, `feature/x`, `origin/main`.
    pub name: String,
    pub is_remote: bool,
    /// Where this branch is checked out, if anywhere (main checkout or a linked worktree).
    pub worktree_path: Option<PathBuf>,
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

/// Local and remote-tracking branches, with where each local one is checked out.
pub async fn list_branches(root: &Path) -> Result<Vec<Branch>> {
    let out = run_text(
        root,
        &[
            "for-each-ref",
            "--format=%(refname)%09%(worktreepath)",
            "refs/heads",
            "refs/remotes",
        ],
    )
    .await?;
    Ok(out.lines().filter_map(parse_branch_line).collect())
}

fn parse_branch_line(line: &str) -> Option<Branch> {
    let (refname, worktree) = line.split_once('\t').unwrap_or((line, ""));
    let (name, is_remote) = match refname.strip_prefix("refs/heads/") {
        Some(name) => (name, false),
        None => (refname.strip_prefix("refs/remotes/")?, true),
    };
    // `origin/HEAD` is a pointer, not a branch.
    if is_remote && name.ends_with("/HEAD") {
        return None;
    }
    Some(Branch {
        name: name.to_string(),
        is_remote,
        worktree_path: (!worktree.is_empty()).then(|| PathBuf::from(worktree)),
    })
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
            parse_branch_line("refs/heads/feature/x\t/work/tree"),
            Some(Branch {
                name: "feature/x".into(),
                is_remote: false,
                worktree_path: Some("/work/tree".into()),
            })
        );
        assert_eq!(
            parse_branch_line("refs/remotes/origin/main\t"),
            Some(Branch {
                name: "origin/main".into(),
                is_remote: true,
                worktree_path: None,
            })
        );
        assert_eq!(parse_branch_line("refs/remotes/origin/HEAD\t"), None);
    }
}
