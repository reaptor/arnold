//! Gives the branch under review a checkout of its own, so it can be edited
//! without switching the branch of any checkout the user already has.

use std::path::{Path, PathBuf};

use crate::cli::{run, run_raw, run_text};
use crate::compare::worktree_of;
use crate::Result;

/// Checkout holding `head`: the one it is already checked out in (main checkout
/// or a linked worktree), or else a linked worktree Arnold adds under `store`.
///
/// A local branch is checked out on itself. Anything else (a remote branch) gets
/// a detached checkout of its tip, moved along with the branch while it has no
/// local changes.
pub async fn ensure_checkout(root: &Path, head: &str, store: &Path) -> Result<PathBuf> {
    let local = run_raw(
        root,
        &["show-ref", "--verify", "--quiet", &format!("refs/heads/{head}")],
    )
    .await?
    .success;
    if local {
        if let Some(existing) = worktree_of(root, head).await? {
            if existing.is_dir() {
                return Ok(existing);
            }
            // Its checkout was deleted without telling git; let git forget it.
            run(root, &["worktree", "prune"]).await?;
        }
    }

    let path = store.join(repo_folder(root).await?).join(if local {
        PathBuf::from(head)
    } else {
        Path::new("detached").join(head)
    });
    if is_registered(root, &path).await? {
        if !local {
            follow_tip(&path, head).await?;
        }
        return Ok(path);
    }

    let target = path.to_string_lossy();
    let mut args = vec!["worktree", "add", "--quiet"];
    if !local {
        args.push("--detach");
    }
    args.extend([target.as_ref(), head]);
    run(root, &args).await?;
    Ok(path)
}

/// Moves a detached checkout to the current tip of `rev`, unless it has
/// changes or commits of its own (which moving would leave behind).
async fn follow_tip(worktree: &Path, rev: &str) -> Result<()> {
    let commit = format!("{rev}^{{commit}}");
    let tip_args = ["rev-parse", "--verify", commit.as_str()];
    let (at, tip, status) = tokio::try_join!(
        run_text(worktree, &["rev-parse", "HEAD"]),
        run_text(worktree, &tip_args),
        run_text(worktree, &["status", "--porcelain"]),
    )?;
    if at == tip || !status.is_empty() {
        return Ok(());
    }
    let behind_only = run_raw(worktree, &["merge-base", "--is-ancestor", &at, &tip])
        .await?
        .success;
    if behind_only {
        run(worktree, &["checkout", "--quiet", "--detach", &tip]).await?;
    }
    Ok(())
}

async fn is_registered(root: &Path, path: &Path) -> Result<bool> {
    let Ok(path) = path.canonicalize() else {
        return Ok(false);
    };
    let out = run_text(root, &["worktree", "list", "--porcelain"]).await?;
    Ok(out
        .lines()
        .filter_map(|line| line.strip_prefix("worktree "))
        .any(|listed| Path::new(listed).canonicalize().is_ok_and(|listed| listed == path)))
}

/// `<folder name>-<hash>`: readable, and distinct for repositories with the same name.
async fn repo_folder(root: &Path) -> Result<String> {
    let common = run_text(root, &["rev-parse", "--path-format=absolute", "--git-common-dir"]).await?;
    let common = Path::new(&common).canonicalize().unwrap_or_else(|_| common.into());
    let repo = if common.file_name().is_some_and(|name| name == ".git") {
        common.parent().unwrap_or(&common)
    } else {
        &common
    };
    let name = repo
        .file_name()
        .map(|name| name.to_string_lossy().trim_end_matches(".git").to_string())
        .unwrap_or_default();
    Ok(format!("{name}-{:08x}", fnv1a(common.to_string_lossy().as_bytes()) as u32))
}

/// Stable across builds, unlike `std`'s hasher, so the same repository keeps its folder.
fn fnv1a(bytes: &[u8]) -> u64 {
    bytes.iter().fold(0xcbf2_9ce4_8422_2325, |hash, byte| {
        (hash ^ u64::from(*byte)).wrapping_mul(0x0100_0000_01b3)
    })
}
