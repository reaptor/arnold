//! Writing the files of a commit to a folder of the app's own, for tools that
//! need real files (a language server, `dotnet restore`).

use std::path::{Path, PathBuf};

use crate::cli::{run_raw_with, GitError};
use crate::Result;

/// `<root>/.arnold`: where the app keeps its copies of commits, inside the
/// checkout like Claude Code's `.claude/worktrees`. A `.gitignore` in it
/// ignores everything there (itself included), so the repository's status
/// does not change.
pub async fn app_folder(root: &Path) -> Result<PathBuf> {
    let folder = root.join(".arnold");
    let failed = |e: std::io::Error| GitError::Failed {
        command: "export".into(),
        stderr: format!("{}: {e}", folder.display()),
    };
    tokio::fs::create_dir_all(&folder).await.map_err(failed)?;
    let ignore = folder.join(".gitignore");
    if !tokio::fs::try_exists(&ignore).await.unwrap_or(false) {
        tokio::fs::write(&ignore, "# Copies of commits made by Arnold for the language server.\n*\n")
            .await
            .map_err(failed)?;
    }
    Ok(folder)
}

/// Makes `dir` hold exactly the files of `commit`.
///
/// The repository is left alone: no worktree is registered, and its index,
/// HEAD and branches are not touched. `index` is a private index file that
/// remembers what was written, so a later commit only rewrites the files that
/// changed and removes the ones it dropped. Files git never wrote there (build
/// output such as `obj/`) are kept, so later restores and builds are quick.
pub async fn export_commit(root: &Path, commit: &str, dir: &Path, index: &Path) -> Result<()> {
    for folder in std::iter::once(dir).chain(index.parent()) {
        tokio::fs::create_dir_all(folder).await.map_err(|e| GitError::Failed {
            command: "export".into(),
            stderr: format!("{}: {e}", folder.display()),
        })?;
    }
    if read_tree(root, commit, dir, index).await.is_ok() {
        return Ok(());
    }
    // An earlier export was interrupted (a stale lock) or its index is
    // unreadable: start over from an empty index. Existing files are
    // overwritten, so nothing is lost but time.
    for stale in [index.to_path_buf(), index.with_extension("lock")] {
        let _ = tokio::fs::remove_file(stale).await;
    }
    read_tree(root, commit, dir, index).await
}

async fn read_tree(root: &Path, commit: &str, dir: &Path, index: &Path) -> Result<()> {
    let args = [
        // The repository's sparse-checkout patterns would leave files out.
        "-c",
        "core.sparseCheckout=false",
        "read-tree",
        "-u",
        "--reset",
        commit,
    ];
    let environment = [
        ("GIT_INDEX_FILE", index.as_os_str()),
        ("GIT_WORK_TREE", dir.as_os_str()),
    ];
    let output = run_raw_with(root, &args, &environment).await?;
    if output.success {
        Ok(())
    } else {
        Err(GitError::Failed {
            command: "read-tree".into(),
            stderr: output.stderr,
        })
    }
}
