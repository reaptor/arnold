use std::ffi::OsStr;
use std::path::Path;

use crate::cli::{run_raw, run_raw_with, run_text, GitError};
use crate::Result;

/// Pushes `commit` to branch `branch` (without `refs/heads/`) of `url`,
/// creating the branch or fast-forwarding it; never forced, so commits on
/// the branch that `commit` does not contain make it fail. Only the commit
/// is pushed, not whatever the local branch points at by now. `extra_header`
/// is sent with every HTTP request (credentials), through the environment
/// as in `fetch_objects`.
pub async fn push_commit(root: &Path, url: &str, commit: &str, branch: &str, extra_header: Option<&str>) -> Result<()> {
    if commit.len() < 7 || !commit.bytes().all(|byte| byte.is_ascii_hexdigit()) {
        return Err(GitError::Parse(format!("not a commit id: {commit}")));
    }
    let refspec = format!("{commit}:refs/heads/{branch}");
    let checked = run_raw(root, &["check-ref-format", &format!("refs/heads/{branch}")]).await?;
    if !checked.success {
        return Err(GitError::Parse(format!("not a branch name: {branch}")));
    }
    let args = ["push", "--quiet", "--no-recurse-submodules", url, &refspec];

    let mut environment: Vec<(&str, &OsStr)> = vec![("GCM_INTERACTIVE", OsStr::new("never"))];
    if let Some(header) = extra_header {
        environment.extend([
            ("GIT_CONFIG_COUNT", OsStr::new("1")),
            ("GIT_CONFIG_KEY_0", OsStr::new("http.extraHeader")),
            ("GIT_CONFIG_VALUE_0", OsStr::new(header)),
        ]);
    }
    let output = run_raw_with(root, &args, &environment).await?;
    if output.success {
        Ok(())
    } else {
        Err(GitError::Failed {
            command: "push".into(),
            stderr: output.stderr,
        })
    }
}

/// Points the remote-tracking branch `<remote>/<branch>` at `commit`, as
/// `git push <remote>` does after a push to that remote.
pub async fn set_remote_tracking(root: &Path, remote: &str, branch: &str, commit: &str) -> Result<()> {
    let name = format!("refs/remotes/{remote}/{branch}");
    run_text(root, &["update-ref", "-m", "push from Arnold", &name, commit]).await?;
    Ok(())
}

/// Whether `ancestor` is `descendant` or one of its ancestors. False also
/// when either is not in the repository.
pub async fn is_ancestor(root: &Path, ancestor: &str, descendant: &str) -> bool {
    matches!(
        run_raw(root, &["merge-base", "--is-ancestor", ancestor, descendant]).await,
        Ok(output) if output.success
    )
}

/// How many commits `to` has that `from` does not; all of its commits
/// without `from`.
pub async fn count_commits(root: &Path, from: Option<&str>, to: &str) -> Result<u32> {
    let not = from.map(|from| format!("^{from}"));
    let mut args = vec!["rev-list", "--count", to];
    args.extend(not.as_deref());
    args.push("--");
    let out = run_text(root, &args).await?;
    out.parse().map_err(|_| GitError::Parse(out))
}
