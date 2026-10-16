use std::ffi::OsStr;
use std::path::Path;
use std::process::Stdio;

use tokio::io::AsyncWriteExt;
use tokio::process::Command;

#[derive(Debug, thiserror::Error)]
pub enum GitError {
    #[error("could not run git: {0}")]
    Spawn(#[from] std::io::Error),
    #[error("git {command} failed: {stderr}")]
    Failed { command: String, stderr: String },
    #[error("unexpected git output: {0}")]
    Parse(String),
}

pub(crate) struct Output {
    pub success: bool,
    pub stdout: Vec<u8>,
    pub stderr: String,
}

/// Runs git in `cwd` and returns its output whatever the exit code.
pub(crate) async fn run_raw(cwd: &Path, args: &[&str]) -> Result<Output, GitError> {
    run_raw_with(cwd, args, &[]).await
}

/// `run_raw` with extra environment variables.
pub(crate) async fn run_raw_with(
    cwd: &Path,
    args: &[&str],
    environment: &[(&str, &OsStr)],
) -> Result<Output, GitError> {
    run_raw_full(cwd, args, environment, None).await
}

/// Runs git with extra environment variables and, if given, `input` on its stdin.
async fn run_raw_full(
    cwd: &Path,
    args: &[&str],
    environment: &[(&str, &OsStr)],
    input: Option<&[u8]>,
) -> Result<Output, GitError> {
    let mut command = Command::new("git");
    command
        .current_dir(cwd)
        // Paths as raw UTF-8, never quoted or octal-escaped.
        .args(["-c", "core.quotepath=false"])
        .args(args)
        // Never write the index as a side effect: an agent may be using the repo,
        // and our own file watcher would see the write and refresh in a loop.
        .env("GIT_OPTIONAL_LOCKS", "0")
        .env("GIT_TERMINAL_PROMPT", "0")
        .envs(environment.iter().copied())
        .stdin(if input.is_some() { Stdio::piped() } else { Stdio::null() })
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .kill_on_drop(true);
    #[cfg(windows)]
    command.creation_flags(0x0800_0000); // CREATE_NO_WINDOW

    let mut child = command.spawn()?;
    if let (Some(input), Some(mut stdin)) = (input, child.stdin.take()) {
        // Written while git runs; git may stop reading early (and fail), which
        // its exit code reports.
        let _ = stdin.write_all(input).await;
    }
    let output = child.wait_with_output().await?;
    Ok(Output {
        success: output.status.success(),
        stdout: output.stdout,
        stderr: String::from_utf8_lossy(&output.stderr).trim().to_string(),
    })
}

/// Runs git and fails on a non-zero exit code.
pub(crate) async fn run(cwd: &Path, args: &[&str]) -> Result<Vec<u8>, GitError> {
    let output = run_raw(cwd, args).await?;
    if output.success {
        Ok(output.stdout)
    } else {
        Err(GitError::Failed {
            command: command_name(args),
            stderr: output.stderr,
        })
    }
}

/// Like [`run`], writing `input` to git's stdin.
pub(crate) async fn run_with_input(cwd: &Path, args: &[&str], input: &[u8]) -> Result<Vec<u8>, GitError> {
    let output = run_raw_full(cwd, args, &[], Some(input)).await?;
    if output.success {
        Ok(output.stdout)
    } else {
        Err(GitError::Failed {
            command: command_name(args),
            stderr: output.stderr,
        })
    }
}

/// The git command in `args`, past any global options.
fn command_name(args: &[&str]) -> String {
    args.iter().find(|arg| !arg.starts_with('-')).copied().unwrap_or_default().to_string()
}

pub(crate) async fn run_text(cwd: &Path, args: &[&str]) -> Result<String, GitError> {
    let stdout = run(cwd, args).await?;
    Ok(String::from_utf8_lossy(&stdout).trim_end().to_string())
}
