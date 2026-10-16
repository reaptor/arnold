//! Pushing the checked-out branch from Commit mode: to its upstream, or to
//! `origin` as a new branch that becomes its upstream; never forced.

use std::path::Path;
use std::process::Command;

use arnold_git::{branch_sync, push_branch, working_status};

fn git(cwd: &Path, args: &[&str]) -> String {
    let output = Command::new("git")
        .current_dir(cwd)
        .args(["-c", "user.name=Test", "-c", "user.email=test@example.com"])
        .args(args)
        .output()
        .expect("git runs");
    assert!(output.status.success(), "git {args:?} failed: {}", String::from_utf8_lossy(&output.stderr));
    String::from_utf8_lossy(&output.stdout).trim().to_string()
}

fn commit(root: &Path, path: &str) {
    std::fs::write(root.join(path), format!("{path}\n")).unwrap();
    git(root, &["add", "--all"]);
    git(root, &["commit", "--quiet", "-m", path]);
}

#[tokio::test]
async fn pushes_the_branch_and_tracks_it() {
    let server = tempfile::tempdir().unwrap();
    git(server.path(), &["init", "--quiet", "--bare", "--initial-branch=main"]);
    let url = format!("file://{}", server.path().display());

    let local = tempfile::tempdir().unwrap();
    let local = local.path();
    git(local, &["init", "--quiet", "--initial-branch=main"]);
    assert!(working_status(local).await.unwrap().sync.is_none(), "no commit, no remote");
    commit(local, "a.txt");
    assert_eq!(branch_sync(local, "main").await.unwrap(), None, "no remote");
    git(local, &["remote", "add", "origin", &url]);
    git(local, &["push", "--quiet", "origin", "main"]);

    git(local, &["checkout", "--quiet", "-b", "agent/task"]);
    commit(local, "b.txt");
    commit(local, "c.txt");
    let sync = working_status(local).await.unwrap().sync.unwrap();
    assert_eq!((sync.remote.as_str(), sync.remote_branch.as_str()), ("origin", "agent/task"));
    assert_eq!((sync.published, sync.ahead, sync.behind), (false, 2, 0));

    push_branch(local, "agent/task", &sync, None).await.unwrap();
    assert_eq!(git(server.path(), &["rev-parse", "agent/task"]), git(local, &["rev-parse", "HEAD"]));
    assert_eq!(git(local, &["rev-parse", "--abbrev-ref", "agent/task@{u}"]), "origin/agent/task");
    let sync = branch_sync(local, "agent/task").await.unwrap().unwrap();
    assert_eq!((sync.published, sync.ahead, sync.behind), (true, 0, 0));

    commit(local, "d.txt");
    let sync = branch_sync(local, "agent/task").await.unwrap().unwrap();
    assert_eq!((sync.ahead, sync.behind), (1, 0));
    push_branch(local, "agent/task", &sync, None).await.unwrap();

    // Someone else pushed to the branch: a push is refused, not forced.
    let other = tempfile::tempdir().unwrap();
    git(other.path(), &["clone", "--quiet", "--branch", "agent/task", &url, "."]);
    commit(other.path(), "e.txt");
    git(other.path(), &["push", "--quiet"]);
    commit(local, "f.txt");
    git(local, &["fetch", "--quiet"]);
    let sync = branch_sync(local, "agent/task").await.unwrap().unwrap();
    assert_eq!((sync.ahead, sync.behind), (1, 1));
    let error = push_branch(local, "agent/task", &sync, None).await.unwrap_err().to_string();
    assert!(error.contains("Pull them first"), "{error}");
}
