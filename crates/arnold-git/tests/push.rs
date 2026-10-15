//! Pushing a reviewed commit to create a pull request: the commit and nothing
//! else, never forced.

use std::path::Path;
use std::process::Command;

use arnold_git::{count_commits, is_ancestor, push_commit, set_remote_tracking};

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

fn commit(root: &Path, path: &str, content: &str) -> String {
    std::fs::write(root.join(path), content).unwrap();
    git(root, &["add", "--all"]);
    git(root, &["commit", "--quiet", "-m", path]);
    git(root, &["rev-parse", "HEAD"])
}

#[tokio::test]
async fn pushes_the_reviewed_commit_without_forcing() {
    let server = tempfile::tempdir().unwrap();
    let server = server.path();
    git(server, &["init", "--quiet", "--bare", "--initial-branch=main"]);
    let url = format!("file://{}", server.display());

    let local = tempfile::tempdir().unwrap();
    let local = local.path();
    git(local, &["init", "--quiet", "--initial-branch=main"]);
    git(local, &["remote", "add", "origin", &url]);
    let base = commit(local, "a.txt", "a\n");
    git(local, &["push", "--quiet", "origin", "main"]);

    // The agent's branch: two commits reviewed, then one more after the review.
    git(local, &["checkout", "--quiet", "-b", "agent/task"]);
    commit(local, "b.txt", "b\n");
    let reviewed = commit(local, "c.txt", "c\n");
    let later = commit(local, "d.txt", "d\n");

    assert_eq!(count_commits(local, Some("main"), &reviewed).await.unwrap(), 2);
    assert_eq!(count_commits(local, None, &reviewed).await.unwrap(), 3);
    assert!(is_ancestor(local, &base, &reviewed).await);
    assert!(!is_ancestor(local, &later, &reviewed).await);
    assert!(!is_ancestor(local, "0123456789abcdef0123456789abcdef01234567", &reviewed).await);

    push_commit(local, &url, &reviewed, "agent/task", Some("X-Test: 1")).await.unwrap();
    assert_eq!(git(server, &["rev-parse", "refs/heads/agent/task"]), reviewed);
    // Only the branch was pushed; the local branch and its upstream are left alone.
    assert_eq!(git(local, &["rev-parse", "agent/task"]), later);
    assert!(!git(local, &["config", "--list"]).contains("branch.agent/task"));

    set_remote_tracking(local, "origin", "agent/task", &reviewed).await.unwrap();
    assert_eq!(git(local, &["rev-parse", "origin/agent/task"]), reviewed);

    // Fast-forwarding works; going back or sideways does not.
    push_commit(local, &url, &later, "agent/task", None).await.unwrap();
    assert_eq!(git(server, &["rev-parse", "refs/heads/agent/task"]), later);
    assert!(push_commit(local, &url, &reviewed, "agent/task", None).await.is_err());
    assert_eq!(git(server, &["rev-parse", "refs/heads/agent/task"]), later);

    assert!(push_commit(local, &url, "--force", "agent/task", None).await.is_err());
    assert!(push_commit(local, &url, &later, "bad..name", None).await.is_err());
}
