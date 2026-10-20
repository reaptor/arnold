//! Fetching every branch of a remote and pulling into the checked-out branch:
//! fast-forwards, merges only in a clean checkout, conflicts undone.

use std::path::Path;
use std::process::Command;

use arnold_git::{branch_sync, branch_worktree, fast_forward_branches, fetch_remote, list_branches, pull_branch, Pulled};

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

fn commit(root: &Path, path: &str, content: &str) {
    std::fs::write(root.join(path), content).unwrap();
    git(root, &["add", "--all"]);
    git(root, &["commit", "--quiet", "-m", path]);
}

/// A bare server with `main`, a clone of only `main` (as `--single-branch`
/// makes) and a second clone that pushes to it.
fn setup() -> (tempfile::TempDir, tempfile::TempDir, tempfile::TempDir) {
    let server = tempfile::tempdir().unwrap();
    git(server.path(), &["init", "--quiet", "--bare", "--initial-branch=main"]);
    let url = format!("file://{}", server.path().display());
    let other = tempfile::tempdir().unwrap();
    git(other.path(), &["clone", "--quiet", &url, "."]);
    commit(other.path(), "a.txt", "a\n");
    git(other.path(), &["push", "--quiet", "origin", "main"]);
    let local = tempfile::tempdir().unwrap();
    git(local.path(), &["clone", "--quiet", "--single-branch", &url, "."]);
    git(local.path(), &["config", "user.name", "Test"]);
    git(local.path(), &["config", "user.email", "test@example.com"]);
    (server, other, local)
}

#[tokio::test]
async fn fetches_every_branch_and_drops_deleted_ones() {
    let (_server, other, local) = setup();
    let (other, local) = (other.path(), local.path());
    git(other, &["checkout", "--quiet", "-b", "agent/one"]);
    commit(other, "b.txt", "b\n");
    git(other, &["push", "--quiet", "origin", "agent/one", "agent/one:agent/two"]);

    fetch_remote(local, "origin", None).await.unwrap();
    let remote = |branches: Vec<arnold_git::Branch>| {
        let mut names: Vec<String> = branches.into_iter().filter(|b| b.is_remote).map(|b| b.name).collect();
        names.sort();
        names
    };
    assert_eq!(remote(list_branches(local).await.unwrap()), ["origin/agent/one", "origin/agent/two", "origin/main"]);
    let tip = list_branches(local).await.unwrap().into_iter().find(|b| b.name == "origin/agent/one").unwrap();
    assert_eq!(tip.last_author, "Test");
    assert!(tip.updated_at > 0);

    git(other, &["push", "--quiet", "origin", "--delete", "agent/two"]);
    fetch_remote(local, "origin", None).await.unwrap();
    assert_eq!(remote(list_branches(local).await.unwrap()), ["origin/agent/one", "origin/main"]);
    assert!(fetch_remote(local, "--upload-pack=x", None).await.is_err());
}

#[tokio::test]
async fn fast_forwards_branches_checked_out_nowhere() {
    let (_server, other, local) = setup();
    let (other, local) = (other.path(), local.path());
    git(other, &["checkout", "--quiet", "-b", "agent/behind"]);
    git(other, &["push", "--quiet", "origin", "agent/behind", "agent/behind:agent/own"]);
    fetch_remote(local, "origin", None).await.unwrap();
    // As Arnold checks them out: the single-branch clone's fetch setting
    // covers only `main`.
    let behind = branch_worktree(local, "origin/agent/behind").await.unwrap();
    let own = branch_worktree(local, "origin/agent/own").await.unwrap();
    assert_eq!(git(local, &["config", "branch.agent/own.merge"]), "refs/heads/agent/own");
    commit(&own.path, "mine.txt", "mine\n");
    git(local, &["worktree", "remove", &behind.path.to_string_lossy()]);
    git(local, &["worktree", "remove", &own.path.to_string_lossy()]);

    commit(other, "b.txt", "b\n");
    git(other, &["push", "--quiet", "origin", "agent/behind", "agent/behind:agent/own", "agent/behind:main"]);
    fetch_remote(local, "origin", None).await.unwrap();
    let mut moved = fast_forward_branches(local).await.unwrap();
    moved.sort();
    // `main` is checked out and `agent/own` has a commit of its own.
    assert_eq!(moved, ["agent/behind"]);
    assert_eq!(git(local, &["rev-parse", "agent/behind"]), git(other, &["rev-parse", "HEAD"]));
}

#[tokio::test]
async fn pulls_into_the_checked_out_branch() {
    let (_server, other, local) = setup();
    let (other, local) = (other.path(), local.path());
    let sync = || async { branch_sync(local, "main").await.unwrap().unwrap() };
    assert_eq!(pull_branch(local, "main", &sync().await).await.unwrap(), Pulled::UpToDate);

    // Behind only, with an uncommitted edit elsewhere: fast-forwarded, edit kept.
    commit(other, "b.txt", "b\n");
    git(other, &["push", "--quiet"]);
    std::fs::write(local.join("a.txt"), "edited\n").unwrap();
    fetch_remote(local, "origin", None).await.unwrap();
    assert_eq!(pull_branch(local, "main", &sync().await).await.unwrap(), Pulled::FastForwarded { commits: 1 });
    assert_eq!(std::fs::read_to_string(local.join("a.txt")).unwrap(), "edited\n");
    assert_eq!(git(local, &["rev-parse", "HEAD"]), git(other, &["rev-parse", "HEAD"]));

    // Both moved on: refused with uncommitted changes, merged when clean.
    commit(other, "c.txt", "c\n");
    git(other, &["push", "--quiet"]);
    git(local, &["checkout", "--quiet", "--", "a.txt"]);
    commit(local, "d.txt", "d\n");
    std::fs::write(local.join("a.txt"), "edited\n").unwrap();
    fetch_remote(local, "origin", None).await.unwrap();
    let error = pull_branch(local, "main", &sync().await).await.unwrap_err().to_string();
    assert!(error.contains("Commit or put back"), "{error}");
    git(local, &["checkout", "--quiet", "--", "a.txt"]);
    assert_eq!(pull_branch(local, "main", &sync().await).await.unwrap(), Pulled::Merged { commits: 1 });
    assert!(local.join("c.txt").exists() && local.join("d.txt").exists());
    let after = sync().await;
    assert_eq!((after.ahead, after.behind), (2, 0));

    // A conflicting merge is undone.
    commit(other, "a.txt", "theirs\n");
    git(other, &["push", "--quiet"]);
    commit(local, "a.txt", "ours\n");
    let before = git(local, &["rev-parse", "HEAD"]);
    fetch_remote(local, "origin", None).await.unwrap();
    let error = pull_branch(local, "main", &sync().await).await.unwrap_err().to_string();
    assert!(error.contains("conflicts in:\na.txt"), "{error}");
    assert_eq!(git(local, &["rev-parse", "HEAD"]), before);
    assert_eq!(git(local, &["status", "--porcelain"]), "");
}

#[tokio::test]
async fn a_branch_not_on_the_remote_has_nothing_to_pull() {
    let (_server, _other, local) = setup();
    let local = local.path();
    git(local, &["checkout", "--quiet", "-b", "agent/new"]);
    let sync = branch_sync(local, "agent/new").await.unwrap().unwrap();
    assert_eq!(pull_branch(local, "agent/new", &sync).await.unwrap(), Pulled::NotPublished);
}
