//! Pushing the checked-out branch from Commit mode: to its upstream, or to
//! `origin` as a new branch that becomes its upstream; never forced.

use std::path::Path;
use std::process::Command;

use arnold_git::{branch_sync, list_branches, push_branch, working_status, BranchSync};

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

#[tokio::test]
async fn the_branch_list_tells_what_is_not_pushed() {
    let server = tempfile::tempdir().unwrap();
    git(server.path(), &["init", "--quiet", "--bare", "--initial-branch=main"]);
    let url = format!("file://{}", server.path().display());
    let local = tempfile::tempdir().unwrap();
    let local = local.path();
    git(local, &["init", "--quiet", "--initial-branch=main"]);
    commit(local, "a.txt");
    let syncs = || async {
        list_branches(local)
            .await
            .unwrap()
            .into_iter()
            .map(|branch| (branch.name, branch.sync))
            .collect::<std::collections::BTreeMap<_, _>>()
    };
    assert_eq!(syncs().await["main"], None, "no remote");

    git(local, &["remote", "add", "origin", &url]);
    git(local, &["push", "--quiet", "--set-upstream", "origin", "main"]);
    let sync = |remote_branch: &str, published, ahead, behind| {
        Some(BranchSync {
            remote: "origin".into(),
            remote_branch: remote_branch.into(),
            published,
            ahead,
            behind,
        })
    };
    // Never pushed, no upstream: counted against everything origin has.
    git(local, &["branch", "agent/new"]);
    git(local, &["checkout", "--quiet", "-b", "agent/work"]);
    commit(local, "b.txt");
    commit(local, "c.txt");
    // Pushed under its own name but without an upstream.
    git(local, &["checkout", "--quiet", "-b", "agent/loose", "main"]);
    git(local, &["push", "--quiet", "origin", "agent/loose"]);
    commit(local, "d.txt");
    commit(local, "e.txt");
    commit(local, "f.txt");
    // An upstream whose branch was deleted on the remote.
    git(local, &["checkout", "--quiet", "-b", "agent/gone", "main"]);
    commit(local, "g.txt");
    git(local, &["push", "--quiet", "--set-upstream", "origin", "agent/gone"]);
    git(local, &["push", "--quiet", "origin", "--delete", "agent/gone"]);

    let all = syncs().await;
    assert_eq!(all["main"], sync("main", true, 0, 0));
    assert_eq!(all["agent/new"], sync("agent/new", false, 0, 0));
    assert_eq!(all["agent/work"], sync("agent/work", false, 2, 0));
    assert_eq!(all["agent/loose"], sync("agent/loose", true, 3, 0));
    assert_eq!(all["agent/gone"], sync("agent/gone", false, 1, 0));
    assert_eq!(all["origin/main"], None, "remote-tracking branches have none");
}
