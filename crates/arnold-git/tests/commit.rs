//! Committing and pushing against real temporary repositories and a bare remote.

use std::path::{Path, PathBuf};
use std::process::Command;

use arnold_git::{checkout_status, commit, ensure_checkout, push};

fn git(cwd: &Path, args: &[&str]) -> String {
    let out = Command::new("git")
        .current_dir(cwd)
        .args(["-c", "user.name=Test", "-c", "user.email=test@example.com"])
        .args(args)
        .output()
        .expect("git runs");
    assert!(out.status.success(), "git {args:?} failed: {}", String::from_utf8_lossy(&out.stderr));
    String::from_utf8_lossy(&out.stdout).trim().to_string()
}

fn write(root: &Path, path: &str, content: &str) {
    let full = root.join(path);
    std::fs::create_dir_all(full.parent().unwrap()).unwrap();
    std::fs::write(full, content).unwrap();
}

struct Fixture {
    _dir: tempfile::TempDir,
    remote: PathBuf,
    clone: PathBuf,
    store: PathBuf,
}

/// A bare remote with `main` and `feature`, cloned; the clone has `main`
/// checked out and commits as Test.
fn fixture() -> Fixture {
    let dir = tempfile::tempdir().unwrap();
    let seed = dir.path().join("seed");
    std::fs::create_dir(&seed).unwrap();
    git(&seed, &["init", "--quiet", "--initial-branch=main"]);
    write(&seed, "a.txt", "one\n");
    git(&seed, &["add", "--all"]);
    git(&seed, &["commit", "--quiet", "-m", "base"]);
    git(&seed, &["branch", "feature"]);

    let remote = dir.path().join("remote.git");
    git(dir.path(), &["clone", "--quiet", "--bare", "seed", "remote.git"]);
    git(dir.path(), &["clone", "--quiet", "remote.git", "clone"]);
    let clone = dir.path().join("clone");
    git(&clone, &["config", "user.name", "Test"]);
    git(&clone, &["config", "user.email", "test@example.com"]);
    let store = dir.path().join("store");
    Fixture { _dir: dir, remote, clone, store }
}

#[tokio::test]
async fn commits_chosen_files_and_publishes_a_new_branch() {
    let f = fixture();
    git(&f.clone, &["branch", "review"]);
    let worktree = ensure_checkout(&f.clone, "review", &f.store).await.unwrap();
    write(&worktree, "a.txt", "one\ntwo\n");
    write(&worktree, "new dir/b.txt", "b\n");
    write(&worktree, "later.txt", "not yet\n");

    let status = checkout_status(&worktree, "review").await.unwrap();
    assert_eq!(status.branch.as_deref(), Some("review"));
    let paths: Vec<_> = status.changes.iter().map(|c| (c.status, c.path.as_str())).collect();
    assert_eq!(paths, vec![('M', "a.txt"), ('A', "later.txt"), ('A', "new dir/b.txt")]);
    let target = status.push.unwrap();
    assert_eq!((target.remote.as_str(), target.branch.as_str()), ("origin", "review"));
    assert!(target.unpublished);

    let chosen = ["a.txt".to_string(), "new dir/b.txt".to_string()];
    commit(&worktree, "Review fixes", Some(&chosen)).await.unwrap();
    let status = checkout_status(&worktree, "review").await.unwrap();
    let left: Vec<_> = status.changes.iter().map(|c| c.path.as_str()).collect();
    assert_eq!(left, vec!["later.txt"]);
    assert_eq!(status.push.unwrap().ahead, 1);

    push(&worktree, "review").await.unwrap();
    assert_eq!(git(&f.remote, &["log", "-1", "--format=%s", "review"]), "Review fixes");
    let target = checkout_status(&worktree, "review").await.unwrap().push.unwrap();
    assert!(!target.unpublished);
    assert_eq!((target.ahead, target.behind), (0, 0));
    // The upstream is set, so plain `git push` / `git pull` work there too.
    assert_eq!(git(&worktree, &["rev-parse", "--abbrev-ref", "@{u}"]), "origin/review");
    // The opened checkout was not touched.
    assert_eq!(git(&f.clone, &["symbolic-ref", "--short", "HEAD"]), "main");
}

#[tokio::test]
async fn commits_everything_and_reports_a_branch_behind_its_remote() {
    let f = fixture();
    git(&f.clone, &["branch", "--track", "feature", "origin/feature"]);
    let worktree = ensure_checkout(&f.clone, "feature", &f.store).await.unwrap();
    std::fs::remove_file(worktree.join("a.txt")).unwrap();
    write(&worktree, "c.txt", "c\n");
    commit(&worktree, "Replace a with c", None).await.unwrap();
    assert!(checkout_status(&worktree, "feature").await.unwrap().changes.is_empty());

    // Someone else pushes to feature meanwhile.
    let other = f.clone.parent().unwrap().join("other");
    git(f.clone.parent().unwrap(), &["clone", "--quiet", "-b", "feature", "remote.git", "other"]);
    write(&other, "theirs.txt", "x\n");
    git(&other, &["add", "--all"]);
    git(&other, &["-c", "user.name=O", "-c", "user.email=o@example.com", "commit", "--quiet", "-m", "theirs"]);
    git(&other, &["push", "--quiet"]);
    git(&f.clone, &["fetch", "--quiet"]);

    let target = checkout_status(&worktree, "feature").await.unwrap().push.unwrap();
    assert_eq!((target.ahead, target.behind), (1, 1));
    assert!(push(&worktree, "feature").await.is_err());
}

#[tokio::test]
async fn pushes_a_detached_remote_branch_back_and_keeps_unpushed_commits() {
    let f = fixture();
    let worktree = ensure_checkout(&f.clone, "origin/feature", &f.store).await.unwrap();
    let status = checkout_status(&worktree, "origin/feature").await.unwrap();
    assert_eq!(status.branch, None);
    let target = status.push.unwrap();
    assert_eq!((target.remote.as_str(), target.branch.as_str()), ("origin", "feature"));

    write(&worktree, "a.txt", "edited\n");
    commit(&worktree, "Edit on the remote branch", None).await.unwrap();
    let unpushed = git(&worktree, &["rev-parse", "HEAD"]);
    // Picking the branch again must not move the checkout off its own commit.
    ensure_checkout(&f.clone, "origin/feature", &f.store).await.unwrap();
    assert_eq!(git(&worktree, &["rev-parse", "HEAD"]), unpushed);

    push(&worktree, "origin/feature").await.unwrap();
    assert_eq!(git(&f.remote, &["rev-parse", "feature"]), unpushed);
    let target = checkout_status(&worktree, "origin/feature").await.unwrap().push.unwrap();
    assert_eq!((target.ahead, target.behind), (0, 0));
}
