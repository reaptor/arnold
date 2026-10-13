//! End-to-end checks against real temporary repositories.

use std::path::Path;
use std::process::Command;

use arnold_git::{compare, open, read_blob, FileStatus};

fn git(cwd: &Path, args: &[&str]) {
    let status = Command::new("git")
        .current_dir(cwd)
        .args(["-c", "user.name=Test", "-c", "user.email=test@example.com"])
        .args(args)
        .status()
        .expect("git runs");
    assert!(status.success(), "git {args:?} failed");
}

fn write(root: &Path, path: &str, content: &str) {
    let full = root.join(path);
    std::fs::create_dir_all(full.parent().unwrap()).unwrap();
    std::fs::write(full, content).unwrap();
}

fn commit_all(root: &Path, message: &str) {
    git(root, &["add", "--all"]);
    git(root, &["commit", "--quiet", "-m", message]);
}

/// main: a.txt, b.txt, old.txt → feature branches off → main gains main-only.txt.
/// feature commits: a.txt modified, b.txt deleted, old.txt renamed to new.txt.
fn fixture() -> tempfile::TempDir {
    let dir = tempfile::tempdir().unwrap();
    let root = dir.path();
    git(root, &["init", "--quiet", "--initial-branch=main"]);
    write(root, "a.txt", "one\ntwo\nthree\n");
    write(root, "b.txt", "bye\n");
    write(root, "old.txt", "line 1\nline 2\nline 3\nline 4\n");
    write(root, ".gitignore", "ignored/\n");
    commit_all(root, "base");
    git(root, &["branch", "feature"]);

    write(root, "main-only.txt", "later\n");
    write(root, "a.txt", "one\ntwo\nthree\nmain moved on\n");
    commit_all(root, "main moves on");

    git(root, &["checkout", "--quiet", "feature"]);
    write(root, "a.txt", "one\nTWO\nthree\n");
    std::fs::remove_file(root.join("b.txt")).unwrap();
    git(root, &["mv", "old.txt", "new.txt"]);
    commit_all(root, "feature work");
    dir
}

#[tokio::test]
async fn compares_from_merge_base_including_uncommitted_and_untracked() {
    let dir = fixture();
    let root = dir.path();
    write(root, "a.txt", "one\nTWO\nthree\nuncommitted\n");
    write(root, "sub/untracked.txt", "x\ny\n");
    write(root, "ignored/skip.txt", "nope\n");

    let comparison = compare(root, "main", "feature").await.unwrap();
    assert!(comparison.worktree_path.is_some());

    let summary: Vec<_> = comparison
        .files
        .iter()
        .map(|f| (f.path.as_str(), f.status, f.additions, f.deletions))
        .collect();
    assert_eq!(
        summary,
        vec![
            // Two lines changed vs the merge-base; main's later edit is not counted.
            ("a.txt", FileStatus::Modified, Some(2), Some(1)),
            ("b.txt", FileStatus::Deleted, Some(0), Some(1)),
            ("new.txt", FileStatus::Renamed, Some(0), Some(0)),
            ("sub/untracked.txt", FileStatus::Untracked, Some(2), Some(0)),
        ]
    );
    assert_eq!(comparison.files[2].old_path.as_deref(), Some("old.txt"));
}

#[tokio::test]
async fn branch_not_checked_out_is_compared_at_its_tip() {
    let dir = fixture();
    let root = dir.path();
    git(root, &["checkout", "--quiet", "main"]);
    write(root, "scratch.txt", "untracked on main\n");

    let comparison = compare(root, "main", "feature").await.unwrap();
    assert_eq!(comparison.worktree_path, None);
    let paths: Vec<_> = comparison.files.iter().map(|f| f.path.as_str()).collect();
    assert_eq!(paths, vec!["a.txt", "b.txt", "new.txt"]);
}

#[tokio::test]
async fn finds_branch_checked_out_in_linked_worktree() {
    let dir = fixture();
    let root = dir.path();
    let linked = tempfile::tempdir().unwrap();
    let linked_path = linked.path().join("wt");
    git(root, &["branch", "feature-wt", "feature"]);
    git(
        root,
        &["worktree", "add", "--quiet", linked_path.to_str().unwrap(), "feature-wt"],
    );
    write(&linked_path, "only-in-worktree.txt", "hi\n");

    let comparison = compare(root, "main", "feature-wt").await.unwrap();
    let worktree = comparison.worktree_path.expect("worktree found");
    assert_eq!(
        worktree.canonicalize().unwrap(),
        linked_path.canonicalize().unwrap()
    );
    assert!(comparison
        .files
        .iter()
        .any(|f| f.path == "only-in-worktree.txt" && f.status == FileStatus::Untracked));

    let info = open(root).await.unwrap();
    assert_eq!(info.worktrees.len(), 2);
    assert_eq!(info.current_branch.as_deref(), Some("feature"));
    assert_eq!(info.default_branch.as_deref(), Some("main"));
}

#[tokio::test]
async fn reads_base_content_and_reports_missing_files() {
    let dir = fixture();
    let root = dir.path();
    let comparison = compare(root, "main", "feature").await.unwrap();

    let base = read_blob(root, &comparison.merge_base, "a.txt").await.unwrap();
    assert_eq!(base.as_deref(), Some("one\ntwo\nthree\n".as_bytes()));
    let missing = read_blob(root, &comparison.merge_base, "new.txt").await.unwrap();
    assert_eq!(missing, None);
}
