//! End-to-end checks against real temporary repositories.

use std::path::Path;
use std::process::Command;

use arnold_git::{compare, export_commit, list_files, open, read_blob, tree_files, FileStatus};

fn git(cwd: &Path, args: &[&str]) {
    let status = Command::new("git")
        .current_dir(cwd)
        .args(["-c", "user.name=Test", "-c", "user.email=test@example.com"])
        .args(args)
        .status()
        .expect("git runs");
    assert!(status.success(), "git {args:?} failed");
}

fn git_output(cwd: &Path, args: &[&str]) -> Vec<u8> {
    let output = Command::new("git").current_dir(cwd).args(args).output().expect("git runs");
    assert!(output.status.success(), "git {args:?} failed");
    output.stdout
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
async fn compares_commits_from_merge_base_ignoring_uncommitted_and_untracked() {
    let dir = fixture();
    let root = dir.path();
    write(root, "a.txt", "one\nTWO\nthree\nuncommitted\n");
    write(root, "sub/untracked.txt", "x\ny\n");
    git(root, &["add", "sub/untracked.txt"]);

    let comparison = compare(root, "main", "feature").await.unwrap();
    let tip = String::from_utf8(git_output(root, &["rev-parse", "feature"])).unwrap();
    assert_eq!(comparison.head_commit, tip.trim());

    let summary: Vec<_> = comparison
        .files
        .iter()
        .map(|f| (f.path.as_str(), f.status, f.additions, f.deletions))
        .collect();
    assert_eq!(
        summary,
        vec![
            // One line changed vs the merge-base; main's later edit and the
            // uncommitted line are not counted.
            ("a.txt", FileStatus::Modified, Some(1), Some(1)),
            ("b.txt", FileStatus::Deleted, Some(0), Some(1)),
            ("new.txt", FileStatus::Renamed, Some(0), Some(0)),
        ]
    );
    assert_eq!(comparison.files[2].old_path.as_deref(), Some("old.txt"));
}

#[tokio::test]
async fn branch_not_checked_out_is_compared_at_its_tip() {
    let dir = fixture();
    let root = dir.path();
    git(root, &["checkout", "--quiet", "main"]);

    let comparison = compare(root, "main", "feature").await.unwrap();
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
    write(&linked_path, "committed.txt", "hi\n");
    git(&linked_path, &["add", "committed.txt"]);
    git(&linked_path, &["commit", "--quiet", "-m", "in the worktree"]);

    let comparison = compare(root, "main", "feature-wt").await.unwrap();
    let paths: Vec<_> = comparison.files.iter().map(|f| f.path.as_str()).collect();
    assert_eq!(paths, vec!["a.txt", "b.txt", "committed.txt", "new.txt"]);

    let info = open(root).await.unwrap();
    assert_eq!(info.worktrees.len(), 2);
    let branch = info.branches.iter().find(|b| b.name == "feature-wt").unwrap();
    assert_eq!(
        branch.worktree_path.as_ref().unwrap().canonicalize().unwrap(),
        linked_path.canonicalize().unwrap()
    );
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

#[tokio::test]
async fn lists_files_of_the_tip_commit_only() {
    let dir = fixture();
    let root = dir.path();
    write(root, "sub/untracked.txt", "x\n");
    std::fs::remove_file(root.join("a.txt")).unwrap();

    let files = list_files(root, "feature").await.unwrap();
    assert_eq!(files, vec![".gitignore", "a.txt", "new.txt"]);
}

#[tokio::test]
async fn lists_files_with_content_ids() {
    let dir = fixture();
    let files = tree_files(dir.path(), "feature").await.unwrap();
    let paths: Vec<_> = files.iter().map(|f| f.path.as_str()).collect();
    assert_eq!(paths, vec![".gitignore", "a.txt", "new.txt"]);
    let base = tree_files(dir.path(), "main").await.unwrap();
    let old = base.iter().find(|f| f.path == "old.txt").unwrap();
    // A pure rename keeps the contents, so the id is the same.
    assert_eq!(files[2].oid, old.oid);
}

#[tokio::test]
async fn exports_commits_without_touching_the_repository() {
    let dir = fixture();
    let root = dir.path();
    write(root, "a.txt", "uncommitted\n");
    let status_before = git_output(root, &["status", "--porcelain"]);
    let target = tempfile::tempdir().unwrap();
    let checkout = target.path().join("checkout");
    let index = target.path().join("index");

    export_commit(root, "main", &checkout, &index).await.unwrap();
    let read = |path: &str| std::fs::read_to_string(checkout.join(path)).ok();
    assert_eq!(read("b.txt").as_deref(), Some("bye\n"));
    assert_eq!(read("a.txt").as_deref(), Some("one\ntwo\nthree\nmain moved on\n"));
    // Build output written next to the files.
    write(&checkout, "obj/out.dll", "built");

    export_commit(root, "feature", &checkout, &index).await.unwrap();
    assert_eq!(read("a.txt").as_deref(), Some("one\nTWO\nthree\n"));
    assert_eq!(read("b.txt"), None, "dropped by the commit");
    assert_eq!(read("old.txt"), None, "renamed by the commit");
    assert!(read("new.txt").is_some());
    assert_eq!(read("obj/out.dll").as_deref(), Some("built"), "untracked output kept");

    // A stale lock from an interrupted export does not get in the way.
    std::fs::write(index.with_extension("lock"), "").unwrap();
    export_commit(root, "main", &checkout, &index).await.unwrap();
    assert_eq!(read("b.txt").as_deref(), Some("bye\n"));

    assert_eq!(git_output(root, &["status", "--porcelain"]), status_before);
    assert_eq!(open(root).await.unwrap().worktrees.len(), 1);
    assert!(!checkout.join(".git").exists());
}
