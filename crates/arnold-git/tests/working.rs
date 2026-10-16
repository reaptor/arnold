//! Staging, unstaging, putting back and committing in real temporary repositories.

use std::path::Path;
use std::process::Command;

use arnold_git::{
    commit, discard, read_index, reset_to_head, set_index_text, stage, undo_edits, unstage, working_status, worktree_files,
    write_worktree, FileStatus,
};

fn git(cwd: &Path, args: &[&str]) -> String {
    let output = Command::new("git")
        .current_dir(cwd)
        .args(["-c", "user.name=Test", "-c", "user.email=test@example.com"])
        .args(args)
        .output()
        .expect("git runs");
    assert!(output.status.success(), "git {args:?} failed: {}", String::from_utf8_lossy(&output.stderr));
    String::from_utf8_lossy(&output.stdout).into_owned()
}

fn write(root: &Path, path: &str, content: &str) {
    let full = root.join(path);
    std::fs::create_dir_all(full.parent().unwrap()).unwrap();
    std::fs::write(full, content).unwrap();
}

fn read(root: &Path, path: &str) -> String {
    std::fs::read_to_string(root.join(path)).unwrap()
}

/// a.txt and b.txt committed on main.
fn fixture() -> tempfile::TempDir {
    let dir = tempfile::tempdir().unwrap();
    let root = dir.path();
    git(root, &["init", "--quiet", "--initial-branch=main"]);
    git(root, &["config", "user.name", "Test"]);
    git(root, &["config", "user.email", "test@example.com"]);
    write(root, "a.txt", "one\ntwo\nthree\n");
    write(root, "b.txt", "bee\n");
    git(root, &["add", "--all"]);
    git(root, &["commit", "--quiet", "-m", "base"]);
    dir
}

fn summary(files: &[arnold_git::ChangedFile]) -> Vec<(&str, FileStatus)> {
    files.iter().map(|file| (file.path.as_str(), file.status)).collect()
}

#[tokio::test]
async fn lists_unstaged_untracked_and_staged_changes() {
    let dir = fixture();
    let root = dir.path();
    write(root, "a.txt", "one\nTWO\nthree\n");
    write(root, "new/c.txt", "c1\nc2\n");
    std::fs::remove_file(root.join("b.txt")).unwrap();
    git(root, &["add", "b.txt"]);

    let status = working_status(root).await.unwrap();
    assert_eq!(status.branch.as_deref(), Some("main"));
    assert!(status.head.is_some());
    assert_eq!(
        summary(&status.unstaged),
        vec![("a.txt", FileStatus::Modified), ("new/c.txt", FileStatus::Added)]
    );
    assert_eq!(status.unstaged[1].additions, Some(2));
    assert_eq!(summary(&status.staged), vec![("b.txt", FileStatus::Deleted)]);
}

#[tokio::test]
async fn stages_and_unstages_whole_files() {
    let dir = fixture();
    let root = dir.path();
    write(root, "a.txt", "changed\n");
    write(root, "c.txt", "new\n");
    std::fs::remove_file(root.join("b.txt")).unwrap();

    let paths = ["a.txt", "b.txt", "c.txt"].map(String::from);
    stage(root, &paths).await.unwrap();
    let status = working_status(root).await.unwrap();
    assert!(status.unstaged.is_empty());
    assert_eq!(
        summary(&status.staged),
        vec![("a.txt", FileStatus::Modified), ("b.txt", FileStatus::Deleted), ("c.txt", FileStatus::Added)]
    );

    unstage(root, &paths).await.unwrap();
    let status = working_status(root).await.unwrap();
    assert!(status.staged.is_empty());
    assert_eq!(status.unstaged.len(), 3);
    assert_eq!(read(root, "a.txt"), "changed\n");
}

#[tokio::test]
async fn stages_some_lines_through_the_index_text_keeping_crlf() {
    let dir = fixture();
    let root = dir.path();
    std::fs::write(root.join("w.txt"), "one\r\ntwo\r\n").unwrap();
    git(root, &["add", "w.txt"]);
    git(root, &["commit", "--quiet", "-m", "crlf"]);
    std::fs::write(root.join("w.txt"), "one\r\nTWO\r\nthree\r\n").unwrap();

    // Only the first change goes into the index.
    set_index_text(root, "w.txt", "one\nTWO\n").await.unwrap();
    assert_eq!(read_index(root, "w.txt").await.unwrap().unwrap(), b"one\r\nTWO\r\n");
    let status = working_status(root).await.unwrap();
    assert_eq!(summary(&status.staged), vec![("w.txt", FileStatus::Modified)]);
    assert_eq!(summary(&status.unstaged), vec![("w.txt", FileStatus::Modified)]);
    // The working tree is untouched.
    assert_eq!(std::fs::read(root.join("w.txt")).unwrap(), b"one\r\nTWO\r\nthree\r\n");
}

#[tokio::test]
async fn stages_lines_of_an_untracked_file() {
    let dir = fixture();
    let root = dir.path();
    write(root, "new.txt", "first\nsecond\n");
    set_index_text(root, "new.txt", "first\n").await.unwrap();
    assert_eq!(read_index(root, "new.txt").await.unwrap().unwrap(), b"first\n");
    let status = working_status(root).await.unwrap();
    assert_eq!(summary(&status.staged), vec![("new.txt", FileStatus::Added)]);
    assert_eq!(summary(&status.unstaged), vec![("new.txt", FileStatus::Modified)]);
}

#[tokio::test]
async fn discards_unstaged_changes_and_resets_to_head() {
    let dir = fixture();
    let root = dir.path();
    write(root, "a.txt", "staged\n");
    git(root, &["add", "a.txt"]);
    write(root, "a.txt", "staged and more\n");
    write(root, "untracked.txt", "x\n");

    discard(root, &["a.txt".into(), "untracked.txt".into()]).await.unwrap();
    assert_eq!(read(root, "a.txt"), "staged\n");
    assert!(!root.join("untracked.txt").exists());

    write(root, "added.txt", "new\n");
    git(root, &["add", "added.txt"]);
    reset_to_head(root, &["a.txt".into(), "added.txt".into()]).await.unwrap();
    assert_eq!(read(root, "a.txt"), "one\ntwo\nthree\n");
    // A file HEAD lacks only leaves the index.
    assert_eq!(read(root, "added.txt"), "new\n");
    let status = working_status(root).await.unwrap();
    assert!(status.staged.is_empty());
    assert_eq!(summary(&status.unstaged), vec![("added.txt", FileStatus::Added)]);
}

#[tokio::test]
async fn saves_edits_keeping_the_files_line_endings() {
    let dir = fixture();
    let root = dir.path();
    std::fs::write(root.join("w.txt"), b"\xEF\xBB\xBFone\r\n").unwrap();
    write_worktree(root, "w.txt", "one\ntwo\n").await.unwrap();
    assert_eq!(std::fs::read(root.join("w.txt")).unwrap(), b"\xEF\xBB\xBFone\r\ntwo\r\n");
    assert!(write_worktree(root, "../outside.txt", "x").await.is_err());
}

#[tokio::test]
async fn commits_what_is_staged() {
    let dir = fixture();
    let root = dir.path();
    write(root, "a.txt", "committed\n");
    write(root, "b.txt", "left out\n");
    stage(root, &["a.txt".into()]).await.unwrap();

    let made = commit(root, "Change a\n\nWith a body.").await.unwrap();
    assert_eq!(made.subject, "Change a");
    assert_eq!(made.branch.as_deref(), Some("main"));
    assert_eq!(git(root, &["rev-parse", "HEAD"]).trim(), made.sha);
    let status = working_status(root).await.unwrap();
    assert!(status.staged.is_empty());
    assert_eq!(summary(&status.unstaged), vec![("b.txt", FileStatus::Modified)]);
    assert!(commit(root, "  ").await.is_err());
}

#[tokio::test]
async fn works_before_the_first_commit() {
    let dir = tempfile::tempdir().unwrap();
    let root = dir.path();
    git(root, &["init", "--quiet", "--initial-branch=main"]);
    write(root, "a.txt", "a\n");
    stage(root, &["a.txt".into()]).await.unwrap();
    let status = working_status(root).await.unwrap();
    assert_eq!(status.head, None);
    assert_eq!(summary(&status.staged), vec![("a.txt", FileStatus::Added)]);
    unstage(root, &["a.txt".into()]).await.unwrap();
    let status = working_status(root).await.unwrap();
    assert!(status.staged.is_empty());
    assert_eq!(worktree_files(root).await.unwrap(), vec!["a.txt".to_string()]);
}

#[tokio::test]
async fn undoes_every_uncommitted_change_to_files() {
    let dir = fixture();
    let root = dir.path();
    write(root, "a.txt", "staged\n");
    git(root, &["add", "a.txt"]);
    write(root, "a.txt", "staged and more\n");
    write(root, "added.txt", "new\n");
    git(root, &["add", "added.txt"]);
    write(root, "untracked.txt", "x\n");

    let paths = ["a.txt".to_string(), "added.txt".into(), "untracked.txt".into(), "missing.txt".into()];
    undo_edits(root, &paths).await.unwrap();
    assert_eq!(read(root, "a.txt"), "one\ntwo\nthree\n");
    assert!(!root.join("added.txt").exists());
    assert!(!root.join("untracked.txt").exists());
    let status = working_status(root).await.unwrap();
    assert!(status.staged.is_empty() && status.unstaged.is_empty());
    assert!(undo_edits(root, &["../outside.txt".into()]).await.is_err());
}
