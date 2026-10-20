//! Unified-diff text of a branch, of a checkout's edits and of staged/unstaged changes.

use std::path::Path;
use std::process::Command;

use arnold_git::{patch, PatchSource};

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

fn write(root: &Path, path: &str, content: &str) {
    let full = root.join(path);
    std::fs::create_dir_all(full.parent().unwrap()).unwrap();
    std::fs::write(full, content).unwrap();
}

/// main has a.txt; the commit after it changes a.txt and adds src/b.rs.
fn fixture() -> (tempfile::TempDir, String, String) {
    let dir = tempfile::tempdir().unwrap();
    let root = dir.path();
    git(root, &["init", "--quiet", "--initial-branch=main"]);
    write(root, "a.txt", "one\ntwo\nthree\n");
    git(root, &["add", "--all"]);
    git(root, &["commit", "--quiet", "-m", "base"]);
    let base = git(root, &["rev-parse", "HEAD"]);
    write(root, "a.txt", "one\nTWO\nthree\n");
    write(root, "src/b.rs", "fn b() {}\n");
    git(root, &["add", "--all"]);
    git(root, &["commit", "--quiet", "-m", "work"]);
    let tip = git(root, &["rev-parse", "HEAD"]);
    (dir, base, tip)
}

#[tokio::test]
async fn diffs_two_commits() {
    let (dir, base, tip) = fixture();
    let patch = patch(dir.path(), PatchSource::Commits { from: &base, to: &tip }, &[], 100_000).await.unwrap();
    assert_eq!(patch.files, ["a.txt", "src/b.rs"]);
    assert!(patch.text.contains("-two\n+TWO\n"), "{}", patch.text);
    assert!(patch.text.contains("+fn b() {}\n"));
}

#[tokio::test]
async fn limits_to_the_paths_asked_for() {
    let (dir, base, tip) = fixture();
    let patch = patch(dir.path(), PatchSource::Commits { from: &base, to: &tip }, &["src".to_string()], 100_000)
        .await
        .unwrap();
    assert_eq!(patch.files, ["src/b.rs"]);
}

#[tokio::test]
async fn on_disk_includes_edits_and_untracked_files() {
    let (dir, base, _) = fixture();
    let root = dir.path();
    write(root, "a.txt", "one\nTWO\nthree\nfour\n");
    write(root, "notes.md", "hello\n");
    let patch = patch(root, PatchSource::OnDisk { from: &base }, &[], 100_000).await.unwrap();
    assert_eq!(patch.files, ["a.txt", "notes.md", "src/b.rs"]);
    assert!(patch.text.contains("+four\n"));
    assert!(patch.text.contains("+++ b/notes.md\n@@ -0,0 +1,1 @@\n+hello\n"));
}

#[tokio::test]
async fn separates_staged_from_unstaged() {
    let (dir, _, _) = fixture();
    let root = dir.path();
    write(root, "a.txt", "staged\n");
    git(root, &["add", "a.txt"]);
    write(root, "a.txt", "unstaged\n");
    write(root, "new.txt", "new\n");
    let staged = patch(root, PatchSource::Staged, &[], 100_000).await.unwrap();
    let unstaged = patch(root, PatchSource::Unstaged, &[], 100_000).await.unwrap();
    assert_eq!(staged.files, ["a.txt"]);
    assert!(staged.text.contains("+staged\n"));
    assert_eq!(unstaged.files, ["a.txt", "new.txt"]);
    assert!(unstaged.text.contains("-staged\n+unstaged\n"));
}
