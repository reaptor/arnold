//! Staging, unstaging, putting back and committing in real temporary repositories.

use std::path::Path;
use std::process::Command;

use arnold_git::{
    commit, delete_files, discard, ignore, ignored_paths, is_tracked, read_index, read_stage, reset_to_head, set_index_text, stage, start_branch, undo_edits, unstage, untrack,
    working_status, worktree_files, write_worktree, FileStatus,
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

/// main and `other` both change the second line of a.txt; `other` also adds
/// c.txt. Merging `other` into main stops with a.txt conflicted.
fn conflicted_merge() -> tempfile::TempDir {
    let dir = fixture();
    let root = dir.path();
    git(root, &["checkout", "--quiet", "-b", "other"]);
    write(root, "a.txt", "one\nTWO\nthree\n");
    write(root, "c.txt", "sea\n");
    git(root, &["add", "--all"]);
    git(root, &["commit", "--quiet", "-m", "theirs"]);
    git(root, &["checkout", "--quiet", "main"]);
    write(root, "a.txt", "one\n2\nthree\n");
    git(root, &["commit", "--quiet", "-am", "ours"]);
    let merge = Command::new("git").current_dir(root).args(["merge", "--no-edit", "other"]).output().unwrap();
    assert!(!merge.status.success(), "the merge conflicts");
    dir
}

#[tokio::test]
async fn lists_a_merge_conflict_once_until_it_is_staged() {
    let dir = conflicted_merge();
    let root = dir.path();

    let status = working_status(root).await.unwrap();
    assert_eq!(summary(&status.unstaged), vec![("a.txt", FileStatus::Conflicted)]);
    assert_eq!(summary(&status.staged), vec![("c.txt", FileStatus::Added)]);
    assert_eq!(status.merge_message.as_deref(), Some("Merge branch 'other'"));
    assert_eq!(read_stage(root, "a.txt", 2).await.unwrap().as_deref(), Some(&b"one\n2\nthree\n"[..]));
    assert_eq!(read_stage(root, "b.txt", 2).await.unwrap(), None);
    assert!(read(root, "a.txt").contains("<<<<<<< HEAD\n2\n=======\nTWO\n>>>>>>> other\n"));

    // Resolved and staged: an ordinary staged change, and the merge commits.
    write(root, "a.txt", "one\n2 and TWO\nthree\n");
    stage(root, &["a.txt".to_string()]).await.unwrap();
    let status = working_status(root).await.unwrap();
    assert!(status.unstaged.is_empty());
    assert_eq!(summary(&status.staged), vec![("a.txt", FileStatus::Modified), ("c.txt", FileStatus::Added)]);
    commit(root, "Merge branch 'other'").await.unwrap();
    assert_eq!(git(root, &["rev-list", "--parents", "-n", "1", "HEAD"]).split_whitespace().count(), 3);
    assert_eq!(working_status(root).await.unwrap().merge_message, None);
}

#[tokio::test]
async fn resetting_a_conflicted_file_brings_its_conflicts_back() {
    let dir = conflicted_merge();
    let root = dir.path();
    write(root, "a.txt", "one\nhalf done\nthree\n");
    discard(root, &["a.txt".to_string(), "b.txt".to_string()]).await.unwrap();
    // Git names the sides ours and theirs this time.
    assert_eq!(read(root, "a.txt"), "one\n<<<<<<< ours\n2\n=======\nTWO\n>>>>>>> theirs\nthree\n");
    let status = working_status(root).await.unwrap();
    assert_eq!(summary(&status.unstaged), vec![("a.txt", FileStatus::Conflicted)]);
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
async fn deletes_files_from_the_working_tree_only() {
    let dir = fixture();
    let root = dir.path();
    write(root, "untracked.txt", "x\n");
    write(root, "added.txt", "new\n");
    git(root, &["add", "added.txt"]);
    std::fs::create_dir(root.join("folder")).unwrap();

    delete_files(root, &["a.txt".into(), "untracked.txt".into(), "added.txt".into(), "gone.txt".into()]).await.unwrap();
    assert!(!root.join("a.txt").exists() && !root.join("untracked.txt").exists() && !root.join("added.txt").exists());
    let status = working_status(root).await.unwrap();
    // The index still has them.
    assert_eq!(summary(&status.staged), vec![("added.txt", FileStatus::Added)]);
    assert_eq!(summary(&status.unstaged), vec![("a.txt", FileStatus::Deleted), ("added.txt", FileStatus::Deleted)]);
    assert!(delete_files(root, &["folder".into()]).await.is_err());
    assert!(delete_files(root, &["../outside.txt".into()]).await.is_err());
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
async fn starts_a_branch_keeping_staged_and_unstaged_changes() {
    let dir = fixture();
    let root = dir.path();
    write(root, "a.txt", "staged\n");
    write(root, "b.txt", "not staged\n");
    stage(root, &["a.txt".into()]).await.unwrap();

    start_branch(root, "feature/new").await.unwrap();
    let status = working_status(root).await.unwrap();
    assert_eq!(status.branch.as_deref(), Some("feature/new"));
    assert_eq!(summary(&status.staged), vec![("a.txt", FileStatus::Modified)]);
    assert_eq!(summary(&status.unstaged), vec![("b.txt", FileStatus::Modified)]);
    assert_eq!(git(root, &["rev-parse", "main"]), git(root, &["rev-parse", "HEAD"]));

    assert!(start_branch(root, "main").await.is_err());
    assert!(start_branch(root, "bad name").await.is_err());
    assert!(start_branch(root, "-x").await.is_err());
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

#[tokio::test]
async fn ignores_files_and_folders_in_the_root_gitignore() {
    let dir = fixture();
    let root = dir.path();
    write(root, "logs/today.log", "x\n");
    write(root, "notes.txt", "x\n");

    let file = ignore(root, "notes.txt", false).await.unwrap();
    assert_eq!((file.pattern.as_str(), file.added, file.tracked), ("/notes.txt", true, false));
    let folder = ignore(root, "logs", true).await.unwrap();
    assert_eq!((folder.pattern.as_str(), folder.added), ("/logs/", true));
    assert_eq!(read(root, ".gitignore"), "/notes.txt\n/logs/\n");
    let unstaged: Vec<_> = working_status(root).await.unwrap().unstaged.into_iter().map(|f| f.path).collect();
    assert_eq!(unstaged, [".gitignore"]);

    // Once only; a tracked file is reported, as ignoring does not untrack it.
    let again = ignore(root, "notes.txt", false).await.unwrap();
    assert!(!again.added);
    let tracked = ignore(root, "a.txt", false).await.unwrap();
    assert!(tracked.added && tracked.tracked);
    assert!(ignore(root, "../outside", false).await.is_err());
}

#[tokio::test]
async fn ignoring_keeps_the_gitignore_line_endings() {
    let dir = fixture();
    let root = dir.path();
    write(root, ".gitignore", "bin\r\nobj");
    ignore(root, "out.log", false).await.unwrap();
    assert_eq!(read(root, ".gitignore"), "bin\r\nobj\r\n/out.log\r\n");
}

#[tokio::test]
async fn untracking_keeps_files_on_disk() {
    let dir = fixture();
    let root = dir.path();
    write(root, "bin/a.dll", "x\n");
    write(root, "bin/b.dll", "y\n");
    git(root, &["add", "."]);
    git(root, &["commit", "--quiet", "-m", "bin"]);

    assert_eq!(untrack(root, "bin").await.unwrap(), 2);
    assert_eq!(untrack(root, "a.txt").await.unwrap(), 1);
    assert!(!is_tracked(root, "bin").await.unwrap() && !is_tracked(root, "a.txt").await.unwrap());
    assert!(is_tracked(root, "b.txt").await.unwrap());
    assert_eq!(read(root, "bin/a.dll"), "x\n");
    assert_eq!(read(root, "a.txt"), "one\ntwo\nthree\n");
    let staged: Vec<_> = working_status(root).await.unwrap().staged.into_iter().map(|f| (f.path, f.status)).collect();
    assert_eq!(
        staged,
        [
            ("a.txt".to_string(), FileStatus::Deleted),
            ("bin/a.dll".to_string(), FileStatus::Deleted),
            ("bin/b.dll".to_string(), FileStatus::Deleted),
        ]
    );
    assert_eq!(untrack(root, "a.txt").await.unwrap(), 0);
}

#[tokio::test]
async fn tells_which_changed_paths_git_ignores() {
    let dir = fixture();
    let root = dir.path();
    write(root, ".gitignore", "obj/\n*.log\n");
    write(root, "kept.log", "tracked although ignored\n");
    git(root, &["add", "--force", ".gitignore", "kept.log"]);
    git(root, &["commit", "--quiet", "-m", "ignore build output"]);
    write(root, "src/obj/project.assets.json", "{}");
    write(root, "src/obj/Debug/net8.0/App.dll", "");
    write(root, "build.log", "");
    write(root, "src/App.fs", "module App\n");

    let build_output = ["src/obj", "src/obj/project.assets.json", "src/obj/Debug/net8.0/App.dll", "build.log"];
    let paths: Vec<String> = build_output.iter().chain(&["kept.log", "src/App.fs", "a.txt"]).map(|path| path.to_string()).collect();
    assert_eq!(ignored_paths(root, &paths).await.unwrap(), build_output);
    // None of them ignored is not a failure.
    assert!(ignored_paths(root, &["src/App.fs".to_string()]).await.unwrap().is_empty());
}
