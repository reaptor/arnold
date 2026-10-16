//! End-to-end checks against real temporary repositories.

use std::path::Path;
use std::process::Command;

use arnold_git::{
    app_folder, compare, export_commit, list_files, open, read_blob, switch_branch, tree_files, CommitRange, FileStatus,
};

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

    let comparison = compare(root, "main", "feature", None, None).await.unwrap();
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

fn only(sha: &str) -> CommitRange {
    CommitRange {
        oldest: sha.to_string(),
        newest: sha.to_string(),
    }
}

#[tokio::test]
async fn lists_branch_commits_and_shows_a_run_of_them() {
    let dir = fixture();
    let root = dir.path();
    write(root, "c.txt", "new\n");
    write(root, "a.txt", "one\nTWO\nthree\nfour\n");
    commit_all(root, "second feature commit");
    write(root, "c.txt", "newer\n");
    commit_all(root, "third feature commit");

    let all = compare(root, "main", "feature", None, None).await.unwrap();
    let subjects: Vec<_> = all.commits.iter().map(|c| c.subject.as_str()).collect();
    assert_eq!(subjects, vec!["third feature commit", "second feature commit", "feature work"]);
    assert_eq!(all.range, None);
    assert_eq!(all.head_commit, all.tip_commit);
    let [third, second, first] = [0, 1, 2].map(|i| all.commits[i].sha.clone());

    let one = compare(root, "main", "feature", Some(&only(&first)), None).await.unwrap();
    assert_eq!(one.range, Some(only(&first)));
    assert_eq!(one.head_commit, first);
    assert_eq!(one.merge_base, all.merge_base);
    assert_eq!(one.tip_commit, all.tip_commit);
    let paths: Vec<_> = one.files.iter().map(|f| f.path.as_str()).collect();
    assert_eq!(paths, vec!["a.txt", "b.txt", "new.txt"]);

    // Ends given the wrong way round are put in order.
    let reversed = CommitRange { oldest: third.clone(), newest: second.clone() };
    let run = compare(root, "main", "feature", Some(&reversed), None).await.unwrap();
    assert_eq!(run.range, Some(CommitRange { oldest: second.clone(), newest: third.clone() }));
    assert_eq!(run.merge_base, first);
    assert_eq!(run.head_commit, third);
    let summary: Vec<_> = run.files.iter().map(|f| (f.path.as_str(), f.status)).collect();
    assert_eq!(summary, vec![("a.txt", FileStatus::Modified), ("c.txt", FileStatus::Added)]);

    // A commit that is not on the branch falls back to all of the changes.
    let main_tip = String::from_utf8(git_output(root, &["rev-parse", "main"])).unwrap();
    let other = compare(root, "main", "feature", Some(&only(main_tip.trim())), None).await.unwrap();
    assert_eq!(other.range, None);
    assert_eq!(other.files.len(), 4);
}

#[tokio::test]
async fn branch_not_checked_out_is_compared_at_its_tip() {
    let dir = fixture();
    let root = dir.path();
    git(root, &["checkout", "--quiet", "main"]);

    let comparison = compare(root, "main", "feature", None, None).await.unwrap();
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

    let comparison = compare(root, "main", "feature-wt", None, None).await.unwrap();
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
    let comparison = compare(root, "main", "feature", None, None).await.unwrap();

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

#[tokio::test]
async fn exports_into_the_ignored_app_folder() {
    let dir = fixture();
    let root = dir.path();
    let status_before = git_output(root, &["status", "--porcelain"]);
    let folder = app_folder(root).await.unwrap();
    assert_eq!(folder, root.join(".arnold"));
    let checkout = folder.join("worktrees").join("feature");

    export_commit(root, "feature", &checkout, &folder.join("state").join("index")).await.unwrap();
    assert!(checkout.join("new.txt").exists());
    assert_eq!(git_output(root, &["status", "--porcelain"]), status_before, "the copy is ignored");
    assert_eq!(open(root).await.unwrap().worktrees.len(), 1);

    // Called again, it keeps the folder and what is in it.
    assert_eq!(app_folder(root).await.unwrap(), folder);
    assert!(checkout.join("new.txt").exists());
}

#[tokio::test]
async fn checked_out_branch_is_compared_with_its_files_on_disk() {
    let dir = fixture();
    let root = dir.path();
    write(root, "a.txt", "one\nTWO\nthree\nuncommitted\n");
    write(root, "sub/untracked.txt", "x\ny\n");
    write(root, "ignored/skip.txt", "x\n");
    std::fs::remove_file(root.join("new.txt")).unwrap();
    // Edited back to what it was at the merge-base: no longer part of the diff, but still an edit.
    write(root, "main-only.txt", "not on the branch\n");
    git(root, &["add", "main-only.txt"]);

    let comparison = compare(root, "main", "feature", None, Some(root)).await.unwrap();
    assert_eq!(comparison.checkout.as_deref(), Some(root));
    assert_eq!(comparison.head_commit, comparison.tip_commit);
    let summary: Vec<_> = comparison
        .files
        .iter()
        .map(|f| (f.path.as_str(), f.status, f.additions, f.uncommitted))
        .collect();
    assert_eq!(
        summary,
        vec![
            ("a.txt", FileStatus::Modified, Some(2), true),
            ("b.txt", FileStatus::Deleted, Some(0), false),
            ("main-only.txt", FileStatus::Added, Some(1), true),
            // Added by the branch and deleted on disk: no diff, but an edit.
            ("new.txt", FileStatus::Deleted, Some(0), true),
            ("old.txt", FileStatus::Deleted, Some(0), false),
            ("sub/untracked.txt", FileStatus::Added, Some(2), true),
        ]
    );
    // a.txt, main-only.txt, new.txt (deleted on disk) and the untracked file.
    assert_eq!(comparison.uncommitted, 4);

    // An edit back to the merge-base's text is still an edit, listed with no lines changed.
    write(root, "a.txt", "one\ntwo\nthree\n");
    let undone = compare(root, "main", "feature", None, Some(root)).await.unwrap();
    let a = undone.files.iter().find(|f| f.path == "a.txt").unwrap();
    assert_eq!((a.status, a.additions, a.deletions, a.uncommitted), (FileStatus::Modified, Some(0), Some(0), true));
    write(root, "a.txt", "one\nTWO\nthree\nuncommitted\n");

    // A run of commits is never read from disk.
    let first = comparison.commits[0].sha.clone();
    let commits = compare(root, "main", "feature", Some(&only(&first)), Some(root)).await.unwrap();
    assert_eq!(commits.checkout, None);
    assert_eq!(commits.uncommitted, 0);
    assert!(commits.files.iter().all(|f| !f.uncommitted));
    assert_eq!(commits.files.len(), 3);
}

#[tokio::test]
async fn switches_to_a_branch_only_in_a_clean_folder() {
    let dir = fixture();
    let root = dir.path();
    git(root, &["checkout", "--quiet", "main"]);
    let remote = tempfile::tempdir().unwrap();
    git(remote.path(), &["init", "--quiet", "--bare"]);
    git(root, &["remote", "add", "origin", remote.path().to_str().unwrap()]);
    git(root, &["push", "--quiet", "origin", "feature:remote-only"]);
    git(root, &["fetch", "--quiet", "origin"]);

    write(root, "a.txt", "dirty\n");
    let error = switch_branch(root, "feature").await.unwrap_err();
    assert!(error.to_string().contains("uncommitted changes"), "{error}");
    assert_eq!(open(root).await.unwrap().current_branch.as_deref(), Some("main"));

    git(root, &["checkout", "--quiet", "--", "a.txt"]);
    assert_eq!(switch_branch(root, "feature").await.unwrap(), "feature");
    assert_eq!(open(root).await.unwrap().current_branch.as_deref(), Some("feature"));

    // A remote branch gets a local one that tracks it.
    assert_eq!(switch_branch(root, "origin/remote-only").await.unwrap(), "remote-only");
    let upstream = git_output(root, &["rev-parse", "--abbrev-ref", "remote-only@{upstream}"]);
    assert_eq!(String::from_utf8(upstream).unwrap().trim(), "origin/remote-only");
}

#[tokio::test]
async fn keeps_a_detached_worktree_of_its_own() {
    use arnold_git::{detached_worktree, find_worktree};
    let dir = fixture();
    let root = dir.path();
    let status_before = git_output(root, &["status", "--porcelain"]);
    let main_tip = String::from_utf8(git_output(root, &["rev-parse", "main"])).unwrap().trim().to_string();
    let feature_tip = String::from_utf8(git_output(root, &["rev-parse", "feature"])).unwrap().trim().to_string();
    assert_eq!(find_worktree(root, "pr-7").await.unwrap(), None);

    let made = detached_worktree(root, "pr-7", &main_tip, false).await.unwrap();
    assert_eq!(made.head, main_tip);
    assert!(!made.edited);
    assert!(made.path.ends_with(".arnold/worktrees/pr-7"));
    assert!(made.path.join("main-only.txt").exists());
    assert_eq!(git_output(root, &["status", "--porcelain"]), status_before, "the opened folder is untouched");
    assert_eq!(open(root).await.unwrap().current_branch.as_deref(), Some("feature"));

    // Clean: it moves to a new commit. Edited: it stays.
    let moved = detached_worktree(root, "pr-7", &feature_tip, false).await.unwrap();
    assert_eq!(moved.head, feature_tip);
    write(&made.path, "a.txt", "edited\n");
    write(&made.path, "new.fs", "untracked\n");
    let kept = detached_worktree(root, "pr-7", &main_tip, false).await.unwrap();
    assert_eq!(kept.head, feature_tip);
    assert!(kept.edited);
    assert_eq!(find_worktree(root, "pr-7").await.unwrap(), Some(kept));

    // Discarding the edits lets it move.
    let fresh = detached_worktree(root, "pr-7", &main_tip, true).await.unwrap();
    assert_eq!(fresh.head, main_tip);
    assert!(!fresh.edited);
    assert!(!made.path.join("new.fs").exists());

    assert!(detached_worktree(root, "../escape", &main_tip, false).await.is_err());
}
