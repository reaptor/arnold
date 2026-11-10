//! End-to-end checks against real temporary repositories.

use std::path::Path;
use std::process::Command;

use arnold_git::{
    app_folder, compare, export_commit, list_files, open, read_blob, tree_files, CommitRange, FileStatus,
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
async fn checks_branches_out_in_worktrees_of_their_own() {
    use arnold_git::branch_worktree;
    let dir = fixture();
    let root = dir.path();
    git(root, &["checkout", "--quiet", "main"]);
    let remote = tempfile::tempdir().unwrap();
    git(remote.path(), &["init", "--quiet", "--bare"]);
    git(root, &["remote", "add", "origin", remote.path().to_str().unwrap()]);
    git(root, &["push", "--quiet", "origin", "feature:remote-only"]);
    git(root, &["fetch", "--quiet", "origin"]);
    // Uncommitted changes in the opened folder are no obstacle, and stay.
    write(root, "a.txt", "dirty\n");
    let status_before = git_output(root, &["status", "--porcelain"]);

    // The branch the folder has needs no worktree.
    let here = branch_worktree(root, "main").await.unwrap();
    assert_eq!(here.path.canonicalize().unwrap(), root.canonicalize().unwrap());
    assert!(!here.created);

    let made = branch_worktree(root, "feature").await.unwrap();
    assert_eq!((made.branch.as_str(), made.created), ("feature", true));
    assert!(made.path.ends_with(".arnold/worktrees/feature"), "{}", made.path.display());
    assert!(made.path.join("new.txt").exists());
    assert_eq!(git_output(root, &["status", "--porcelain"]), status_before, "the opened folder is untouched");
    assert_eq!(open(root).await.unwrap().current_branch.as_deref(), Some("main"));
    let listed = arnold_git::list_branches(root).await.unwrap();
    let feature = listed.iter().find(|branch| branch.name == "feature").unwrap();
    assert_eq!(feature.worktree_path.as_deref().map(|p| p.canonicalize().unwrap()), Some(made.path.canonicalize().unwrap()));

    // Asked again: the same worktree.
    let again = branch_worktree(root, "feature").await.unwrap();
    assert_eq!((again.path.canonicalize().unwrap(), again.created), (made.path.canonicalize().unwrap(), false));

    // A remote branch gets a local one that tracks it.
    let tracked = branch_worktree(root, "origin/remote-only").await.unwrap();
    assert_eq!(tracked.branch, "remote-only");
    assert!(tracked.path.ends_with(".arnold/worktrees/remote-only"));
    let upstream = git_output(root, &["rev-parse", "--abbrev-ref", "remote-only@{upstream}"]);
    assert_eq!(String::from_utf8(upstream).unwrap().trim(), "origin/remote-only");

    // Deleted by hand: listed as checked out nowhere, and made again.
    std::fs::remove_dir_all(&made.path).unwrap();
    let listed = arnold_git::list_branches(root).await.unwrap();
    assert_eq!(listed.iter().find(|branch| branch.name == "feature").unwrap().worktree_path, None);
    let remade = branch_worktree(root, "feature").await.unwrap();
    assert!(remade.created && remade.path.join("new.txt").exists());
}

#[tokio::test]
async fn checks_branches_out_in_the_opened_folder() {
    use arnold_git::{branch_checkout, BranchCheckout, CheckedOut, Placement};
    let dir = fixture();
    let root = dir.path();
    git(root, &["checkout", "--quiet", "main"]);
    let remote = tempfile::tempdir().unwrap();
    git(remote.path(), &["init", "--quiet", "--bare"]);
    git(root, &["remote", "add", "origin", remote.path().to_str().unwrap()]);
    git(root, &["push", "--quiet", "origin", "feature:remote-only"]);
    git(root, &["fetch", "--quiet", "origin"]);
    let done = |checkout: CheckedOut<BranchCheckout>| match checkout {
        CheckedOut::Done(done) => done,
        other => panic!("not checked out: {other:?}"),
    };

    // Clean: the folder switches to the branch.
    let switched = done(branch_checkout(root, "feature", Placement::Here).await.unwrap());
    assert_eq!((switched.branch.as_str(), switched.path.as_path(), switched.created), ("feature", root, true));
    assert_eq!(open(root).await.unwrap().current_branch.as_deref(), Some("feature"));
    // Checked out already: left as it is.
    let again = done(branch_checkout(root, "feature", Placement::Here).await.unwrap());
    assert!(!again.created);

    // Uncommitted changes: refused, and they stay.
    write(root, "a.txt", "dirty\n");
    write(root, "new/untracked.txt", "new\n");
    let refused = branch_checkout(root, "main", Placement::Here).await.unwrap();
    assert_eq!(refused, CheckedOut::Changes { files: 2, untracked: 1 });
    assert_eq!(open(root).await.unwrap().current_branch.as_deref(), Some("feature"));
    assert_eq!(std::fs::read_to_string(root.join("a.txt")).unwrap(), "dirty\n");

    // Stashed: the folder switches, and the stash brings them back.
    let stashed = done(branch_checkout(root, "main", Placement::Stash).await.unwrap());
    assert_eq!((stashed.branch.as_str(), stashed.path.as_path()), ("main", root));
    assert!(git_output(root, &["status", "--porcelain"]).is_empty());
    let stash = String::from_utf8(git_output(root, &["stash", "list"])).unwrap();
    assert!(stash.contains("Arnold: before checking out main"), "{stash}");
    git(root, &["switch", "--quiet", "feature"]);
    git(root, &["stash", "pop", "--quiet"]);
    assert_eq!(std::fs::read_to_string(root.join("a.txt")).unwrap(), "dirty\n");
    assert!(root.join("new/untracked.txt").exists());

    // Into a worktree instead: the changes stay in the folder.
    let aside = done(branch_checkout(root, "main", Placement::Worktree).await.unwrap());
    assert!(aside.path.ends_with(".arnold/worktrees/main"), "{}", aside.path.display());
    assert_eq!(std::fs::read_to_string(root.join("a.txt")).unwrap(), "dirty\n");

    // Thrown away: untracked files too, then the folder switches.
    let tracked = done(branch_checkout(root, "origin/remote-only", Placement::Discard).await.unwrap());
    assert_eq!((tracked.branch.as_str(), tracked.path.as_path()), ("remote-only", root));
    assert!(!root.join("new/untracked.txt").exists());
    assert!(git_output(root, &["status", "--porcelain"]).is_empty());
    let upstream = git_output(root, &["rev-parse", "--abbrev-ref", "remote-only@{upstream}"]);
    assert_eq!(String::from_utf8(upstream).unwrap().trim(), "origin/remote-only");
}

/// A new commit on top of `parent`, with its files, on no branch: a new push of a pull request.
fn push_on(root: &Path, parent: &str) -> String {
    let output = Command::new("git")
        .current_dir(root)
        .args(["-c", "user.name=Test", "-c", "user.email=test@example.com"])
        .args(["commit-tree", &format!("{parent}^{{tree}}"), "-p", parent, "-m", "new push"])
        .output()
        .expect("git runs");
    assert!(output.status.success());
    String::from_utf8(output.stdout).unwrap().trim().to_string()
}

#[tokio::test]
async fn checks_pull_requests_out_on_their_branch() {
    use arnold_git::{pull_request_checkout, CheckedOut, Placement, Worktree};
    let dir = fixture();
    let root = dir.path();
    git(root, &["checkout", "--quiet", "main"]);
    let remote = tempfile::tempdir().unwrap();
    git(remote.path(), &["init", "--quiet", "--bare"]);
    git(root, &["remote", "add", "origin", remote.path().to_str().unwrap()]);
    let main_tip = String::from_utf8(git_output(root, &["rev-parse", "main"])).unwrap().trim().to_string();
    let feature_tip = String::from_utf8(git_output(root, &["rev-parse", "feature"])).unwrap().trim().to_string();
    let done = |checkout: Option<CheckedOut<Worktree>>| match checkout {
        Some(CheckedOut::Done(done)) => done,
        other => panic!("not checked out: {other:?}"),
    };
    let origin = Some("origin");
    // Pushed to the remote straight from its URL: its remote-tracking branches are fetched by the checkout.
    let url = remote.path().to_str().unwrap().to_string();
    let publish = |commit: &str, branch: &str| git(root, &["push", "--quiet", "--force", &url, &format!("{commit}:refs/heads/{branch}")]);
    publish(&feature_tip, "topic");
    assert_eq!(pull_request_checkout(root, 7, &feature_tip, "topic", origin, None, false, Placement::Here).await.unwrap(), None);

    // Clean: the folder switches to the source branch, made at the pull request's commit to track the remote's.
    let made = done(pull_request_checkout(root, 7, &feature_tip, "topic", origin, None, true, Placement::Here).await.unwrap());
    assert_eq!((made.path.as_path(), made.head.as_str(), made.edited, made.created), (root, feature_tip.as_str(), false, true));
    assert_eq!(made.branch, "topic");
    assert_eq!(open(root).await.unwrap().current_branch.as_deref(), Some("topic"));
    assert_eq!(String::from_utf8(git_output(root, &["config", "branch.topic.remote"])).unwrap().trim(), "origin");
    assert_eq!(String::from_utf8(git_output(root, &["config", "branch.topic.merge"])).unwrap().trim(), "refs/heads/topic");

    // Found again without `create`, and moved up to a new push while clean.
    let next = push_on(root, &feature_tip);
    publish(&next, "topic");
    let moved = done(pull_request_checkout(root, 7, &next, "topic", origin, None, false, Placement::Here).await.unwrap());
    assert_eq!((moved.path.as_path(), moved.head.as_str(), moved.created), (root, next.as_str(), false));
    assert_eq!(open(root).await.unwrap().current_branch.as_deref(), Some("topic"));
    // Edited: it stays where it is, until the edits are thrown away.
    let later = push_on(root, &next);
    publish(&later, "topic");
    write(root, "a.txt", "edited\n");
    let kept = done(pull_request_checkout(root, 7, &later, "topic", origin, None, false, Placement::Here).await.unwrap());
    assert_eq!((kept.head.as_str(), kept.edited), (next.as_str(), true));
    let fresh = done(pull_request_checkout(root, 7, &later, "topic", origin, None, false, Placement::Discard).await.unwrap());
    assert_eq!((fresh.head.as_str(), fresh.edited), (later.as_str(), false));
    // Never moved back, nor onto a commit that is not ahead of it.
    publish(&main_tip, "topic");
    let kept = done(pull_request_checkout(root, 7, &main_tip, "topic", origin, None, false, Placement::Here).await.unwrap());
    assert_eq!(kept.head, later);

    // Another pull request while the folder has edits: refused, or a worktree of its branch.
    write(root, "a.txt", "edited\n");
    publish(&main_tip, "other");
    assert_eq!(
        pull_request_checkout(root, 8, &main_tip, "other", origin, None, true, Placement::Here).await.unwrap(),
        Some(CheckedOut::Changes { files: 1, untracked: 0 })
    );
    let aside = done(pull_request_checkout(root, 8, &main_tip, "other", origin, None, true, Placement::Worktree).await.unwrap());
    assert!(aside.path.ends_with(".arnold/worktrees/other"), "{}", aside.path.display());
    assert_eq!((aside.branch.as_str(), aside.head.as_str(), aside.created), ("other", main_tip.as_str(), true));
    assert_eq!(String::from_utf8(git_output(&aside.path, &["branch", "--show-current"])).unwrap().trim(), "other");
    assert_eq!(std::fs::read_to_string(root.join("a.txt")).unwrap(), "edited\n");
    assert_eq!(open(root).await.unwrap().current_branch.as_deref(), Some("topic"));

    // An existing local branch checked out nowhere is switched to, and brought up to the pull request.
    git(root, &["checkout", "--quiet", "--force", "main"]);
    let ahead = push_on(root, &feature_tip);
    publish(&ahead, "feature");
    let switched = done(pull_request_checkout(root, 9, &ahead, "feature", origin, None, true, Placement::Here).await.unwrap());
    assert_eq!((switched.path.as_path(), switched.head.as_str()), (root, ahead.as_str()));
    assert_eq!(open(root).await.unwrap().current_branch.as_deref(), Some("feature"));

    // A pull request from a fork's `main`, ahead of the remote's: your `main` is not moved, nor
    // is a new branch made under that name; it gets one of its own, followed to new pushes.
    publish(&main_tip, "main");
    let forked = push_on(root, &main_tip);
    let fork = done(pull_request_checkout(root, 11, &forked, "main", origin, None, true, Placement::Here).await.unwrap());
    assert_eq!((fork.branch.as_str(), fork.head.as_str(), fork.path.as_path()), ("pr/11/main", forked.as_str(), root));
    assert_eq!(String::from_utf8(git_output(root, &["rev-parse", "main"])).unwrap().trim(), main_tip);
    assert!(String::from_utf8(git_output(root, &["config", "--get-regexp", "^branch\\."])).unwrap().lines().all(|line| !line.contains("pr/11")));
    let forked_again = push_on(root, &forked);
    let fork = done(pull_request_checkout(root, 11, &forked_again, "main", origin, None, false, Placement::Here).await.unwrap());
    assert_eq!((fork.branch.as_str(), fork.head.as_str()), ("pr/11/main", forked_again.as_str()));
    assert_eq!(String::from_utf8(git_output(root, &["rev-parse", "main"])).unwrap().trim(), main_tip);
    // Nor is a branch of yours with that name moved when there is no remote to check it against.
    git(root, &["checkout", "--quiet", "--force", "main"]);
    let unknown = push_on(root, &main_tip);
    let aside = done(pull_request_checkout(root, 12, &unknown, "main", None, None, true, Placement::Here).await.unwrap());
    assert_eq!((aside.branch.as_str(), aside.head.as_str()), ("pr/12/main", unknown.as_str()));
    assert_eq!(String::from_utf8(git_output(root, &["rev-parse", "main"])).unwrap().trim(), main_tip);

    assert!(pull_request_checkout(root, 10, &main_tip, "--force", origin, None, true, Placement::Here).await.is_err());
    assert!(pull_request_checkout(root, 10, &main_tip, "x", Some("nowhere"), None, true, Placement::Here).await.is_err());
}

#[tokio::test]
async fn edits_your_own_pull_request_on_its_checked_out_branch() {
    use arnold_git::{pull_request_checkout, CheckedOut, Placement, Worktree};
    let dir = fixture();
    let root = dir.path();
    let main_tip = String::from_utf8(git_output(root, &["rev-parse", "main"])).unwrap().trim().to_string();
    let feature_tip = String::from_utf8(git_output(root, &["rev-parse", "feature"])).unwrap().trim().to_string();
    let done = |checkout: Option<CheckedOut<Worktree>>| match checkout {
        Some(CheckedOut::Done(done)) => done,
        other => panic!("not checked out: {other:?}"),
    };

    // The folder has the branch, with edits: used as it is, edits and all.
    write(root, "a.txt", "edited\n");
    let found = done(pull_request_checkout(root, 7, &feature_tip, "feature", None, None, true, Placement::Here).await.unwrap());
    // As git names it (canonical, as `list_branches` gives it).
    assert_eq!(found.path.canonicalize().unwrap(), root.canonicalize().unwrap());
    assert_eq!((found.head.as_str(), found.edited, found.created), (feature_tip.as_str(), true, false));
    assert_eq!(found.branch, "feature");
    assert_eq!(open(root).await.unwrap().current_branch.as_deref(), Some("feature"));
    assert_eq!(std::fs::read_to_string(root.join("a.txt")).unwrap(), "edited\n");
    // Not at the pull request's commit, without a remote to check that against: never moved.
    git(root, &["checkout", "--quiet", "--", "a.txt"]);
    assert_eq!(pull_request_checkout(root, 7, &main_tip, "feature", None, None, false, Placement::Here).await.unwrap(), None);
    assert_eq!(String::from_utf8(git_output(root, &["rev-parse", "feature"])).unwrap().trim(), feature_tip);

    // A branch checked out in a linked worktree is used there.
    let elsewhere = tempfile::tempdir().unwrap();
    let linked = elsewhere.path().join("main");
    git(root, &["worktree", "add", "--quiet", linked.to_str().unwrap(), "main"]);
    let there = done(pull_request_checkout(root, 8, &main_tip, "main", None, None, false, Placement::Here).await.unwrap());
    assert_eq!(there.path.canonicalize().unwrap(), linked.canonicalize().unwrap());
    assert_eq!((there.head.as_str(), there.branch.as_str()), (main_tip.as_str(), "main"));

    // A branch checked out nowhere: found nowhere without `create`.
    git(root, &["branch", "elsewhere", "main"]);
    assert_eq!(pull_request_checkout(root, 9, &main_tip, "elsewhere", None, None, false, Placement::Here).await.unwrap(), None);
}
