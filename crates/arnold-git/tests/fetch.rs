//! Fetching a pull request's commits leaves every ref of the repository alone.

use std::path::Path;
use std::process::Command;

use arnold_git::{compare, fetch_objects, has_commits, remotes};

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

fn commit(root: &Path, path: &str, content: &str) -> String {
    std::fs::write(root.join(path), content).unwrap();
    git(root, &["add", "--all"]);
    git(root, &["commit", "--quiet", "-m", path]);
    git(root, &["rev-parse", "HEAD"])
}

#[tokio::test]
async fn fetches_objects_without_writing_refs() {
    let server = tempfile::tempdir().unwrap();
    let server = server.path();
    git(server, &["init", "--quiet", "--initial-branch=main"]);
    commit(server, "a.txt", "a\n");

    let local = tempfile::tempdir().unwrap();
    let local = local.path();
    git(local.parent().unwrap(), &["clone", "--quiet", server.to_str().unwrap(), local.to_str().unwrap()]);

    // The pull request: a feature branch the clone has never seen, and main moved on.
    git(server, &["checkout", "--quiet", "-b", "feature/pr"]);
    let source = commit(server, "b.txt", "b\n");
    git(server, &["checkout", "--quiet", "main"]);
    let target = commit(server, "c.txt", "c\n");

    let refs_before = git(local, &["for-each-ref"]);
    assert!(!has_commits(local, &[&source, &target]).await);

    let url = format!("file://{}", server.display());
    fetch_objects(local, &url, &["refs/heads/feature/pr", "refs/heads/main"], Some("X-Test: 1"))
        .await
        .unwrap();

    assert!(has_commits(local, &[&source, &target]).await);
    assert_eq!(git(local, &["for-each-ref"]), refs_before);
    assert!(!local.join(".git/FETCH_HEAD").exists());

    let comparison = compare(local, &target, &source, None, None).await.unwrap();
    let paths: Vec<_> = comparison.files.iter().map(|file| file.path.as_str()).collect();
    assert_eq!(paths, ["b.txt"]);

    let names: Vec<_> = remotes(local).await.unwrap().into_iter().map(|remote| remote.name).collect();
    assert_eq!(names, ["origin"]);

    assert!(fetch_objects(local, &url, &["--upload-pack=evil"], None).await.is_err());
}
