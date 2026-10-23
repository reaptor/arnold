//! The uncommitted changes of a checkout, for committing them: what is
//! staged and what is not, staging and unstaging (whole files, or the text a
//! file should have in the index), putting files back, saving edits and
//! committing.
//!
//! Unlike the rest of the crate this writes to the repository: the checkout's
//! files, its index and, with `commit`, its branch. It only ever acts on the
//! checkout it is given, and nothing here pushes (`publish` does).

use std::path::{Component, Path, PathBuf};

use serde::Serialize;

use crate::cli::{run, run_raw, run_raw_with_input, run_text, run_with_input, GitError};
use crate::compare::{diff_files, ChangedFile, FileStatus};
use crate::publish::{branch_sync, BranchSync};
use crate::Result;

const BOM: &[u8] = &[0xEF, 0xBB, 0xBF];
/// Untracked files above this size are not counted line by line.
const MAX_COUNTED_BYTES: u64 = 5 * 1024 * 1024;

#[derive(Debug, Clone, Serialize, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct WorkingStatus {
    /// Branch checked out; `None` when HEAD is detached.
    pub branch: Option<String>,
    /// The commit checked out; `None` before the first commit.
    pub head: Option<String>,
    /// Working tree against the index, untracked files included (as added).
    pub unstaged: Vec<ChangedFile>,
    /// Index against HEAD: what a commit would take.
    pub staged: Vec<ChangedFile>,
    /// Where the branch is pushed and how far it is ahead; `None` when
    /// detached or the repository has no remote.
    pub sync: Option<BranchSync>,
}

/// What `checkout` has not committed yet, staged and not.
pub async fn working_status(checkout: &Path) -> Result<WorkingStatus> {
    let (branch, head, staged, mut unstaged, untracked) = tokio::try_join!(
        current_branch(checkout),
        head_commit(checkout),
        diff_files(checkout, &["--cached"]),
        diff_files(checkout, &[]),
        untracked_files(checkout),
    )?;
    unstaged.extend(untracked);
    unstaged.sort_by(|a, b| a.path.cmp(&b.path));
    let mut staged = staged;
    staged.sort_by(|a, b| a.path.cmp(&b.path));
    let sync = match (&branch, &head) {
        (Some(branch), Some(_)) => branch_sync(checkout, branch).await?,
        _ => None,
    };
    Ok(WorkingStatus {
        branch,
        head,
        unstaged,
        staged,
        sync,
    })
}

/// Files git does not track and does not ignore, as added files.
pub(crate) async fn untracked_files(checkout: &Path) -> Result<Vec<ChangedFile>> {
    let out = run(checkout, &["ls-files", "-z", "--others", "--exclude-standard"]).await?;
    let mut files = Vec::new();
    for path in String::from_utf8_lossy(&out).split('\0').filter(|path| !path.is_empty()) {
        files.push(untracked_file(checkout, path).await);
    }
    Ok(files)
}

async fn untracked_file(checkout: &Path, path: &str) -> ChangedFile {
    let mut file = ChangedFile {
        path: path.to_string(),
        old_path: None,
        status: FileStatus::Added,
        additions: None,
        deletions: Some(0),
        binary: false,
        uncommitted: false,
    };
    let full = checkout.join(path);
    let small = tokio::fs::metadata(&full).await.is_ok_and(|meta| meta.len() <= MAX_COUNTED_BYTES);
    if let (true, Ok(bytes)) = (small, tokio::fs::read(&full).await) {
        if bytes.contains(&0) {
            file.binary = true;
        } else {
            let lines = bytes.iter().filter(|&&byte| byte == b'\n').count();
            let unterminated = bytes.last().is_some_and(|&byte| byte != b'\n');
            file.additions = Some((lines + usize::from(unterminated)) as u32);
        }
    }
    file
}

async fn current_branch(checkout: &Path) -> Result<Option<String>> {
    let out = run_raw(checkout, &["symbolic-ref", "--short", "-q", "HEAD"]).await?;
    let name = String::from_utf8_lossy(&out.stdout).trim().to_string();
    Ok((out.success && !name.is_empty()).then_some(name))
}

async fn head_commit(checkout: &Path) -> Result<Option<String>> {
    let out = run_raw(checkout, &["rev-parse", "--verify", "--quiet", "HEAD^{commit}"]).await?;
    let sha = String::from_utf8_lossy(&out.stdout).trim().to_string();
    Ok((out.success && !sha.is_empty()).then_some(sha))
}

/// Contents of `path` in the index of `checkout`, or `None` if it has none.
pub async fn read_index(checkout: &Path, path: &str) -> Result<Option<Vec<u8>>> {
    let out = run_raw(checkout, &["cat-file", "blob", &format!(":0:{path}")]).await?;
    Ok(out.success.then_some(out.stdout))
}

/// Contents of `path` in the working tree of `checkout`, or `None` if it does not exist.
pub async fn read_worktree(checkout: &Path, path: &str) -> Result<Option<Vec<u8>>> {
    let full = resolve(checkout, path)?;
    match tokio::fs::read(&full).await {
        Ok(bytes) => Ok(Some(bytes)),
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => Ok(None),
        Err(error) => Err(io_error(path, error)),
    }
}

/// Every file of the working tree git would see: tracked ones and untracked
/// ones that are not ignored, sorted by path.
pub async fn worktree_files(checkout: &Path) -> Result<Vec<String>> {
    let out = run(checkout, &["ls-files", "-z", "--cached", "--others", "--exclude-standard"]).await?;
    let mut paths: Vec<String> = String::from_utf8_lossy(&out)
        .split('\0')
        .filter(|path| !path.is_empty())
        .map(str::to_string)
        .collect();
    paths.sort();
    paths.dedup();
    Ok(paths)
}

/// Which of `paths` (relative to `checkout`, `/`-separated) git ignores, such
/// as the `obj/` and `bin/` folders a restore or build writes. Tracked files
/// never are, whatever the ignore rules say.
pub async fn ignored_paths(checkout: &Path, paths: &[String]) -> Result<Vec<String>> {
    if paths.is_empty() {
        return Ok(Vec::new());
    }
    let args = ["check-ignore", "-z", "--stdin"];
    let output = run_raw_with_input(checkout, &args, &path_list(paths)).await?;
    // Exit code 1 without a message: none of them is ignored.
    if !output.success && !output.stderr.is_empty() {
        return Err(GitError::Failed {
            command: "check-ignore".into(),
            stderr: output.stderr,
        });
    }
    Ok(String::from_utf8_lossy(&output.stdout)
        .split('\0')
        .filter(|path| !path.is_empty())
        .map(str::to_string)
        .collect())
}

/// Paths go to git through stdin: a long list could pass the command line's limit.
fn path_list(paths: &[String]) -> Vec<u8> {
    let mut list = Vec::new();
    for path in paths {
        list.extend_from_slice(path.as_bytes());
        list.push(0);
    }
    list
}

const FROM_STDIN: [&str; 2] = ["--pathspec-from-file=-", "--pathspec-file-nul"];

/// Stages `paths` as they are in the working tree, deletions included.
pub async fn stage(checkout: &Path, paths: &[String]) -> Result<()> {
    if paths.is_empty() {
        return Ok(());
    }
    let mut args = vec!["--literal-pathspecs", "add", "--all"];
    args.extend(FROM_STDIN);
    run_with_input(checkout, &args, &path_list(paths)).await?;
    Ok(())
}

/// Puts `paths` in the index back to how HEAD has them (out of the index if
/// HEAD lacks them). The working tree is left alone.
pub async fn unstage(checkout: &Path, paths: &[String]) -> Result<()> {
    if paths.is_empty() {
        return Ok(());
    }
    if head_commit(checkout).await?.is_some() {
        let mut args = vec!["--literal-pathspecs", "reset", "--quiet", "HEAD"];
        args.extend(FROM_STDIN);
        run_with_input(checkout, &args, &path_list(paths)).await?;
    } else {
        // Before the first commit everything staged is new.
        run_with_input(checkout, &["update-index", "--force-remove", "-z", "--stdin"], &path_list(paths)).await?;
    }
    Ok(())
}

/// Throws away the unstaged changes of `paths`: tracked files go back to how
/// the index has them, untracked ones are deleted.
pub async fn discard(checkout: &Path, paths: &[String]) -> Result<()> {
    let mut tracked = Vec::new();
    for path in paths {
        if is_tracked(checkout, path).await? {
            tracked.push(path.clone());
        } else if let Some(full) = existing(checkout, path)? {
            tokio::fs::remove_file(&full).await.map_err(|e| io_error(path, e))?;
        }
    }
    if !tracked.is_empty() {
        let mut args = vec!["--literal-pathspecs", "checkout", "--quiet"];
        args.extend(FROM_STDIN);
        run_with_input(checkout, &args, &path_list(&tracked)).await?;
    }
    Ok(())
}

/// Deletes `paths` from the working tree, tracked or not; the index is left
/// alone, so a tracked file shows as an unstaged deletion. A path already
/// gone is skipped; a folder is refused.
pub async fn delete_files(checkout: &Path, paths: &[String]) -> Result<()> {
    for path in paths {
        let Some(full) = existing(checkout, path)? else { continue };
        let metadata = tokio::fs::symlink_metadata(&full).await.map_err(|e| io_error(path, e))?;
        if metadata.is_dir() {
            return Err(GitError::Parse(format!("{path} is a folder, not a file")));
        }
        tokio::fs::remove_file(&full).await.map_err(|e| io_error(path, e))?;
    }
    Ok(())
}

/// Puts `paths` back to how HEAD has them, in the index and the working
/// tree. A file HEAD does not have is only taken out of the index; it stays
/// on disk, untracked.
pub async fn reset_to_head(checkout: &Path, paths: &[String]) -> Result<()> {
    let has_head = head_commit(checkout).await?.is_some();
    let mut in_head = Vec::new();
    let mut new = Vec::new();
    for path in paths {
        let found = has_head && crate::read_blob(checkout, "HEAD", path).await?.is_some();
        if found { in_head.push(path.clone()) } else { new.push(path.clone()) }
    }
    if !in_head.is_empty() {
        let mut args = vec!["--literal-pathspecs", "checkout", "--quiet", "HEAD"];
        args.extend(FROM_STDIN);
        run_with_input(checkout, &args, &path_list(&in_head)).await?;
    }
    if !new.is_empty() {
        run_with_input(checkout, &["update-index", "--force-remove", "-z", "--stdin"], &path_list(&new)).await?;
    }
    Ok(())
}

/// Undoes every uncommitted change to `paths`: back to how HEAD has them, in
/// the index and the working tree. A file HEAD does not have is taken out of
/// the index and deleted.
pub async fn undo_edits(checkout: &Path, paths: &[String]) -> Result<()> {
    reset_to_head(checkout, paths).await?;
    let has_head = head_commit(checkout).await?.is_some();
    for path in paths {
        let in_head = has_head && crate::read_blob(checkout, "HEAD", path).await?.is_some();
        if in_head {
            continue;
        }
        let full = resolve(checkout, path)?;
        match tokio::fs::remove_file(&full).await {
            Ok(()) => {}
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => {}
            Err(error) => return Err(io_error(path, error)),
        }
    }
    Ok(())
}

/// What `ignore` did.
#[derive(Debug, Clone, Serialize, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct Ignored {
    /// The line it ignores `path` with, as written to `.gitignore`.
    pub pattern: String,
    /// False when `.gitignore` already had that line.
    pub added: bool,
    /// Git tracks `path` (or files in the folder) all the same: `.gitignore`
    /// only keeps untracked files out.
    pub tracked: bool,
}

/// Ignores `path` (a file, or a folder with `folder`) in `checkout`: a line
/// matching only it, anchored at the root, at the end of the root `.gitignore`,
/// which is made if need be and keeps its line endings.
pub async fn ignore(checkout: &Path, path: &str, folder: bool) -> Result<Ignored> {
    resolve(checkout, path)?;
    let pattern = ignore_pattern(path, folder);
    let file = resolve(checkout, ".gitignore")?;
    let current = match tokio::fs::read(&file).await {
        Ok(bytes) => bytes,
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => Vec::new(),
        Err(error) => return Err(io_error(".gitignore", error)),
    };
    let text = String::from_utf8_lossy(current.strip_prefix(BOM).unwrap_or(&current)).replace("\r\n", "\n");
    let added = !text.lines().any(|line| line == pattern);
    if added {
        let mut text = text;
        if !text.is_empty() && !text.ends_with('\n') {
            text.push('\n');
        }
        text.push_str(&pattern);
        text.push('\n');
        tokio::fs::write(&file, encode_like(&text, &current))
            .await
            .map_err(|e| io_error(".gitignore", e))?;
    }
    let tracked = is_tracked(checkout, path).await?;
    Ok(Ignored { pattern, added, tracked })
}

/// A `.gitignore` line matching `path` and nothing else: from the root, its
/// wildcards and trailing spaces escaped, ending in `/` for a folder.
fn ignore_pattern(path: &str, folder: bool) -> String {
    let mut pattern = String::from("/");
    for c in path.trim_end_matches('/').chars() {
        if matches!(c, '\\' | '*' | '?' | '[') {
            pattern.push('\\');
        }
        pattern.push(c);
    }
    let spaces = pattern.len() - pattern.trim_end_matches(' ').len();
    pattern.truncate(pattern.len() - spaces);
    pattern.push_str(&"\\ ".repeat(spaces));
    if folder {
        pattern.push('/');
    }
    pattern
}

/// Whether git tracks `path` in `checkout`: the file, or any file in the folder.
pub async fn is_tracked(checkout: &Path, path: &str) -> Result<bool> {
    let out = run(checkout, &["--literal-pathspecs", "ls-files", "-z", "--", path]).await?;
    Ok(!out.is_empty())
}

/// Stops tracking `path` (a file, or every file in a folder): takes it out of
/// the index, which stages its removal, and leaves it on disk. Returns how
/// many files git no longer tracks. Git refuses a file whose staged text
/// differs from both HEAD and the file on disk, which would be lost.
pub async fn untrack(checkout: &Path, path: &str) -> Result<usize> {
    resolve(checkout, path)?;
    let out = run(
        checkout,
        &["--literal-pathspecs", "rm", "--cached", "-r", "--ignore-unmatch", "--", path],
    )
    .await?;
    Ok(String::from_utf8_lossy(&out).lines().filter(|line| line.starts_with("rm '")).count())
}

/// Makes the index hold `text` (with `\n` line endings, as the editor has
/// it) for `path`, leaving the working tree alone: staging or unstaging some
/// lines of a file. The file keeps the line endings and byte-order mark it
/// has in the index; a file new to the index gets those of the working tree,
/// through the repository's own conversion (`core.autocrlf`).
pub async fn set_index_text(checkout: &Path, path: &str, text: &str) -> Result<()> {
    let mode = file_mode(checkout, path).await?;
    let sha = match read_index(checkout, path).await? {
        Some(current) => {
            let bytes = encode_like(text, &current);
            run_text_with_input(checkout, &["hash-object", "-w", "--stdin", "--no-filters"], &bytes).await?
        }
        None => {
            let disk = read_worktree(checkout, path).await?.unwrap_or_default();
            let bytes = encode_like(text, &disk);
            let path_arg = format!("--path={path}");
            run_text_with_input(checkout, &["hash-object", "-w", "--stdin", &path_arg], &bytes).await?
        }
    };
    run(checkout, &["update-index", "--add", "--cacheinfo", &mode, &sha, path]).await?;
    Ok(())
}

async fn run_text_with_input(checkout: &Path, args: &[&str], input: &[u8]) -> Result<String> {
    let out = run_with_input(checkout, args, input).await?;
    Ok(String::from_utf8_lossy(&out).trim().to_string())
}

/// The mode `path` has in the index, else in HEAD, else on disk.
async fn file_mode(checkout: &Path, path: &str) -> Result<String> {
    let staged = run(checkout, &["--literal-pathspecs", "ls-files", "-s", "-z", "--", path]).await?;
    if let Some(mode) = String::from_utf8_lossy(&staged).split(' ').next().filter(|mode| mode.len() == 6) {
        return Ok(mode.to_string());
    }
    let committed = run_raw(checkout, &["--literal-pathspecs", "ls-tree", "-z", "HEAD", "--", path]).await?;
    if let Some(mode) = String::from_utf8_lossy(&committed.stdout).split(' ').next().filter(|mode| mode.len() == 6) {
        if committed.success {
            return Ok(mode.to_string());
        }
    }
    Ok(if executable(&checkout.join(path)) { "100755" } else { "100644" }.to_string())
}

#[cfg(unix)]
fn executable(path: &Path) -> bool {
    use std::os::unix::fs::PermissionsExt;
    std::fs::metadata(path).is_ok_and(|meta| meta.permissions().mode() & 0o111 != 0)
}

#[cfg(not(unix))]
fn executable(_path: &Path) -> bool {
    false
}

/// Saves `text` (with `\n` line endings) to `path` in the working tree,
/// keeping the file's line endings and byte-order mark.
pub async fn write_worktree(checkout: &Path, path: &str, text: &str) -> Result<()> {
    let full = resolve(checkout, path)?;
    let current = tokio::fs::read(&full).await.unwrap_or_default();
    if let Some(parent) = full.parent() {
        tokio::fs::create_dir_all(parent).await.map_err(|e| io_error(path, e))?;
    }
    tokio::fs::write(&full, encode_like(text, &current)).await.map_err(|e| io_error(path, e))
}

/// The commit made by `commit`.
#[derive(Debug, Clone, Serialize, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct NewCommit {
    pub sha: String,
    pub subject: String,
    /// The branch it went on; `None` when HEAD is detached.
    pub branch: Option<String>,
}

/// Commits what is staged in `checkout`, running the repository's hooks.
pub async fn commit(checkout: &Path, message: &str) -> Result<NewCommit> {
    if message.trim().is_empty() {
        return Err(GitError::Parse("the commit message is empty".into()));
    }
    run_with_input(checkout, &["commit", "--quiet", "--file=-"], message.as_bytes()).await?;
    let sha = run_text(checkout, &["rev-parse", "HEAD"]).await?;
    let subject = run_text(checkout, &["log", "-1", "--format=%s", &sha]).await?;
    Ok(NewCommit {
        sha,
        subject,
        branch: current_branch(checkout).await?,
    })
}

/// Creates branch `name` at HEAD of `checkout` and checks it out there,
/// keeping what is staged and what is not, so it can be committed on the new
/// branch instead.
pub async fn start_branch(checkout: &Path, name: &str) -> Result<()> {
    let valid = run_raw(checkout, &["check-ref-format", "--branch", name]).await?;
    if !valid.success || name.starts_with('-') {
        return Err(GitError::Parse(format!("{name} is not a valid branch name")));
    }
    let branch = format!("refs/heads/{name}");
    if run_raw(checkout, &["rev-parse", "--verify", "--quiet", &branch]).await?.success {
        return Err(GitError::Parse(format!("a branch named {name} exists already")));
    }
    run(checkout, &["checkout", "--quiet", "-b", name]).await?;
    Ok(())
}

/// `text` with `\n` line endings, written the way `original` is: CRLF line
/// endings if most of its lines have them, and its byte-order mark.
pub fn encode_like(text: &str, original: &[u8]) -> Vec<u8> {
    let (bom, body) = match original.strip_prefix(BOM) {
        Some(rest) => (true, rest),
        None => (false, original),
    };
    let crlf = body.windows(2).filter(|pair| pair == b"\r\n").count();
    let lf = body.iter().filter(|&&byte| byte == b'\n').count() - crlf;
    let text = text.replace("\r\n", "\n");
    let body = if crlf > lf { text.replace('\n', "\r\n") } else { text };
    let mut bytes = Vec::with_capacity(body.len() + BOM.len());
    if bom {
        bytes.extend_from_slice(BOM);
    }
    bytes.extend_from_slice(body.as_bytes());
    bytes
}

fn io_error(path: &str, error: std::io::Error) -> GitError {
    GitError::Failed {
        command: "file".into(),
        stderr: format!("{path}: {error}"),
    }
}

/// `relative` inside `checkout`, refusing anything that would land outside
/// it (absolute paths, `..`, symlinked parents) or inside `.git`.
fn resolve(checkout: &Path, relative: &str) -> Result<PathBuf> {
    let refused = || GitError::Parse(format!("not a path inside the checkout: {relative}"));
    let relative_path = Path::new(relative);
    let only_normal = relative_path
        .components()
        .all(|component| matches!(component, Component::Normal(_) | Component::CurDir));
    let in_git = relative_path.components().next().is_some_and(|first| first.as_os_str() == ".git");
    if relative.is_empty() || !only_normal || in_git {
        return Err(refused());
    }
    let full = checkout.join(relative_path);
    let canonical_root = checkout.canonicalize().map_err(|e| io_error(relative, e))?;
    let mut existing = full.as_path();
    while !existing.exists() {
        existing = existing.parent().ok_or_else(refused)?;
    }
    let canonical = existing.canonicalize().map_err(|e| io_error(relative, e))?;
    if !canonical.starts_with(&canonical_root) {
        return Err(refused());
    }
    Ok(full)
}

/// Where `relative` is on disk inside `checkout`, for showing it in the
/// file manager: the path itself, or the nearest folder above it that exists
/// (a deleted file). Refused as `resolve` refuses; `true` when it is the path itself.
pub fn on_disk(checkout: &Path, relative: &str) -> Result<(PathBuf, bool)> {
    let mut full = resolve(checkout, relative)?;
    let exact = full.exists();
    // `resolve` made sure a folder inside the checkout holds it.
    while !full.exists() && full.pop() {}
    Ok((full, exact))
}

/// `relative` inside `checkout` if it exists there.
fn existing(checkout: &Path, relative: &str) -> Result<Option<PathBuf>> {
    let full = resolve(checkout, relative)?;
    Ok(full.symlink_metadata().is_ok().then_some(full))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ignore_patterns_match_only_the_path() {
        assert_eq!(ignore_pattern("bin/out.log", false), "/bin/out.log");
        assert_eq!(ignore_pattern("obj", true), "/obj/");
        assert_eq!(ignore_pattern("a*b?[c]\\d.txt", false), "/a\\*b\\?\\[c]\\\\d.txt");
        assert_eq!(ignore_pattern("#notes ", false), "/#notes\\ ");
    }

    #[test]
    fn encodes_like_the_original() {
        assert_eq!(encode_like("a\nb\n", b"x\r\ny\r\n"), b"a\r\nb\r\n");
        assert_eq!(encode_like("a\nb\n", b"\xEF\xBB\xBFx\ny\n"), b"\xEF\xBB\xBFa\nb\n");
        assert_eq!(encode_like("a\nb", b""), b"a\nb");
        // Mostly LF: stays LF.
        assert_eq!(encode_like("a\n", b"x\ny\nz\r\n"), b"a\n");
    }

    #[test]
    fn refuses_paths_outside_the_checkout() {
        let dir = tempfile::tempdir().unwrap();
        assert!(resolve(dir.path(), "../x").is_err());
        assert!(resolve(dir.path(), "/etc/passwd").is_err());
        assert!(resolve(dir.path(), ".git/config").is_err());
        assert!(resolve(dir.path(), "src/new.fs").is_ok());
    }

    #[test]
    fn finds_the_nearest_folder_on_disk() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::create_dir_all(dir.path().join("src")).unwrap();
        std::fs::write(dir.path().join("src/a.fs"), "").unwrap();
        assert_eq!(on_disk(dir.path(), "src/a.fs").unwrap(), (dir.path().join("src/a.fs"), true));
        assert_eq!(on_disk(dir.path(), "src/gone/b.fs").unwrap(), (dir.path().join("src"), false));
        assert!(on_disk(dir.path(), "../x").is_err());
    }
}
