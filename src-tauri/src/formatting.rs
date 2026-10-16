//! Fantomas before a commit: whether the code base is formatted, and
//! formatting the files it would change with only their formatting staged.

use std::collections::HashSet;
use std::path::Path;

use arnold_lsp::dotnet::{self, Dotnet};
use arnold_lsp::fantomas;
use serde::Serialize;
use tauri::State;

use crate::commands::{text_error, AppState};

/// Whether the code base is formatted, when the repository uses Fantomas
/// (has it in its .NET tool manifest); None when it does not.
#[tauri::command]
pub async fn check_formatting(checkout: String, state: State<'_, AppState>) -> Result<Option<fantomas::FormatCheck>, String> {
    let checkout = state.checked_root(&checkout)?;
    let Some(command) = fantomas::tool_command(&checkout) else {
        return Ok(None);
    };
    let dotnet = dotnet::locate(&checkout).await?;
    fantomas::check(&dotnet, &checkout, &command).await.map(Some)
}

/// What `format_and_stage` did.
#[derive(Debug, Default, Serialize, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct Formatted {
    /// Files whose formatting is staged.
    pub staged: Vec<String>,
    /// Files Fantomas could not format (as they are on disk, or as staged);
    /// their staged text is left as it was.
    pub failed: Vec<String>,
}

/// Formats `paths` with the repository's Fantomas and stages the formatting,
/// and nothing else: other unstaged changes stay unstaged.
#[tauri::command]
pub async fn format_and_stage(checkout: String, paths: Vec<String>, state: State<'_, AppState>) -> Result<Formatted, String> {
    let checkout = state.checked_root(&checkout)?;
    let command = fantomas::tool_command(&checkout).ok_or("This repository does not use Fantomas.")?;
    let dotnet = dotnet::locate(&checkout).await?;
    format_and_stage_files(&dotnet, &checkout, &command, &paths).await
}

/// A file whose working tree matches the index is formatted and staged
/// whole. One with other unstaged changes keeps them unstaged: its staged
/// text is formatted on its own, as a copy beside it (so the same
/// `.editorconfig` applies), and put in the index. A file with nothing in
/// the index (untracked) is only formatted on disk.
async fn format_and_stage_files(dotnet: &Dotnet, checkout: &Path, command: &str, paths: &[String]) -> Result<Formatted, String> {
    let status = arnold_git::working_status(checkout).await.map_err(text_error)?;
    let has_unstaged: HashSet<&str> = status.unstaged.iter().map(|file| file.path.as_str()).collect();

    // (path, its copy holding the staged text)
    let mut copies: Vec<(String, String)> = Vec::new();
    let mut targets: Vec<String> = paths.to_vec();
    let mut result = Ok(Vec::new());
    for path in paths.iter().filter(|path| has_unstaged.contains(path.as_str())) {
        let staged = match arnold_git::read_index(checkout, path).await {
            Ok(Some(staged)) => staged,
            Ok(None) => continue,
            Err(error) => {
                result = Err(text_error(error));
                break;
            }
        };
        let copy = copy_name(checkout, path);
        if let Err(error) = tokio::fs::write(checkout.join(&copy), staged).await {
            result = Err(format!("could not write {copy}: {error}"));
            break;
        }
        targets.push(copy.clone());
        copies.push((path.clone(), copy));
    }
    if result.is_ok() {
        result = fantomas::format_files(dotnet, checkout, command, &targets).await;
    }
    // The copies go whatever happened.
    let mut formatted_copies = Vec::new();
    for (path, copy) in &copies {
        let text = tokio::fs::read(checkout.join(copy)).await;
        let _ = tokio::fs::remove_file(checkout.join(copy)).await;
        formatted_copies.push((path, copy, text));
    }
    let unformattable = result?;

    let mut done = Formatted::default();
    let whole: Vec<String> = paths
        .iter()
        .filter(|path| !has_unstaged.contains(path.as_str()) && !unformattable.contains(path))
        .cloned()
        .collect();
    arnold_git::stage(checkout, &whole).await.map_err(text_error)?;
    done.staged.extend(whole);
    for (path, copy, text) in formatted_copies {
        if unformattable.contains(copy) || unformattable.contains(path) {
            continue;
        }
        let text = text.map_err(|error| format!("could not read {copy}: {error}"))?;
        arnold_git::set_index_text(checkout, path, &as_editor_text(&text)).await.map_err(text_error)?;
        done.staged.push(path.clone());
    }
    for path in paths {
        let copy_failed = copies.iter().any(|(original, copy)| original == path && unformattable.contains(copy));
        if unformattable.contains(path) || copy_failed {
            done.failed.push(path.clone());
        }
    }
    Ok(done)
}

/// `src/Orders.fs` → `src/Orders.arnold-staged.fs`, numbered if that is taken.
fn copy_name(checkout: &Path, path: &str) -> String {
    let (stem, extension) = match path.rfind('.').filter(|&dot| dot > path.rfind('/').map_or(0, |slash| slash + 1)) {
        Some(dot) => (&path[..dot], &path[dot..]),
        None => (path, ""),
    };
    let mut number = 0;
    loop {
        let suffix = if number == 0 { String::new() } else { format!("-{number}") };
        let copy = format!("{stem}.arnold-staged{suffix}{extension}");
        if !checkout.join(&copy).exists() {
            return copy;
        }
        number += 1;
    }
}

/// File bytes as the `\n` text `set_index_text` takes.
fn as_editor_text(bytes: &[u8]) -> String {
    let text = String::from_utf8_lossy(bytes);
    text.strip_prefix('\u{feff}').unwrap_or(&text).replace("\r\n", "\n")
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::process::Command;

    #[test]
    fn names_the_copy_beside_the_file() {
        let dir = std::env::temp_dir().join(format!("arnold-copy-name-{}", std::process::id()));
        std::fs::create_dir_all(dir.join("src")).unwrap();
        assert_eq!(copy_name(&dir, "src/Orders.fs"), "src/Orders.arnold-staged.fs");
        std::fs::write(dir.join("src/Orders.arnold-staged.fs"), "").unwrap();
        assert_eq!(copy_name(&dir, "src/Orders.fs"), "src/Orders.arnold-staged-1.fs");
        assert_eq!(copy_name(&dir, "a.b/Script"), "a.b/Script.arnold-staged");
        std::fs::remove_dir_all(&dir).unwrap();
    }

    fn git(dir: &Path, args: &[&str]) -> String {
        let output = Command::new("git").current_dir(dir).args(args).output().unwrap();
        assert!(output.status.success(), "git {args:?}: {}", String::from_utf8_lossy(&output.stderr));
        String::from_utf8_lossy(&output.stdout).into_owned()
    }

    /// Runs the real Fantomas (needs the .NET SDK, and network access or
    /// Fantomas 7.0.3 in the NuGet cache):
    /// `cargo test -p arnold formatting -- --ignored`
    #[tokio::test]
    #[ignore]
    async fn stages_only_the_formatting() {
        let dir = std::env::temp_dir().join(format!("arnold-format-stage-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(dir.join(".config")).unwrap();
        std::fs::write(
            dir.join(".config/dotnet-tools.json"),
            r#"{ "version": 1, "isRoot": true, "tools": { "fantomas": { "version": "7.0.3", "commands": ["fantomas"], "rollForward": false } } }"#,
        )
        .unwrap();
        std::fs::write(dir.join("Clean.fs"), "module Clean\n\nlet a   =  1\n").unwrap();
        std::fs::write(dir.join("Mixed.fs"), "module Mixed\n\nlet b   =  2\n").unwrap();
        std::fs::write(dir.join("Other.fs"), "module Other\n\nlet c = 3\n").unwrap();
        git(&dir, &["init", "-q"]);
        git(&dir, &["add", "."]);
        git(&dir, &["-c", "user.name=t", "-c", "user.email=t@t", "commit", "-qm", "start"]);
        // Mixed.fs: an unstaged edit that must stay unstaged. Other.fs: unstaged, formatted already.
        std::fs::write(dir.join("Mixed.fs"), "module Mixed\n\nlet b   =  2\nlet wip   = 0\n").unwrap();
        std::fs::write(dir.join("Other.fs"), "module Other\n\nlet c = 4\n").unwrap();

        let dotnet = dotnet::locate(&dir).await.unwrap();
        let check = fantomas::check(&dotnet, &dir, "fantomas").await.unwrap();
        assert_eq!(check.unformatted.len(), 2, "{check:?}");
        let done = format_and_stage_files(&dotnet, &dir, "fantomas", &check.unformatted).await.unwrap();
        assert!(done.failed.is_empty(), "{done:?}");
        assert_eq!(done.staged.len(), 2);

        assert_eq!(git(&dir, &["show", ":Clean.fs"]), "module Clean\n\nlet a = 1\n");
        assert_eq!(git(&dir, &["show", ":Mixed.fs"]), "module Mixed\n\nlet b = 2\n");
        assert_eq!(std::fs::read_to_string(dir.join("Mixed.fs")).unwrap(), "module Mixed\n\nlet b = 2\nlet wip = 0\n");
        assert_eq!(git(&dir, &["show", ":Other.fs"]), "module Other\n\nlet c = 3\n");
        assert_eq!(git(&dir, &["status", "--porcelain"]), "M  Clean.fs\nMM Mixed.fs\n M Other.fs\n");
        std::fs::remove_dir_all(&dir).unwrap();
    }
}
