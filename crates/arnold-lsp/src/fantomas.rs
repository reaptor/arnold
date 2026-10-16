//! Fantomas, the F# formatter, run as the repository's local .NET tool: does
//! the code base need formatting, and formatting it.

use std::path::Path;

use serde::Serialize;

use crate::dotnet::Dotnet;
use crate::workspace;

/// What `fantomas --check` found in a checkout.
#[derive(Debug, Clone, Default, PartialEq, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct FormatCheck {
    /// Files Fantomas would change, relative to the checkout.
    pub unformatted: Vec<String>,
    /// Files Fantomas could not format (they do not parse), with why.
    pub errors: Vec<String>,
}

impl FormatCheck {
    pub fn is_clean(&self) -> bool {
        self.unformatted.is_empty() && self.errors.is_empty()
    }
}

/// The command of Fantomas in the checkout's tool manifest; None when the
/// repository does not use it.
pub fn tool_command(checkout: &Path) -> Option<String> {
    [".config/dotnet-tools.json", "dotnet-tools.json"]
        .iter()
        .filter_map(|path| std::fs::read_to_string(checkout.join(path)).ok())
        .find_map(|manifest| command_in_manifest(&manifest))
}

fn command_in_manifest(manifest: &str) -> Option<String> {
    let manifest: serde_json::Value = serde_json::from_str(manifest).ok()?;
    let (_, tool) = manifest
        .get("tools")?
        .as_object()?
        .iter()
        // `fantomas-tool` before version 5.
        .find(|(id, _)| matches!(id.to_ascii_lowercase().as_str(), "fantomas" | "fantomas-tool"))?;
    let command = tool
        .get("commands")
        .and_then(|commands| commands.get(0))
        .and_then(|command| command.as_str())
        .unwrap_or("fantomas");
    Some(command.to_string())
}

/// Runs `fantomas --check .` in the checkout, restoring the repository's
/// tools first if they are not there yet.
pub async fn check(dotnet: &Dotnet, checkout: &Path, command: &str) -> Result<FormatCheck, String> {
    let (code, output) = run_tool(dotnet, checkout, command, &["--check", "."]).await?;
    let found = parse_check(&output);
    match code {
        Some(0) => Ok(FormatCheck::default()),
        // 99: files need formatting; 1 with files listed: some could not be parsed.
        _ if !found.is_clean() => Ok(found),
        _ => Err(failure(command, code, &output)),
    }
}

/// Most files on one command line (Windows caps its length).
const FILES_PER_RUN: usize = 100;

/// Formats `paths` (relative to the checkout) in place. Returns the paths
/// Fantomas could not format because they do not parse; the others are formatted.
pub async fn format_files(dotnet: &Dotnet, checkout: &Path, command: &str, paths: &[String]) -> Result<Vec<String>, String> {
    let mut failed = Vec::new();
    for chunk in paths.chunks(FILES_PER_RUN) {
        let args: Vec<&str> = chunk.iter().map(String::as_str).collect();
        let (code, output) = run_tool(dotnet, checkout, command, &args).await?;
        if code == Some(0) {
            continue;
        }
        let unparsed = unparsed_files(&output, chunk);
        if unparsed.is_empty() {
            return Err(failure(command, code, &output));
        }
        failed.extend(unparsed);
    }
    Ok(failed)
}

/// Runs the tool, and once more after `dotnet tool restore` when it is not restored.
async fn run_tool(dotnet: &Dotnet, checkout: &Path, command: &str, args: &[&str]) -> Result<(Option<i32>, String), String> {
    let full: Vec<&str> = ["tool", "run", command].iter().chain(args).copied().collect();
    let (code, output) = workspace::dotnet_output(dotnet, checkout, &full).await?;
    if code != Some(0) && output.contains("dotnet tool restore") {
        workspace::run_dotnet(dotnet, checkout, &["tool", "restore"], &|_| {}).await?;
        return workspace::dotnet_output(dotnet, checkout, &full).await;
    }
    Ok((code, output))
}

fn parse_check(output: &str) -> FormatCheck {
    let mut check = FormatCheck::default();
    for line in output.lines() {
        if let Some(path) = line.trim().strip_suffix(" needs formatting") {
            let path = relative(path);
            if !check.unformatted.contains(&path) {
                check.unformatted.push(path);
            }
        } else if let Some(error) = line.trim().strip_prefix("error: ") {
            // `error: Failed to format ./src/A.fs: Fantomas.Core.ParseException: ...`
            let error = match (failed_path(error), error.split_once(": ")) {
                (Some(path), Some((_, why))) => format!("Failed to format {path}: {why}"),
                _ => error.to_string(),
            };
            check.errors.push(error);
        }
    }
    // A file that does not parse is listed as needing formatting too, but
    // formatting cannot change it.
    let unparsed: Vec<String> = check.errors.iter().filter_map(|error| failed_path(error)).collect();
    check.unformatted.retain(|path| !unparsed.contains(path));
    check
}

/// The file of a `Failed to format <path>: <why>` error.
fn failed_path(error: &str) -> Option<String> {
    let rest = error.strip_prefix("Failed to format ")?;
    let end = rest.find(": ")?;
    Some(relative(&rest[..end]))
}

/// Which of `paths` Fantomas reported it could not format. Formatting says
/// `Failed to format file: src/C.fs : Could not parse file.`, with only the
/// file name when given one file.
fn unparsed_files(output: &str, paths: &[String]) -> Vec<String> {
    let reported: Vec<String> = output
        .lines()
        .filter_map(|line| {
            let rest = line.trim().strip_prefix("Failed to format file: ")?;
            Some(relative(&rest[..rest.find(" : ")?]))
        })
        .collect();
    paths
        .iter()
        .filter(|path| {
            let path = relative(path);
            reported.iter().any(|name| path == *name || path.ends_with(&format!("/{name}")))
        })
        .cloned()
        .collect()
}

/// A path as Fantomas prints it, as the checkout-relative path git uses.
fn relative(path: &str) -> String {
    let path = path.trim();
    let path = path.strip_prefix("./").or_else(|| path.strip_prefix(".\\")).unwrap_or(path);
    path.replace('\\', "/")
}

fn failure(command: &str, code: Option<i32>, output: &str) -> String {
    let code = code.map_or("no exit code".to_string(), |code| format!("exit code {code}"));
    let output = output.trim();
    if output.is_empty() {
        format!("dotnet tool run {command} failed ({code})")
    } else {
        format!("dotnet tool run {command} failed ({code}): {output}")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn finds_fantomas_in_a_manifest() {
        let manifest = r#"{ "version": 1, "isRoot": true, "tools": {
            "paket": { "version": "8.0.0", "commands": ["paket"] },
            "fantomas": { "version": "7.0.3", "commands": ["fantomas"], "rollForward": false } } }"#;
        assert_eq!(command_in_manifest(manifest), Some("fantomas".into()));
        let old = r#"{ "tools": { "Fantomas-Tool": { "version": "4.7.9", "commands": ["fantomas"] } } }"#;
        assert_eq!(command_in_manifest(old), Some("fantomas".into()));
        let without = r#"{ "tools": { "paket": { "version": "8.0.0", "commands": ["paket"] } } }"#;
        assert_eq!(command_in_manifest(without), None);
        assert_eq!(command_in_manifest("not json"), None);
    }

    #[test]
    fn reads_the_check_output() {
        let output = "error: Failed to format ./src/C.fs: Fantomas.Core.ParseException: ParseException\n  [{ Severity = Error\n./src/C.fs needs formatting\n./src/A.fs needs formatting\n";
        assert_eq!(
            parse_check(output),
            FormatCheck {
                unformatted: vec!["src/A.fs".into()],
                errors: vec!["Failed to format src/C.fs: Fantomas.Core.ParseException: ParseException".into()],
            }
        );
        assert!(parse_check("").is_clean());
        assert_eq!(parse_check("src\\Win.fs needs formatting").unformatted, vec!["src/Win.fs".to_string()]);
        assert_eq!(failed_path("Failed to format src/C.fs: ParseException"), Some("src/C.fs".into()));
        let paths = ["src/C.fs".to_string(), "src/A.fs".to_string()];
        assert_eq!(unparsed_files("Failed to format file: src/C.fs : Could not parse file.\n", &paths), vec!["src/C.fs".to_string()]);
        assert_eq!(unparsed_files("Failed to format file: C.fs : Could not parse file.\n", &paths[..1]), vec!["src/C.fs".to_string()]);
        assert!(unparsed_files("something else", &paths).is_empty());
    }
}
