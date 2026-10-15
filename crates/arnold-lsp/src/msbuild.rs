//! Making sense of the MSBuild logs FsAutoComplete reports when it cannot
//! load a project: the log has every target and task the design-time build
//! ran, and somewhere in it the one line that says what went wrong.

/// Most cause lines a summary keeps.
const MAX_CAUSES: usize = 3;
/// Longest cause line a summary keeps, in characters.
const MAX_CAUSE_LENGTH: usize = 400;

/// What FsAutoComplete said about a project it could not load: a short
/// message for the user and, when that was taken from a build log, the log.
pub fn explain(message: &str) -> (String, Option<String>) {
    let message = message.trim();
    if !is_build_log(message) {
        let text = if message.is_empty() { "could not be loaded" } else { message };
        return (text.to_string(), None);
    }
    let summary = causes(message)
        .map(|causes| causes.join("; "))
        .unwrap_or_else(|| "its design-time build failed (see the build log)".to_string());
    (summary, Some(message.to_string()))
}

fn is_build_log(message: &str) -> bool {
    message.starts_with("Build started") || message.contains("\nBuild FAILED.")
}

/// The lines of a build log that explain the failure: MSBuild errors if
/// there are any, else the output of the task that failed.
fn causes(log: &str) -> Option<Vec<String>> {
    let mut errors: Vec<String> = Vec::new();
    for line in log.lines() {
        if let Some(error) = error_text(line) {
            if !errors.contains(&error) {
                errors.push(error);
            }
        }
    }
    if !errors.is_empty() {
        return Some(errors.into_iter().take(MAX_CAUSES).collect());
    }
    failed_task_output(log)
}

/// `path : error NU1101: text [project]` → `NU1101: text`.
fn error_text(line: &str) -> Option<String> {
    let line = line.trim();
    let start = if line.starts_with("error ") {
        0
    } else {
        line.find(": error ")? + 2
    };
    let text = line[start + "error ".len()..].trim();
    // MSBuild appends the project the error belongs to.
    let text = match text.rfind(" [") {
        Some(at) if text.ends_with(']') => &text[..at],
        _ => text,
    };
    (!text.is_empty()).then(|| shorten(text))
}

/// What the first failed task logged between `Task "X"` and
/// `Done executing task "X" -- FAILED.`
fn failed_task_output(log: &str) -> Option<Vec<String>> {
    let mut output: Vec<&str> = Vec::new();
    for line in log.lines() {
        let line = line.trim();
        if line.starts_with("Task \"") {
            output.clear();
        } else if line.starts_with("Done executing task \"") {
            if line.ends_with("-- FAILED.") {
                // The last lines: a task tends to end with what went wrong.
                let lines: Vec<&str> = output.iter().copied().filter(|line| !line.is_empty()).collect();
                let kept: Vec<String> = lines[lines.len().saturating_sub(MAX_CAUSES)..]
                    .iter()
                    .map(|line| shorten(line))
                    .collect();
                if !kept.is_empty() {
                    return Some(kept);
                }
            }
            output.clear();
        } else {
            output.push(line);
        }
    }
    None
}

fn shorten(text: &str) -> String {
    if text.chars().count() <= MAX_CAUSE_LENGTH {
        return text.to_string();
    }
    let cut: String = text.chars().take(MAX_CAUSE_LENGTH).collect();
    format!("{cut}…")
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Shortened from what FsAutoComplete reported for a project whose
    /// packages were not restored.
    const MISSING_PACKAGE: &str = r#"Build started.

Project "App.fsproj" (ResolveAssemblyReferencesDesignTime;CoreCompile target(s)):
Target "ApplyImplicitVersions" in file "/sdk/Microsoft.NET.Sdk.DefaultItems.Shared.targets" from project "/w/App.fsproj" (target "ProcessFrameworkReferences" depends on it):
Using "ApplyImplicitVersions" task from assembly "/sdk/Microsoft.NET.Build.Tasks.dll".
Task "ApplyImplicitVersions"
Done executing task "ApplyImplicitVersions".
Done building target "ApplyImplicitVersions" in project "App.fsproj".
Target "ResolvePackageAssets" in file "/sdk/Microsoft.PackageDependencyResolution.targets" from project "/w/App.fsproj" (target "ResolveLockFileReferences" depends on it):
Using "ResolvePackageAssets" task from assembly "/sdk/Microsoft.NET.Build.Tasks.dll".
Task "ResolvePackageAssets"
Package Fable.AST, version 4.4.0 was not found. It might have been deleted since NuGet restore.
Done executing task "ResolvePackageAssets" -- FAILED.
Done building target "ResolvePackageAssets" in project "App.fsproj" -- FAILED.
Done building project "App.fsproj" -- FAILED.
Build FAILED.
"#;

    #[test]
    fn takes_the_output_of_the_failed_task() {
        let (message, details) = explain(MISSING_PACKAGE);
        assert_eq!(
            message,
            "Package Fable.AST, version 4.4.0 was not found. It might have been deleted since NuGet restore."
        );
        assert_eq!(details.as_deref(), Some(MISSING_PACKAGE.trim()));
    }

    #[test]
    fn prefers_msbuild_errors() {
        let log = "Build started.\n\
            Task \"Restore\"\n\
            /w/App.fsproj : error NU1101: Unable to find package Nope. [/w/App.fsproj]\n\
            error MSB4019: The imported project was not found.\n\
            /w/App.fsproj : error NU1101: Unable to find package Nope. [/w/App.fsproj]\n\
            Done executing task \"Restore\" -- FAILED.\n\
            Build FAILED.";
        assert_eq!(
            explain(log).0,
            "NU1101: Unable to find package Nope.; MSB4019: The imported project was not found."
        );
    }

    #[test]
    fn says_so_when_the_log_has_no_cause() {
        let (message, details) = explain("Build started.\nTask \"X\"\nDone executing task \"X\".\nBuild FAILED.");
        assert_eq!(message, "its design-time build failed (see the build log)");
        assert!(details.is_some());
    }

    #[test]
    fn keeps_plain_messages() {
        assert_eq!(explain("Project not restored\n"), ("Project not restored".to_string(), None));
        assert_eq!(explain(""), ("could not be loaded".to_string(), None));
    }
}
