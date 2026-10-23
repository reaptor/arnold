//! Building the solution on request (Ctrl+Shift+B): what `dotnet build` runs
//! on, and with which arguments. The errors and warnings it prints are read
//! by the webview.

use std::path::Path;

/// Folders that never hold the projects of a build: output, packages, and
/// Arnold's own copies of other commits.
const SKIPPED: [&str; 4] = ["bin", "obj", "node_modules", "packages"];

/// How deep projects are looked for below the checkout.
const MAX_DEPTH: usize = 6;

/// What `dotnet build` runs on in `checkout`: the solution nearest the top,
/// else every F# project, else every project. Paths are relative, with `/`.
pub fn targets(checkout: &Path) -> Vec<String> {
    let mut found = Vec::new();
    collect(checkout, "", 0, &mut found);
    choose(&found)
}

fn collect(dir: &Path, prefix: &str, depth: usize, found: &mut Vec<String>) {
    let Ok(entries) = std::fs::read_dir(dir) else { return };
    for entry in entries.flatten() {
        let name = entry.file_name().to_string_lossy().into_owned();
        let path = format!("{prefix}{name}");
        let Ok(kind) = entry.file_type() else { continue };
        if kind.is_dir() {
            let skipped = name.starts_with('.') || SKIPPED.iter().any(|skip| name.eq_ignore_ascii_case(skip));
            if !skipped && depth < MAX_DEPTH {
                collect(&entry.path(), &format!("{path}/"), depth + 1, found);
            }
        } else if is_solution(&name) || is_project(&name) {
            found.push(path);
        }
    }
}

fn is_solution(path: &str) -> bool {
    let lower = path.to_ascii_lowercase();
    lower.ends_with(".sln") || lower.ends_with(".slnx")
}

fn is_project(path: &str) -> bool {
    let lower = path.to_ascii_lowercase();
    [".fsproj", ".csproj", ".vbproj"].iter().any(|extension| lower.ends_with(extension))
}

/// The targets among the solution and project files `paths`.
pub fn choose(paths: &[String]) -> Vec<String> {
    let depth = |path: &String| path.matches('/').count();
    let mut solutions: Vec<&String> = paths.iter().filter(|path| is_solution(path)).collect();
    solutions.sort_by(|a, b| depth(a).cmp(&depth(b)).then_with(|| a.cmp(b)));
    if let Some(solution) = solutions.first() {
        return vec![solution.to_string()];
    }
    let mut fsharp: Vec<String> = paths.iter().filter(|path| path.to_ascii_lowercase().ends_with(".fsproj")).cloned().collect();
    if fsharp.is_empty() {
        fsharp = paths.iter().filter(|path| is_project(path)).cloned().collect();
    }
    fsharp.sort();
    fsharp
}

/// The arguments of `dotnet` for building `target`: full paths in messages,
/// no summary repeating them, and no build servers left running.
pub fn arguments(target: &str) -> Vec<&str> {
    vec![
        "build",
        target,
        "-nodeReuse:false",
        "-v:minimal",
        "-clp:NoSummary",
        "-p:GenerateFullPaths=true",
        "-p:UseSharedCompilation=false",
    ]
}

#[cfg(test)]
mod tests {
    use super::*;

    fn paths(paths: &[&str]) -> Vec<String> {
        paths.iter().map(|path| path.to_string()).collect()
    }

    #[test]
    fn the_solution_nearest_the_top_is_built() {
        let found = paths(&["src/Shop.fsproj", "samples/Demo.sln", "Shop.slnx", "Shop.sln"]);
        assert_eq!(choose(&found), ["Shop.sln"]);
    }

    #[test]
    fn without_a_solution_every_fsharp_project_is_built() {
        let found = paths(&["tests/Tests.fsproj", "src/Api/Api.csproj", "src/Core/Core.fsproj"]);
        assert_eq!(choose(&found), ["src/Core/Core.fsproj", "tests/Tests.fsproj"]);
    }

    #[test]
    fn without_fsharp_projects_every_project_is_built() {
        assert_eq!(choose(&paths(&["b/B.vbproj", "a/A.csproj"])), ["a/A.csproj", "b/B.vbproj"]);
        assert!(choose(&[]).is_empty());
    }

    #[test]
    fn targets_are_found_on_disk_outside_output_folders() {
        let dir = tempfile::tempdir().unwrap();
        let write = |path: &str| {
            let path = dir.path().join(path);
            std::fs::create_dir_all(path.parent().unwrap()).unwrap();
            std::fs::write(path, "").unwrap();
        };
        write("src/Core/Core.fsproj");
        write("src/Core/bin/Debug/Stale.fsproj");
        write(".arnold/worktrees/x/Other.sln");
        write("README.md");
        assert_eq!(targets(dir.path()), ["src/Core/Core.fsproj"]);
        write("App.sln");
        assert_eq!(targets(dir.path()), ["App.sln"]);
    }
}
