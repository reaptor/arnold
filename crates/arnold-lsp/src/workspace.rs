//! Getting a checkout ready for FsAutoComplete with as little work as possible.
//!
//! FsAutoComplete reads each project's restore output (`obj/project.assets.json`)
//! and loads F# project references from source. It needs compiled assemblies
//! only for projects in other languages (a C# project an F# project references).
//! So: restore, and build only those other projects, and skip either step when
//! its inputs have not changed since it last succeeded in this checkout.

use std::collections::{BTreeSet, HashMap};
use std::path::Path;
use std::process::Stdio;

use serde::{Deserialize, Serialize};
use tokio::io::{AsyncBufReadExt, AsyncRead, BufReader};
use tokio::process::Command;

use crate::dotnet::{self, Dotnet};

/// The .NET work a checkout needs. Paths are relative to the checkout.
#[derive(Debug, Clone, PartialEq)]
pub struct Plan {
    /// The repository has a tool manifest: tools such as Paket may be needed
    /// to restore.
    pub tool_restore: bool,
    /// What `dotnet restore` runs on: the F# project when there is one, else
    /// the solution at the top of the repository, else every F# project.
    pub restore: Vec<String>,
    /// Projects in other languages that F# projects reference.
    pub build: Vec<String>,
    /// Changes when anything restoring depends on changes.
    pub restore_stamp: String,
    /// Changes when anything building depends on changes.
    pub build_stamp: String,
}

/// Steps that succeeded in a checkout, by stamp.
#[derive(Debug, Default, Serialize, Deserialize)]
pub struct Done {
    pub restore: Option<String>,
    pub build: Option<String>,
}

impl Done {
    pub fn load(path: &Path) -> Done {
        std::fs::read(path)
            .ok()
            .and_then(|bytes| serde_json::from_slice(&bytes).ok())
            .unwrap_or_default()
    }

    pub fn save(&self, path: &Path) {
        if let Ok(json) = serde_json::to_vec(self) {
            let _ = std::fs::write(path, json);
        }
    }
}

const SOLUTION_EXTENSIONS: [&str; 2] = [".sln", ".slnx"];

/// Files that decide what restoring does (projects, package versions, feeds).
fn affects_restore(path: &str) -> bool {
    let lower = path.to_ascii_lowercase();
    let name = lower.rsplit('/').next().unwrap_or(&lower);
    [".fsproj", ".csproj", ".vbproj", ".props", ".targets", ".sln", ".slnx", ".slnf"]
        .iter()
        .any(|extension| name.ends_with(extension))
        || matches!(
            name,
            "global.json"
                | "nuget.config"
                | "packages.lock.json"
                | "dotnet-tools.json"
                | "paket.dependencies"
                | "paket.lock"
                | "paket.references"
        )
}

/// F# sources: FsAutoComplete reads them itself, so they never need a build.
fn is_fsharp_source(path: &str) -> bool {
    let lower = path.to_ascii_lowercase();
    [".fs", ".fsi", ".fsx"].iter().any(|extension| lower.ends_with(extension))
}

pub fn is_fsharp_project(path: &str) -> bool {
    path.to_ascii_lowercase().ends_with(".fsproj")
}

/// What to do for a commit with these `(path, content id)` files, given the
/// text of each F# project. `None` when there are no F# projects.
pub fn plan(files: &[(String, String)], projects: &HashMap<String, String>) -> Option<Plan> {
    let fsharp: Vec<&str> = files
        .iter()
        .map(|(path, _)| path.as_str())
        .filter(|path| is_fsharp_project(path))
        .collect();
    if fsharp.is_empty() {
        return None;
    }
    let exists = |path: &str| files.iter().any(|(file, _)| file == path);

    let tool_restore = exists(".config/dotnet-tools.json") || exists("dotnet-tools.json");
    let solution = files
        .iter()
        .map(|(path, _)| path.as_str())
        .find(|path| !path.contains('/') && SOLUTION_EXTENSIONS.iter().any(|e| path.to_ascii_lowercase().ends_with(e)));
    let restore: Vec<String> = match (fsharp.as_slice(), solution) {
        ([single], _) => vec![single.to_string()],
        (_, Some(solution)) => vec![solution.to_string()],
        (all, None) => all.iter().map(|path| path.to_string()).collect(),
    };

    let mut build = BTreeSet::new();
    for project in &fsharp {
        let Some(text) = projects.get(*project) else { continue };
        for include in project_references(text) {
            if is_fsharp_project(&include) {
                continue;
            }
            match resolve(project, &include) {
                Some(target) if exists(&target) => {
                    build.insert(target);
                }
                // A reference that is not in the commit is left to the build of its project.
                Some(_) => {}
                // A path built from MSBuild properties: building the F#
                // project builds whatever it references.
                None => {
                    build.insert(project.to_string());
                }
            }
        }
    }
    let build: Vec<String> = build.into_iter().collect();

    let restore_stamp = stamp(
        &restore,
        files.iter().filter(|(path, _)| affects_restore(path)),
    );
    let build_stamp = stamp(
        &build,
        files.iter().filter(|(path, _)| !is_fsharp_source(path)),
    );
    Some(Plan {
        tool_restore,
        restore,
        build,
        restore_stamp,
        build_stamp,
    })
}

/// The `Include` of each `<ProjectReference>` in a project file.
fn project_references(project: &str) -> Vec<String> {
    let mut found = Vec::new();
    let mut rest = project;
    while let Some(start) = rest.find("<ProjectReference") {
        rest = &rest[start + "<ProjectReference".len()..];
        let element = &rest[..rest.find('>').unwrap_or(rest.len())];
        if let Some(value) = attribute(element, "Include") {
            found.push(value.to_string());
        }
    }
    found
}

fn attribute<'a>(element: &'a str, name: &str) -> Option<&'a str> {
    let at = element.find(&format!("{name}="))? + name.len() + 1;
    let quote = element[at..].chars().next().filter(|c| *c == '"' || *c == '\'')?;
    let value = &element[at + 1..];
    Some(&value[..value.find(quote)?])
}

/// `include`, written relative to `project`, as a path from the top of the
/// repository. `None` when it uses MSBuild properties or leaves the repository.
fn resolve(project: &str, include: &str) -> Option<String> {
    if include.contains("$(") {
        return None;
    }
    let mut parts: Vec<&str> = project.split('/').collect();
    parts.pop();
    for part in include.split(['/', '\\']) {
        match part {
            "" | "." => {}
            ".." => {
                parts.pop()?;
            }
            part => parts.push(part),
        }
    }
    Some(parts.join("/"))
}

/// A short fingerprint of the targets and of the files they depend on.
fn stamp<'a>(targets: &[String], files: impl Iterator<Item = &'a (String, String)>) -> String {
    // FNV-1a: stable across runs and platforms, unlike std's hasher.
    let mut hash: u64 = 0xcbf2_9ce4_8422_2325;
    let mut feed = |bytes: &[u8]| {
        for byte in bytes {
            hash ^= u64::from(*byte);
            hash = hash.wrapping_mul(0x0100_0000_01b3);
        }
    };
    for target in targets {
        feed(target.as_bytes());
        feed(b"\n");
    }
    for (path, oid) in files {
        feed(path.as_bytes());
        feed(b"\0");
        feed(oid.as_bytes());
        feed(b"\n");
    }
    format!("{hash:016x}")
}

/// Runs `dotnet` with `args` in `dir`, handing each line it prints to
/// `on_line`. Dropping the future stops the process.
pub async fn run_dotnet(
    dotnet: &Dotnet,
    dir: &Path,
    args: &[&str],
    on_line: &(dyn Fn(String) + Send + Sync),
) -> Result<(), String> {
    let mut command = Command::new(&dotnet.path);
    command
        .args(args)
        .current_dir(dir)
        .envs(dotnet::child_environment(&dotnet.path))
        // Plain line output, no prompts, and no build processes left behind
        // holding the checkout open.
        .env("MSBUILDTERMINALLOGGER", "off")
        .env("MSBUILDDISABLENODEREUSE", "1")
        .env("DOTNET_CLI_TELEMETRY_OPTOUT", "1")
        .env("DOTNET_NOLOGO", "1")
        .env("DOTNET_SKIP_FIRST_TIME_EXPERIENCE", "1")
        .env("DOTNET_CLI_UI_LANGUAGE", "en")
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .kill_on_drop(true);
    #[cfg(windows)]
    command.creation_flags(0x0800_0000); // CREATE_NO_WINDOW

    let mut child = command
        .spawn()
        .map_err(|e| format!("could not run {}: {e}", dotnet.path.display()))?;
    let stdout = child.stdout.take();
    let stderr = child.stderr.take();
    tokio::join!(forward_lines(stdout, on_line), forward_lines(stderr, on_line));
    let status = child.wait().await.map_err(|e| e.to_string())?;
    if status.success() {
        Ok(())
    } else {
        Err(format!(
            "dotnet {} failed ({status})",
            args.first().copied().unwrap_or_default()
        ))
    }
}

async fn forward_lines(stream: Option<impl AsyncRead + Unpin>, on_line: &(dyn Fn(String) + Send + Sync)) {
    let Some(stream) = stream else { return };
    let mut lines = BufReader::new(stream).lines();
    while let Ok(Some(line)) = lines.next_line().await {
        if !line.trim().is_empty() {
            on_line(line);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn files(paths: &[&str]) -> Vec<(String, String)> {
        paths.iter().map(|path| (path.to_string(), format!("id-{path}"))).collect()
    }

    fn projects(entries: &[(&str, &str)]) -> HashMap<String, String> {
        entries.iter().map(|(path, text)| (path.to_string(), text.to_string())).collect()
    }

    #[test]
    fn nothing_to_do_without_fsharp_projects() {
        let files = files(&["App.sln", "src/Api/Api.csproj", "src/Api/Program.cs"]);
        assert_eq!(plan(&files, &HashMap::new()), None);
    }

    #[test]
    fn a_lone_fsharp_project_is_only_restored() {
        let files = files(&["App.sln", "src/App/App.fsproj", "src/App/Program.fs"]);
        let plan = plan(&files, &projects(&[("src/App/App.fsproj", "<Project />")])).unwrap();
        assert!(!plan.tool_restore);
        assert_eq!(plan.restore, vec!["src/App/App.fsproj"]);
        assert!(plan.build.is_empty());
    }

    #[test]
    fn several_projects_restore_through_the_solution_or_one_by_one() {
        let paths = ["src/A/A.fsproj", "src/B/B.fsproj", ".config/dotnet-tools.json"];
        let none = HashMap::new();
        let without_solution = plan(&files(&paths), &none).unwrap();
        assert!(without_solution.tool_restore);
        assert_eq!(without_solution.restore, vec!["src/A/A.fsproj", "src/B/B.fsproj"]);

        let mut with_solution = paths.to_vec();
        with_solution.push("All.slnx");
        assert_eq!(plan(&files(&with_solution), &none).unwrap().restore, vec!["All.slnx"]);
    }

    #[test]
    fn builds_only_projects_in_other_languages() {
        let files = files(&["src/App/App.fsproj", "src/Core/Core.fsproj", "src/Data/Data.csproj"]);
        let app = r#"<Project Sdk="Microsoft.NET.Sdk">
  <ItemGroup>
    <ProjectReference Include="..\Core\Core.fsproj" />
    <ProjectReference
        Include='../Data/Data.csproj' />
    <ProjectReference Include="..\Gone\Gone.csproj" />
  </ItemGroup>
</Project>"#;
        let plan = plan(&files, &projects(&[("src/App/App.fsproj", app)])).unwrap();
        assert_eq!(plan.build, vec!["src/Data/Data.csproj"]);
    }

    #[test]
    fn a_reference_built_from_properties_builds_its_fsharp_project() {
        let files = files(&["src/App/App.fsproj"]);
        let app = r#"<ProjectReference Include="$(RepoRoot)/src/Data/Data.csproj" />"#;
        let plan = plan(&files, &projects(&[("src/App/App.fsproj", app)])).unwrap();
        assert_eq!(plan.build, vec!["src/App/App.fsproj"]);
    }

    #[test]
    fn fsharp_source_changes_need_neither_restore_nor_build() {
        let before = files(&["App.fsproj", "Program.fs", "Data/Data.csproj", "Data/Store.cs"]);
        let app = projects(&[("App.fsproj", r#"<ProjectReference Include="Data/Data.csproj" />"#)]);
        let first = plan(&before, &app).unwrap();

        let mut fsharp_edit = before.clone();
        fsharp_edit[1].1 = "changed".into();
        let second = plan(&fsharp_edit, &app).unwrap();
        assert_eq!(first.restore_stamp, second.restore_stamp);
        assert_eq!(first.build_stamp, second.build_stamp);

        let mut csharp_edit = before.clone();
        csharp_edit[3].1 = "changed".into();
        let third = plan(&csharp_edit, &app).unwrap();
        assert_eq!(first.restore_stamp, third.restore_stamp);
        assert_ne!(first.build_stamp, third.build_stamp);

        let mut project_edit = before.clone();
        project_edit[0].1 = "changed".into();
        assert_ne!(first.restore_stamp, plan(&project_edit, &app).unwrap().restore_stamp);
    }

    #[test]
    fn resolves_relative_references() {
        assert_eq!(resolve("src/App/App.fsproj", "..\\Data\\Data.csproj").as_deref(), Some("src/Data/Data.csproj"));
        assert_eq!(resolve("App.fsproj", "./lib/Lib.csproj").as_deref(), Some("lib/Lib.csproj"));
        assert_eq!(resolve("App.fsproj", "../Outside.csproj"), None);
    }
}
