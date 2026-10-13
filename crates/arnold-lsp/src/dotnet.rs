//! Finding the .NET SDK. Apps started from the Finder or the Start menu get a
//! minimal PATH, so the usual install locations are checked as well.

use std::path::{Path, PathBuf};

use tokio::process::Command;

#[derive(Debug, Clone)]
pub struct Dotnet {
    /// The `dotnet` executable.
    pub path: PathBuf,
    /// Major version of the SDK selected for the repository (honours `global.json`).
    pub sdk_major: u32,
}

const EXECUTABLE: &str = if cfg!(windows) { "dotnet.exe" } else { "dotnet" };

/// Locates `dotnet` and asks which SDK it would use in `directory`.
pub async fn locate(directory: &Path) -> Result<Dotnet, String> {
    let path = find_executable().ok_or(
        "The .NET SDK was not found. Install it from https://dotnet.microsoft.com/download to get language features.",
    )?;
    let mut command = Command::new(&path);
    command.arg("--version").current_dir(directory);
    #[cfg(windows)]
    command.creation_flags(0x0800_0000); // CREATE_NO_WINDOW
    let output = command
        .output()
        .await
        .map_err(|e| format!("could not run {}: {e}", path.display()))?;
    let version = String::from_utf8_lossy(&output.stdout);
    let sdk_major = parse_major(&version).ok_or_else(|| {
        format!(
            "No usable .NET SDK for this repository: {}",
            String::from_utf8_lossy(&output.stderr).trim()
        )
    })?;
    Ok(Dotnet { path, sdk_major })
}

fn parse_major(version: &str) -> Option<u32> {
    version.trim().split('.').next()?.parse().ok()
}

fn find_executable() -> Option<PathBuf> {
    let mut candidates: Vec<PathBuf> = Vec::new();
    if let Some(root) = std::env::var_os("DOTNET_ROOT") {
        candidates.push(PathBuf::from(root).join(EXECUTABLE));
    }
    if let Some(path) = std::env::var_os("PATH") {
        candidates.extend(std::env::split_paths(&path).map(|dir| dir.join(EXECUTABLE)));
    }
    if cfg!(windows) {
        for variable in ["ProgramFiles", "ProgramW6432"] {
            if let Some(dir) = std::env::var_os(variable) {
                candidates.push(PathBuf::from(dir).join("dotnet").join(EXECUTABLE));
            }
        }
    } else {
        for dir in [
            "/usr/local/share/dotnet",
            "/usr/local/bin",
            "/opt/homebrew/bin",
            "/usr/share/dotnet",
            "/usr/lib/dotnet",
        ] {
            candidates.push(Path::new(dir).join(EXECUTABLE));
        }
    }
    if let Some(home) = std::env::var_os(if cfg!(windows) { "USERPROFILE" } else { "HOME" }) {
        candidates.push(PathBuf::from(home).join(".dotnet").join(EXECUTABLE));
    }
    candidates.into_iter().find(|candidate| candidate.is_file())
}

/// Environment for a .NET child process that itself runs `dotnet` (as language
/// servers do to load projects): make sure it can find the same installation.
pub fn child_environment(dotnet: &Path) -> Vec<(String, std::ffi::OsString)> {
    let mut environment = Vec::new();
    // `dotnet` on PATH is often a symlink into the real installation folder.
    let real = dotnet.canonicalize().unwrap_or_else(|_| dotnet.to_path_buf());
    let Some(install_dir) = real.parent() else {
        return environment;
    };
    let mut paths = vec![install_dir.to_path_buf()];
    if let Some(existing) = std::env::var_os("PATH") {
        paths.extend(std::env::split_paths(&existing));
    }
    if let Ok(joined) = std::env::join_paths(paths) {
        environment.push(("PATH".to_string(), joined));
    }
    if std::env::var_os("DOTNET_ROOT").is_none() {
        environment.push(("DOTNET_ROOT".to_string(), install_dir.as_os_str().to_os_string()));
    }
    environment
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_sdk_versions() {
        assert_eq!(parse_major("10.0.203\n"), Some(10));
        assert_eq!(parse_major("9.0.100-rc.2.24474.11"), Some(9));
        assert_eq!(parse_major(""), None);
        assert_eq!(parse_major("The command could not be loaded"), None);
    }
}
