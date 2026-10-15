//! FsAutoComplete, the F# language server: installed from NuGet into Arnold's
//! own data folder on first use and kept up to date from then on.
//!
//! Layout under the install root:
//! ```text
//! 0.85.3/.complete            marks a finished install
//! 0.85.3/net10.0/fsautocomplete.dll  (one folder per target framework)
//! .last-update-check          touched when NuGet was last asked for a newer version
//! ```

use std::io::{Read, Write};
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::{Duration, SystemTime};

use base64::Engine;
use serde_json::{json, Value};
use sha2::{Digest, Sha512};
use tokio::process::Command;

use crate::dotnet::{self, Dotnet};
use crate::proxy::SessionConfig;

const PACKAGE: &str = "fsautocomplete";
const FLAT_CONTAINER: &str = "https://api.nuget.org/v3-flatcontainer";
const REGISTRATION: &str = "https://api.nuget.org/v3/registration5-gz-semver2";
const UPDATE_CHECK_INTERVAL: Duration = Duration::from_secs(24 * 60 * 60);
const MAX_PACKAGE_BYTES: u64 = 1024 * 1024 * 1024;
const COMPLETE_MARKER: &str = ".complete";
const UPDATE_STAMP: &str = ".last-update-check";

/// A release version such as `0.85.3`.
pub type Version = (u64, u64, u64);

pub fn format_version(version: Version) -> String {
    format!("{}.{}.{}", version.0, version.1, version.2)
}

/// Parses a release version; pre-releases (`1.0.0-beta`) are not used.
fn parse_version(text: &str) -> Option<Version> {
    let mut parts = text.split('.');
    let version = (
        parts.next()?.parse().ok()?,
        parts.next()?.parse().ok()?,
        parts.next()?.parse().ok()?,
    );
    parts.next().is_none().then_some(version)
}

/// Reports what the installer is doing, for the status bar.
pub type Status = Arc<dyn Fn(String) + Send + Sync>;

#[derive(Debug, Clone, PartialEq)]
pub struct Installed {
    pub version: Version,
    pub dll: PathBuf,
    /// The SDK is newer than any build in the package: run with roll-forward.
    pub roll_forward: bool,
}

/// Returns an installed FsAutoComplete that can run on the given SDK,
/// downloading it first if there is none. When one is already installed it is
/// used straight away and a newer version, if any, is fetched in the
/// background for the next start.
pub async fn ensure(install_root: &Path, sdk_major: u32, status: Status) -> Result<Installed, String> {
    let root = install_root.to_path_buf();
    tokio::fs::create_dir_all(&root)
        .await
        .map_err(|e| format!("{}: {e}", root.display()))?;

    let current = {
        let root = root.clone();
        blocking(move || Ok(tidy(&root))).await?
    };
    let version = match current {
        Some(version) => {
            spawn_update_check(root.clone(), version, status);
            version
        }
        None => {
            status("Looking up the latest FsAutoComplete…".into());
            let root = root.clone();
            blocking(move || {
                let latest = fetch_latest_version()?;
                status(format!("Downloading FsAutoComplete {}…", format_version(latest)));
                install(&root, latest)?;
                touch(&root.join(UPDATE_STAMP));
                Ok(latest)
            })
            .await?
        }
    };
    select_build(&root, version, sdk_major)
}

/// The command that starts the server for a repository.
pub fn command(dotnet: &Dotnet, installed: &Installed, repository: &Path) -> Command {
    let mut command = Command::new(&dotnet.path);
    command.arg(&installed.dll).current_dir(repository);
    for (name, value) in dotnet::child_environment(&dotnet.path) {
        command.env(name, value);
    }
    if installed.roll_forward {
        command.env("DOTNET_ROLL_FORWARD", "LatestMajor");
    }
    command
}

pub fn session_config() -> SessionConfig {
    SessionConfig {
        initialization_options: json!({
            // Without this the server waits for an explicit "load workspace" request.
            "AutomaticWorkspaceInit": true,
            // Off by default; reported as hints tagged "unnecessary".
            "UnusedOpensAnalyzer": true,
        }),
    }
}

async fn blocking<T: Send + 'static>(
    work: impl FnOnce() -> Result<T, String> + Send + 'static,
) -> Result<T, String> {
    tokio::task::spawn_blocking(work)
        .await
        .map_err(|e| e.to_string())?
}

fn spawn_update_check(root: PathBuf, current: Version, status: Status) {
    tokio::task::spawn_blocking(move || {
        let stamp = root.join(UPDATE_STAMP);
        let checked_recently = std::fs::metadata(&stamp)
            .and_then(|m| m.modified())
            .ok()
            .and_then(|at| SystemTime::now().duration_since(at).ok())
            .is_some_and(|age| age < UPDATE_CHECK_INTERVAL);
        if checked_recently {
            return;
        }
        // Offline or NuGet unreachable: keep using what is installed.
        let Ok(latest) = fetch_latest_version() else { return };
        touch(&stamp);
        if latest > current && install(&root, latest).is_ok() {
            status(format!(
                "FsAutoComplete {} downloaded; it is used the next time the F# server starts.",
                format_version(latest)
            ));
        }
    });
}

fn touch(path: &Path) {
    let _ = std::fs::write(path, b"");
}

/// Removes leftovers (interrupted installs, superseded versions) and returns
/// the newest complete install.
fn tidy(root: &Path) -> Option<Version> {
    let mut complete = Vec::new();
    let mut leftovers = Vec::new();
    for entry in std::fs::read_dir(root).into_iter().flatten().flatten() {
        let path = entry.path();
        let name = entry.file_name().to_string_lossy().into_owned();
        match parse_version(&name) {
            Some(version) if path.join(COMPLETE_MARKER).is_file() => complete.push((version, path)),
            _ if name == UPDATE_STAMP => {}
            _ => leftovers.push(path),
        }
    }
    complete.sort();
    let newest = complete.pop();
    for path in leftovers.into_iter().chain(complete.into_iter().map(|(_, path)| path)) {
        // Best effort: on Windows files of a running server cannot be deleted.
        let _ = if path.is_dir() {
            std::fs::remove_dir_all(&path)
        } else {
            std::fs::remove_file(&path)
        };
    }
    newest.map(|(version, _)| version)
}

/// Picks the build (target framework) of an installed version for an SDK.
fn select_build(root: &Path, version: Version, sdk_major: u32) -> Result<Installed, String> {
    let version_dir = root.join(format_version(version));
    let mut frameworks: Vec<u32> = std::fs::read_dir(&version_dir)
        .into_iter()
        .flatten()
        .flatten()
        .filter(|entry| entry.path().join("fsautocomplete.dll").is_file())
        .filter_map(|entry| framework_major(&entry.file_name().to_string_lossy()))
        .collect();
    frameworks.sort_unstable();
    let (Some(oldest), Some(newest)) = (frameworks.first().copied(), frameworks.last().copied()) else {
        return Err(format!(
            "The FsAutoComplete install in {} is incomplete.",
            version_dir.display()
        ));
    };
    if sdk_major < oldest {
        return Err(format!(
            "FsAutoComplete {} needs the .NET {oldest} SDK or newer; this repository uses .NET {sdk_major}.",
            format_version(version)
        ));
    }
    let chosen = frameworks
        .iter()
        .copied()
        .filter(|major| *major <= sdk_major)
        .max()
        .unwrap_or(newest);
    Ok(Installed {
        version,
        dll: version_dir.join(format!("net{chosen}.0")).join("fsautocomplete.dll"),
        roll_forward: sdk_major > newest,
    })
}

/// `net10.0` → 10.
fn framework_major(name: &str) -> Option<u32> {
    name.strip_prefix("net")?.strip_suffix(".0")?.parse().ok()
}

// ------------------------------------------------------------------ download

fn http() -> ureq::Agent {
    ureq::Agent::config_builder()
        .timeout_connect(Some(Duration::from_secs(20)))
        .build()
        .into()
}

fn get_json(url: &str) -> Result<Value, String> {
    let text = http()
        .get(url)
        .call()
        .map_err(|e| format!("{url}: {e}"))?
        .body_mut()
        .read_to_string()
        .map_err(|e| format!("{url}: {e}"))?;
    serde_json::from_str(&text).map_err(|e| format!("{url}: {e}"))
}

fn fetch_latest_version() -> Result<Version, String> {
    let index = get_json(&format!("{FLAT_CONTAINER}/{PACKAGE}/index.json"))?;
    index["versions"]
        .as_array()
        .into_iter()
        .flatten()
        .filter_map(|version| parse_version(version.as_str()?))
        .max()
        .ok_or_else(|| "NuGet lists no FsAutoComplete release".to_string())
}

/// SHA-512 of the package as recorded by NuGet (base64).
fn fetch_package_hash(version: &str) -> Result<String, String> {
    let registration = get_json(&format!("{REGISTRATION}/{PACKAGE}/{version}.json"))?;
    let catalog_url = registration["catalogEntry"]
        .as_str()
        .ok_or("NuGet gave no catalog entry for the package")?;
    let entry = get_json(catalog_url)?;
    match (entry["packageHashAlgorithm"].as_str(), entry["packageHash"].as_str()) {
        (Some("SHA512"), Some(hash)) => Ok(hash.to_string()),
        _ => Err("NuGet gave no SHA-512 hash for the package".to_string()),
    }
}

/// Downloads, verifies and unpacks one version. Safe to interrupt: the
/// version folder only appears once it is complete.
fn install(root: &Path, version: Version) -> Result<(), String> {
    let name = format_version(version);
    let expected_hash = fetch_package_hash(&name)?;
    let package_path = root.join(format!("{name}.nupkg.part"));
    let staging = root.join(format!("{name}.tmp"));
    let result = (|| {
        let url = format!("{FLAT_CONTAINER}/{PACKAGE}/{name}/{PACKAGE}.{name}.nupkg");
        let actual_hash = download(&url, &package_path)?;
        if actual_hash != expected_hash {
            return Err(format!("The download of FsAutoComplete {name} is corrupt (hash mismatch)."));
        }
        let _ = std::fs::remove_dir_all(&staging);
        let package = std::fs::File::open(&package_path).map_err(|e| e.to_string())?;
        if unpack_tools(package, &staging)? == 0 {
            return Err(format!("FsAutoComplete {name} contains no server builds."));
        }
        std::fs::write(staging.join(COMPLETE_MARKER), b"").map_err(|e| e.to_string())?;
        let target = root.join(&name);
        let _ = std::fs::remove_dir_all(&target);
        std::fs::rename(&staging, &target).map_err(|e| e.to_string())
    })();
    let _ = std::fs::remove_file(&package_path);
    if result.is_err() {
        let _ = std::fs::remove_dir_all(&staging);
    }
    result
}

/// Streams `url` into `target` and returns the base64 SHA-512 of what was written.
fn download(url: &str, target: &Path) -> Result<String, String> {
    let response = http().get(url).call().map_err(|e| format!("{url}: {e}"))?;
    let mut body = response
        .into_body()
        .into_with_config()
        .limit(MAX_PACKAGE_BYTES)
        .reader();
    let mut file = std::fs::File::create(target).map_err(|e| format!("{}: {e}", target.display()))?;
    let mut hasher = Sha512::new();
    let mut buffer = vec![0u8; 256 * 1024];
    loop {
        let read = body.read(&mut buffer).map_err(|e| format!("{url}: {e}"))?;
        if read == 0 {
            break;
        }
        hasher.update(&buffer[..read]);
        file.write_all(&buffer[..read]).map_err(|e| e.to_string())?;
    }
    Ok(base64::engine::general_purpose::STANDARD.encode(hasher.finalize()))
}

/// Unpacks `tools/<framework>/any/**` from a NuGet tool package into
/// `<target>/<framework>/**`. Returns the number of files written.
fn unpack_tools(package: impl Read + std::io::Seek, target: &Path) -> Result<usize, String> {
    let mut archive = zip::ZipArchive::new(package).map_err(|e| e.to_string())?;
    let mut written = 0;
    for index in 0..archive.len() {
        let mut entry = archive.by_index(index).map_err(|e| e.to_string())?;
        if entry.is_dir() {
            continue;
        }
        // `enclosed_name` rejects absolute paths and `..` components.
        let Some(path) = entry.enclosed_name() else { continue };
        let mut components = path.components();
        let (Some(first), Some(framework), Some(rid)) =
            (components.next(), components.next(), components.next())
        else {
            continue;
        };
        if first.as_os_str() != "tools" || rid.as_os_str() != "any" {
            continue;
        }
        let rest = components.as_path();
        if rest.as_os_str().is_empty() {
            continue;
        }
        let destination = target.join(framework.as_os_str()).join(rest);
        if let Some(parent) = destination.parent() {
            std::fs::create_dir_all(parent).map_err(|e| e.to_string())?;
        }
        let mut file = std::fs::File::create(&destination).map_err(|e| e.to_string())?;
        std::io::copy(&mut entry, &mut file).map_err(|e| e.to_string())?;
        written += 1;
    }
    Ok(written)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Cursor;
    use zip::write::SimpleFileOptions;

    #[test]
    fn parses_release_versions_only() {
        assert_eq!(parse_version("0.85.3"), Some((0, 85, 3)));
        assert_eq!(parse_version("0.85.3-beta.1"), None);
        assert_eq!(parse_version("1.2"), None);
        assert_eq!(parse_version("1.2.3.4"), None);
        assert!(parse_version("0.100.0") > parse_version("0.85.3"));
    }

    fn fake_install(root: &Path, version: &str, frameworks: &[&str], complete: bool) {
        let dir = root.join(version);
        for framework in frameworks {
            std::fs::create_dir_all(dir.join(framework)).unwrap();
            std::fs::write(dir.join(framework).join("fsautocomplete.dll"), b"").unwrap();
        }
        if complete {
            std::fs::write(dir.join(COMPLETE_MARKER), b"").unwrap();
        }
    }

    #[test]
    fn tidy_keeps_only_the_newest_complete_install() {
        let dir = tempfile::tempdir().unwrap();
        let root = dir.path();
        fake_install(root, "0.84.0", &["net9.0"], true);
        fake_install(root, "0.85.3", &["net10.0"], true);
        fake_install(root, "0.86.0", &["net10.0"], false); // interrupted
        std::fs::create_dir(root.join("0.86.0.tmp")).unwrap();
        std::fs::write(root.join("0.86.0.nupkg.part"), b"partial").unwrap();
        touch(&root.join(UPDATE_STAMP));

        assert_eq!(tidy(root), Some((0, 85, 3)));
        let mut left: Vec<_> = std::fs::read_dir(root)
            .unwrap()
            .map(|e| e.unwrap().file_name().to_string_lossy().into_owned())
            .collect();
        left.sort();
        assert_eq!(left, vec![UPDATE_STAMP, "0.85.3"]);

        assert_eq!(tidy(tempfile::tempdir().unwrap().path()), None);
    }

    #[test]
    fn selects_the_build_matching_the_sdk() {
        let dir = tempfile::tempdir().unwrap();
        let root = dir.path();
        fake_install(root, "0.85.3", &["net8.0", "net9.0", "net10.0"], true);
        let pick = |sdk| select_build(root, (0, 85, 3), sdk);

        let ten = pick(10).unwrap();
        assert!(ten.dll.ends_with("0.85.3/net10.0/fsautocomplete.dll"));
        assert!(!ten.roll_forward);
        assert!(pick(9).unwrap().dll.ends_with("net9.0/fsautocomplete.dll"));

        let eleven = pick(11).unwrap();
        assert!(eleven.dll.ends_with("net10.0/fsautocomplete.dll"));
        assert!(eleven.roll_forward);

        assert!(pick(6).unwrap_err().contains(".NET 8 SDK or newer"));
    }

    #[test]
    fn unpacks_only_server_builds_and_stays_inside_the_target() {
        let mut bytes = Cursor::new(Vec::new());
        {
            let mut writer = zip::ZipWriter::new(&mut bytes);
            let options = SimpleFileOptions::default();
            for (name, content) in [
                ("tools/net10.0/any/fsautocomplete.dll", "dll"),
                ("tools/net10.0/any/runtimes/unix/lib/native.so", "so"),
                ("tools/net9.0/any/fsautocomplete.dll", "dll9"),
                ("fsautocomplete.nuspec", "metadata"),
                ("tools/net10.0/any/../../../../escape.txt", "evil"),
            ] {
                writer.start_file(name, options).unwrap();
                writer.write_all(content.as_bytes()).unwrap();
            }
            writer.finish().unwrap();
        }
        let dir = tempfile::tempdir().unwrap();
        let target = dir.path().join("out");
        bytes.set_position(0);

        assert_eq!(unpack_tools(bytes, &target).unwrap(), 3);
        assert_eq!(std::fs::read(target.join("net10.0/fsautocomplete.dll")).unwrap(), b"dll");
        assert!(target.join("net10.0/runtimes/unix/lib/native.so").is_file());
        assert!(target.join("net9.0/fsautocomplete.dll").is_file());
        assert!(!dir.path().join("escape.txt").exists());
        assert!(!target.join("fsautocomplete.nuspec").exists());
    }
}
