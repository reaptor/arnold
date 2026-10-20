//! The GitHub Copilot CLI, which runs the Copilot conversations: downloaded
//! from GitHub's releases into Arnold's data folder the first time it is
//! needed, never bundled with the app.
//!
//! The version is pinned to the one the SDK crate was built and tested
//! against (its `cli-version.txt`), so the two always speak the same
//! protocol; a new version arrives with an Arnold update that moves the pin.
//!
//! Layout under the install root:
//! ```text
//! 1.0.100/.complete   marks a finished install
//! 1.0.100/copilot     (copilot.exe on Windows)
//! ```

use std::io::{Read, Write};
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::Duration;

use sha2::{Digest, Sha256};

/// Keep in step with `cli-version.txt` of the `github-copilot-sdk` version in Cargo.toml.
pub const VERSION: &str = "1.0.100";
const RELEASES: &str = "https://github.com/github/copilot-cli/releases/download";
const COMPLETE_MARKER: &str = ".complete";
const MAX_ARCHIVE_BYTES: u64 = 1024 * 1024 * 1024;

/// Reports what the installer is doing.
pub type Status = Arc<dyn Fn(String) + Send + Sync>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Asset {
    name: &'static str,
    sha256: &'static str,
    binary: &'static str,
}

/// The release asset for this platform (hashes from the release's SHA256SUMS.txt).
fn asset() -> Option<Asset> {
    let (name, sha256) = match (std::env::consts::OS, std::env::consts::ARCH) {
        ("macos", "aarch64") => ("copilot-darwin-arm64.tar.gz", "4af0815e798158641bdd8e76e622fee0975e24ef91c5f0daf9ada4d9e0eac5a0"),
        ("macos", "x86_64") => ("copilot-darwin-x64.tar.gz", "dedf0a0b57ef845067d28195ac1ba9668269de45d521a72bb81f74a3f13a5a7b"),
        ("windows", "x86_64") => ("copilot-win32-x64.zip", "28c02a668474305f765c285d8dcc529e66ae93da31537c92c7467df7714e73e0"),
        ("windows", "aarch64") => ("copilot-win32-arm64.zip", "d2fa14db0da65735daca2a690c254380614d5630840a431c98f39dbcaad4ae23"),
        ("linux", "x86_64") => ("copilot-linux-x64.tar.gz", "65dfb019192afebed7abb385323a232a5ac32fa8c40a230880d4013cffb6b0b3"),
        ("linux", "aarch64") => ("copilot-linux-arm64.tar.gz", "e7eee160da29e47fe471997f43366cc0606e3a8a1ae97ac74df612f1894dde6a"),
        _ => return None,
    };
    let binary = if cfg!(windows) { "copilot.exe" } else { "copilot" };
    Some(Asset { name, sha256, binary })
}

/// The installed CLI, if the pinned version is there.
pub fn installed(root: &Path) -> Option<PathBuf> {
    if let Some(path) = std::env::var_os("COPILOT_CLI_PATH").map(PathBuf::from).filter(|p| p.is_file()) {
        return Some(path);
    }
    let folder = root.join(VERSION);
    let binary = folder.join(asset()?.binary);
    (folder.join(COMPLETE_MARKER).is_file() && binary.is_file()).then_some(binary)
}

/// Returns the CLI, downloading it first if it is not installed yet.
/// `COPILOT_CLI_PATH` points at another copy (for development).
pub async fn ensure(root: &Path, status: Status) -> Result<PathBuf, String> {
    if let Some(path) = installed(root) {
        return Ok(path);
    }
    let asset = asset().ok_or_else(|| {
        format!(
            "GitHub Copilot is not available for {} on {}.",
            std::env::consts::OS,
            std::env::consts::ARCH
        )
    })?;
    let root = root.to_path_buf();
    tokio::task::spawn_blocking(move || {
        std::fs::create_dir_all(&root).map_err(|e| format!("{}: {e}", root.display()))?;
        install(&root, asset, &status)?;
        tidy(&root);
        Ok(root.join(VERSION).join(asset.binary))
    })
    .await
    .map_err(|e| e.to_string())?
}

/// Removes other versions and interrupted installs (best effort: a running
/// CLI cannot be deleted on Windows).
fn tidy(root: &Path) {
    for entry in std::fs::read_dir(root).into_iter().flatten().flatten() {
        if entry.file_name() != VERSION {
            let path = entry.path();
            let _ = if path.is_dir() {
                std::fs::remove_dir_all(&path)
            } else {
                std::fs::remove_file(&path)
            };
        }
    }
}

/// Downloads, verifies and unpacks the CLI. Safe to interrupt: the version
/// folder only appears once it is complete.
fn install(root: &Path, asset: Asset, status: &Status) -> Result<(), String> {
    let archive = root.join(format!("{}.part", asset.name));
    let staging = root.join(format!("{VERSION}.tmp"));
    let result = (|| {
        let url = format!("{RELEASES}/v{VERSION}/{}", asset.name);
        let hash = download(&url, &archive, status)?;
        if hash != asset.sha256 {
            return Err(format!("The download of GitHub Copilot CLI {VERSION} is corrupt (hash mismatch)."));
        }
        status("Unpacking GitHub Copilot CLI…".into());
        let _ = std::fs::remove_dir_all(&staging);
        std::fs::create_dir_all(&staging).map_err(|e| e.to_string())?;
        let file = std::fs::File::open(&archive).map_err(|e| e.to_string())?;
        let target = staging.join(asset.binary);
        if asset.name.ends_with(".zip") {
            unpack_zip(file, asset.binary, &target)?;
        } else {
            unpack_tar_gz(file, asset.binary, &target)?;
        }
        std::fs::write(staging.join(COMPLETE_MARKER), b"").map_err(|e| e.to_string())?;
        let folder = root.join(VERSION);
        let _ = std::fs::remove_dir_all(&folder);
        std::fs::rename(&staging, &folder).map_err(|e| e.to_string())
    })();
    let _ = std::fs::remove_file(&archive);
    if result.is_err() {
        let _ = std::fs::remove_dir_all(&staging);
    }
    result
}

fn http() -> ureq::Agent {
    ureq::Agent::config_builder()
        .timeout_connect(Some(Duration::from_secs(20)))
        // The system's certificate store, which may hold a company root.
        .tls_config(
            ureq::tls::TlsConfig::builder()
                .root_certs(ureq::tls::RootCerts::PlatformVerifier)
                .build(),
        )
        .build()
        .into()
}

/// Streams `url` into `target`, reporting progress, and returns the hex SHA-256 of what was written.
fn download(url: &str, target: &Path, status: &Status) -> Result<String, String> {
    status("Downloading GitHub Copilot CLI…".into());
    let response = http().get(url).call().map_err(|e| crate::network_error(url, &e))?;
    let total = response
        .headers()
        .get("content-length")
        .and_then(|value| value.to_str().ok())
        .and_then(|value| value.parse::<u64>().ok());
    let mut body = response
        .into_body()
        .into_with_config()
        .limit(MAX_ARCHIVE_BYTES)
        .reader();
    let mut file = std::fs::File::create(target).map_err(|e| format!("{}: {e}", target.display()))?;
    let mut hasher = Sha256::new();
    let mut buffer = vec![0u8; 256 * 1024];
    let mut written: u64 = 0;
    let mut reported = 0;
    loop {
        let read = body.read(&mut buffer).map_err(|e| format!("{url}: {e}"))?;
        if read == 0 {
            break;
        }
        hasher.update(&buffer[..read]);
        file.write_all(&buffer[..read]).map_err(|e| e.to_string())?;
        written += read as u64;
        if let Some(total) = total.filter(|total| *total > 0) {
            let percent = (written * 100 / total).min(100);
            if percent >= reported + 2 {
                reported = percent;
                status(format!("Downloading GitHub Copilot CLI… {percent}% of {} MB", total / 1_000_000));
            }
        }
    }
    Ok(hasher
        .finalize()
        .iter()
        .map(|byte| format!("{byte:02x}"))
        .collect())
}

fn unpack_tar_gz(archive: impl Read, binary: &str, target: &Path) -> Result<(), String> {
    let mut archive = tar::Archive::new(flate2::read::GzDecoder::new(archive));
    for entry in archive.entries().map_err(|e| e.to_string())? {
        let mut entry = entry.map_err(|e| e.to_string())?;
        let path = entry.path().map_err(|e| e.to_string())?;
        if path.file_name().is_some_and(|name| name == binary) && entry.header().entry_type().is_file() {
            let mut file = std::fs::File::create(target).map_err(|e| e.to_string())?;
            std::io::copy(&mut entry, &mut file).map_err(|e| e.to_string())?;
            make_executable(target)?;
            return Ok(());
        }
    }
    Err(format!("The GitHub Copilot CLI download has no {binary}."))
}

fn unpack_zip(archive: impl Read + std::io::Seek, binary: &str, target: &Path) -> Result<(), String> {
    let mut archive = zip::ZipArchive::new(archive).map_err(|e| e.to_string())?;
    for index in 0..archive.len() {
        let mut entry = archive.by_index(index).map_err(|e| e.to_string())?;
        let matches = entry
            .enclosed_name()
            .is_some_and(|path| path.file_name().is_some_and(|name| name == binary));
        if matches && entry.is_file() {
            let mut file = std::fs::File::create(target).map_err(|e| e.to_string())?;
            std::io::copy(&mut entry, &mut file).map_err(|e| e.to_string())?;
            make_executable(target)?;
            return Ok(());
        }
    }
    Err(format!("The GitHub Copilot CLI download has no {binary}."))
}

fn make_executable(path: &Path) -> Result<(), String> {
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        std::fs::set_permissions(path, std::fs::Permissions::from_mode(0o755)).map_err(|e| e.to_string())?;
    }
    let _ = path;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Cursor;

    #[test]
    fn has_an_asset_for_the_release_targets() {
        let asset = asset().expect("this platform has a Copilot CLI build");
        assert_eq!(asset.sha256.len(), 64);
        if cfg!(all(target_os = "macos", target_arch = "aarch64")) {
            assert_eq!(asset.name, "copilot-darwin-arm64.tar.gz");
        }
    }

    #[test]
    fn finds_an_install_only_once_complete() {
        let root = tempfile::tempdir().unwrap();
        let folder = root.path().join(VERSION);
        std::fs::create_dir_all(&folder).unwrap();
        std::fs::write(folder.join(asset().unwrap().binary), b"").unwrap();
        if std::env::var_os("COPILOT_CLI_PATH").is_none() {
            assert_eq!(installed(root.path()), None);
            std::fs::write(folder.join(COMPLETE_MARKER), b"").unwrap();
            assert_eq!(installed(root.path()), Some(folder.join(asset().unwrap().binary)));
        }
    }

    #[test]
    fn tidy_keeps_only_the_pinned_version() {
        let root = tempfile::tempdir().unwrap();
        for name in [VERSION, "0.9.0", "1.0.100.tmp"] {
            std::fs::create_dir_all(root.path().join(name)).unwrap();
        }
        std::fs::write(root.path().join("x.tar.gz.part"), b"").unwrap();
        tidy(root.path());
        let left: Vec<_> = std::fs::read_dir(root.path()).unwrap().flatten().map(|e| e.file_name()).collect();
        assert_eq!(left, [VERSION]);
    }

    #[test]
    fn unpacks_the_binary_from_a_tarball() {
        let mut builder = tar::Builder::new(flate2::write::GzEncoder::new(Vec::new(), flate2::Compression::fast()));
        let mut header = tar::Header::new_gnu();
        header.set_size(5);
        header.set_mode(0o644);
        header.set_cksum();
        builder.append_data(&mut header, "copilot", &b"hello"[..]).unwrap();
        let archive = builder.into_inner().unwrap().finish().unwrap();
        let dir = tempfile::tempdir().unwrap();
        let target = dir.path().join("copilot");
        unpack_tar_gz(Cursor::new(archive), "copilot", &target).unwrap();
        assert_eq!(std::fs::read(&target).unwrap(), b"hello");
        assert!(unpack_tar_gz(Cursor::new(Vec::<u8>::new()), "copilot", &target).is_err());
    }
}
