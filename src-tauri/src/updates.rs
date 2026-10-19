//! Arnold's own updates, from the GitHub releases: `latest.json` there names the newest
//! version and its signed download per platform (see .github/workflows/release.yml).
//!
//! The webview drives it: it checks now and then, downloads what was found in the
//! background, and installs once the user chooses to restart. Debug builds check but never
//! download or install, since there is no installed app to replace.

use std::sync::Mutex;

use serde::Serialize;
use tauri::ipc::Channel;
use tauri::{AppHandle, State};
use tauri_plugin_updater::{Update, UpdaterExt};

#[derive(Default)]
pub struct UpdateState {
    /// The update the last check found.
    found: Mutex<Option<Update>>,
    /// The downloaded, verified package of `found`, by its version.
    downloaded: Mutex<Option<(String, Vec<u8>)>>,
}

/// Whether this build can update itself.
const CAN_INSTALL: bool = !cfg!(debug_assertions);

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct UpdaterInfo {
    version: String,
    can_install: bool,
}

#[tauri::command]
pub fn updater_info(app: AppHandle) -> UpdaterInfo {
    UpdaterInfo {
        version: app.package_info().version.to_string(),
        can_install: CAN_INSTALL,
    }
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct AvailableUpdate {
    version: String,
    /// Release notes (Markdown).
    notes: Option<String>,
}

/// Asks for a newer version than the running one; `None` when this is the newest.
#[tauri::command]
pub async fn check_for_update(app: AppHandle, state: State<'_, UpdateState>) -> Result<Option<AvailableUpdate>, String> {
    let update = app
        .updater()
        .map_err(|error| error.to_string())?
        .check()
        .await
        .map_err(|error| error.to_string())?;
    let available = update.as_ref().map(|update| AvailableUpdate {
        version: update.version.clone(),
        notes: update.body.clone().filter(|notes| !notes.trim().is_empty()),
    });
    *state.found.lock().unwrap() = update;
    Ok(available)
}

#[derive(Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct DownloadProgress {
    downloaded: u64,
    total: Option<u64>,
}

/// Downloads and verifies the update the last check found, ready for `install_update`.
#[tauri::command]
pub async fn download_update(on_progress: Channel<DownloadProgress>, state: State<'_, UpdateState>) -> Result<(), String> {
    if !CAN_INSTALL {
        return Err("Development builds do not update themselves.".into());
    }
    let update = state.found.lock().unwrap().clone().ok_or("No update was found to download.")?;
    if state.downloaded.lock().unwrap().as_ref().is_some_and(|(version, _)| *version == update.version) {
        return Ok(());
    }
    let mut downloaded = 0u64;
    let mut reported = 0u64;
    let bytes = update
        .download(
            |chunk, total| {
                downloaded += chunk as u64;
                // Every chunk would be thousands of messages; a percent at a time is plenty.
                let step = total.map_or(1 << 20, |total| (total / 100).max(1));
                if downloaded - reported >= step || Some(downloaded) == total {
                    reported = downloaded;
                    let _ = on_progress.send(DownloadProgress { downloaded, total });
                }
            },
            || {},
        )
        .await
        .map_err(|error| error.to_string())?;
    *state.downloaded.lock().unwrap() = Some((update.version.clone(), bytes));
    Ok(())
}

/// Installs the downloaded update and starts the new version. On Windows the installer
/// takes over: it closes Arnold, updates it and starts it again.
#[tauri::command]
pub fn install_update(app: AppHandle, state: State<'_, UpdateState>) -> Result<(), String> {
    if !CAN_INSTALL {
        return Err("Development builds do not update themselves.".into());
    }
    let update = state.found.lock().unwrap().clone().ok_or("No update was found to install.")?;
    let (version, bytes) = state.downloaded.lock().unwrap().take().ok_or("The update has not been downloaded.")?;
    if version != update.version {
        return Err("The update has not been downloaded.".into());
    }
    if let Err(error) = update.install(&bytes) {
        *state.downloaded.lock().unwrap() = Some((version, bytes));
        return Err(error.to_string());
    }
    app.restart();
}
