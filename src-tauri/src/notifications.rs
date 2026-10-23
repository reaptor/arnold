//! Notifications of the operating system, for news about pull requests the
//! user follows. A click on one brings Arnold to the front and tells the
//! webview which it was, so it can go there.

use tauri::AppHandle;
#[cfg(any(target_os = "macos", windows))]
use tauri::{Emitter, Manager};

/// Shows a notification. A click on it brings the window to the front and
/// emits `notification-clicked` with `key`. Each one waits on a thread of its
/// own for what the user does with it: on macOS until it is clicked or
/// cleared from the Notification Center, on Windows while the toast is on
/// screen (a click in the Action Center is not reported). Other systems show
/// nothing.
#[tauri::command]
pub fn notify(app: AppHandle, title: String, body: String, key: String) -> Result<(), String> {
    #[cfg(any(target_os = "macos", windows))]
    std::thread::Builder::new()
        .name("notification".into())
        .spawn(move || show(&app, &title, &body, &key))
        .map_err(|e| e.to_string())?;
    #[cfg(not(any(target_os = "macos", windows)))]
    let _ = (app, title, body, key);
    Ok(())
}

#[cfg(any(target_os = "macos", windows))]
fn show(app: &AppHandle, title: &str, body: &str, key: &str) {
    use notify_rust::NotificationResponse;

    let identifier = &app.config().identifier;
    let mut notification = notify_rust::Notification::new();
    notification.summary(title).body(body);
    // A development build is no app bundle: its notifications go out as Terminal's.
    #[cfg(target_os = "macos")]
    let _ = notify_rust::set_application(if tauri::is_dev() { "com.apple.Terminal" } else { identifier });
    // Toasts need the id the installer gave the Start menu shortcut; a build
    // run from `target` has none, and shows them as PowerShell's.
    #[cfg(windows)]
    if !from_target_folder() {
        notification.app_id(identifier);
    }
    let Ok(handle) = notification.show() else { return };
    let _ = handle.wait_for_response(|response: &NotificationResponse| {
        if !matches!(response, NotificationResponse::Default | NotificationResponse::Action(_)) {
            return;
        }
        if let Some(window) = app.get_webview_window("main") {
            let _ = window.unminimize();
            let _ = window.show();
            let _ = window.set_focus();
        }
        let _ = app.emit("notification-clicked", key);
    });
}

#[cfg(windows)]
fn from_target_folder() -> bool {
    let Ok(exe) = tauri::utils::platform::current_exe() else { return true };
    exe.parent().is_some_and(|folder| {
        folder.ends_with("target/debug") || folder.ends_with("target/release")
    })
}
