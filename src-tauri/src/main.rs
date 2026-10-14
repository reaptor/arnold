// No console window next to the app on Windows release builds.
#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]

mod commands;
mod fsio;
mod lsp;
mod watcher;

use tauri_plugin_window_state::StateFlags;

fn main() {
    tauri::Builder::default()
        .plugin(tauri_plugin_dialog::init())
        // Reopens the window where it was, maximized if it was. The window
        // starts hidden (tauri.conf.json) and the plugin shows it once placed,
        // so it does not flash at the default size first.
        .plugin(
            tauri_plugin_window_state::Builder::default()
                .with_state_flags(
                    StateFlags::POSITION
                        | StateFlags::SIZE
                        | StateFlags::MAXIMIZED
                        | StateFlags::VISIBLE,
                )
                .build(),
        )
        .manage(commands::AppState::default())
        .manage(lsp::LspState::default())
        .invoke_handler(tauri::generate_handler![
            commands::dev_trace,
            commands::startup_path,
            commands::open_repo,
            commands::compare_branches,
            commands::list_files,
            commands::read_file_pair,
            commands::write_file,
            commands::reset_file,
            lsp::lsp_start,
            lsp::lsp_send,
            lsp::lsp_open_background,
            lsp::lsp_stop,
        ])
        .run(tauri::generate_context!())
        .expect("error while running Arnold");
}
