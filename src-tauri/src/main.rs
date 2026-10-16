// No console window next to the app on Windows release builds.
#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]

mod commands;
mod formatting;
mod fsio;
mod lsp;
mod pull_requests;
mod watcher;
mod workspace;
mod working;

use tauri::WebviewWindowBuilder;
use tauri_plugin_window_state::StateFlags;

fn main() {
    tauri::Builder::default()
        .plugin(tauri_plugin_dialog::init())
        .plugin(tauri_plugin_opener::init())
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
        // The main window is created here rather than from tauri.conf.json so
        // it can differ per platform: the toolbar is the title bar, as in
        // Rider. macOS keeps its window buttons over the toolbar (the config's
        // overlay title bar); Windows gets no system title bar and the
        // toolbar draws minimize/maximize/close itself.
        .setup(|app| {
            let mut config = app
                .config()
                .app
                .windows
                .iter()
                .find(|window| window.label == "main")
                .cloned()
                .expect("tauri.conf.json defines the main window");
            if cfg!(windows) {
                config.decorations = false;
            }
            WebviewWindowBuilder::from_config(app.handle(), &config)?.build()?;
            Ok(())
        })
        .manage(commands::AppState::default())
        .manage(lsp::LspState::default())
        .manage(workspace::PrepareState::default())
        .manage(pull_requests::PrState::default())
        .invoke_handler(tauri::generate_handler![
            commands::dev_trace,
            commands::startup_path,
            commands::open_repo,
            commands::list_branches,
            commands::compare_branches,
            commands::list_files,
            commands::read_file_pair,
            working::working_status,
            working::stop_watching_checkout,
            working::read_working_pair,
            working::worktree_files,
            working::stage_files,
            working::unstage_files,
            working::discard_files,
            working::reset_files_to_head,
            working::set_index_text,
            working::save_worktree_file,
            working::commit_staged,
            working::push_checkout_branch,
            formatting::check_formatting,
            formatting::format_and_stage,
            pull_requests::pull_request_remotes,
            pull_requests::list_pull_requests,
            pull_requests::set_pull_request_token,
            pull_requests::remove_pull_request_token,
            pull_requests::open_pull_request,
            pull_requests::pull_request_threads,
            pull_requests::create_pull_request_thread,
            pull_requests::reply_to_pull_request_thread,
            pull_requests::vote_on_pull_request,
            pull_requests::update_pull_request,
            pull_requests::plan_pull_request,
            pull_requests::create_pull_request,
            pull_requests::work_item,
            workspace::prepare_workspace,
            lsp::lsp_start,
            lsp::lsp_send,
            lsp::lsp_open_background,
            lsp::lsp_stop,
        ])
        .run(tauri::generate_context!())
        .expect("error while running Arnold");
}
