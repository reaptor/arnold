//! Runs the real FsAutoComplete: downloads it if needed (about 80 MB, kept
//! under `target/` between runs), starts it on a small project and waits for a
//! compiler error to come back through the proxy.
//!
//! Needs the .NET SDK and network access, so it only runs on request:
//! `cargo test -p arnold-lsp --test fsautocomplete_live -- --ignored --nocapture`

use std::path::{Path, PathBuf};
use std::sync::{mpsc, Arc, Mutex};
use std::time::{Duration, Instant};

use arnold_lsp::{dotnet, fsautocomplete, Event, Session};
use serde_json::{json, Value};

const SOURCE: &str = "module Lib\n\nlet answer: int = \"forty-two\"\n";

fn file_uri(path: &Path) -> String {
    format!("file://{}", path.to_string_lossy().replace('\\', "/"))
}

fn write_project(dir: &Path, sdk_major: u32) {
    let project = format!(
        r#"<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup><TargetFramework>net{sdk_major}.0</TargetFramework></PropertyGroup>
  <ItemGroup><Compile Include="Lib.fs" /></ItemGroup>
</Project>
"#
    );
    std::fs::write(dir.join("Live.fsproj"), project).unwrap();
    std::fs::write(dir.join("Lib.fs"), SOURCE).unwrap();
}

/// Waits for an event that `pick` accepts, printing server logs meanwhile.
fn wait_for<T>(events: &mpsc::Receiver<Event>, limit: Duration, mut pick: impl FnMut(&Event) -> Option<T>) -> T {
    let deadline = Instant::now() + limit;
    loop {
        let remaining = deadline.saturating_duration_since(Instant::now());
        let event = events.recv_timeout(remaining).expect("the server answered in time");
        match &event {
            Event::Log(line) => eprintln!("[server] {line}"),
            Event::Progress(Some(text)) => eprintln!("[progress] {text}"),
            Event::Exited => panic!("the server exited unexpectedly"),
            _ => {}
        }
        if let Some(found) = pick(&event) {
            return found;
        }
    }
}

#[tokio::test(flavor = "multi_thread")]
#[ignore = "downloads and runs the real FsAutoComplete"]
async fn reports_a_compiler_error_from_the_real_server() {
    let project = tempfile::tempdir().unwrap();
    // The server reports canonical paths (on macOS /var is a symlink).
    let dir = project.path().canonicalize().unwrap();
    let dotnet = dotnet::locate(&dir).await.expect(".NET SDK");
    write_project(&dir, dotnet.sdk_major);
    let restored = std::process::Command::new(&dotnet.path)
        .args(["restore", "--nologo"])
        .current_dir(&dir)
        .status()
        .unwrap();
    assert!(restored.success(), "dotnet restore");

    let servers = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../target/arnold-test-servers/fsautocomplete");
    let status: fsautocomplete::Status = Arc::new(|text| eprintln!("[install] {text}"));
    let installed = fsautocomplete::ensure(&servers, dotnet.sdk_major, status)
        .await
        .expect("FsAutoComplete installed");
    eprintln!("[install] using {}", installed.dll.display());
    assert!(installed.dll.is_file());

    let (sender, events) = mpsc::channel();
    let sender = Mutex::new(sender);
    let session = Session::spawn(
        fsautocomplete::command(&dotnet, &installed, &dir),
        fsautocomplete::session_config(),
        move |event| {
            let _ = sender.lock().unwrap().send(event);
        },
    )
    .expect("server starts");

    session.send(
        json!({
            "jsonrpc": "2.0", "id": 1, "method": "initialize",
            "params": { "rootUri": file_uri(&dir), "capabilities": { "textDocument": { "publishDiagnostics": {} } } }
        })
        .to_string(),
    );
    let capabilities = wait_for(&events, Duration::from_secs(120), |event| match event {
        Event::Message(text) => {
            let message: Value = serde_json::from_str(text).ok()?;
            (message["id"] == json!(1)).then(|| message["result"]["capabilities"].clone())
        }
        _ => None,
    });
    assert!(capabilities["hoverProvider"].as_bool().unwrap_or(false) || capabilities["hoverProvider"].is_object());
    assert!(!capabilities["definitionProvider"].is_null());

    session.send(json!({ "jsonrpc": "2.0", "method": "initialized", "params": {} }).to_string());
    let uri = file_uri(&dir.join("Lib.fs"));
    session.send(
        json!({
            "jsonrpc": "2.0", "method": "textDocument/didOpen",
            "params": { "textDocument": { "uri": uri, "languageId": "fsharp", "version": 1, "text": SOURCE } }
        })
        .to_string(),
    );

    let errors = wait_for(&events, Duration::from_secs(180), |event| match event {
        Event::Diagnostics { uri, errors, .. } if uri.ends_with("Lib.fs") && *errors > 0 => Some(*errors),
        _ => None,
    });
    assert_eq!(errors, 1, "exactly the planted type error");

    session.shutdown().await;
}
