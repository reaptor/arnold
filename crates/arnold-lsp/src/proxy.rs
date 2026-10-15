//! Sits between the editor's LSP client (in the webview) and a language server
//! process.
//!
//! Most messages pass straight through. The proxy steps in where the editor
//! client is deliberately small:
//! - it completes the `initialize` request (process id, workspace folders,
//!   server-specific options);
//! - it answers the housekeeping requests servers send to clients, which the
//!   editor client does not handle;
//! - it reports progress, per-file diagnostic counts and FsAutoComplete's
//!   workspace loading as events, for the status bar and the files dock;
//! - it can keep files open with the server that no editor shows, so that
//!   errors are known for the whole change, and hands such a file over
//!   cleanly when an editor opens it.

use std::collections::{HashMap, HashSet};
use std::process::Stdio;
use std::sync::{Arc, Mutex};
use std::time::Duration;

use serde_json::{json, Value};
use tokio::io::{AsyncBufReadExt, AsyncRead, AsyncWrite, BufReader};
use tokio::process::{Child, Command};
use tokio::sync::mpsc;

use crate::framing::{read_message, write_message};
use crate::msbuild;

#[derive(Debug, Clone, PartialEq)]
pub enum Event {
    /// A message for the editor's LSP client.
    Message(String),
    /// What the server is busy with; `None` when it is idle.
    Progress(Option<String>),
    /// Diagnostics the server published for one file.
    Diagnostics { uri: String, errors: u32, warnings: u32 },
    /// FsAutoComplete started loading a project (it also announces every
    /// project of the workspace this way before it starts).
    ProjectLoading { project: String },
    /// FsAutoComplete loaded a project.
    ProjectLoaded { project: String },
    /// FsAutoComplete could not load a project. `message` is short; `details`
    /// holds the build log it was taken from, if any.
    ProjectFailed {
        project: String,
        message: String,
        details: Option<String>,
    },
    /// FsAutoComplete finished loading the projects of the workspace.
    WorkspaceLoaded,
    /// A line the server wrote to stderr.
    Log(String),
    /// The server's output ended: it exited or crashed.
    Exited,
}

#[derive(Debug, Clone, Default)]
pub struct SessionConfig {
    /// Merged into the `initializationOptions` of the `initialize` request.
    pub initialization_options: Value,
}

type EventSink = Arc<dyn Fn(Event) + Send + Sync>;

/// State shared between the two directions of the proxy.
#[derive(Default)]
struct Shared {
    workspace_folders: Mutex<Value>,
    documents: Mutex<Documents>,
}

/// Which files are open with the server, and on whose behalf.
#[derive(Default)]
struct Documents {
    /// Opened by the editor's client.
    client: HashSet<String>,
    /// Opened by the proxy itself (see `Session::open_in_background`).
    background: HashSet<String>,
}

/// A running language server and the proxy around it.
pub struct Session {
    to_server: mpsc::UnboundedSender<String>,
    config: SessionConfig,
    shared: Arc<Shared>,
    child: Option<Child>,
}

impl Session {
    /// Starts `command` as a language server speaking LSP on stdin/stdout.
    pub fn spawn(
        mut command: Command,
        config: SessionConfig,
        on_event: impl Fn(Event) + Send + Sync + 'static,
    ) -> std::io::Result<Session> {
        command
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .kill_on_drop(true);
        #[cfg(windows)]
        command.creation_flags(0x0800_0000); // CREATE_NO_WINDOW

        let mut child = command.spawn()?;
        let stdin = child.stdin.take().expect("stdin is piped");
        let stdout = child.stdout.take().expect("stdout is piped");
        let stderr = child.stderr.take().expect("stderr is piped");

        let on_event: EventSink = Arc::new(on_event);
        let log_sink = on_event.clone();
        tokio::spawn(async move {
            let mut lines = BufReader::new(stderr).lines();
            while let Ok(Some(line)) = lines.next_line().await {
                log_sink(Event::Log(line));
            }
        });

        let mut session = Session::over_streams(stdout, stdin, config, move |event| on_event(event));
        session.child = Some(child);
        Ok(session)
    }

    /// Runs the proxy over arbitrary streams (`from_server` is what the server
    /// writes, `to_server` what it reads).
    pub fn over_streams(
        from_server: impl AsyncRead + Send + Unpin + 'static,
        to_server: impl AsyncWrite + Send + Unpin + 'static,
        config: SessionConfig,
        on_event: impl Fn(Event) + Send + Sync + 'static,
    ) -> Session {
        let on_event: EventSink = Arc::new(on_event);
        let shared = Arc::new(Shared::default());
        let (sender, mut outgoing) = mpsc::unbounded_channel::<String>();

        tokio::spawn(async move {
            let mut to_server = to_server;
            while let Some(message) = outgoing.recv().await {
                if write_message(&mut to_server, &message).await.is_err() {
                    break;
                }
            }
        });

        let replies = sender.clone();
        let reader_shared = shared.clone();
        tokio::spawn(async move {
            let mut reader = BufReader::new(from_server);
            let mut progress = ProgressTracker::default();
            while let Ok(Some(message)) = read_message(&mut reader).await {
                let handled = handle_server_message(&message, &reader_shared, &mut progress);
                if let Some(reply) = handled.reply {
                    let _ = replies.send(reply);
                }
                for event in handled.events {
                    on_event(event);
                }
            }
            on_event(Event::Exited);
        });

        Session {
            to_server: sender,
            config,
            shared,
            child: None,
        }
    }

    /// Passes a message from the editor's client on to the server.
    pub fn send(&self, message: String) {
        // A file may be open only once: close the background copy before the
        // editor's own `didOpen` goes through.
        if let Some(uri) = track_documents(&message, &self.shared) {
            let _ = self.to_server.send(did_close(&uri));
        }
        let message = prepare_client_message(message, &self.config, &self.shared);
        let _ = self.to_server.send(message);
    }

    /// Opens a file with the server although no editor shows it, so that the
    /// server reports its diagnostics. Calling it again replaces the text.
    /// Does nothing while the editor's client has the file open.
    pub fn open_in_background(&self, uri: &str, language_id: &str, text: &str) {
        {
            let mut documents = self.shared.documents.lock().unwrap();
            if documents.client.contains(uri) {
                return;
            }
            if !documents.background.insert(uri.to_string()) {
                let _ = self.to_server.send(did_close(uri));
            }
        }
        let open = json!({
            "jsonrpc": "2.0",
            "method": "textDocument/didOpen",
            "params": { "textDocument": { "uri": uri, "languageId": language_id, "version": 0, "text": text } },
        });
        let _ = self.to_server.send(open.to_string());
    }

    /// Asks the server to exit, and kills it if it does not do so promptly.
    pub async fn shutdown(mut self) {
        let _ = self
            .to_server
            .send(json!({"jsonrpc": "2.0", "id": "arnold-shutdown", "method": "shutdown"}).to_string());
        let _ = self
            .to_server
            .send(json!({"jsonrpc": "2.0", "method": "exit"}).to_string());
        if let Some(mut child) = self.child.take() {
            if tokio::time::timeout(Duration::from_secs(2), child.wait())
                .await
                .is_err()
            {
                let _ = child.kill().await;
            }
        }
    }
}

fn did_close(uri: &str) -> String {
    json!({
        "jsonrpc": "2.0",
        "method": "textDocument/didClose",
        "params": { "textDocument": { "uri": uri } },
    })
    .to_string()
}

/// Notes which files the editor's client opens and closes. Returns the URI of
/// a background copy that must be closed before this message is passed on.
fn track_documents(message: &str, shared: &Shared) -> Option<String> {
    let opens = message.contains("\"textDocument/didOpen\"");
    if !opens && !message.contains("\"textDocument/didClose\"") {
        return None;
    }
    let value: Value = serde_json::from_str(message).ok()?;
    let method = value["method"].as_str()?;
    let uri = value["params"]["textDocument"]["uri"].as_str()?;
    let mut documents = shared.documents.lock().unwrap();
    match method {
        "textDocument/didOpen" => {
            documents.client.insert(uri.to_string());
            documents.background.remove(uri).then(|| uri.to_string())
        }
        "textDocument/didClose" => {
            documents.client.remove(uri);
            None
        }
        _ => None,
    }
}

fn prepare_client_message(message: String, config: &SessionConfig, shared: &Shared) -> String {
    // Cheap check first: only `initialize` is rewritten.
    if !message.contains("\"initialize\"") {
        return message;
    }
    let Ok(mut value) = serde_json::from_str::<Value>(&message) else {
        return message;
    };
    if value.get("method").and_then(Value::as_str) != Some("initialize") {
        return message;
    }
    let params = &mut value["params"];
    if !params.is_object() {
        *params = json!({});
    }
    // Lets the server notice when Arnold is gone.
    params["processId"] = json!(std::process::id());

    if let Some(extra) = config.initialization_options.as_object() {
        if !params["initializationOptions"].is_object() {
            params["initializationOptions"] = json!({});
        }
        for (key, option) in extra {
            params["initializationOptions"][key] = option.clone();
        }
    }

    if let Some(root_uri) = params["rootUri"].as_str().map(str::to_string) {
        if !params["workspaceFolders"].is_array() {
            let name = root_uri.trim_end_matches('/').rsplit('/').next().unwrap_or("");
            params["workspaceFolders"] = json!([{ "uri": root_uri, "name": name }]);
        }
    }
    *shared.workspace_folders.lock().unwrap() = params["workspaceFolders"].clone();

    // The proxy answers these on the client's behalf, so it may advertise them.
    params["capabilities"]["workspace"]["configuration"] = json!(true);
    params["capabilities"]["workspace"]["workspaceFolders"] = json!(true);
    params["capabilities"]["window"]["workDoneProgress"] = json!(true);
    value.to_string()
}

#[derive(Default)]
struct Handled {
    reply: Option<String>,
    events: Vec<Event>,
}

/// Work-done progress per token, so the status shows whatever is still running.
#[derive(Default)]
struct ProgressTracker {
    active: HashMap<String, String>,
    order: Vec<String>,
}

impl ProgressTracker {
    fn update(&mut self, params: &Value) -> Option<Event> {
        let token = params["token"].to_string();
        let value = &params["value"];
        match value["kind"].as_str()? {
            "end" => {
                self.active.remove(&token);
                self.order.retain(|t| *t != token);
            }
            kind => {
                let mut text = if kind == "begin" {
                    value["title"].as_str().unwrap_or_default().to_string()
                } else {
                    // A report repeats neither title nor earlier message.
                    self.active
                        .get(&token)
                        .map(|t| t.split(": ").next().unwrap_or_default().to_string())
                        .unwrap_or_default()
                };
                if let Some(message) = value["message"].as_str().filter(|m| !m.is_empty()) {
                    if !text.is_empty() {
                        text.push_str(": ");
                    }
                    text.push_str(message);
                }
                if !self.order.contains(&token) {
                    self.order.push(token.clone());
                }
                self.active.insert(token, text);
            }
        }
        let current = self.order.last().and_then(|t| self.active.get(t)).cloned();
        Some(Event::Progress(current))
    }
}

fn handle_server_message(message: &str, shared: &Shared, progress: &mut ProgressTracker) -> Handled {
    let forward = || Handled {
        reply: None,
        events: vec![Event::Message(message.to_string())],
    };
    let Ok(value) = serde_json::from_str::<Value>(message) else {
        return forward();
    };
    let Some(method) = value.get("method").and_then(Value::as_str) else {
        return forward(); // a response to one of the client's requests
    };

    if let Some(id) = value.get("id").filter(|id| !id.is_null()) {
        // A request from the server. The editor client has no handlers for
        // these, and an unanswered request can stall a server.
        let reply = match answer_server_request(method, &value["params"], shared) {
            Ok(result) => json!({ "jsonrpc": "2.0", "id": id, "result": result }),
            Err(()) => json!({
                "jsonrpc": "2.0",
                "id": id,
                "error": { "code": -32601, "message": format!("{method} is not supported") },
            }),
        };
        return Handled {
            reply: Some(reply.to_string()),
            events: Vec::new(),
        };
    }

    let mut handled = forward();
    match method {
        "$/progress" => handled.events.extend(progress.update(&value["params"])),
        "textDocument/publishDiagnostics" => {
            let params = &value["params"];
            let count = |severity: u64| {
                params["diagnostics"]
                    .as_array()
                    .map(|all| {
                        all.iter()
                            // A missing severity is treated as an error, as the spec suggests.
                            .filter(|d| d["severity"].as_u64().unwrap_or(1) == severity)
                            .count() as u32
                    })
                    .unwrap_or(0)
            };
            if let Some(uri) = params["uri"].as_str() {
                handled.events.push(Event::Diagnostics {
                    uri: uri.to_string(),
                    errors: count(1),
                    warnings: count(2),
                });
            }
        }
        "fsharp/notifyWorkspace" => handled.events.extend(workspace_event(&value["params"])),
        _ => {}
    }
    handled
}

/// FsAutoComplete reports workspace loading as JSON text in `content`.
fn workspace_event(params: &Value) -> Option<Event> {
    let content: Value = serde_json::from_str(params["content"].as_str()?).ok()?;
    let data = &content["Data"];
    let project = |data: &Value| data["Project"].as_str().unwrap_or_default().to_string();
    match content["Kind"].as_str()? {
        "projectLoading" => Some(Event::ProjectLoading { project: project(data) }),
        "project" => Some(Event::ProjectLoaded { project: project(data) }),
        "workspaceLoad" if data["Status"] == "finished" => Some(Event::WorkspaceLoaded),
        "error" => {
            let (message, details) = msbuild::explain(data["Message"].as_str().unwrap_or_default());
            Some(Event::ProjectFailed {
                // Errors carry the project in `AdditionalData`.
                project: project(&data["AdditionalData"]),
                message,
                details,
            })
        }
        _ => None,
    }
}

fn answer_server_request(method: &str, params: &Value, shared: &Shared) -> Result<Value, ()> {
    Ok(match method {
        // No client-side settings: one null per requested section means "use defaults".
        "workspace/configuration" => {
            let count = params["items"].as_array().map_or(0, Vec::len);
            Value::Array(vec![Value::Null; count])
        }
        "workspace/workspaceFolders" => shared.workspace_folders.lock().unwrap().clone(),
        "client/registerCapability"
        | "client/unregisterCapability"
        | "window/workDoneProgress/create"
        | "window/showMessageRequest"
        | "workspace/codeLens/refresh"
        | "workspace/diagnostic/refresh"
        | "workspace/inlayHint/refresh"
        | "workspace/inlineValue/refresh"
        | "workspace/semanticTokens/refresh" => Value::Null,
        "workspace/applyEdit" => json!({ "applied": false }),
        _ => return Err(()),
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::mpsc as std_mpsc;
    use tokio::io::{duplex, DuplexStream};

    /// A proxy session wired to an in-memory "server".
    struct Harness {
        session: Session,
        /// The fake server writes here.
        server_out: DuplexStream,
        /// The fake server reads from here.
        server_in: BufReader<DuplexStream>,
        events: std_mpsc::Receiver<Event>,
    }

    fn harness(config: SessionConfig) -> Harness {
        let (server_out, proxy_in) = duplex(64 * 1024);
        let (proxy_out, server_in) = duplex(64 * 1024);
        let (sender, events) = std_mpsc::channel();
        let sender = Mutex::new(sender);
        let session = Session::over_streams(proxy_in, proxy_out, config, move |event| {
            let _ = sender.lock().unwrap().send(event);
        });
        Harness {
            session,
            server_out,
            server_in: BufReader::new(server_in),
            events,
        }
    }

    impl Harness {
        async fn server_says(&mut self, message: Value) {
            write_message(&mut self.server_out, &message.to_string()).await.unwrap();
        }

        async fn server_hears(&mut self) -> Value {
            let message = read_message(&mut self.server_in).await.unwrap().expect("a message");
            serde_json::from_str(&message).unwrap()
        }

        fn next_event(&self) -> Event {
            self.events.recv_timeout(Duration::from_secs(2)).expect("an event")
        }
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn completes_the_initialize_request() {
        let mut h = harness(SessionConfig {
            initialization_options: json!({ "AutomaticWorkspaceInit": true }),
        });
        h.session.send(
            json!({
                "jsonrpc": "2.0", "id": 1, "method": "initialize",
                "params": { "rootUri": "file:///work/repo", "capabilities": { "textDocument": {} } }
            })
            .to_string(),
        );
        let seen = h.server_hears().await;
        let params = &seen["params"];
        assert_eq!(params["processId"], json!(std::process::id()));
        assert_eq!(params["initializationOptions"]["AutomaticWorkspaceInit"], json!(true));
        assert_eq!(
            params["workspaceFolders"],
            json!([{ "uri": "file:///work/repo", "name": "repo" }])
        );
        assert_eq!(params["capabilities"]["workspace"]["configuration"], json!(true));
        assert_eq!(params["capabilities"]["textDocument"], json!({}));
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn passes_other_client_messages_through_unchanged() {
        let mut h = harness(SessionConfig::default());
        let message = r#"{"jsonrpc":"2.0","id":7,"method":"textDocument/hover","params":{"note":"initialize"}}"#;
        h.session.send(message.to_string());
        assert_eq!(h.server_hears().await, serde_json::from_str::<Value>(message).unwrap());
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn answers_server_requests_without_involving_the_client() {
        let mut h = harness(SessionConfig::default());
        h.server_says(json!({
            "jsonrpc": "2.0", "id": 5, "method": "workspace/configuration",
            "params": { "items": [{ "section": "FSharp" }, { "section": "other" }] }
        }))
        .await;
        assert_eq!(
            h.server_hears().await,
            json!({ "jsonrpc": "2.0", "id": 5, "result": [null, null] })
        );

        h.server_says(json!({ "jsonrpc": "2.0", "id": "x", "method": "custom/unknown" })).await;
        assert_eq!(h.server_hears().await["error"]["code"], json!(-32601));

        // Nothing reached the client; the next thing it sees is this response.
        h.server_says(json!({ "jsonrpc": "2.0", "id": 1, "result": {} })).await;
        assert_eq!(
            h.next_event(),
            Event::Message(json!({ "jsonrpc": "2.0", "id": 1, "result": {} }).to_string())
        );
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn reports_diagnostic_counts_and_forwards_the_notification() {
        let mut h = harness(SessionConfig::default());
        let notification = json!({
            "jsonrpc": "2.0", "method": "textDocument/publishDiagnostics",
            "params": { "uri": "file:///work/repo/Orders.fs", "diagnostics": [
                { "severity": 1, "message": "type mismatch" },
                { "message": "no severity given" },
                { "severity": 2, "message": "unused" },
                { "severity": 4, "message": "hint" }
            ] }
        });
        h.server_says(notification.clone()).await;
        assert_eq!(h.next_event(), Event::Message(notification.to_string()));
        assert_eq!(
            h.next_event(),
            Event::Diagnostics {
                uri: "file:///work/repo/Orders.fs".into(),
                errors: 2,
                warnings: 1
            }
        );
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn tracks_progress_until_everything_ends() {
        let mut h = harness(SessionConfig::default());
        let progress = |token: &str, value: Value| {
            json!({ "jsonrpc": "2.0", "method": "$/progress", "params": { "token": token, "value": value } })
        };
        let mut seen = Vec::new();
        for message in [
            progress("a", json!({ "kind": "begin", "title": "Loading projects" })),
            progress("a", json!({ "kind": "report", "message": "ZedTrial.fsproj" })),
            progress("b", json!({ "kind": "begin", "title": "Checking" })),
            progress("b", json!({ "kind": "end" })),
            progress("a", json!({ "kind": "end" })),
        ] {
            h.server_says(message).await;
            assert!(matches!(h.next_event(), Event::Message(_)));
            seen.push(h.next_event());
        }
        assert_eq!(
            seen,
            vec![
                Event::Progress(Some("Loading projects".into())),
                Event::Progress(Some("Loading projects: ZedTrial.fsproj".into())),
                Event::Progress(Some("Checking".into())),
                Event::Progress(Some("Loading projects: ZedTrial.fsproj".into())),
                Event::Progress(None),
            ]
        );
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn reports_workspace_loading() {
        let mut h = harness(SessionConfig::default());
        let notify = |content: Value| {
            json!({ "jsonrpc": "2.0", "method": "fsharp/notifyWorkspace", "params": { "content": content.to_string() } })
        };
        // The shapes FsAutoComplete 0.85 sends.
        h.server_says(notify(json!({ "Kind": "projectLoading", "Data": { "Project": "/w/A.fsproj" } }))).await;
        h.server_says(notify(json!({ "Kind": "project", "Data": { "Project": "/w/A.fsproj", "Files": [] } }))).await;
        h.server_says(notify(json!({
            "Kind": "error",
            "Data": { "Code": 100, "Message": "Project not restored", "AdditionalData": { "Project": "/w/B.fsproj" } }
        })))
        .await;
        h.server_says(notify(json!({ "Kind": "workspaceLoad", "Data": { "Status": "finished" } }))).await;
        let events: Vec<_> = (0..8).map(|_| h.next_event()).filter(|e| !matches!(e, Event::Message(_))).collect();
        assert_eq!(
            events,
            vec![
                Event::ProjectLoading {
                    project: "/w/A.fsproj".into()
                },
                Event::ProjectLoaded {
                    project: "/w/A.fsproj".into()
                },
                Event::ProjectFailed {
                    project: "/w/B.fsproj".into(),
                    message: "Project not restored".into(),
                    details: None,
                },
                Event::WorkspaceLoaded,
            ]
        );
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn hands_background_files_over_to_the_editor() {
        let mut h = harness(SessionConfig::default());
        let uri = "file:///work/repo/Orders.fs";
        let client_open = json!({
            "jsonrpc": "2.0", "method": "textDocument/didOpen",
            "params": { "textDocument": { "uri": uri, "languageId": "fsharp", "version": 1, "text": "edited" } }
        });
        let method_and_text = |message: &Value| {
            (
                message["method"].as_str().unwrap().to_string(),
                message["params"]["textDocument"]["text"].as_str().map(str::to_string),
            )
        };

        // Opened in the background, then refreshed with new text: close + open again.
        h.session.open_in_background(uri, "fsharp", "one");
        h.session.open_in_background(uri, "fsharp", "two");
        let mut seen = Vec::new();
        for _ in 0..3 {
            seen.push(method_and_text(&h.server_hears().await));
        }
        // The editor takes over: the background copy is closed first.
        h.session.send(client_open.to_string());
        for _ in 0..2 {
            seen.push(method_and_text(&h.server_hears().await));
        }
        assert_eq!(
            seen,
            vec![
                ("textDocument/didOpen".to_string(), Some("one".to_string())),
                ("textDocument/didClose".to_string(), None),
                ("textDocument/didOpen".to_string(), Some("two".to_string())),
                ("textDocument/didClose".to_string(), None),
                ("textDocument/didOpen".to_string(), Some("edited".to_string())),
            ]
        );

        // While the editor has the file, background opens are ignored.
        h.session.open_in_background(uri, "fsharp", "stale");
        h.session.send(did_close(uri));
        assert_eq!(method_and_text(&h.server_hears().await).0, "textDocument/didClose");
        // Afterwards the file can go back to the background.
        h.session.open_in_background(uri, "fsharp", "saved");
        assert_eq!(
            method_and_text(&h.server_hears().await),
            ("textDocument/didOpen".to_string(), Some("saved".to_string()))
        );
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn reports_when_the_server_goes_away() {
        let h = harness(SessionConfig::default());
        drop(h.server_out);
        assert_eq!(h.events.recv_timeout(Duration::from_secs(2)).unwrap(), Event::Exited);
    }
}
