//! Against the real GitHub Copilot: downloads the CLI into a temporary folder
//! and asks one question. Needs a GitHub token with Copilot in `GH_TOKEN`
//! (e.g. `GH_TOKEN=$(gh auth token)`) and no Arnold sign-in in the keychain:
//! `cargo test -p arnold-ai --test copilot_live -- --ignored --nocapture`

use std::sync::{Arc, Mutex};

use arnold_ai::{AiEvent, AskRequest, ContextItem, ContextKind, Copilot, Provider};

#[tokio::test]
#[ignore]
async fn explains_a_diff() {
    let data = tempfile::tempdir().unwrap();
    let copilot = Copilot::new(data.path());
    let seen = Arc::new(Mutex::new(Vec::new()));
    let events = {
        let seen = seen.clone();
        Arc::new(move |event: AiEvent| {
            if let AiEvent::Status { text } = &event {
                eprintln!("{text}");
            }
            seen.lock().unwrap().push(event);
        }) as arnold_ai::Events
    };
    let request = AskRequest {
        conversation: "test".into(),
        model: None,
        question: "Explain this change in two sentences.".into(),
        context: vec![ContextItem {
            label: "src/math.rs".into(),
            kind: ContextKind::Diff,
            text: "--- a/src/math.rs\n+++ b/src/math.rs\n@@ -1,3 +1,3 @@\n fn average(xs: &[f64]) -> f64 {\n-    xs.iter().sum::<f64>() / xs.len() as f64\n+    if xs.is_empty() { 0.0 } else { xs.iter().sum::<f64>() / xs.len() as f64 }\n }\n".into(),
        }],
        cwd: None,
    };
    copilot.ask(request.clone(), events.clone()).await.unwrap();
    let answer: String = seen
        .lock()
        .unwrap()
        .iter()
        .filter_map(|event| match event {
            AiEvent::Delta { text } => Some(text.as_str()),
            _ => None,
        })
        .collect();
    eprintln!("---\n{answer}\n---");
    assert!(answer.to_lowercase().contains("empty"), "{answer}");
    assert!(matches!(seen.lock().unwrap().last(), Some(AiEvent::Done { .. })));

    // A follow-up in the same conversation sees the first question.
    seen.lock().unwrap().clear();
    let follow_up = AskRequest { question: "Which function was it? Answer with its name only.".into(), context: vec![], ..request };
    copilot.ask(follow_up, events).await.unwrap();
    let answer: String = seen.lock().unwrap().iter().filter_map(|e| match e { AiEvent::Delta { text } => Some(text.clone()), _ => None }).collect();
    eprintln!("---\n{answer}\n---");
    assert!(answer.contains("average"), "{answer}");
    eprintln!("models: {:?}", copilot.models().await);
    copilot.stop().await;
    assert!(data.path().join("copilot/home").is_dir());
}

/// Stopping an answer ends it promptly (Copilot reports the session idle after an abort).
#[tokio::test]
#[ignore]
async fn stops_an_answer() {
    let data = tempfile::tempdir().unwrap();
    let copilot = Arc::new(Copilot::new(data.path()));
    let started = Arc::new(tokio::sync::Notify::new());
    let events = {
        let started = started.clone();
        Arc::new(move |event: AiEvent| {
            if matches!(event, AiEvent::Delta { .. }) {
                started.notify_one();
            }
        }) as arnold_ai::Events
    };
    let request = AskRequest {
        conversation: "stop".into(),
        model: None,
        question: "Write a 600-word essay about code review.".into(),
        context: vec![],
        cwd: None,
    };
    let asking = {
        let copilot = copilot.clone();
        tokio::spawn(async move { copilot.ask(request, events).await })
    };
    tokio::time::timeout(std::time::Duration::from_secs(120), started.notified()).await.expect("an answer starts");
    let stopped_at = std::time::Instant::now();
    copilot.cancel("stop").await;
    let result = tokio::time::timeout(std::time::Duration::from_secs(20), asking).await.expect("ask returns after cancel");
    eprintln!("ask returned {:?} {:?} after cancel", result.unwrap(), stopped_at.elapsed());
    copilot.stop().await;
}
