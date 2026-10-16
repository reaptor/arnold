//! Runs the real Fantomas as a local .NET tool: checks a folder with an
//! unformatted file, formats it, checks again, and meets a file that does not parse.
//!
//! Needs the .NET SDK and network access (or Fantomas 7.0.3 in the NuGet
//! cache), so it only runs on request:
//! `cargo test -p arnold-lsp --test fantomas_live -- --ignored --nocapture`

use arnold_lsp::{dotnet, fantomas};

const MANIFEST: &str = r#"{ "version": 1, "isRoot": true, "tools": {
  "fantomas": { "version": "7.0.3", "commands": ["fantomas"], "rollForward": false } } }"#;

#[tokio::test]
#[ignore]
async fn checks_and_formats() {
    let dir = tempfile::tempdir().unwrap();
    let root = dir.path();
    std::fs::create_dir_all(root.join(".config")).unwrap();
    std::fs::create_dir_all(root.join("src")).unwrap();
    std::fs::write(root.join(".config/dotnet-tools.json"), MANIFEST).unwrap();
    std::fs::write(root.join("src/A.fs"), "module A\nlet x   =  1\n").unwrap();
    std::fs::write(root.join("src/B.fs"), "module B\n\nlet y = 2\n").unwrap();

    let command = fantomas::tool_command(root).expect("Fantomas in the manifest");
    let dotnet = dotnet::locate(root).await.unwrap();

    let check = fantomas::check(&dotnet, root, &command).await.unwrap();
    assert_eq!(check.unformatted, vec!["src/A.fs".to_string()]);
    assert!(check.errors.is_empty());

    assert!(fantomas::format_files(&dotnet, root, &command, &check.unformatted).await.unwrap().is_empty());
    assert_eq!(std::fs::read_to_string(root.join("src/A.fs")).unwrap(), "module A\n\nlet x = 1\n");
    assert!(fantomas::check(&dotnet, root, &command).await.unwrap().is_clean());

    std::fs::write(root.join("src/C.fs"), "module C\nlet z = (\n").unwrap();
    let check = fantomas::check(&dotnet, root, &command).await.unwrap();
    assert_eq!(check.errors.len(), 1, "{check:?}");
    assert!(check.unformatted.is_empty(), "{check:?}");
    let failed = fantomas::format_files(&dotnet, root, &command, &["src/C.fs".into()]).await.unwrap();
    assert_eq!(failed, vec!["src/C.fs".to_string()]);
}
