use std::ffi::OsStr;
use std::path::Path;

use serde::Serialize;

use crate::cli::{run_raw, run_raw_with, run_text, GitError};
use crate::Result;

/// A remote of the repository and the URL it fetches from.
#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
pub struct Remote {
    pub name: String,
    pub url: String,
}

/// The repository's remotes, in the order git lists them.
pub async fn remotes(root: &Path) -> Result<Vec<Remote>> {
    let out = run_text(root, &["remote", "-v"]).await?;
    Ok(parse_remotes(&out))
}

fn parse_remotes(out: &str) -> Vec<Remote> {
    out.lines()
        .filter_map(|line| {
            // "origin\thttps://host/repo (fetch)"
            let (name, rest) = line.split_once('\t')?;
            let url = rest.strip_suffix(" (fetch)")?;
            Some(Remote {
                name: name.to_string(),
                url: url.to_string(),
            })
        })
        .collect()
}

/// An HTTP header (credentials) git sends only to URLs under `url`, through
/// `http.<url>.extraHeader`: when a command goes by remote name, a `pushurl`
/// or an `insteadOf` rewrite can make git talk to another host than the
/// remote's URL names, and the header must not go there.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ScopedHeader {
    pub url: String,
    pub value: String,
}

impl ScopedHeader {
    /// The environment that hands the header to git without it showing up
    /// in a process list.
    pub(crate) fn environment(&self) -> [(&'static str, std::ffi::OsString); 4] {
        [
            // Git Credential Manager would otherwise open a sign-in window.
            ("GCM_INTERACTIVE", "never".into()),
            ("GIT_CONFIG_COUNT", "1".into()),
            ("GIT_CONFIG_KEY_0", format!("http.{}.extraHeader", self.url).into()),
            ("GIT_CONFIG_VALUE_0", self.value.clone().into()),
        ]
    }
}

/// Whether every one of `commits` is in the repository.
pub async fn has_commits(root: &Path, commits: &[&str]) -> bool {
    for commit in commits {
        let found = run_raw(root, &["cat-file", "-e", &format!("{commit}^{{commit}}")]).await;
        if !matches!(found, Ok(output) if output.success) {
            return false;
        }
    }
    true
}

/// Fetches the commits of `refs` (full names such as `refs/heads/main`) from
/// `url` into the repository's object store, and nothing else: no branch,
/// remote-tracking branch, tag or `FETCH_HEAD` is written. `extra_header` is
/// sent with every HTTP request (credentials); it goes through the
/// environment, so it never shows up in a process list.
pub async fn fetch_objects(root: &Path, url: &str, refs: &[&str], extra_header: Option<&str>) -> Result<()> {
    if let Some(bad) = refs.iter().find(|name| !name.starts_with("refs/")) {
        return Err(GitError::Parse(format!("not a full ref name: {bad}")));
    }
    let mut args = vec!["fetch", "--quiet", "--no-tags", "--no-write-fetch-head", "--no-recurse-submodules", url];
    args.extend_from_slice(refs);

    let mut environment: Vec<(&str, &OsStr)> = vec![
        // Git Credential Manager would otherwise open a sign-in window.
        ("GCM_INTERACTIVE", OsStr::new("never")),
    ];
    if let Some(header) = extra_header {
        environment.extend([
            ("GIT_CONFIG_COUNT", OsStr::new("1")),
            ("GIT_CONFIG_KEY_0", OsStr::new("http.extraHeader")),
            ("GIT_CONFIG_VALUE_0", OsStr::new(header)),
        ]);
    }
    let output = run_raw_with(root, &args, &environment).await?;
    if output.success {
        Ok(())
    } else {
        Err(GitError::Failed {
            command: "fetch".into(),
            stderr: output.stderr,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// What git itself makes of the scope: the header for the remote's
    /// collection, none for another host or plain http.
    #[tokio::test]
    async fn git_sends_a_scoped_header_only_under_its_url() {
        let header = ScopedHeader {
            url: "https://dev.azure.com/contoso".into(),
            value: "Authorization: Basic abc".into(),
        };
        let environment = header.environment();
        let environment: Vec<(&str, &OsStr)> = environment.iter().map(|(key, value)| (*key, value.as_os_str())).collect();
        let dir = tempfile::tempdir().unwrap();
        for (url, sent) in [
            ("https://dev.azure.com/contoso/Shop/_git/shop-api", true),
            ("https://me@dev.azure.com/contoso/Shop/_git/shop-api", true),
            ("https://dev.azure.com/fabrikam/Shop/_git/shop-api", false),
            ("http://dev.azure.com/contoso/Shop/_git/shop-api", false),
            ("https://evil.example?@dev.azure.com/contoso/Shop/_git/shop-api", false),
            ("https://evil.example/contoso/Shop/_git/shop-api", false),
        ] {
            let output = run_raw_with(dir.path(), &["config", "--get-urlmatch", "http.extraHeader", url], &environment)
                .await
                .unwrap();
            assert_eq!(output.success, sent, "{url}");
        }
    }

    #[test]
    fn parses_fetch_urls_only() {
        let remotes = parse_remotes(
            "origin\thttps://dev.azure.com/org/p/_git/r (fetch)\norigin\thttps://dev.azure.com/org/p/_git/r (push)\nfork\tgit@github.com:me/r.git (fetch)\nfork\tgit@github.com:me/r.git (push)",
        );
        assert_eq!(
            remotes,
            vec![
                Remote {
                    name: "origin".into(),
                    url: "https://dev.azure.com/org/p/_git/r".into()
                },
                Remote {
                    name: "fork".into(),
                    url: "git@github.com:me/r.git".into()
                },
            ]
        );
    }
}
