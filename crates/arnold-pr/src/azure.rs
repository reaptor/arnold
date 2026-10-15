//! Azure DevOps Services (dev.azure.com, *.visualstudio.com) and Azure DevOps
//! Server: recognizing their remotes and reading active pull requests with a
//! personal access token.

use std::time::Duration;

use base64::Engine;
use serde::Serialize;
use serde_json::{json, Value};
use ureq::tls::{RootCerts, TlsConfig};

use crate::{Comment, NewThreadPlace, PrError, PullRequest, Reviewer, Side, Thread, ThreadPosition, Vote};

/// A repository hosted on Azure DevOps.
#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
pub struct AzureRepo {
    /// `https://dev.azure.com/contoso`, or a server's collection URL such as
    /// `https://tfs.example.com/tfs/DefaultCollection`.
    pub collection_url: String,
    /// The organization (or the server's collection), for display.
    pub organization: String,
    /// Project and repository as they appear in URLs (percent-encoded).
    pub project: String,
    pub repository: String,
}

impl AzureRepo {
    /// The repository's page, which is also its HTTPS clone URL.
    pub fn web_url(&self) -> String {
        format!("{}/{}/_git/{}", self.collection_url, self.project, self.repository)
    }

    pub fn pull_request_url(&self, id: u64) -> String {
        format!("{}/pullrequest/{id}", self.web_url())
    }

    /// Where a personal access token is created.
    pub fn token_settings_url(&self) -> String {
        format!("{}/_usersSettings/tokens", self.collection_url)
    }

    /// A token works for one organization, so it is saved per organization.
    pub fn token_key(&self) -> String {
        format!("azure-devops:{}", self.collection_url.to_lowercase())
    }

    /// `contoso / Shop / shop-api`.
    pub fn display_name(&self) -> String {
        format!(
            "{} / {} / {}",
            self.organization,
            percent_decode(&self.project),
            percent_decode(&self.repository)
        )
    }

    fn api(&self, path: &str) -> String {
        format!(
            "{}/{}/_apis/git/repositories/{}{path}",
            self.collection_url, self.project, self.repository
        )
    }
}

/// The Azure DevOps repository a remote URL points at, if it is one.
///
/// Understood: `https://[user@]dev.azure.com/org/project/_git/repo`,
/// `https://org.visualstudio.com/[DefaultCollection/]project/_git/repo`,
/// `git@ssh.dev.azure.com:v3/org/project/repo`,
/// `org@vs-ssh.visualstudio.com:v3/org/project/repo` (also as `ssh://`), and
/// a server's `https://host/[path/]collection/project/_git/repo`. A URL
/// without the project (`…/org/_git/repo`) means the project has the
/// repository's name.
pub fn parse_remote_url(url: &str) -> Option<AzureRepo> {
    let url = url.trim();
    if let Some(repo) = parse_ssh(url) {
        return Some(repo);
    }
    let (scheme, rest) = url.split_once("://")?;
    if scheme != "https" && scheme != "http" {
        return None;
    }
    let (authority, path) = rest.split_once('/')?;
    // Drop `user@` (and any password) in front of the host.
    let host = authority.rsplit('@').next()?.to_lowercase();
    let segments: Vec<&str> = path
        .split(['?', '#'])
        .next()?
        .split('/')
        .filter(|segment| !segment.is_empty())
        .collect();
    let at = segments.iter().position(|segment| *segment == "_git")?;
    let mut repository = *segments.get(at + 1)?;
    // `_git/_optimized/repo` and `_git/_full/repo` are other views of the same repository.
    if matches!(repository, "_optimized" | "_full") {
        repository = segments.get(at + 2)?;
    }
    let repository = repository.strip_suffix(".git").unwrap_or(repository).to_string();
    let before = &segments[..at];

    if host == "dev.azure.com" {
        let organization = before.first()?.to_string();
        let project = before.get(1).map_or_else(|| repository.clone(), |p| p.to_string());
        return Some(services(organization, project, repository));
    }
    if let Some(organization) = host.strip_suffix(".visualstudio.com") {
        let before = match before.first() {
            Some(first) if first.eq_ignore_ascii_case("DefaultCollection") => &before[1..],
            _ => before,
        };
        let project = before.last().map_or_else(|| repository.clone(), |p| p.to_string());
        return Some(services(organization.to_string(), project, repository));
    }
    // Azure DevOps Server: …/collection/project/_git/repo.
    let (project, collection) = before.split_last()?;
    let organization = percent_decode(collection.last()?);
    Some(AzureRepo {
        collection_url: format!("{scheme}://{host}/{}", collection.join("/")),
        organization,
        project: project.to_string(),
        repository,
    })
}

fn services(organization: String, project: String, repository: String) -> AzureRepo {
    AzureRepo {
        // visualstudio.com organizations answer on dev.azure.com too, so a
        // token is shared between remotes written either way.
        collection_url: format!("https://dev.azure.com/{organization}"),
        organization,
        project,
        repository,
    }
}

fn parse_ssh(url: &str) -> Option<AzureRepo> {
    // `ssh://git@ssh.dev.azure.com/v3/…` or the scp-like `git@ssh.dev.azure.com:v3/…`.
    let (host, path) = match url.strip_prefix("ssh://") {
        Some(rest) => {
            let (authority, path) = rest.split_once('/')?;
            (authority.rsplit('@').next()?, path)
        }
        None => {
            let (authority, path) = url.split_once(':')?;
            if authority.contains('/') {
                return None;
            }
            (authority.rsplit('@').next()?, path)
        }
    };
    let host = host.split(':').next()?.to_lowercase();
    if host != "ssh.dev.azure.com" && host != "vs-ssh.visualstudio.com" {
        return None;
    }
    let mut segments = path.trim_matches('/').split('/');
    if segments.next()? != "v3" {
        return None;
    }
    let organization = segments.next()?.to_string();
    let project = segments.next()?.to_string();
    let repository = segments.next()?;
    let repository = repository.strip_suffix(".git").unwrap_or(repository).to_string();
    Some(services(organization, project, repository))
}

fn percent_decode(text: &str) -> String {
    let bytes = text.as_bytes();
    let mut out = Vec::with_capacity(bytes.len());
    let mut i = 0;
    while i < bytes.len() {
        let hex = bytes.get(i + 1..i + 3).and_then(|h| std::str::from_utf8(h).ok());
        match (bytes[i], hex.and_then(|h| u8::from_str_radix(h, 16).ok())) {
            (b'%', Some(byte)) => {
                out.push(byte);
                i += 3;
            }
            (byte, _) => {
                out.push(byte);
                i += 1;
            }
        }
    }
    String::from_utf8_lossy(&out).into_owned()
}

// ------------------------------------------------------------------- the API

/// Older servers do not know newer versions; this one is in every Azure
/// DevOps Server since 2020.
const API_VERSION: &str = "6.0";

/// The value of an `Authorization` header for a personal access token:
/// basic authentication with an empty user name. Git takes the same.
pub fn authorization(token: &str) -> String {
    let encoded = base64::engine::general_purpose::STANDARD.encode(format!(":{token}"));
    format!("Basic {encoded}")
}

/// The active pull requests of `repo`, newest first.
pub fn active_pull_requests(repo: &AzureRepo, token: &str) -> Result<Vec<PullRequest>, PrError> {
    let url = repo.api(&format!(
        "/pullrequests?searchCriteria.status=active&$top=500&api-version={API_VERSION}"
    ));
    let body = get(&url, token)?;
    let list = body["value"]
        .as_array()
        .ok_or_else(|| PrError::Other("Azure DevOps sent an unexpected answer".into()))?;
    Ok(list.iter().filter_map(|value| parse_pull_request(value, repo)).collect())
}

/// One pull request of `repo`, whatever its state.
pub fn pull_request(repo: &AzureRepo, token: &str, id: u64) -> Result<PullRequest, PrError> {
    let url = repo.api(&format!("/pullrequests/{id}?api-version={API_VERSION}"));
    let body = get(&url, token)?;
    parse_pull_request(&body, repo).ok_or_else(|| PrError::Other("Azure DevOps sent an unexpected answer".into()))
}

/// The discussions of pull request `id`. Positions in files are tracked by
/// the host up to the latest push, compared with the merge-base, which is
/// the diff Arnold shows.
pub fn threads(repo: &AzureRepo, token: &str, id: u64) -> Result<Vec<Thread>, PrError> {
    let latest = latest_iteration(repo, token, id)?;
    let tracking = latest.map_or_else(String::new, |latest| format!("&$iteration={latest}&$baseIteration=0"));
    let url = repo.api(&format!("/pullRequests/{id}/threads?api-version={API_VERSION}{tracking}"));
    let body = get(&url, token)?;
    let list = body["value"]
        .as_array()
        .ok_or_else(|| PrError::Other("Azure DevOps sent an unexpected answer".into()))?;
    Ok(list.iter().filter_map(parse_thread).collect())
}

fn latest_iteration(repo: &AzureRepo, token: &str, id: u64) -> Result<Option<u64>, PrError> {
    let iterations = get(&repo.api(&format!("/pullRequests/{id}/iterations?api-version={API_VERSION}")), token)?;
    Ok(iterations["value"]
        .as_array()
        .and_then(|list| list.iter().filter_map(|iteration| iteration["id"].as_u64()).max()))
}

/// Starts a discussion on pull request `id`: about lines of a file
/// (`place`, as of the latest push against the merge-base) or, without
/// one, about the pull request as a whole. `content` is Markdown.
pub fn create_thread(
    repo: &AzureRepo,
    token: &str,
    id: u64,
    place: Option<&NewThreadPlace>,
    content: &str,
) -> Result<Thread, PrError> {
    let (latest, tracking) = match place {
        Some(place) => {
            let latest = latest_iteration(repo, token, id)?;
            let tracking = latest.and_then(|latest| change_tracking_id(repo, token, id, latest, &repo_path(&place.path)));
            (latest, tracking)
        }
        None => (None, None),
    };
    let body = thread_body(place, content, latest, tracking);
    let url = repo.api(&format!("/pullRequests/{id}/threads?api-version={API_VERSION}"));
    let answer = post(&url, token, &body)?;
    parse_thread(&answer).ok_or_else(|| PrError::Other("Azure DevOps sent an unexpected answer".into()))
}

/// `/src/file.cs`, as the host writes paths.
fn repo_path(path: &str) -> String {
    format!("/{}", path.trim_start_matches('/'))
}

/// What creating a thread sends. `latest` is the pull request's latest
/// iteration and `tracking` the host's id of the file in it, when known.
fn thread_body(place: Option<&NewThreadPlace>, content: &str, latest: Option<u64>, tracking: Option<u64>) -> Value {
    let mut body = json!({
        "comments": [{ "parentCommentId": 0, "content": content, "commentType": 1 }],
        "status": 1,
    });
    let Some(place) = place else { return body };
    let path = repo_path(&place.path);
    body["threadContext"] = match place.lines {
        None => json!({ "filePath": path }),
        Some(lines) => {
            let start = json!({ "line": lines.start_line, "offset": lines.start_offset });
            let end = json!({ "line": lines.end_line, "offset": lines.end_offset });
            match lines.side {
                Side::Head => json!({ "filePath": path, "rightFileStart": start, "rightFileEnd": end }),
                Side::Base => json!({ "filePath": path, "leftFileStart": start, "leftFileEnd": end }),
            }
        }
    };
    // Which diff the lines are from, as the web page sends it: every push
    // against the merge-base. With the file's tracking id the host can
    // follow the lines through later pushes.
    if let Some(latest) = latest {
        let mut context = json!({
            "iterationContext": { "firstComparingIteration": 1, "secondComparingIteration": latest },
        });
        if let Some(tracking) = tracking {
            context["changeTrackingId"] = json!(tracking);
        }
        body["pullRequestThreadContext"] = context;
    }
    body
}

/// Answers thread `thread` of pull request `id`. `content` is Markdown.
pub fn reply(repo: &AzureRepo, token: &str, id: u64, thread: u64, parent: u64, content: &str) -> Result<(), PrError> {
    let url = repo.api(&format!("/pullRequests/{id}/threads/{thread}/comments?api-version={API_VERSION}"));
    post(&url, token, &json!({ "parentCommentId": parent, "content": content, "commentType": 1 }))?;
    Ok(())
}

/// The host's id for `path` in the changes of iteration `iteration`; a
/// thread without it still shows, so failing here is not an error.
fn change_tracking_id(repo: &AzureRepo, token: &str, id: u64, iteration: u64, path: &str) -> Option<u64> {
    let url = repo.api(&format!(
        "/pullRequests/{id}/iterations/{iteration}/changes?$compareTo=0&$top=2000&api-version={API_VERSION}"
    ));
    let changes = get(&url, token).ok()?;
    changes["changeEntries"]
        .as_array()?
        .iter()
        .find(|entry| entry["item"]["path"].as_str() == Some(path))?["changeTrackingId"]
        .as_u64()
}

fn agent() -> ureq::Agent {
    ureq::Agent::config_builder()
        .timeout_connect(Some(Duration::from_secs(15)))
        // Certificates are checked against the system's store, as git does: a
        // company proxy or an Azure DevOps Server may use a root installed only there.
        .tls_config(TlsConfig::builder().root_certs(RootCerts::PlatformVerifier).build())
        .timeout_global(Some(Duration::from_secs(60)))
        // Status codes are told apart below.
        .http_status_as_error(false)
        .build()
        .into()
}

fn get(url: &str, token: &str) -> Result<Value, PrError> {
    let mut response = agent()
        .get(url)
        .header("Authorization", authorization(token))
        .header("Accept", "application/json")
        // Without it a bad token gets a sign-in page (status 203) instead of a 401.
        .header("X-TFS-FedAuthRedirect", "Suppress")
        .call()
        .map_err(|e| PrError::Network(network_error(url, &e)))?;
    let status = response.status().as_u16();
    let text = response
        .body_mut()
        .read_to_string()
        .map_err(|e| PrError::Network(format!("Could not read the answer from Azure DevOps: {e}")))?;
    classify(status, &text)
}

fn post(url: &str, token: &str, body: &Value) -> Result<Value, PrError> {
    let mut response = agent()
        .post(url)
        .header("Authorization", authorization(token))
        .header("Accept", "application/json")
        .header("Content-Type", "application/json")
        .header("X-TFS-FedAuthRedirect", "Suppress")
        .send(body.to_string())
        .map_err(|e| PrError::Network(network_error(url, &e)))?;
    let status = response.status().as_u16();
    let text = response
        .body_mut()
        .read_to_string()
        .map_err(|e| PrError::Network(format!("Could not read the answer from Azure DevOps: {e}")))?;
    classify(status, &text)
}

fn network_error(url: &str, error: &ureq::Error) -> String {
    let host = url.split('/').nth(2).unwrap_or(url);
    let text = error.to_string();
    if text.contains("certificate") {
        format!(
            "Could not reach Azure DevOps: the certificate of {host} is not trusted ({text}). \
             On a company network, its root certificate has to be installed in the system's certificate store \
             (Keychain Access on macOS, Manage computer certificates on Windows); your IT department usually does this."
        )
    } else {
        format!("Could not reach Azure DevOps at {host}: {text}")
    }
}

fn classify(status: u16, text: &str) -> Result<Value, PrError> {
    let json: Option<Value> = serde_json::from_str(text).ok();
    let message = json
        .as_ref()
        .and_then(|value| value["message"].as_str())
        .unwrap_or_default()
        .to_string();
    match (status, json) {
        (200 | 201, Some(value)) => Ok(value),
        // A sign-in page instead of JSON: the token was not accepted.
        (200 | 203 | 401 | 403, _) => Err(PrError::TokenRejected(message)),
        (404, _) => Err(PrError::NotFound(if message.is_empty() {
            "Azure DevOps does not know this repository, or the token has no access to it.".into()
        } else {
            message
        })),
        (status, _) => Err(PrError::Other(format!(
            "Azure DevOps answered with status {status}{}",
            if message.is_empty() { String::new() } else { format!(": {message}") }
        ))),
    }
}

fn parse_pull_request(value: &Value, repo: &AzureRepo) -> Option<PullRequest> {
    let id = value["pullRequestId"].as_u64()?;
    let text = |value: &Value| value.as_str().unwrap_or_default().to_string();
    let branch = |value: &Value| {
        let name = value.as_str().unwrap_or_default();
        name.strip_prefix("refs/heads/").unwrap_or(name).to_string()
    };
    let commit = |value: &Value| value["commitId"].as_str().map(str::to_string);
    let reviewers = value["reviewers"]
        .as_array()
        .map(|list| {
            list.iter()
                .map(|reviewer| Reviewer {
                    name: text(&reviewer["displayName"]),
                    vote: match reviewer["vote"].as_i64().unwrap_or(0) {
                        10 => Vote::Approved,
                        5 => Vote::ApprovedWithSuggestions,
                        -5 => Vote::WaitingForAuthor,
                        -10 => Vote::Rejected,
                        _ => Vote::None,
                    },
                    group: reviewer["isContainer"].as_bool().unwrap_or(false),
                })
                .collect()
        })
        .unwrap_or_default();
    Some(PullRequest {
        id,
        title: text(&value["title"]),
        description: text(&value["description"]),
        author: text(&value["createdBy"]["displayName"]),
        created: text(&value["creationDate"]),
        draft: value["isDraft"].as_bool().unwrap_or(false),
        status: text(&value["status"]),
        source_branch: branch(&value["sourceRefName"]),
        target_branch: branch(&value["targetRefName"]),
        source_commit: commit(&value["lastMergeSourceCommit"]),
        target_commit: commit(&value["lastMergeTargetCommit"]),
        url: repo.pull_request_url(id),
        reviewers,
    })
}

/// A thread with something people wrote in it; votes, pushes and other
/// notes of the host are system comments and are left out.
fn parse_thread(value: &Value) -> Option<Thread> {
    if value["isDeleted"].as_bool().unwrap_or(false) {
        return None;
    }
    let text = |value: &Value| value.as_str().unwrap_or_default().to_string();
    let comments: Vec<Comment> = value["comments"]
        .as_array()?
        .iter()
        .filter(|comment| comment["commentType"].as_str() == Some("text"))
        .filter(|comment| !comment["isDeleted"].as_bool().unwrap_or(false))
        .filter_map(|comment| {
            let content = text(&comment["content"]);
            if content.trim().is_empty() {
                return None;
            }
            let published = text(&comment["publishedDate"]);
            let updated = text(&comment["lastContentUpdatedDate"]);
            Some(Comment {
                id: comment["id"].as_u64()?,
                parent_id: comment["parentCommentId"].as_u64().unwrap_or(0),
                author: text(&comment["author"]["displayName"]),
                content,
                edited: !updated.is_empty() && updated != published,
                published,
            })
        })
        .collect();
    if comments.is_empty() {
        return None;
    }
    let context = &value["threadContext"];
    let path = context["filePath"]
        .as_str()
        .map(|path| path.trim_start_matches('/').to_string())
        .filter(|path| !path.is_empty());
    let lines = |start: &Value, end: &Value| {
        let start = start["line"].as_u64().filter(|line| *line > 0)? as u32;
        let end = end["line"].as_u64().map_or(start, |line| line as u32).max(start);
        Some((start, end))
    };
    let position = path.as_ref().and_then(|_| {
        let (side, (start_line, end_line)) = match lines(&context["rightFileStart"], &context["rightFileEnd"]) {
            Some(lines) => (Side::Head, lines),
            None => (Side::Base, lines(&context["leftFileStart"], &context["leftFileEnd"])?),
        };
        Some(ThreadPosition { side, start_line, end_line })
    });
    Some(Thread {
        id: value["id"].as_u64()?,
        path,
        position,
        status: value["status"].as_str().unwrap_or("unknown").to_string(),
        comments,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn repo(collection_url: &str, organization: &str, project: &str, repository: &str) -> Option<AzureRepo> {
        Some(AzureRepo {
            collection_url: collection_url.into(),
            organization: organization.into(),
            project: project.into(),
            repository: repository.into(),
        })
    }

    #[test]
    fn recognizes_services_urls() {
        let expected = repo("https://dev.azure.com/contoso", "contoso", "Shop", "shop-api");
        for url in [
            "https://dev.azure.com/contoso/Shop/_git/shop-api",
            "https://contoso@dev.azure.com/contoso/Shop/_git/shop-api",
            "https://contoso@dev.azure.com/contoso/Shop/_git/shop-api/",
            "https://contoso.visualstudio.com/Shop/_git/shop-api",
            "https://contoso.visualstudio.com/DefaultCollection/Shop/_git/shop-api",
            "https://contoso.visualstudio.com/Shop/_git/_optimized/shop-api",
            "git@ssh.dev.azure.com:v3/contoso/Shop/shop-api",
            "ssh://git@ssh.dev.azure.com/v3/contoso/Shop/shop-api",
            "contoso@vs-ssh.visualstudio.com:v3/contoso/Shop/shop-api",
            "ssh://contoso@vs-ssh.visualstudio.com:22/v3/contoso/Shop/shop-api",
        ] {
            assert_eq!(parse_remote_url(url), expected, "{url}");
        }
    }

    #[test]
    fn project_defaults_to_repository_and_names_stay_encoded() {
        assert_eq!(
            parse_remote_url("https://dev.azure.com/contoso/_git/Shop"),
            repo("https://dev.azure.com/contoso", "contoso", "Shop", "Shop")
        );
        let spaced = parse_remote_url("https://dev.azure.com/contoso/My%20Shop/_git/Web%20App").unwrap();
        assert_eq!(spaced.web_url(), "https://dev.azure.com/contoso/My%20Shop/_git/Web%20App");
        assert_eq!(spaced.display_name(), "contoso / My Shop / Web App");
    }

    #[test]
    fn recognizes_server_urls() {
        assert_eq!(
            parse_remote_url("https://tfs.example.com/tfs/DefaultCollection/Shop/_git/shop-api"),
            repo("https://tfs.example.com/tfs/DefaultCollection", "DefaultCollection", "Shop", "shop-api")
        );
        // Not enough to tell the collection from the project.
        assert_eq!(parse_remote_url("https://tfs.example.com/_git/shop-api"), None);
    }

    #[test]
    fn ignores_other_hosts() {
        for url in [
            "https://github.com/reaptor/arnold.git",
            "git@github.com:reaptor/arnold.git",
            "/local/path/repo",
            "file:///local/path/repo",
            "git@ssh.dev.azure.com:v2/contoso/Shop/shop-api",
        ] {
            assert_eq!(parse_remote_url(url), None, "{url}");
        }
    }

    #[test]
    fn builds_urls_and_keys() {
        let repo = parse_remote_url("https://contoso.visualstudio.com/Shop/_git/shop-api").unwrap();
        assert_eq!(repo.token_settings_url(), "https://dev.azure.com/contoso/_usersSettings/tokens");
        assert_eq!(repo.token_key(), "azure-devops:https://dev.azure.com/contoso");
        assert_eq!(repo.pull_request_url(7), "https://dev.azure.com/contoso/Shop/_git/shop-api/pullrequest/7");
        assert_eq!(
            repo.api("/pullrequests"),
            "https://dev.azure.com/contoso/Shop/_apis/git/repositories/shop-api/pullrequests"
        );
        assert_eq!(authorization("abc"), "Basic OmFiYw==");
    }

    #[test]
    fn tells_errors_apart() {
        assert!(classify(200, "{\"value\":[]}").is_ok());
        assert!(matches!(classify(203, "<html>sign in</html>"), Err(PrError::TokenRejected(_))));
        assert!(matches!(classify(200, "<html>sign in</html>"), Err(PrError::TokenRejected(_))));
        assert_eq!(
            classify(401, "{\"message\":\"TF400813: not authorized\"}"),
            Err(PrError::TokenRejected("TF400813: not authorized".into()))
        );
        assert!(matches!(classify(404, ""), Err(PrError::NotFound(_))));
        assert_eq!(
            classify(500, "{\"message\":\"boom\"}"),
            Err(PrError::Other("Azure DevOps answered with status 500: boom".into()))
        );
    }

    /// Against the real service: `cargo test -p arnold-pr -- --ignored`.
    #[test]
    #[ignore = "talks to dev.azure.com"]
    fn live_service_turns_down_a_bogus_token() {
        let repo = parse_remote_url("https://dev.azure.com/microsoft/OS/_git/os").unwrap();
        let result = active_pull_requests(&repo, "not-a-real-token");
        assert!(matches!(result, Err(PrError::TokenRejected(_))), "{result:?}");
    }

    #[test]
    fn parses_threads() {
        let value: Value = serde_json::from_str(
            r#"{ "value": [
              {
                "id": 1,
                "status": "active",
                "threadContext": {
                  "filePath": "/src/Api/OrderController.cs",
                  "rightFileStart": { "line": 14, "offset": 1 },
                  "rightFileEnd": { "line": 15, "offset": 20 }
                },
                "comments": [
                  { "id": 1, "parentCommentId": 0, "author": { "displayName": "Bo" }, "content": "Why 404?",
                    "publishedDate": "2026-10-14T08:30:00Z", "lastContentUpdatedDate": "2026-10-14T08:30:00Z",
                    "commentType": "text" },
                  { "id": 2, "parentCommentId": 1, "author": { "displayName": "Ada" }, "content": "As agreed.",
                    "publishedDate": "2026-10-14T09:00:00Z", "lastContentUpdatedDate": "2026-10-14T09:05:00Z",
                    "commentType": "text" },
                  { "id": 3, "author": { "displayName": "Cy" }, "content": "gone", "commentType": "text", "isDeleted": true }
                ]
              },
              {
                "id": 2,
                "status": "fixed",
                "threadContext": { "filePath": "/docs/Legacy.md", "leftFileStart": { "line": 3 }, "leftFileEnd": { "line": 3 } },
                "comments": [ { "id": 1, "author": { "displayName": "Bo" }, "content": "Keep this?", "commentType": "text" } ]
              },
              {
                "id": 3,
                "status": "active",
                "threadContext": null,
                "comments": [ { "id": 1, "author": { "displayName": "Bo" }, "content": "Looks good overall", "commentType": "text" } ]
              },
              {
                "id": 4,
                "comments": [ { "id": 1, "author": { "displayName": "Azure DevOps" }, "content": "Bo voted 10", "commentType": "system" } ]
              },
              {
                "id": 5,
                "status": "active",
                "isDeleted": true,
                "comments": [ { "id": 1, "author": { "displayName": "Bo" }, "content": "x", "commentType": "text" } ]
              }
            ] }"#,
        )
        .unwrap();
        let threads: Vec<Thread> = value["value"].as_array().unwrap().iter().filter_map(parse_thread).collect();
        assert_eq!(threads.iter().map(|thread| thread.id).collect::<Vec<_>>(), vec![1, 2, 3]);

        let first = &threads[0];
        assert_eq!(first.path.as_deref(), Some("src/Api/OrderController.cs"));
        assert_eq!(first.position, Some(ThreadPosition { side: Side::Head, start_line: 14, end_line: 15 }));
        assert_eq!(first.comments.len(), 2);
        assert_eq!((first.comments[1].parent_id, first.comments[1].edited), (1, true));
        assert!(!first.comments[0].edited);

        assert_eq!(threads[1].position, Some(ThreadPosition { side: Side::Base, start_line: 3, end_line: 3 }));
        assert_eq!(threads[1].status, "fixed");
        assert_eq!((threads[2].path.as_deref(), threads[2].position), (None, None));
    }

    #[test]
    fn builds_thread_bodies() {
        let place = NewThreadPlace {
            path: "src/Api/OrderController.cs".into(),
            lines: Some(crate::LineRange { side: Side::Base, start_line: 14, start_offset: 9, end_line: 15, end_offset: 1 }),
        };
        let body = thread_body(Some(&place), "Why?", Some(3), Some(7));
        assert_eq!(
            body,
            json!({
                "comments": [{ "parentCommentId": 0, "content": "Why?", "commentType": 1 }],
                "status": 1,
                "threadContext": {
                    "filePath": "/src/Api/OrderController.cs",
                    "leftFileStart": { "line": 14, "offset": 9 },
                    "leftFileEnd": { "line": 15, "offset": 1 }
                },
                "pullRequestThreadContext": {
                    "iterationContext": { "firstComparingIteration": 1, "secondComparingIteration": 3 },
                    "changeTrackingId": 7
                }
            })
        );
        let file = NewThreadPlace { path: "docs/Legacy.md".into(), lines: None };
        assert_eq!(
            thread_body(Some(&file), "Keep?", Some(3), None)["threadContext"],
            json!({ "filePath": "/docs/Legacy.md" })
        );
        let general = thread_body(None, "Looks good", None, None);
        assert!(general.get("threadContext").is_none());
        assert!(general.get("pullRequestThreadContext").is_none());
    }

    #[test]
    fn parses_a_pull_request() {
        let repo = parse_remote_url("https://dev.azure.com/contoso/Shop/_git/shop-api").unwrap();
        let value: Value = serde_json::from_str(
            r#"{
              "pullRequestId": 42,
              "title": "Add loyalty discount",
              "description": "Customers who return get 10%.",
              "createdBy": { "displayName": "Ada Lovelace", "uniqueName": "ada@contoso.com" },
              "creationDate": "2026-10-14T08:30:00Z",
              "isDraft": true,
              "status": "active",
              "sourceRefName": "refs/heads/feature/loyalty",
              "targetRefName": "refs/heads/main",
              "lastMergeSourceCommit": { "commitId": "aaaa" },
              "lastMergeTargetCommit": { "commitId": "bbbb" },
              "reviewers": [
                { "displayName": "Bo", "vote": 10 },
                { "displayName": "[Shop]\\Shop Team", "vote": 0, "isContainer": true },
                { "displayName": "Cy", "vote": -5 }
              ]
            }"#,
        )
        .unwrap();
        let pr = parse_pull_request(&value, &repo).unwrap();
        assert_eq!(pr.id, 42);
        assert_eq!(pr.author, "Ada Lovelace");
        assert!(pr.draft);
        assert_eq!(pr.status, "active");
        assert_eq!(pr.source_branch, "feature/loyalty");
        assert_eq!(pr.target_branch, "main");
        assert_eq!(pr.source_commit.as_deref(), Some("aaaa"));
        assert_eq!(pr.target_commit.as_deref(), Some("bbbb"));
        assert_eq!(pr.url, "https://dev.azure.com/contoso/Shop/_git/shop-api/pullrequest/42");
        assert_eq!(
            pr.reviewers.iter().map(|r| (r.vote, r.group)).collect::<Vec<_>>(),
            vec![(Vote::Approved, false), (Vote::None, true), (Vote::WaitingForAuthor, false)]
        );
    }
}
