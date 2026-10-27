//! Azure DevOps Services (dev.azure.com, *.visualstudio.com) and Azure DevOps
//! Server: recognizing their remotes and reading active pull requests with a
//! personal access token.

use std::time::Duration;

use base64::Engine;
use serde::Serialize;
use serde_json::{json, Value};
use ureq::tls::{RootCerts, TlsConfig};

use crate::{
    Comment, CompletionPolicies, LinkedWorkItem, LinkedWorkItems, MergeStrategy, NewPullRequest, NewThreadPlace, PrChange, PrError,
    PullRequest, Reviewer, Side, Thread, ThreadPosition, UnmetPolicy, Vote, WorkItem, WorkItemLookup,
};

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

    /// Azure DevOps Services (dev.azure.com) rather than a server, which
    /// could be any host a remote names.
    pub fn is_services(&self) -> bool {
        self.collection_url.starts_with("https://dev.azure.com/")
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
/// repository's name. Plain `http://` remotes are not taken: the token
/// would travel unencrypted.
pub fn parse_remote_url(url: &str) -> Option<AzureRepo> {
    let url = url.trim();
    if let Some(repo) = parse_ssh(url) {
        return Some(repo);
    }
    let (host, path) = split_https(url)?;
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
        collection_url: format!("https://{host}/{}", collection.join("/")),
        organization,
        project: project.to_string(),
        repository,
    })
}

/// Where git may send credentials for the remote at `url`: the collection
/// the URL is under (`https://dev.azure.com/org`, a server's collection URL),
/// or an `org.visualstudio.com` host. Scoping git's `http.<url>.extraHeader`
/// to it keeps the token from going anywhere else, such as a `pushurl` or an
/// `insteadOf` rewrite that points at another host. `None` when `url` is not
/// an https Azure DevOps remote.
pub fn credential_scope(url: &str) -> Option<String> {
    let repo = parse_remote_url(url)?;
    let (host, _) = split_https(url.trim())?;
    Some(if host.ends_with(".visualstudio.com") { format!("https://{host}/") } else { repo.collection_url })
}

/// The lowercase host (with any port) and the path of an `https://` URL,
/// without `user@`. Authorities that git or curl could read as another host
/// (`https://evil.example?@dev.azure.com/…`, backslashes, a second `@`,
/// whitespace) are refused rather than guessed at.
fn split_https(url: &str) -> Option<(String, &str)> {
    let rest = url.strip_prefix("https://")?;
    let end = rest.find(['/', '?', '#', '\\'])?;
    let (authority, path) = (&rest[..end], rest[end..].strip_prefix('/')?);
    if authority.bytes().any(|b| b.is_ascii_whitespace() || b.is_ascii_control()) {
        return None;
    }
    let host = match authority.split_once('@') {
        Some((_, host)) if host.contains('@') => return None,
        Some((_, host)) => host,
        None => authority,
    };
    let (name, port) = host.split_once(':').unwrap_or((host, ""));
    let name_ok = !name.is_empty() && name.bytes().all(|b| b.is_ascii_alphanumeric() || b == b'.' || b == b'-');
    if !name_ok || !port.bytes().all(|b| b.is_ascii_digit()) {
        return None;
    }
    Some((host.to_lowercase(), path))
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

/// The active pull requests of `repo` from `source` into `target` (branch
/// names without `refs/heads/`); the host allows only one.
pub fn active_pull_requests_between(
    repo: &AzureRepo,
    token: &str,
    source: &str,
    target: &str,
) -> Result<Vec<PullRequest>, PrError> {
    let url = repo.api(&format!(
        "/pullrequests?searchCriteria.status=active&searchCriteria.sourceRefName={}&searchCriteria.targetRefName={}&api-version={API_VERSION}",
        query_encode(&format!("refs/heads/{source}")),
        query_encode(&format!("refs/heads/{target}")),
    ));
    let body = get(&url, token)?;
    let list = body["value"]
        .as_array()
        .ok_or_else(|| PrError::Other("Azure DevOps sent an unexpected answer".into()))?;
    Ok(list.iter().filter_map(|value| parse_pull_request(value, repo)).collect())
}

/// The commit branch `branch` (without `refs/heads/`) of `repo` points at,
/// or `None` when the host has no such branch.
pub fn branch_commit(repo: &AzureRepo, token: &str, branch: &str) -> Result<Option<String>, PrError> {
    let name = format!("refs/heads/{branch}");
    // `filter` matches the start of names: `heads/feature` also finds `heads/feature-2`.
    let url = repo.api(&format!("/refs?filter={}&api-version={API_VERSION}", query_encode(&name["refs/".len()..])));
    let body = get(&url, token)?;
    let list = body["value"]
        .as_array()
        .ok_or_else(|| PrError::Other("Azure DevOps sent an unexpected answer".into()))?;
    Ok(exact_ref(list, &name))
}

/// Who created each branch of `repo`: (branch without `refs/heads/`,
/// creator's display name). Git itself does not record it.
pub fn branch_creators(repo: &AzureRepo, token: &str) -> Result<Vec<(String, String)>, PrError> {
    let url = repo.api(&format!("/refs?filter=heads/&api-version={API_VERSION}"));
    let body = get(&url, token)?;
    let list = body["value"]
        .as_array()
        .ok_or_else(|| PrError::Other("Azure DevOps sent an unexpected answer".into()))?;
    Ok(parse_creators(list))
}

fn parse_creators(list: &[Value]) -> Vec<(String, String)> {
    list.iter()
        .filter_map(|entry| {
            let branch = entry["name"].as_str()?.strip_prefix("refs/heads/")?;
            let creator = entry["creator"]["displayName"].as_str().filter(|name| !name.is_empty())?;
            Some((branch.to_string(), creator.to_string()))
        })
        .collect()
}

fn exact_ref(list: &[Value], name: &str) -> Option<String> {
    list.iter()
        .find(|entry| entry["name"].as_str() == Some(name))
        .and_then(|entry| entry["objectId"].as_str())
        .map(str::to_string)
}

/// Opens a pull request; its source branch has to be on the host.
pub fn create_pull_request(repo: &AzureRepo, token: &str, new: &NewPullRequest) -> Result<PullRequest, PrError> {
    let url = repo.api(&format!("/pullrequests?api-version={API_VERSION}"));
    let answer = post(&url, token, &create_body(new))?;
    parse_pull_request(&answer, repo).ok_or_else(|| PrError::Other("Azure DevOps sent an unexpected answer".into()))
}

fn create_body(new: &NewPullRequest) -> Value {
    let mut body = json!({
        "sourceRefName": format!("refs/heads/{}", new.source_branch),
        "targetRefName": format!("refs/heads/{}", new.target_branch),
        "title": new.title,
        "description": new.description,
        "isDraft": new.draft,
    });
    if !new.work_items.is_empty() {
        body["workItemRefs"] = new.work_items.iter().map(|id| json!({ "id": id.to_string() })).collect();
    }
    body
}

/// Work item `id`, for showing what a new pull request will be linked to.
/// Reading it takes the Work Items → Read scope, which linking does not.
pub fn work_item(repo: &AzureRepo, token: &str, id: u64) -> Result<WorkItemLookup, PrError> {
    // Organization-wide: the work item may belong to another project.
    let url = format!(
        "{}/_apis/wit/workitems/{id}?fields=System.Title,System.WorkItemType,System.State&api-version={API_VERSION}",
        repo.collection_url
    );
    match get(&url, token) {
        Ok(body) => Ok(WorkItemLookup::Found {
            item: parse_work_item(&body, repo, id),
        }),
        Err(PrError::NotFound(_)) => Ok(WorkItemLookup::NotFound),
        Err(PrError::TokenRejected(_)) => Ok(WorkItemLookup::NoAccess),
        Err(error) => Err(error),
    }
}

fn parse_work_item(body: &Value, repo: &AzureRepo, id: u64) -> WorkItem {
    let field = |name: &str| body["fields"][name].as_str().unwrap_or_default().to_string();
    WorkItem {
        id: body["id"].as_u64().unwrap_or(id),
        title: field("System.Title"),
        kind: field("System.WorkItemType"),
        state: field("System.State"),
        url: format!("{}/{}/_workitems/edit/{id}", repo.collection_url, repo.project),
    }
}

/// The work items linked to pull request `id`, with their descriptions.
/// Reading them takes the Work Items → Read scope; without it only their ids
/// are known.
pub fn linked_work_items(repo: &AzureRepo, token: &str, id: u64) -> Result<LinkedWorkItems, PrError> {
    let refs = get(&repo.api(&format!("/pullrequests/{id}/workitems?api-version={API_VERSION}")), token)?;
    let ids = parse_work_item_refs(&refs);
    if ids.is_empty() {
        return Ok(LinkedWorkItems::Read { items: vec![] });
    }
    // Organization-wide, as they may belong to other projects; one the token's owner cannot see is left out.
    let list = ids.iter().map(u64::to_string).collect::<Vec<_>>().join(",");
    let url = format!(
        "{}/_apis/wit/workitems?ids={list}&fields=System.Title,System.WorkItemType,System.State,System.Description\
         &errorPolicy=omit&api-version={API_VERSION}",
        repo.collection_url
    );
    match get(&url, token) {
        Ok(body) => Ok(LinkedWorkItems::Read {
            items: parse_linked_work_items(&body, repo),
        }),
        Err(PrError::TokenRejected(_)) => Ok(LinkedWorkItems::NoAccess { ids }),
        Err(error) => Err(error),
    }
}

fn parse_work_item_refs(body: &Value) -> Vec<u64> {
    let mut ids: Vec<u64> = vec![];
    for entry in body["value"].as_array().into_iter().flatten() {
        // A string in the answer, though a number elsewhere.
        let id = entry["id"].as_str().and_then(|id| id.parse().ok()).or_else(|| entry["id"].as_u64());
        if let Some(id) = id.filter(|id| !ids.contains(id)) {
            ids.push(id);
        }
    }
    ids
}

fn parse_linked_work_items(body: &Value, repo: &AzureRepo) -> Vec<LinkedWorkItem> {
    body["value"]
        .as_array()
        .into_iter()
        .flatten()
        // `errorPolicy=omit` sends null for a work item it could not read.
        .filter_map(|entry| {
            let id = entry["id"].as_u64()?;
            Some(LinkedWorkItem {
                item: parse_work_item(entry, repo, id),
                description: entry["fields"]["System.Description"].as_str().unwrap_or_default().to_string(),
                markdown: entry["multilineFieldsFormat"]["System.Description"]
                    .as_str()
                    .is_some_and(|format| format.eq_ignore_ascii_case("markdown")),
            })
        })
        .collect()
}

/// The names of the required branch policies of `repo` that apply to
/// `branch` (without `refs/heads/`). A branch with any of them takes changes
/// only through pull requests: a push to it is refused.
pub fn required_branch_policies(repo: &AzureRepo, token: &str, branch: &str) -> Result<Vec<String>, PrError> {
    let (repository, _) = repository_ids(repo, token)?;
    Ok(blocking_branch_policies(&policy_configurations(repo, token, &repository, branch)?))
}

/// What the branch policies of `target_branch` allow when completing pull
/// request `id`: the merge types, and which required policies it does not
/// meet yet.
pub fn completion_policies(repo: &AzureRepo, token: &str, id: u64, target_branch: &str) -> Result<CompletionPolicies, PrError> {
    let (repository, project) = repository_ids(repo, token)?;
    let configurations = policy_configurations(repo, token, &repository, target_branch)?;
    let url = format!(
        "{}/{}/_apis/policy/evaluations?artifactId={}&api-version=6.0-preview.1",
        repo.collection_url,
        repo.project,
        query_encode(&format!("vstfs:///CodeReview/CodeReviewId/{project}/{id}")),
    );
    let body = get(&url, token)?;
    let evaluations = body["value"]
        .as_array()
        .ok_or_else(|| PrError::Other("Azure DevOps sent an unexpected answer".into()))?;
    Ok(CompletionPolicies {
        merge_strategies: allowed_merge_strategies(&configurations),
        unmet: unmet_policies(evaluations),
    })
}

/// Queues policy evaluation `evaluation_id` (from `UnmetPolicy`) again, as
/// Azure DevOps' Re-queue does: an expired build policy runs a new build.
pub fn requeue_policy(repo: &AzureRepo, token: &str, evaluation_id: &str) -> Result<(), PrError> {
    let url = format!(
        "{}/{}/_apis/policy/evaluations/{}?api-version=6.0-preview.1",
        repo.collection_url,
        repo.project,
        query_encode(evaluation_id),
    );
    send(agent().patch(&url), &url, token, &json!({}))?;
    Ok(())
}

/// The ids of the repository and of its project, which policies go by.
fn repository_ids(repo: &AzureRepo, token: &str) -> Result<(String, String), PrError> {
    let about = get(&repo.api(&format!("?api-version={API_VERSION}")), token)?;
    match (about["id"].as_str(), about["project"]["id"].as_str()) {
        (Some(repository), Some(project)) => Ok((repository.to_string(), project.to_string())),
        _ => Err(PrError::Other("Azure DevOps sent an unexpected answer".into())),
    }
}

/// The policies set up for `branch` of the repository with id `repository`.
fn policy_configurations(repo: &AzureRepo, token: &str, repository: &str, branch: &str) -> Result<Vec<Value>, PrError> {
    // Given the repository and the branch, the host works out which policies
    // apply: those set on the branch itself, on a folder of branches it is
    // in, and (for the default branch) across repositories.
    let url = format!(
        "{}/{}/_apis/git/policy/configurations?repositoryId={repository}&refName={}&$top=1000&api-version=6.0-preview.1",
        repo.collection_url,
        repo.project,
        query_encode(&format!("refs/heads/{branch}")),
    );
    let mut body = get(&url, token)?;
    match body["value"].take() {
        Value::Array(list) => Ok(list),
        _ => Err(PrError::Other("Azure DevOps sent an unexpected answer".into())),
    }
}

/// Enabled, required and not deleted: the host enforces it.
fn is_required(policy: &Value) -> bool {
    policy["isEnabled"].as_bool() == Some(true)
        && policy["isBlocking"].as_bool() == Some(true)
        && policy["isDeleted"].as_bool() != Some(true)
}

/// The type of the "Limit merge types" policy.
const MERGE_STRATEGY_POLICY: &str = "fa4e907d-c16b-4a4c-9dfa-4916e5d171ab";

fn is_merge_strategy_policy(policy: &Value) -> bool {
    policy["type"]["id"].as_str().is_some_and(|id| id.eq_ignore_ascii_case(MERGE_STRATEGY_POLICY))
}

/// The types of the "Minimum number of reviewers" and "Required reviewers" policies.
const APPROVAL_POLICIES: [&str; 2] = ["fa4e907d-c16b-4a4c-9dfa-4906e5d171dd", "fd2167ab-b0be-447a-8ec8-39368250530e"];

/// A policy that reviewers meet by approving.
fn is_approval_policy(policy: &Value) -> bool {
    let kind = &policy["type"];
    kind["id"].as_str().is_some_and(|id| APPROVAL_POLICIES.iter().any(|known| id.eq_ignore_ascii_case(known)))
        || matches!(kind["displayName"].as_str(), Some("Minimum number of reviewers" | "Required reviewers"))
}

/// The merge types every required "Limit merge types" policy allows; all of
/// them when there is none.
fn allowed_merge_strategies(configurations: &[Value]) -> Vec<MergeStrategy> {
    let mut allowed = MergeStrategy::ALL.to_vec();
    for policy in configurations.iter().filter(|policy| is_required(policy) && is_merge_strategy_policy(policy)) {
        let settings = &policy["settings"];
        let flag = |strategy: MergeStrategy| match strategy {
            MergeStrategy::NoFastForward => "allowNoFastForward",
            MergeStrategy::Squash => "allowSquash",
            MergeStrategy::Rebase => "allowRebase",
            MergeStrategy::RebaseMerge => "allowRebaseMerge",
        };
        let limits = MergeStrategy::ALL.iter().any(|strategy| !settings[flag(*strategy)].is_null());
        allowed.retain(|strategy| {
            if limits {
                settings[flag(*strategy)].as_bool() == Some(true)
            } else {
                // Set up before merge types could be picked one by one: squash only, or nothing limited.
                settings["useSquashMerge"].as_bool() != Some(true) || *strategy == MergeStrategy::Squash
            }
        });
    }
    allowed
}

/// The required policies, from the pull request's policy evaluations, that
/// it does not meet yet. The merge type is chosen when completing, so that
/// one is left out.
fn unmet_policies(evaluations: &[Value]) -> Vec<UnmetPolicy> {
    let mut unmet: Vec<UnmetPolicy> = Vec::new();
    for evaluation in evaluations {
        let policy = &evaluation["configuration"];
        let status = evaluation["status"].as_str().unwrap_or("queued");
        if !is_required(policy) || is_merge_strategy_policy(policy) || matches!(status, "approved" | "notApplicable") {
            continue;
        }
        // A build policy's build goes out of date as the target branch moves on.
        let status = if evaluation["context"]["isExpired"].as_bool() == Some(true) { "expired" } else { status };
        let kind = policy["type"]["displayName"].as_str().unwrap_or("Branch policy");
        // Build and status policies are told apart by their own names.
        let name = match policy["settings"]["displayName"].as_str().filter(|name| !name.is_empty()) {
            Some(own) => format!("{kind}: {own}"),
            None => kind.to_string(),
        };
        if !unmet.iter().any(|known| known.name == name) {
            unmet.push(UnmetPolicy {
                name,
                status: status.to_string(),
                evaluation_id: evaluation["evaluationId"].as_str().unwrap_or_default().to_string(),
                approval: is_approval_policy(policy),
            });
        }
    }
    unmet
}

/// Names of the enabled, required policies set on branches. Those set on the
/// repository as a whole (file size, path length, ...) only check what is
/// pushed, so they are left out.
fn blocking_branch_policies(list: &[Value]) -> Vec<String> {
    let mut names: Vec<String> = Vec::new();
    for policy in list {
        let required = is_required(policy);
        let on_branches = policy["settings"]["scope"].as_array().is_some_and(|scopes| {
            scopes.iter().any(|scope| {
                scope["refName"].is_string() || scope["matchKind"].as_str().is_some_and(|kind| kind.eq_ignore_ascii_case("defaultBranch"))
            })
        });
        let name = policy["type"]["displayName"].as_str().unwrap_or("Branch policy");
        if required && on_branches && !names.iter().any(|known| known == name) {
            names.push(name.to_string());
        }
    }
    names
}

/// Percent-encodes a query parameter's value; `/` is left as it is.
fn query_encode(text: &str) -> String {
    let mut out = String::with_capacity(text.len());
    for byte in text.bytes() {
        if byte.is_ascii_alphanumeric() || b"-._~/".contains(&byte) {
            out.push(byte as char);
        } else {
            out.push_str(&format!("%{byte:02X}"));
        }
    }
    out
}

/// The host's id of the token's owner, which is how a reviewer is known.
pub fn current_user(repo: &AzureRepo, token: &str) -> Result<String, PrError> {
    // Not versioned: it has looked the same since the first servers.
    let body = get(&format!("{}/_apis/connectionData", repo.collection_url), token)?;
    body["authenticatedUser"]["id"]
        .as_str()
        .map(str::to_string)
        .ok_or_else(|| PrError::Other("Azure DevOps did not say who the token belongs to".into()))
}

/// Votes on pull request `id` as `user` (from `current_user`), who becomes a
/// reviewer if not one already. `Vote::None` resets the vote.
pub fn vote(repo: &AzureRepo, token: &str, id: u64, user: &str, vote: Vote) -> Result<(), PrError> {
    let url = repo.api(&format!("/pullRequests/{id}/reviewers/{user}?api-version={API_VERSION}"));
    send(agent().put(&url), &url, token, &json!({ "vote": vote_value(vote) }))?;
    Ok(())
}

fn vote_value(vote: Vote) -> i64 {
    match vote {
        Vote::Approved => 10,
        Vote::ApprovedWithSuggestions => 5,
        Vote::None => 0,
        Vote::WaitingForAuthor => -5,
        Vote::Rejected => -10,
    }
}

/// Completes, abandons, reactivates, drafts or publishes pull request `id`.
/// Resolves to the pull request as the host has it afterwards; completing
/// may still be going on then (`merge_status` `queued`).
pub fn update(repo: &AzureRepo, token: &str, id: u64, change: &PrChange) -> Result<PullRequest, PrError> {
    let url = repo.api(&format!("/pullRequests/{id}?api-version={API_VERSION}"));
    let answer = send(agent().patch(&url), &url, token, &update_body(change))?;
    parse_pull_request(&answer, repo).ok_or_else(|| PrError::Other("Azure DevOps sent an unexpected answer".into()))
}

fn update_body(change: &PrChange) -> Value {
    match change {
        PrChange::Complete(completion) => json!({
            "status": "completed",
            "lastMergeSourceCommit": { "commitId": completion.source_commit },
            "completionOptions": {
                "mergeStrategy": completion.merge_strategy,
                "mergeCommitMessage": completion.message,
                "deleteSourceBranch": completion.delete_source_branch,
                "transitionWorkItems": completion.complete_work_items,
            },
        }),
        PrChange::Abandon => json!({ "status": "abandoned" }),
        PrChange::Reactivate => json!({ "status": "active" }),
        PrChange::Draft { draft } => json!({ "isDraft": draft }),
    }
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

/// How many discussions of pull request `id` are still to be resolved: those
/// with something people wrote, as `threads` lists them.
pub fn open_thread_count(repo: &AzureRepo, token: &str, id: u64) -> Result<usize, PrError> {
    let body = get(&repo.api(&format!("/pullRequests/{id}/threads?api-version={API_VERSION}")), token)?;
    let list = body["value"]
        .as_array()
        .ok_or_else(|| PrError::Other("Azure DevOps sent an unexpected answer".into()))?;
    Ok(list.iter().filter_map(parse_thread).filter(Thread::is_open).count())
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

/// Changes the text of comment `comment` of thread `thread` of pull request
/// `id` to `content` (Markdown); only its author may.
pub fn edit_comment(repo: &AzureRepo, token: &str, id: u64, thread: u64, comment: u64, content: &str) -> Result<(), PrError> {
    let url = repo.api(&format!("/pullRequests/{id}/threads/{thread}/comments/{comment}?api-version={API_VERSION}"));
    send(agent().patch(&url), &url, token, &json!({ "content": content }))?;
    Ok(())
}

/// Marks thread `thread` of pull request `id` resolved.
pub fn resolve_thread(repo: &AzureRepo, token: &str, id: u64, thread: u64) -> Result<(), PrError> {
    let url = repo.api(&format!("/pullRequests/{id}/threads/{thread}?api-version={API_VERSION}"));
    // 2: fixed, which the web page calls Resolved.
    send(agent().patch(&url), &url, token, &json!({ "status": 2 }))?;
    Ok(())
}

/// Commits `content` as the new contents of `path` on `branch` (without
/// `refs/heads/`), on top of `commit`: the host refuses when the branch is
/// somewhere else by now. Resolves to the new commit.
pub fn push_file(
    repo: &AzureRepo,
    token: &str,
    branch: &str,
    commit: &str,
    path: &str,
    content: &[u8],
    message: &str,
) -> Result<String, PrError> {
    let url = repo.api(&format!("/pushes?api-version={API_VERSION}"));
    let answer = post(&url, token, &push_body(branch, commit, path, content, message))?;
    answer["refUpdates"][0]["newObjectId"]
        .as_str()
        .or_else(|| answer["commits"][0]["commitId"].as_str())
        .map(str::to_string)
        .ok_or_else(|| PrError::Other("Azure DevOps sent an unexpected answer".into()))
}

/// What `push_file` sends: the bytes as they are, in base64, so line endings
/// and encoding are kept.
fn push_body(branch: &str, commit: &str, path: &str, content: &[u8], message: &str) -> Value {
    json!({
        "refUpdates": [{ "name": format!("refs/heads/{branch}"), "oldObjectId": commit }],
        "commits": [{
            "comment": message,
            "changes": [{
                "changeType": "edit",
                "item": { "path": repo_path(path) },
                "newContent": {
                    "content": base64::engine::general_purpose::STANDARD.encode(content),
                    "contentType": "base64encoded",
                },
            }],
        }],
    })
}

/// Deletes comment `comment` of thread `thread` of pull request `id`; only its
/// author may. A thread whose comments are all deleted is gone.
pub fn delete_comment(repo: &AzureRepo, token: &str, id: u64, thread: u64, comment: u64) -> Result<(), PrError> {
    let url = repo.api(&format!("/pullRequests/{id}/threads/{thread}/comments/{comment}?api-version={API_VERSION}"));
    let mut response = agent()
        .delete(&url)
        .header("Authorization", authorization(token))
        .header("Accept", "application/json")
        .header("X-TFS-FedAuthRedirect", "Suppress")
        .call()
        .map_err(|e| PrError::Network(network_error(&url, &e)))?;
    let status = response.status().as_u16();
    let text = response.body_mut().read_to_string().unwrap_or_default();
    // Answered with no body when done.
    if matches!(status, 200 | 204) && !text.trim_start().starts_with('<') {
        return Ok(());
    }
    classify(status, &text).map(|_| ())
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
    send(agent().post(url), url, token, body)
}

/// Sends `body` as JSON with `request` (a POST, PUT or PATCH of `url`).
fn send(request: ureq::RequestBuilder<ureq::typestate::WithBody>, url: &str, token: &str, body: &Value) -> Result<Value, PrError> {
    let mut response = request
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
                    id: text(&reviewer["id"]),
                    name: text(&reviewer["displayName"]),
                    vote: match reviewer["vote"].as_i64().unwrap_or(0) {
                        10 => Vote::Approved,
                        5 => Vote::ApprovedWithSuggestions,
                        -5 => Vote::WaitingForAuthor,
                        -10 => Vote::Rejected,
                        _ => Vote::None,
                    },
                    group: reviewer["isContainer"].as_bool().unwrap_or(false),
                    you: false,
                })
                .collect()
        })
        .unwrap_or_default();
    Some(PullRequest {
        id,
        title: text(&value["title"]),
        description: text(&value["description"]),
        author: text(&value["createdBy"]["displayName"]),
        author_id: text(&value["createdBy"]["id"]),
        yours: false,
        created: text(&value["creationDate"]),
        draft: value["isDraft"].as_bool().unwrap_or(false),
        status: text(&value["status"]),
        source_branch: branch(&value["sourceRefName"]),
        target_branch: branch(&value["targetRefName"]),
        source_commit: commit(&value["lastMergeSourceCommit"]),
        target_commit: commit(&value["lastMergeTargetCommit"]),
        url: repo.pull_request_url(id),
        reviewers,
        merge_status: value["mergeStatus"].as_str().unwrap_or("notSet").to_string(),
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
                author_id: text(&comment["author"]["id"]),
                you: false,
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
    fn pushes_the_file_on_top_of_the_reviewed_commit() {
        let body = push_body("feature/x", "abc", "src/A.fs", b"let a = 1\r\n", "Apply suggestion");
        assert_eq!(body["refUpdates"][0], json!({ "name": "refs/heads/feature/x", "oldObjectId": "abc" }));
        let change = &body["commits"][0]["changes"][0];
        assert_eq!(change["item"]["path"], "/src/A.fs");
        assert_eq!(change["newContent"]["contentType"], "base64encoded");
        assert_eq!(change["newContent"]["content"], "bGV0IGEgPSAxDQo=");
        assert_eq!(body["commits"][0]["comment"], "Apply suggestion");
    }

    #[test]
    fn reads_branch_creators() {
        let list = [
            json!({ "name": "refs/heads/feature/x", "objectId": "a", "creator": { "displayName": "Ada Lovelace", "uniqueName": "ada@x" } }),
            json!({ "name": "refs/heads/old", "objectId": "b" }),
            json!({ "name": "refs/tags/v1", "objectId": "c", "creator": { "displayName": "Ada Lovelace" } }),
        ];
        assert_eq!(parse_creators(&list), vec![("feature/x".to_string(), "Ada Lovelace".to_string())]);
    }

    #[test]
    fn merge_types_are_limited_by_every_required_policy() {
        let policy = |enabled: bool, settings: Value| {
            json!({
                "isEnabled": enabled,
                "isBlocking": true,
                "type": { "id": MERGE_STRATEGY_POLICY, "displayName": "Require a merge strategy" },
                "settings": settings,
            })
        };
        let reviewers = json!({ "isEnabled": true, "isBlocking": true, "type": { "id": "fa4e907d-c16b-4a4c-9dfa-4916e5d171aa" }, "settings": {} });
        assert_eq!(allowed_merge_strategies(&[reviewers.clone()]), MergeStrategy::ALL.to_vec());
        let squash_or_semi_linear = policy(
            true,
            json!({ "allowNoFastForward": false, "allowSquash": true, "allowRebase": false, "allowRebaseMerge": true }),
        );
        assert_eq!(
            allowed_merge_strategies(&[reviewers, squash_or_semi_linear.clone()]),
            vec![MergeStrategy::Squash, MergeStrategy::RebaseMerge]
        );
        // A missing flag is not allowed once any is there.
        let only_rebase_merge = policy(true, json!({ "allowRebaseMerge": true }));
        assert_eq!(
            allowed_merge_strategies(&[squash_or_semi_linear.clone(), only_rebase_merge]),
            vec![MergeStrategy::RebaseMerge]
        );
        let disabled = policy(false, json!({ "allowSquash": true }));
        assert_eq!(allowed_merge_strategies(&[disabled]), MergeStrategy::ALL.to_vec());
        assert_eq!(allowed_merge_strategies(&[policy(true, json!({ "useSquashMerge": true }))]), vec![MergeStrategy::Squash]);
        assert_eq!(
            allowed_merge_strategies(&[policy(true, json!({ "useSquashMerge": false }))]),
            MergeStrategy::ALL.to_vec()
        );
    }

    #[test]
    fn unmet_policies_are_the_required_ones_not_approved() {
        let evaluation = |name: &str, settings: Value, blocking: bool, status: &str| {
            json!({
                "evaluationId": format!("{name} {status}"),
                "status": status,
                "configuration": {
                    "isEnabled": true,
                    "isBlocking": blocking,
                    "type": { "id": "x", "displayName": name },
                    "settings": settings,
                },
            })
        };
        let merge_types = json!({
            "status": "rejected",
            "configuration": { "isEnabled": true, "isBlocking": true, "type": { "id": MERGE_STRATEGY_POLICY }, "settings": {} },
        });
        let list = [
            evaluation("Minimum number of reviewers", json!({ "minimumApproverCount": 2 }), true, "rejected"),
            evaluation("Build", json!({ "displayName": "CI" }), true, "running"),
            evaluation("Build", json!({ "displayName": "Nightly" }), false, "queued"),
            evaluation("Comment requirements", json!({}), true, "approved"),
            evaluation("Work item linking", json!({}), true, "notApplicable"),
            merge_types,
        ];
        let mut expired = evaluation("Build", json!({ "displayName": "Release" }), true, "queued");
        expired["context"] = json!({ "buildId": 12, "isExpired": true });
        let list = [&list[..], &[expired]].concat();
        assert_eq!(
            unmet_policies(&list),
            vec![
                UnmetPolicy {
                    name: "Minimum number of reviewers".into(),
                    status: "rejected".into(),
                    evaluation_id: "Minimum number of reviewers rejected".into(),
                    approval: true,
                },
                UnmetPolicy { name: "Build: CI".into(), status: "running".into(), evaluation_id: "Build running".into(), approval: false },
                UnmetPolicy { name: "Build: Release".into(), status: "expired".into(), evaluation_id: "Build queued".into(), approval: false },
            ]
        );
    }

    #[test]
    fn required_branch_policies_leave_out_optional_disabled_and_repository_wide_ones() {
        let policy = |name: &str, enabled: bool, blocking: bool, scope: Value| {
            json!({
                "isEnabled": enabled,
                "isBlocking": blocking,
                "isDeleted": false,
                "type": { "displayName": name },
                "settings": { "scope": [scope] },
            })
        };
        let branch = json!({ "refName": "refs/heads/main", "matchKind": "Exact", "repositoryId": "r" });
        let folder = json!({ "refName": "refs/heads/release/", "matchKind": "Prefix", "repositoryId": "r" });
        let default_branch = json!({ "matchKind": "DefaultBranch", "repositoryId": null });
        let repository = json!({ "repositoryId": "r" });
        let list = [
            policy("Minimum number of reviewers", true, true, branch.clone()),
            policy("Build", true, true, folder),
            policy("Minimum number of reviewers", true, true, default_branch.clone()),
            policy("Comment requirements", true, false, branch.clone()),
            policy("Work item linking", false, true, branch),
            policy("File size restriction", true, true, repository),
            policy("Required reviewers", true, true, default_branch),
        ];
        assert_eq!(
            blocking_branch_policies(&list),
            vec!["Minimum number of reviewers", "Build", "Required reviewers"]
        );
        assert!(blocking_branch_policies(&[]).is_empty());
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
        assert_eq!(parse_remote_url("http://tfs.example.com/tfs/DefaultCollection/Shop/_git/shop-api"), None);
        assert_eq!(parse_remote_url("http://dev.azure.com/contoso/Shop/_git/shop-api"), None);
        assert_eq!(
            parse_remote_url("https://tfs.example.com:8443/tfs/DefaultCollection/Shop/_git/shop-api").unwrap().collection_url,
            "https://tfs.example.com:8443/tfs/DefaultCollection"
        );
    }

    #[test]
    fn refuses_authorities_git_could_read_as_another_host() {
        for url in [
            "https://evil.example?@dev.azure.com/contoso/Shop/_git/shop-api",
            "https://evil.example#@dev.azure.com/contoso/Shop/_git/shop-api",
            "https://evil.example?.visualstudio.com/Shop/_git/shop-api",
            "https://evil.example\\@dev.azure.com/contoso/Shop/_git/shop-api",
            "https://a@evil.example@dev.azure.com/contoso/Shop/_git/shop-api",
            "https://evil example@dev.azure.com/contoso/Shop/_git/shop-api",
        ] {
            assert_eq!(parse_remote_url(url), None, "{url}");
            assert_eq!(credential_scope(url), None, "{url}");
        }
    }

    #[test]
    fn scopes_credentials_to_the_remote() {
        assert_eq!(
            credential_scope("https://contoso@dev.azure.com/contoso/Shop/_git/shop-api").as_deref(),
            Some("https://dev.azure.com/contoso")
        );
        assert_eq!(
            credential_scope("https://Contoso.visualstudio.com/DefaultCollection/Shop/_git/shop-api").as_deref(),
            Some("https://contoso.visualstudio.com/")
        );
        assert_eq!(
            credential_scope("https://tfs.example.com/tfs/DefaultCollection/Shop/_git/shop-api").as_deref(),
            Some("https://tfs.example.com/tfs/DefaultCollection")
        );
        // ssh remotes never carry the header.
        assert_eq!(credential_scope("git@ssh.dev.azure.com:v3/contoso/Shop/shop-api"), None);
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
        assert!(repo.is_services());
        let server = parse_remote_url("https://dev.azure.com.example/tfs/DefaultCollection/Shop/_git/shop-api").unwrap();
        assert!(!server.is_services());
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
        // Only the threads of people count, and of those the ones not resolved.
        assert_eq!(threads.iter().filter(|thread| thread.is_open()).count(), 2);
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
              "mergeStatus": "conflicts",
              "reviewers": [
                { "id": "b0", "displayName": "Bo", "vote": 10 },
                { "id": "t1", "displayName": "[Shop]\\Shop Team", "vote": 0, "isContainer": true },
                { "id": "c2", "displayName": "Cy", "vote": -5 }
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
            pr.reviewers.iter().map(|r| (r.id.as_str(), r.vote, r.group)).collect::<Vec<_>>(),
            vec![("b0", Vote::Approved, false), ("t1", Vote::None, true), ("c2", Vote::WaitingForAuthor, false)]
        );
        assert_eq!(pr.merge_status, "conflicts");
    }

    #[test]
    fn finds_exact_refs_and_builds_create_bodies() {
        let list: Vec<Value> = serde_json::from_value(json!([
            { "name": "refs/heads/agent/task-2", "objectId": "2222" },
            { "name": "refs/heads/agent/task", "objectId": "1111" },
        ]))
        .unwrap();
        assert_eq!(exact_ref(&list, "refs/heads/agent/task").as_deref(), Some("1111"));
        assert_eq!(exact_ref(&list, "refs/heads/agent"), None);
        assert_eq!(query_encode("heads/feature/a b+c&d"), "heads/feature/a%20b%2Bc%26d");
        assert_eq!(query_encode("heads/öl"), "heads/%C3%B6l");

        let new = NewPullRequest {
            source_branch: "agent/task".into(),
            target_branch: "main".into(),
            title: "Add loyalty discount".into(),
            description: "- one\n- two".into(),
            draft: true,
            work_items: vec![],
        };
        assert_eq!(
            create_body(&new),
            json!({
                "sourceRefName": "refs/heads/agent/task",
                "targetRefName": "refs/heads/main",
                "title": "Add loyalty discount",
                "description": "- one\n- two",
                "isDraft": true,
            })
        );
        let linked = NewPullRequest { work_items: vec![1234], ..new };
        assert_eq!(create_body(&linked)["workItemRefs"], json!([{ "id": "1234" }]));

        let repo = parse_remote_url("https://dev.azure.com/contoso/Shop/_git/shop-api").unwrap();
        let body = json!({ "id": 1234, "fields": {
            "System.Title": "Loyalty discount", "System.WorkItemType": "User Story", "System.State": "Active" } });
        assert_eq!(
            parse_work_item(&body, &repo, 1234),
            WorkItem {
                id: 1234,
                title: "Loyalty discount".into(),
                kind: "User Story".into(),
                state: "Active".into(),
                url: "https://dev.azure.com/contoso/Shop/_workitems/edit/1234".into(),
            }
        );

        let refs = json!({ "count": 3, "value": [
            { "id": "1234", "url": "…" }, { "id": "77", "url": "…" }, { "id": "1234", "url": "…" } ] });
        assert_eq!(parse_work_item_refs(&refs), vec![1234, 77]);
        let items = json!({ "count": 3, "value": [
            { "id": 1234, "fields": { "System.Title": "Loyalty discount", "System.WorkItemType": "User Story",
                "System.State": "Active", "System.Description": "<div>Ten percent off</div>" } },
            null,
            { "id": 77, "fields": { "System.Title": "Rounding", "System.WorkItemType": "Bug", "System.State": "New",
                "System.Description": "**Off** by one" }, "multilineFieldsFormat": { "System.Description": "markdown" } },
        ] });
        let linked = parse_linked_work_items(&items, &repo);
        assert_eq!(
            linked.iter().map(|each| (each.item.id, each.description.as_str(), each.markdown)).collect::<Vec<_>>(),
            vec![(1234, "<div>Ten percent off</div>", false), (77, "**Off** by one", true)]
        );
        assert_eq!(linked[1].item.title, "Rounding");
    }

    #[test]
    fn builds_update_bodies() {
        let change: PrChange = serde_json::from_value(json!({
            "kind": "complete",
            "sourceCommit": "aaaa",
            "mergeStrategy": "squash",
            "message": "Merged PR 42: Add loyalty discount",
            "deleteSourceBranch": true,
            "completeWorkItems": false,
        }))
        .unwrap();
        assert_eq!(
            update_body(&change),
            json!({
                "status": "completed",
                "lastMergeSourceCommit": { "commitId": "aaaa" },
                "completionOptions": {
                    "mergeStrategy": "squash",
                    "mergeCommitMessage": "Merged PR 42: Add loyalty discount",
                    "deleteSourceBranch": true,
                    "transitionWorkItems": false,
                },
            })
        );
        let parse = |value: Value| serde_json::from_value::<PrChange>(value).unwrap();
        assert_eq!(update_body(&parse(json!({ "kind": "abandon" }))), json!({ "status": "abandoned" }));
        assert_eq!(update_body(&parse(json!({ "kind": "reactivate" }))), json!({ "status": "active" }));
        assert_eq!(update_body(&parse(json!({ "kind": "draft", "draft": true }))), json!({ "isDraft": true }));
        assert_eq!(
            [Vote::Approved, Vote::ApprovedWithSuggestions, Vote::None, Vote::WaitingForAuthor, Vote::Rejected].map(vote_value),
            [10, 5, 0, -5, -10]
        );
    }
}
