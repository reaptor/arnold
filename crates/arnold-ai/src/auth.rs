//! Signing in to GitHub for Copilot, with GitHub's device flow: Arnold shows
//! a code, the user enters it at github.com/login/device and approves the
//! "Arnold" OAuth app, and the token is kept in the system's credential store.

use std::time::Duration;

use serde::Serialize;
use serde_json::Value;

/// The "Arnold" OAuth app (owned by reaptor, device flow enabled). Public by
/// design: a desktop app cannot keep a secret, and device flow needs none.
const CLIENT_ID: &str = "Ov23liZKVI6KAH6K9m6Y";
/// Lets the user's login name be shown; Copilot itself needs no scope.
const SCOPE: &str = "read:user";
const SERVICE: &str = "app.arnold.review";
const KEY: &str = "github-copilot";

/// Read, in this order, when the user has not signed in from Arnold; the
/// Copilot CLI and SDK use the same names.
pub const ENVIRONMENT_VARIABLES: [&str; 3] = ["COPILOT_GITHUB_TOKEN", "GH_TOKEN", "GITHUB_TOKEN"];

fn client_id() -> String {
    std::env::var("ARNOLD_GITHUB_CLIENT_ID").unwrap_or_else(|_| CLIENT_ID.to_string())
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
#[serde(rename_all = "camelCase", tag = "kind", content = "name")]
pub enum TokenSource {
    /// Signed in from Arnold.
    Keychain,
    /// One of `ENVIRONMENT_VARIABLES`.
    Environment(String),
}

fn entry() -> Result<keyring::Entry, String> {
    keyring::Entry::new(SERVICE, KEY).map_err(|e| e.to_string())
}

/// The GitHub token to use for Copilot, if there is one.
pub fn load() -> Result<Option<(String, TokenSource)>, String> {
    match entry()?.get_password() {
        Ok(token) => return Ok(Some((token, TokenSource::Keychain))),
        Err(keyring::Error::NoEntry) => {}
        Err(error) => return Err(format!("could not read the GitHub sign-in from the credential store: {error}")),
    }
    Ok(ENVIRONMENT_VARIABLES.iter().find_map(|name| {
        let token = std::env::var(name).ok()?.trim().to_string();
        (!token.is_empty()).then(|| (token, TokenSource::Environment(name.to_string())))
    }))
}

fn save(token: &str) -> Result<(), String> {
    entry()?
        .set_password(token)
        .map_err(|e| format!("could not save the GitHub sign-in in the credential store: {e}"))
}

/// Forgets the sign-in; a token from the environment stays.
pub fn sign_out() -> Result<(), String> {
    match entry()?.delete_credential() {
        Ok(()) | Err(keyring::Error::NoEntry) => Ok(()),
        Err(error) => Err(format!("could not remove the GitHub sign-in from the credential store: {error}")),
    }
}

/// What the user has to do to sign in.
#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
pub struct DeviceCode {
    #[serde(skip)]
    pub device_code: String,
    pub user_code: String,
    pub verification_uri: String,
    #[serde(skip)]
    pub interval: u64,
    #[serde(skip)]
    pub expires_in: u64,
}

fn http() -> ureq::Agent {
    ureq::Agent::config_builder()
        .timeout_connect(Some(Duration::from_secs(15)))
        .timeout_global(Some(Duration::from_secs(30)))
        .http_status_as_error(false)
        .tls_config(
            ureq::tls::TlsConfig::builder()
                .root_certs(ureq::tls::RootCerts::PlatformVerifier)
                .build(),
        )
        .build()
        .into()
}

fn post_form(url: &str, form: &[(&str, &str)]) -> Result<Value, String> {
    let mut response = http()
        .post(url)
        .header("Accept", "application/json")
        .send_form(form.iter().copied())
        .map_err(|e| crate::network_error(url, &e))?;
    let status = response.status().as_u16();
    let text = response.body_mut().read_to_string().map_err(|e| format!("{url}: {e}"))?;
    let json: Value = serde_json::from_str(&text).map_err(|_| format!("GitHub answered {status}: {text}"))?;
    Ok(json)
}

/// Starts signing in: the code to show the user.
pub fn request_code() -> Result<DeviceCode, String> {
    let json = post_form(
        "https://github.com/login/device/code",
        &[("client_id", &client_id()), ("scope", SCOPE)],
    )?;
    parse_device_code(&json)
}

fn parse_device_code(json: &Value) -> Result<DeviceCode, String> {
    if let Some(error) = json["error"].as_str() {
        return Err(describe_error(error, json));
    }
    let text = |key: &str| {
        json[key]
            .as_str()
            .map(str::to_string)
            .ok_or_else(|| format!("GitHub gave no {key} for signing in"))
    };
    Ok(DeviceCode {
        device_code: text("device_code")?,
        user_code: text("user_code")?,
        verification_uri: text("verification_uri")?,
        interval: json["interval"].as_u64().unwrap_or(5).max(1),
        expires_in: json["expires_in"].as_u64().unwrap_or(900),
    })
}

#[derive(Debug, PartialEq, Eq)]
enum Poll {
    Pending,
    SlowDown(u64),
    Token(String),
}

fn poll(device_code: &str) -> Result<Poll, String> {
    let json = post_form(
        "https://github.com/login/oauth/access_token",
        &[
            ("client_id", &client_id()),
            ("device_code", device_code),
            ("grant_type", "urn:ietf:params:oauth:grant-type:device_code"),
        ],
    )?;
    parse_poll(&json)
}

fn parse_poll(json: &Value) -> Result<Poll, String> {
    if let Some(token) = json["access_token"].as_str() {
        return Ok(Poll::Token(token.to_string()));
    }
    match json["error"].as_str() {
        Some("authorization_pending") => Ok(Poll::Pending),
        Some("slow_down") => Ok(Poll::SlowDown(json["interval"].as_u64().unwrap_or(10))),
        Some(error) => Err(describe_error(error, json)),
        None => Err(format!("unexpected answer from GitHub: {json}")),
    }
}

fn describe_error(error: &str, json: &Value) -> String {
    match error {
        "expired_token" | "token_expired" => "The code expired before it was entered. Try signing in again.".into(),
        "access_denied" => "Signing in was cancelled on GitHub.".into(),
        "device_flow_disabled" => "Signing in with a code is turned off for Arnold's GitHub app.".into(),
        "incorrect_client_credentials" => "GitHub does not know Arnold's app (wrong client id).".into(),
        _ => json["error_description"].as_str().unwrap_or(error).to_string(),
    }
}

/// The login name of the account `token` belongs to. `Ok(None)` when GitHub
/// does not accept the token (expired or revoked).
pub fn login(token: &str) -> Result<Option<String>, String> {
    let url = "https://api.github.com/user";
    let mut response = http()
        .get(url)
        .header("Accept", "application/vnd.github+json")
        .header("Authorization", &format!("Bearer {token}"))
        .header("User-Agent", "Arnold")
        .call()
        .map_err(|e| crate::network_error(url, &e))?;
    match response.status().as_u16() {
        200 => {
            let json: Value = serde_json::from_str(&response.body_mut().read_to_string().map_err(|e| e.to_string())?)
                .map_err(|e| e.to_string())?;
            Ok(json["login"].as_str().map(str::to_string))
        }
        401 => Ok(None),
        status => Err(format!("GitHub answered {status} when asked who is signed in")),
    }
}

/// Signs in: shows `code` to the user (through `on_code`), waits until it is
/// entered on GitHub, then saves the token. Returns the login name.
pub async fn sign_in(on_code: impl Fn(&DeviceCode)) -> Result<String, String> {
    let code = blocking(request_code).await?;
    on_code(&code);
    let mut interval = code.interval;
    let deadline = tokio::time::Instant::now() + Duration::from_secs(code.expires_in);
    loop {
        tokio::time::sleep(Duration::from_secs(interval)).await;
        if tokio::time::Instant::now() > deadline {
            return Err(describe_error("expired_token", &Value::Null));
        }
        let device_code = code.device_code.clone();
        match blocking(move || poll(&device_code)).await? {
            Poll::Pending => {}
            Poll::SlowDown(next) => interval = next.max(interval + 5),
            Poll::Token(token) => {
                let login = {
                    let token = token.clone();
                    blocking(move || login(&token)).await?
                };
                save(&token)?;
                return Ok(login.unwrap_or_default());
            }
        }
    }
}

async fn blocking<T: Send + 'static>(work: impl FnOnce() -> Result<T, String> + Send + 'static) -> Result<T, String> {
    tokio::task::spawn_blocking(work).await.map_err(|e| e.to_string())?
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn reads_the_device_code() {
        let code = parse_device_code(&json!({
            "device_code": "dc", "user_code": "ABCD-1234",
            "verification_uri": "https://github.com/login/device", "expires_in": 899, "interval": 5
        }))
        .unwrap();
        assert_eq!(code.user_code, "ABCD-1234");
        assert_eq!(code.interval, 5);
        assert_eq!(
            serde_json::to_value(&code).unwrap(),
            json!({"userCode": "ABCD-1234", "verificationUri": "https://github.com/login/device"})
        );
        assert!(parse_device_code(&json!({"error": "device_flow_disabled"})).unwrap_err().contains("turned off"));
    }

    #[test]
    fn reads_poll_answers() {
        assert_eq!(parse_poll(&json!({"error": "authorization_pending"})), Ok(Poll::Pending));
        assert_eq!(parse_poll(&json!({"error": "slow_down", "interval": 10})), Ok(Poll::SlowDown(10)));
        assert_eq!(
            parse_poll(&json!({"access_token": "gho_x", "token_type": "bearer"})),
            Ok(Poll::Token("gho_x".into()))
        );
        assert!(parse_poll(&json!({"error": "access_denied"})).unwrap_err().contains("cancelled"));
        assert!(parse_poll(&json!({"error": "expired_token"})).unwrap_err().contains("expired"));
    }
}
