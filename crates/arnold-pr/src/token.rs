//! Personal access tokens, kept in the system's credential store (the macOS
//! Keychain, the Windows Credential Manager), one per organization.

use serde::Serialize;

/// Keychain service the tokens are filed under.
const SERVICE: &str = "app.arnold.review";

/// Read when no token was saved in Arnold; the Azure CLI uses the same name.
pub const ENVIRONMENT_VARIABLE: &str = "AZURE_DEVOPS_EXT_PAT";

/// Where a token came from.
#[derive(Debug, Clone, Copy, Serialize, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
pub enum TokenSource {
    /// Saved from Arnold, in the system's credential store.
    Keychain,
    /// The `AZURE_DEVOPS_EXT_PAT` environment variable.
    Environment,
}

fn entry(key: &str) -> Result<keyring::Entry, String> {
    keyring::Entry::new(SERVICE, key).map_err(|e| e.to_string())
}

/// The token for `key` (see `AzureRepo::token_key`), if there is one.
pub fn load(key: &str) -> Result<Option<(String, TokenSource)>, String> {
    match entry(key)?.get_password() {
        Ok(token) => return Ok(Some((token, TokenSource::Keychain))),
        Err(keyring::Error::NoEntry) => {}
        Err(error) => return Err(format!("could not read the token from the credential store: {error}")),
    }
    Ok(std::env::var(ENVIRONMENT_VARIABLE)
        .ok()
        .map(|token| token.trim().to_string())
        .filter(|token| !token.is_empty())
        .map(|token| (token, TokenSource::Environment)))
}

pub fn save(key: &str, token: &str) -> Result<(), String> {
    entry(key)?
        .set_password(token)
        .map_err(|e| format!("could not save the token in the credential store: {e}"))
}

/// Forgets the saved token; one from the environment stays.
pub fn delete(key: &str) -> Result<(), String> {
    match entry(key)?.delete_credential() {
        Ok(()) | Err(keyring::Error::NoEntry) => Ok(()),
        Err(error) => Err(format!("could not remove the token from the credential store: {error}")),
    }
}
