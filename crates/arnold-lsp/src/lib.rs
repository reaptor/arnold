//! Language-server support that lives outside the webview: finding .NET,
//! installing and updating servers, preparing a checkout for them, and a proxy that sits between the editor's
//! LSP client and the server process.

pub mod dotnet;
mod framing;
pub mod fsautocomplete;
pub mod proxy;
pub mod workspace;

pub use proxy::{Event, Session, SessionConfig};
