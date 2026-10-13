//! Language-server support that lives outside the webview: finding .NET,
//! installing and updating servers, and a proxy that sits between the editor's
//! LSP client and the server process.

pub mod dotnet;
mod framing;
pub mod fsautocomplete;
pub mod proxy;

pub use proxy::{Event, Session, SessionConfig};
