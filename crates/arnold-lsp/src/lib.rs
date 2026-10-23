//! Language-server support that lives outside the webview: finding .NET,
//! installing and updating servers, preparing a checkout for them, and a proxy that sits between the editor's
//! LSP client and the server process.

pub mod build;
pub mod dotnet;
pub mod fantomas;
mod framing;
pub mod fsautocomplete;
mod msbuild;
pub mod proxy;
pub mod workspace;

pub use proxy::{Event, Session, SessionConfig};
