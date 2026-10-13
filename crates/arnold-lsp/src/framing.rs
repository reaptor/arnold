//! LSP base protocol: `Content-Length` framed JSON messages.

use tokio::io::{AsyncBufRead, AsyncBufReadExt, AsyncReadExt, AsyncWrite, AsyncWriteExt};

/// A server gone wrong must not make us allocate without bound.
const MAX_MESSAGE_BYTES: usize = 256 * 1024 * 1024;

/// Reads one message. `Ok(None)` means the stream ended cleanly.
pub async fn read_message<R: AsyncBufRead + Unpin>(reader: &mut R) -> std::io::Result<Option<String>> {
    let mut content_length: Option<usize> = None;
    let mut line = String::new();
    loop {
        line.clear();
        if reader.read_line(&mut line).await? == 0 {
            return Ok(None);
        }
        let header = line.trim_end();
        if header.is_empty() {
            break;
        }
        if let Some((name, value)) = header.split_once(':') {
            if name.eq_ignore_ascii_case("content-length") {
                content_length = value.trim().parse().ok();
            }
        }
    }
    let length = content_length
        .filter(|length| *length <= MAX_MESSAGE_BYTES)
        .ok_or_else(|| std::io::Error::new(std::io::ErrorKind::InvalidData, "bad Content-Length"))?;
    let mut body = vec![0; length];
    reader.read_exact(&mut body).await?;
    String::from_utf8(body)
        .map(Some)
        .map_err(|e| std::io::Error::new(std::io::ErrorKind::InvalidData, e))
}

pub async fn write_message<W: AsyncWrite + Unpin>(writer: &mut W, message: &str) -> std::io::Result<()> {
    let header = format!("Content-Length: {}\r\n\r\n", message.len());
    writer.write_all(header.as_bytes()).await?;
    writer.write_all(message.as_bytes()).await?;
    writer.flush().await
}

#[cfg(test)]
mod tests {
    use super::*;
    use tokio::io::BufReader;

    #[tokio::test]
    async fn round_trips_messages_including_non_ascii() {
        let mut wire = Vec::new();
        write_message(&mut wire, r#"{"a":1}"#).await.unwrap();
        write_message(&mut wire, r#"{"text":"å→ö"}"#).await.unwrap();

        let mut reader = BufReader::new(wire.as_slice());
        assert_eq!(read_message(&mut reader).await.unwrap().as_deref(), Some(r#"{"a":1}"#));
        assert_eq!(
            read_message(&mut reader).await.unwrap().as_deref(),
            Some(r#"{"text":"å→ö"}"#)
        );
        assert_eq!(read_message(&mut reader).await.unwrap(), None);
    }

    #[tokio::test]
    async fn accepts_extra_headers_and_rejects_missing_length() {
        let wire = b"Content-Type: application/vscode-jsonrpc; charset=utf-8\r\ncontent-length: 2\r\n\r\n{}";
        let mut reader = BufReader::new(wire.as_slice());
        assert_eq!(read_message(&mut reader).await.unwrap().as_deref(), Some("{}"));

        let mut bad = BufReader::new(b"X-Other: 1\r\n\r\n{}".as_slice());
        assert!(read_message(&mut bad).await.is_err());
    }
}
