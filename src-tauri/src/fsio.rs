//! Decoding file contents from git for the editor.
//!
//! The editor works with `\n` line endings and no byte-order mark, so both are
//! taken off when a file is read.

const BOM: &[u8] = &[0xEF, 0xBB, 0xBF];
/// Files above this size are not loaded into the editor.
pub const MAX_TEXT_BYTES: usize = 5 * 1024 * 1024;

#[derive(Debug, PartialEq)]
pub enum Decoded {
    Text(String),
    Binary,
    TooLarge,
}

pub fn decode(bytes: &[u8]) -> Decoded {
    if bytes.len() > MAX_TEXT_BYTES {
        return Decoded::TooLarge;
    }
    let body = bytes.strip_prefix(BOM).unwrap_or(bytes);
    if body.contains(&0) {
        return Decoded::Binary;
    }
    match std::str::from_utf8(body) {
        Ok(text) => Decoded::Text(text.replace("\r\n", "\n")),
        Err(_) => Decoded::Binary,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn strips_crlf_and_bom() {
        let decoded = decode(b"\xEF\xBB\xBFnamespace A\r\n{\r\n}\r\n");
        assert_eq!(decoded, Decoded::Text("namespace A\n{\n}\n".to_string()));
    }

    #[test]
    fn keeps_plain_lf() {
        let decoded = decode(b"let x = 1\nlet y = 2");
        assert_eq!(decoded, Decoded::Text("let x = 1\nlet y = 2".to_string()));
    }

    #[test]
    fn detects_binary() {
        assert_eq!(decode(b"PNG\0\x01\x02"), Decoded::Binary);
        assert_eq!(decode(&[0xFF, 0xFE, 0x41]), Decoded::Binary);
    }
}
