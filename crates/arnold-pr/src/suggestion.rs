//! Accepting a code suggestion: a comment holding a ```suggestion block with
//! the lines that replace the ones its thread is about.

const BOM: &[u8] = b"\xEF\xBB\xBF";

/// The lines of the first ```suggestion block of `content` (a comment's
/// Markdown), or `None` when it has none. An empty block suggests removing
/// the lines.
pub fn suggested_lines(content: &str) -> Option<Vec<String>> {
    let mut lines = content.lines();
    let fence = lines.by_ref().find_map(opening_fence)?;
    let mut suggested = Vec::new();
    for line in lines {
        if is_closing_fence(line, fence) {
            return Some(suggested);
        }
        suggested.push(line.trim_end_matches('\r').to_string());
    }
    // Never closed: Markdown runs the block to the end.
    Some(suggested)
}

/// The fence character and its length, when `line` opens a suggestion block.
fn opening_fence(line: &str) -> Option<(char, usize)> {
    let trimmed = line.trim_start();
    let mark = trimmed.chars().next().filter(|mark| *mark == '`' || *mark == '~')?;
    let length = trimmed.chars().take_while(|c| *c == mark).count();
    if length < 3 {
        return None;
    }
    let info = trimmed[length..].trim();
    let word = info.split(|c: char| !c.is_alphanumeric()).next().unwrap_or_default();
    (word == "suggestion").then_some((mark, length))
}

fn is_closing_fence(line: &str, (mark, length): (char, usize)) -> bool {
    let trimmed = line.trim();
    trimmed.len() >= length && trimmed.chars().all(|c| c == mark)
}

/// `original` (a file's bytes) with lines `start` to `end` (1-based, both
/// included) replaced by `lines`. The new lines take the file's line endings
/// (CRLF if most of its lines have them); its byte-order mark and its other
/// bytes stay as they are.
pub fn replace_lines(original: &[u8], start: u32, end: u32, lines: &[String]) -> Result<Vec<u8>, String> {
    let (bom, body) = match original.strip_prefix(BOM) {
        Some(rest) => (true, rest),
        None => (false, original),
    };
    // Each line with its line ending; the last one may have none.
    let file_lines: Vec<&[u8]> = body.split_inclusive(|byte| *byte == b'\n').collect();
    let (start, end) = (start as usize, end as usize);
    if start == 0 || end < start || end > file_lines.len() {
        return Err(format!(
            "The suggestion is about lines {start}–{end}, but the file has {} lines.",
            file_lines.len()
        ));
    }
    let crlf = file_lines.iter().filter(|line| line.ends_with(b"\r\n")).count();
    let lf = file_lines.iter().filter(|line| line.ends_with(b"\n")).count() - crlf;
    let ending: &[u8] = if crlf > lf { b"\r\n" } else { b"\n" };
    // The file's last line without a line ending keeps going without one.
    let ends_open = !file_lines[end - 1].ends_with(b"\n");

    let mut bytes = Vec::with_capacity(original.len());
    if bom {
        bytes.extend_from_slice(BOM);
    }
    for line in &file_lines[..start - 1] {
        bytes.extend_from_slice(line);
    }
    for (index, line) in lines.iter().enumerate() {
        bytes.extend_from_slice(line.as_bytes());
        if !(ends_open && index == lines.len() - 1) {
            bytes.extend_from_slice(ending);
        }
    }
    if ends_open && lines.is_empty() && bytes.ends_with(ending) {
        // The last lines are removed: the file now ends where the line before them did.
        bytes.truncate(bytes.len() - ending.len());
    }
    for line in &file_lines[end..] {
        bytes.extend_from_slice(line);
    }
    Ok(bytes)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn strings(lines: &[&str]) -> Vec<String> {
        lines.iter().map(|line| line.to_string()).collect()
    }

    #[test]
    fn reads_the_suggestion_block() {
        let comment = "Shorter:\n\n```suggestion\n    max 0m reduced\n```\n\nWhat do you think?";
        assert_eq!(suggested_lines(comment), Some(strings(&["    max 0m reduced"])));
        assert_eq!(suggested_lines("~~~~ suggestion\r\na\r\nb\r\n~~~~\r\n"), Some(strings(&["a", "b"])));
        assert_eq!(suggested_lines("```suggestion\n```"), Some(vec![]));
        // A shorter fence inside does not close it.
        assert_eq!(suggested_lines("````suggestion\n```\n````"), Some(strings(&["```"])));
    }

    #[test]
    fn other_blocks_are_no_suggestion() {
        assert_eq!(suggested_lines("```fsharp\nlet x = 1\n```"), None);
        assert_eq!(suggested_lines("``suggestion\nx\n``"), None);
        assert_eq!(suggested_lines("Just words."), None);
    }

    #[test]
    fn replaces_lines() {
        let file = b"a\nb\nc\n";
        assert_eq!(replace_lines(file, 2, 2, &strings(&["B"])).unwrap(), b"a\nB\nc\n");
        assert_eq!(replace_lines(file, 1, 2, &strings(&["x", "y", "z"])).unwrap(), b"x\ny\nz\nc\n");
        assert_eq!(replace_lines(file, 2, 3, &[]).unwrap(), b"a\n");
    }

    #[test]
    fn keeps_line_endings_and_byte_order_mark() {
        let file = b"\xEF\xBB\xBFa\r\nb\r\nc";
        assert_eq!(replace_lines(file, 1, 1, &strings(&["A", "A2"])).unwrap(), b"\xEF\xBB\xBFA\r\nA2\r\nb\r\nc");
        // The last line had no line ending, and still has none.
        assert_eq!(replace_lines(file, 3, 3, &strings(&["C"])).unwrap(), b"\xEF\xBB\xBFa\r\nb\r\nC");
        assert_eq!(replace_lines(file, 3, 3, &[]).unwrap(), b"\xEF\xBB\xBFa\r\nb");
    }

    #[test]
    fn refuses_lines_the_file_does_not_have() {
        assert!(replace_lines(b"a\nb\n", 3, 3, &strings(&["x"])).is_err());
        assert!(replace_lines(b"a\nb\n", 0, 1, &strings(&["x"])).is_err());
        assert!(replace_lines(b"", 1, 1, &strings(&["x"])).is_err());
    }
}
