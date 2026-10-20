//! What Arnold tells the model.

use crate::provider::{ContextItem, ContextKind};

/// Replaces the provider's own instructions (Copilot's are for an agent with tools).
pub const SYSTEM: &str = "\
You are the code review assistant in Arnold, a desktop app where developers review \
git branches and pull requests, often changes written by AI coding agents, before \
they are merged.

The developer asks about code they are looking at: a selection, a file, or all the \
changes of a branch. The code is given with the question, as unified diffs or file \
text. You cannot read other files or run anything; when something you need is not \
in what was given, say what is missing rather than guessing.

How to answer:
- Lead with a short summary of what the code or change does and why it seems to \
be made. Then explain the parts that matter, in the order a reviewer would read them.
- For a diff, explain the change (old behaviour → new behaviour), not only the new code.
- If you notice likely bugs, risky changes or missing pieces, list them at the end \
under a heading \"Worth checking\". Do not invent problems; leave the section out if \
there are none.
- Refer to code as `path:line` (line numbers of the new version unless you say otherwise).
- Use Markdown with fenced code blocks tagged with their language. Be concise: no \
preamble, no repeating the question, no closing offers of further help.
- Answer in the language the developer writes in.";

/// The message sent for a question: the code it is about, then the question.
pub fn message(question: &str, context: &[ContextItem]) -> String {
    let mut out = String::new();
    for item in context {
        let kind = match item.kind {
            ContextKind::Selection => "selection",
            ContextKind::File => "file",
            ContextKind::Diff => "diff",
            ContextKind::Note => "note",
        };
        let fence = fence_for(&item.text);
        let language = match item.kind {
            ContextKind::Diff => "diff",
            _ => "",
        };
        out.push_str(&format!(
            "<context kind=\"{kind}\" source=\"{}\">\n{fence}{language}\n{}{}{fence}\n</context>\n\n",
            item.label.replace('"', "'"),
            item.text,
            if item.text.ends_with('\n') { "" } else { "\n" },
        ));
    }
    out.push_str(question.trim());
    out
}

/// A backtick fence longer than any run of backticks in `text`.
fn fence_for(text: &str) -> String {
    let mut longest = 0;
    let mut run = 0;
    for c in text.chars() {
        if c == '`' {
            run += 1;
            longest = longest.max(run);
        } else {
            run = 0;
        }
    }
    "`".repeat((longest + 1).max(3))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn puts_the_code_before_the_question() {
        let context = [ContextItem {
            label: "src/a.rs, lines 1–2 (new version)".into(),
            kind: ContextKind::Selection,
            text: "fn a() {}\nfn b() {}".into(),
        }];
        assert_eq!(
            message("  Explain this.\n", &context),
            "<context kind=\"selection\" source=\"src/a.rs, lines 1–2 (new version)\">\n```\nfn a() {}\nfn b() {}\n```\n</context>\n\nExplain this."
        );
    }

    #[test]
    fn fences_diffs_and_survives_backticks_in_the_code() {
        let context = [ContextItem {
            label: "All changes".into(),
            kind: ContextKind::Diff,
            text: "+let s = \"```\"\n".into(),
        }];
        let text = message("What changed?", &context);
        assert!(text.starts_with("<context kind=\"diff\" source=\"All changes\">\n````diff\n+let s"), "{text}");
        assert!(text.contains("\n````\n</context>"));
    }
}
