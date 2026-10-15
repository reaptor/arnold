// Markdown from the code host (pull request descriptions), rendered as HTML.
// It is someone else's text: the HTML is sanitized, pictures are left out
// (they need the host's sign-in, and the page may not load them anyway) and
// links open in the browser instead of inside Arnold.

import DOMPurify from "dompurify";
import { Marked } from "marked";
import { openInBrowser } from "./DevOpsSetup";

// Azure DevOps keeps single line breaks, as GitHub does in comments.
const marked = new Marked({ gfm: true, breaks: true, async: false });

export function renderMarkdown(text: string): string {
  const html = marked.parse(text) as string;
  return DOMPurify.sanitize(html, {
    FORBID_TAGS: ["img", "picture", "video", "audio", "style", "form", "input", "button", "textarea", "select"],
    FORBID_ATTR: ["style", "class", "id"],
  });
}

/** Opens a clicked link of rendered Markdown in the browser (never inside Arnold). Returns whether it was one. */
export function openClickedLink(event: MouseEvent): boolean {
  const link = (event.target as Element).closest("a");
  if (!link) return false;
  event.preventDefault();
  event.stopPropagation();
  const href = link.getAttribute("href") ?? "";
  if (/^(https?|mailto):/i.test(href)) openInBrowser(href);
  return true;
}

interface Props {
  text: string;
  class?: string;
}

/** Rendered Markdown. */
export function Markdown(props: Props) {
  return <div class={`markdown ${props.class ?? ""}`} innerHTML={renderMarkdown(props.text)} onClick={openClickedLink} />;
}
