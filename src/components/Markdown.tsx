// Markdown from the code host (pull request descriptions and comments),
// rendered as HTML the way Azure DevOps does:
// https://learn.microsoft.com/en-us/azure/devops/project/wiki/markdown-guidance?view=azure-devops
// GitHub-flavored Markdown (tables, task lists, strikethrough) with single
// line breaks kept, :emoji: shortcodes, #123 work item and !123 pull request
// links, and ```suggestion blocks shown as suggested changes. Not supported:
// math, Mermaid diagrams and syntax colors in code blocks.
//
// It is someone else's text: the HTML is sanitized, pictures are left out
// (they need the host's sign-in, and the page may not load them anyway) and
// links open in the browser instead of inside Arnold.

import DOMPurify from "dompurify";
import { nameToEmoji } from "gemoji";
import { Marked, type TokenizerAndRendererExtension } from "marked";
import { openInBrowser } from "./DevOpsSetup";

/** Where the #123 and !123 references of the repository under review point. */
export interface HostLinks {
  workItem(id: number): string;
  pullRequest(id: number): string;
}

let hostLinks: HostLinks | null = null;

/** Sets where #123 and !123 link to; null leaves them as text. */
export function setHostLinks(links: HostLinks | null) {
  hostLinks = links;
}

/** Links of an Azure DevOps pull request page (`…/_git/repo/pullrequest/7`), worked out from its URL. */
export function azureHostLinks(pullRequestUrl: string): HostLinks | null {
  const repository = /^(.*\/_git\/[^/]+)\/pullrequest\/\d+$/.exec(pullRequestUrl)?.[1];
  const project = repository?.replace(/\/_git\/[^/]+$/, "");
  if (!repository || !project) return null;
  return {
    workItem: (id) => `${project}/_workitems/edit/${id}`,
    pullRequest: (id) => `${repository}/pullrequest/${id}`,
  };
}

const escapeHtml = (text: string) =>
  text.replace(/[&<>"']/g, (char) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;", "'": "&#39;" })[char]!);

/** An inline token starting with `prefix`, where it begins a word. */
const wordStart = (prefix: string) => (source: string) => {
  const match = new RegExp(`(^|[\\s(\\[])${prefix}`).exec(source);
  return match ? match.index + match[1].length : undefined;
};

const emoji: TokenizerAndRendererExtension = {
  name: "emoji",
  level: "inline",
  start: (source) => source.match(/:[\w+-]+:/)?.index,
  tokenizer(source) {
    const match = /^:([\w+-]+):/.exec(source);
    const character = match && nameToEmoji[match[1]];
    return character ? { type: "emoji", raw: match[0], character } : undefined;
  },
  renderer: (token) => token.character,
};

/** `#123` (a work item) or `!123` (a pull request). */
function reference(name: string, sign: string, url: (links: HostLinks, id: number) => string): TokenizerAndRendererExtension {
  return {
    name,
    level: "inline",
    start: wordStart(`\\${sign}\\d`),
    tokenizer(source) {
      const match = new RegExp(`^\\${sign}(\\d+)\\b`).exec(source);
      return match && hostLinks ? { type: name, raw: match[0], id: Number(match[1]) } : undefined;
    },
    renderer: (token) => `<a href="${escapeHtml(url(hostLinks!, token.id))}">${sign}${token.id}</a>`,
  };
}

// Azure DevOps keeps single line breaks in comments.
const marked = new Marked({ gfm: true, breaks: true, async: false });
marked.use({
  extensions: [
    emoji,
    reference("workItem", "#", (links, id) => links.workItem(id)),
    reference("pullRequestLink", "!", (links, id) => links.pullRequest(id)),
  ],
  renderer: {
    code({ text, lang }) {
      if (lang?.trim() !== "suggestion") return false;
      return `<div data-suggestion><div data-suggestion-title>Suggested change</div><pre><code>${escapeHtml(text)}</code></pre></div>`;
    },
  },
});

// Task list boxes are the only inputs kept, and they cannot be ticked.
DOMPurify.addHook("afterSanitizeAttributes", (node) => {
  if (node.nodeName !== "INPUT") return;
  if (node.getAttribute("type") === "checkbox") node.setAttribute("disabled", "");
  else node.remove();
});

export function renderMarkdown(text: string): string {
  const html = marked.parse(text) as string;
  return DOMPurify.sanitize(html, {
    FORBID_TAGS: ["img", "picture", "video", "audio", "style", "form", "button", "textarea", "select"],
    FORBID_ATTR: ["style", "class", "id"],
  });
}

/** The page about Markdown in Azure DevOps. */
export const MARKDOWN_GUIDE = "https://learn.microsoft.com/en-us/azure/devops/project/wiki/markdown-guidance?view=azure-devops";

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
