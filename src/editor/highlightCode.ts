// Token colors for code blocks outside editors (answers in the AI panel),
// with the editors' languages and theme.

import { type HighlightStyle, LanguageDescription, type Language } from "@codemirror/language";
import { languages } from "@codemirror/language-data";
import { highlightTree } from "@lezer/highlight";
import { csharp } from "@replit/codemirror-lang-csharp";
import { StyleModule } from "style-mod";
import { fSharpLanguage } from "./fsharpLanguage";
import { codeHighlight } from "./riderIslandsTheme";

/** Names a code block may be tagged with for the languages Arnold picks itself. */
const OWN: Record<string, () => Language> = {
  fsharp: () => fSharpLanguage,
  "f#": () => fSharpLanguage,
  fs: () => fSharpLanguage,
  fsx: () => fSharpLanguage,
  csharp: () => csharp().language,
  "c#": () => csharp().language,
  cs: () => csharp().language,
};

const mounted = new Set<HighlightStyle>();

/** The language a code block tagged `name` (```ts) is in, loaded if need be. */
async function languageNamed(name: string): Promise<Language | null> {
  const key = name.trim().toLowerCase();
  if (!key) return null;
  const own = OWN[key];
  if (own) return own();
  const description = LanguageDescription.matchLanguageName(languages, key, true);
  if (!description) return null;
  return (description.support ?? (await description.load())).language;
}

/** Colors the text of `code` (a `<code>` of a code block) as language `name`; leaves it as it is when unknown. */
export async function highlightCode(code: HTMLElement, name: string) {
  // Read before any await: an effect calling this runs again when the theme changes.
  const highlight = codeHighlight();
  const text = code.textContent ?? "";
  const language = await languageNamed(name).catch(() => null);
  if (!language || code.textContent !== text) return;
  if (!mounted.has(highlight) && highlight.module) {
    StyleModule.mount(document, highlight.module);
    mounted.add(highlight);
  }
  const tree = language.parser.parse(text);
  const fragment = document.createDocumentFragment();
  let at = 0;
  const plain = (to: number) => {
    if (to > at) fragment.append(text.slice(at, to));
    at = Math.max(at, to);
  };
  highlightTree(tree, highlight, (from, to, classes) => {
    plain(from);
    const span = document.createElement("span");
    span.className = classes;
    span.textContent = text.slice(from, to);
    fragment.append(span);
    at = to;
  });
  plain(text.length);
  code.replaceChildren(fragment);
}
