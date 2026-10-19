import { json } from "@codemirror/lang-json";
import { markdown } from "@codemirror/lang-markdown";
import { MSSQL, sql } from "@codemirror/lang-sql";
import { xml } from "@codemirror/lang-xml";
import { LanguageDescription, type LanguageSupport } from "@codemirror/language";
import { languages } from "@codemirror/language-data";
import { Compartment, type Extension } from "@codemirror/state";
import { ViewPlugin } from "@codemirror/view";
import { csharp } from "@replit/codemirror-lang-csharp";
import { fSharpLanguage } from "./fsharpLanguage";
import { sqlInStrings } from "./sqlInStrings";

const extensionOf = (path: string) => path.slice(path.lastIndexOf(".") + 1).toLowerCase();
const nameOf = (path: string) => path.slice(path.lastIndexOf("/") + 1);

const XML_LIKE = new Set(["xml", "csproj", "fsproj", "props", "targets", "xaml", "config", "resx", "nuspec", "slnx"]);

/** Files `@codemirror/language-data` does not know by name, as the language that suits them. */
const ALIASES: Record<string, string> = {
  zsh: "Shell",
  jsonc: "JSON",
  json5: "JSON",
  mdx: "Markdown",
  cshtml: "HTML",
  razor: "HTML",
  svelte: "HTML",
  astro: "HTML",
  env: "Properties files",
  editorconfig: "Properties files",
  gitattributes: "Properties files",
  cfg: "Properties files",
};

/** Syntax highlighting for a file, chosen by its name. */
export function languageFor(path: string): Extension {
  const own = ownLanguage(path);
  const description = own ? null : languageDescription(path);
  const language = own ?? (description ? loaded(description) : []);
  // SQL files are SQL already.
  return extensionOf(path) === "sql" ? language : [language, sqlInStrings];
}

/** The languages Arnold picks itself, rather than `@codemirror/language-data`'s for the name. */
function ownLanguage(path: string): Extension | null {
  const extension = extensionOf(path);
  if (extension === "fs" || extension === "fsx" || extension === "fsi") return fSharpLanguage;
  if (extension === "cs" || extension === "csx") return csharp();
  if (XML_LIKE.has(extension)) return xml();
  if (extension === "json") return json();
  // Code blocks in their own language, loaded when one shows.
  if (extension === "md" || extension === "markdown") return markdown({ codeLanguages: languages });
  // T-SQL: SQL Server keywords, types and functions, @variables, [quoted] names.
  if (extension === "sql") return sql({ dialect: MSSQL });
  return null;
}

/**
 * While `path`'s language is still to be loaded: when it has been. Editors
 * pick it up by themselves, except for the deletions of a unified diff,
 * which are drawn once and need the editor rebuilt.
 */
export function languageLoading(path: string): Promise<unknown> | null {
  if (ownLanguage(path)) return null;
  const description = languageDescription(path);
  return description && !description.support ? description.load() : null;
}

/** The language of the many `@codemirror/language-data` knows that `path` is in. */
export function languageDescription(path: string): LanguageDescription | null {
  const name = nameOf(path);
  const alias = ALIASES[extensionOf(name)] ?? (/^(Dockerfile|Containerfile)\b/i.test(name) ? "Dockerfile" : undefined);
  if (alias) return LanguageDescription.matchLanguageName(languages, alias, false);
  return LanguageDescription.matchFilename(languages, name);
}

/**
 * `description`'s language. Each is a separate chunk, loaded the first time
 * a file needs it: until then the editor shows the file plain, and gets the
 * highlighting as soon as it has arrived.
 */
function loaded(description: LanguageDescription): Extension {
  if (description.support) return description.support;
  const slot = new Compartment();
  return [
    slot.of([]),
    ViewPlugin.define((view) => {
      let destroyed = false;
      description.load().then(
        (support: LanguageSupport) => {
          if (!destroyed) view.dispatch({ effects: slot.reconfigure(support) });
        },
        () => {
          // Stays plain if the chunk cannot be loaded.
        },
      );
      return {
        destroy() {
          destroyed = true;
        },
      };
    }),
  ];
}

/** LSP language identifier, for the files a language server is started for. */
export function languageIdFor(path: string): "fsharp" | "csharp" | null {
  const extension = extensionOf(path);
  if (extension === "fs" || extension === "fsx" || extension === "fsi") return "fsharp";
  if (extension === "cs" || extension === "csx") return "csharp";
  return null;
}
