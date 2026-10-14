import { json } from "@codemirror/lang-json";
import { markdown } from "@codemirror/lang-markdown";
import { xml } from "@codemirror/lang-xml";
import { StreamLanguage } from "@codemirror/language";
import { fSharp } from "@codemirror/legacy-modes/mode/mllike";
import type { Extension } from "@codemirror/state";
import { csharp } from "@replit/codemirror-lang-csharp";

const extensionOf = (path: string) => path.slice(path.lastIndexOf(".") + 1).toLowerCase();

/** F# modules the stream parser calls built-ins; Rider colors them as types. */
const FSHARP_MODULES = new Set(["List", "Seq", "Map", "Set", "Option"]);

/**
 * The legacy F# parser, with its "builtin" words split the way Rider colors
 * them: true/false as keywords, modules as types, the rest stays function-like.
 * The language server's symbol kinds refine this once it has checked the file.
 */
const fSharpLanguage = StreamLanguage.define<unknown>({
  ...fSharp,
  token(stream, state) {
    const style = fSharp.token(stream, state);
    if (style !== "builtin") return style;
    const word = stream.current();
    if (word === "true" || word === "false") return "keyword";
    if (FSHARP_MODULES.has(word)) return "type";
    return style;
  },
});

const XML_LIKE = new Set(["xml", "csproj", "fsproj", "props", "targets", "xaml", "config", "resx", "nuspec", "slnx"]);

/** Syntax highlighting for a file, chosen by its extension. */
export function languageFor(path: string): Extension {
  const extension = extensionOf(path);
  if (extension === "fs" || extension === "fsx" || extension === "fsi") return fSharpLanguage;
  if (extension === "cs" || extension === "csx") return csharp();
  if (XML_LIKE.has(extension)) return xml();
  if (extension === "json") return json();
  if (extension === "md") return markdown();
  return [];
}

/** LSP language identifier, for the files a language server is started for. */
export function languageIdFor(path: string): "fsharp" | "csharp" | null {
  const extension = extensionOf(path);
  if (extension === "fs" || extension === "fsx" || extension === "fsi") return "fsharp";
  if (extension === "cs" || extension === "csx") return "csharp";
  return null;
}
