import { json } from "@codemirror/lang-json";
import { markdown } from "@codemirror/lang-markdown";
import { xml } from "@codemirror/lang-xml";
import type { Extension } from "@codemirror/state";
import { csharp } from "@replit/codemirror-lang-csharp";
import { fSharpLanguage } from "./fsharpLanguage";

const extensionOf = (path: string) => path.slice(path.lastIndexOf(".") + 1).toLowerCase();

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
