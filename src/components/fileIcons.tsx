// File-type icons for the explorer: Rider's own where JetBrains has published
// them, 16×16 in the new UI style.
import { type JSX, Show } from "solid-js";
import { isDark } from "../state/theme";

type Kind =
  | "fsharp"
  | "fsharpSignature"
  | "fsharpScript"
  | "fsharpProject"
  | "csharp"
  | "csharpProject"
  | "solution"
  | "json"
  | "markup"
  | "html"
  | "yaml"
  | "toml"
  | "markdown"
  | "text"
  | "csv"
  | "git"
  | "image"
  | "script"
  | "javascript"
  | "typescript"
  | "css"
  | "sql"
  | "file";

const BY_EXTENSION: Record<string, Kind> = {
  fs: "fsharp",
  fsi: "fsharpSignature",
  fsx: "fsharpScript",
  fsscript: "fsharpScript",
  fsproj: "fsharpProject",
  cs: "csharp",
  csx: "csharp",
  csproj: "csharpProject",
  sln: "solution",
  slnx: "solution",
  slnf: "solution",
  json: "json",
  jsonc: "json",
  xml: "markup",
  props: "markup",
  targets: "markup",
  config: "markup",
  resx: "markup",
  xaml: "markup",
  axaml: "markup",
  nuspec: "markup",
  html: "html",
  htm: "html",
  astro: "html",
  yml: "yaml",
  yaml: "yaml",
  toml: "toml",
  md: "markdown",
  markdown: "markdown",
  txt: "text",
  log: "text",
  csv: "csv",
  png: "image",
  jpg: "image",
  jpeg: "image",
  gif: "image",
  webp: "image",
  ico: "image",
  icns: "image",
  svg: "image",
  sh: "script",
  bash: "script",
  zsh: "script",
  ps1: "script",
  cmd: "script",
  bat: "script",
  js: "javascript",
  mjs: "javascript",
  cjs: "javascript",
  jsx: "javascript",
  ts: "typescript",
  mts: "typescript",
  cts: "typescript",
  tsx: "typescript",
  css: "css",
  scss: "css",
  sql: "sql",
};

const BY_NAME: Record<string, Kind> = {
  ".gitignore": "git",
  ".gitattributes": "git",
  ".gitmodules": "git",
  ".gitkeep": "git",
  license: "text",
  "license.md": "markdown",
};

function kindOf(name: string): Kind {
  const lower = name.toLowerCase();
  const named = BY_NAME[lower];
  if (named) return named;
  const dot = lower.lastIndexOf(".");
  return (dot > 0 && BY_EXTENSION[lower.slice(dot + 1)]) || "file";
}

// JetBrains' own icons, the new UI set: the IntelliJ Platform's
// (intellij-community) and the F# plugin's (fsharp-support), both Apache 2.0.
// Each has a `_dark` twin for the dark theme, but for .gitignore's.
const SVGS = import.meta.glob<string>("../assets/fileIcons/*.svg", { eager: true, query: "?url", import: "default" });
const svg = (name: string) => SVGS[`../assets/fileIcons/${name}.svg`];

const JETBRAINS: Partial<Record<Kind | "folder", string>> = {
  folder: "folder",
  fsharp: "Fsharp",
  fsharpSignature: "FsharpSignature",
  fsharpScript: "FsharpScript",
  csharp: "Csharp",
  json: "json",
  markup: "xml",
  html: "html",
  yaml: "yaml",
  toml: "toml",
  markdown: "markdown",
  text: "text",
  csv: "csv",
  git: "gitignore",
  image: "image",
  script: "shell",
  javascript: "javaScript",
  typescript: "typeScript",
  css: "css",
  sql: "sql",
  file: "anyType",
};

function JetBrainsIcon(props: { name: string }) {
  return (
    <img
      class="file-icon"
      src={(isDark() && svg(`${props.name}_dark`)) || svg(props.name)}
      alt=""
      aria-hidden="true"
      draggable={false}
    />
  );
}

// No open-source JetBrains icon for these (Rider's own are not open source),
// so they stay drawn: a project's frame with its language inside. The tones
// are CSS variables (`.tone-*` in styles.css), so both themes work.

/** The F# logo: a thick "<" and a smaller ">" beside it. */
const FSharpLogo = (props: { transform?: string }) => (
  <g class="tone-fsharp" transform={props.transform}>
    <path class="solid" d="M7.5 1.5 1 8l6.5 6.5V11L4.5 8l3-3z" />
    <path class="solid faint" d="M9 3.5 13.5 8 9 12.5V9.75L10.75 8 9 6.25z" />
  </g>
);

/** "C#": an open C with a small # beside it. */
const CSharpGlyph = (props: { transform?: string }) => (
  <g class="tone-purple" transform={props.transform}>
    <path class="line bold" d="M7.5 4.6A3.6 3.6 0 1 0 7.5 11.4" />
    <path class="line" d="M10 6.5h4.5M10 9.5h4.5M11.25 4.5v7M13.25 4.5v7" />
  </g>
);

const frame = "M3 1.5h10A1.5 1.5 0 0 1 14.5 3v10a1.5 1.5 0 0 1-1.5 1.5H3A1.5 1.5 0 0 1 1.5 13V3A1.5 1.5 0 0 1 3 1.5z";

const DRAWINGS: Partial<Record<Kind, () => JSX.Element>> = {
  fsharpProject: () => (
    <>
      <path class="tone-fsharp shape" d={frame} />
      <FSharpLogo transform="translate(3.2 3.2) scale(0.6)" />
    </>
  ),
  csharpProject: () => (
    <>
      <path class="tone-purple shape" d={frame} />
      <CSharpGlyph transform="translate(3.2 3.2) scale(0.6)" />
    </>
  ),
  solution: () => (
    <g class="tone-purple">
      <path class="shape" d={frame} />
      <path class="line" d="M8 1.5v13M1.5 8h13" />
    </g>
  ),
};

export const PROJECT_EXTENSIONS: Readonly<Record<string, "fsharpProject" | "csharpProject">> = {
  fsproj: "fsharpProject",
  csproj: "csharpProject",
};

function FileTypeSvg(props: { children: JSX.Element }) {
  return (
    <svg class="file-icon" viewBox="0 0 16 16" aria-hidden="true">
      {props.children}
    </svg>
  );
}

function KindIcon(props: { kind: Kind }) {
  return (
    <Show when={DRAWINGS[props.kind]} fallback={<JetBrainsIcon name={JETBRAINS[props.kind] ?? "anyType"} />}>
      {(drawing) => <FileTypeSvg>{drawing()()}</FileTypeSvg>}
    </Show>
  );
}

/** Icon for a file, picked by its name. */
export function FileTypeIcon(props: { name: string }) {
  return <KindIcon kind={kindOf(props.name)} />;
}

/**
 * Icon for a folder. A folder holding a project file is shown as that project,
 * as Rider's solution explorer does.
 */
export function FolderIcon(props: { project?: "fsharpProject" | "csharpProject" }) {
  return (
    <Show when={props.project} fallback={<JetBrainsIcon name="folder" />}>
      {(project) => <KindIcon kind={project()} />}
    </Show>
  );
}
