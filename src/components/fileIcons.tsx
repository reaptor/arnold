// File-type icons for the explorer, after Rider's "new UI" set: 16×16, a
// tinted fill with a 1px stroke of the same hue, F# files as the F# logo.
// The tones are CSS variables (`.tone-*` in styles.css), so both themes work.
import type { JSX } from "solid-js";

type Tone = "gray" | "fsharp" | "blue" | "purple" | "green" | "orange" | "red" | "yellow";

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
  | "yaml"
  | "markdown"
  | "text"
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
  html: "markup",
  htm: "markup",
  astro: "markup",
  yml: "yaml",
  yaml: "yaml",
  toml: "yaml",
  md: "markdown",
  markdown: "markdown",
  txt: "text",
  log: "text",
  csv: "text",
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

/** The F# logo: a thick "<" and a smaller ">" beside it. */
const FSharpLogo = (props: { tone: Tone; transform?: string }) => (
  <g class={`tone-${props.tone}`} transform={props.transform}>
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

const page = "M4 1.5h5.5l3 3V14a.5.5 0 0 1-.5.5H4a.5.5 0 0 1-.5-.5V2a.5.5 0 0 1 .5-.5z";
const frame = "M3 1.5h10A1.5 1.5 0 0 1 14.5 3v10a1.5 1.5 0 0 1-1.5 1.5H3A1.5 1.5 0 0 1 1.5 13V3A1.5 1.5 0 0 1 3 1.5z";

const DRAWINGS: Record<Kind, () => JSX.Element> = {
  fsharp: () => <FSharpLogo tone="fsharp" />,
  fsharpSignature: () => <FSharpLogo tone="purple" />,
  fsharpScript: () => <FSharpLogo tone="green" />,
  fsharpProject: () => (
    <>
      <path class="tone-fsharp shape" d={frame} />
      <FSharpLogo tone="fsharp" transform="translate(3.2 3.2) scale(0.6)" />
    </>
  ),
  csharp: () => <CSharpGlyph />,
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
  json: () => (
    <path
      class="tone-orange line bold"
      d="M6 2.5h-.5A1.5 1.5 0 0 0 4 4v2.2c0 1-.7 1.8-1.5 1.8.8 0 1.5.8 1.5 1.8V12a1.5 1.5 0 0 0 1.5 1.5H6M10 2.5h.5A1.5 1.5 0 0 1 12 4v2.2c0 1 .7 1.8 1.5 1.8-.8 0-1.5.8-1.5 1.8V12a1.5 1.5 0 0 1-1.5 1.5H10"
    />
  ),
  markup: () => <path class="tone-orange line bold" d="M5 4.5 1.5 8 5 11.5M11 4.5 14.5 8 11 11.5M9.25 3.5l-2.5 9" />,
  yaml: () => (
    <g class="tone-red">
      <path class="line bold" d="M5 4h8.5M7 8h6.5M7 12h6.5" />
      <circle class="solid" cx="2.5" cy="4" r="1" />
      <circle class="solid" cx="4.5" cy="8" r="1" />
      <circle class="solid" cx="4.5" cy="12" r="1" />
    </g>
  ),
  markdown: () => (
    <g class="tone-blue">
      <rect class="shape" x="1.5" y="3.5" width="13" height="9" rx="1.5" />
      <path class="line" d="M4 10.5v-5l2 2.5 2-2.5v5M11.5 5.5v5M10 9l1.5 1.5L13 9" />
    </g>
  ),
  text: () => (
    <g class="tone-gray">
      <path class="shape" d={page} />
      <path class="line" d="M6 7.5h4M6 10h4M6 12.5h2.5" />
    </g>
  ),
  git: () => (
    <g class="tone-orange">
      <path class="shape" d="M8 1.2 14.8 8 8 14.8 1.2 8z" />
      <path class="line" d="M6.25 5.25v5.5M6.25 5.25l3.25 3.25" />
      <circle class="solid" cx="6.25" cy="10.75" r="1.1" />
      <circle class="solid" cx="9.75" cy="8.75" r="1.1" />
    </g>
  ),
  image: () => (
    <g class="tone-green">
      <rect class="shape" x="1.5" y="2.5" width="13" height="11" rx="1.5" />
      <path class="line" d="M1.5 11.5 5.5 7.5l3.5 3.5 2-2 3.5 3.5" />
      <circle class="line" cx="10.5" cy="5.75" r="1.25" />
    </g>
  ),
  script: () => (
    <g class="tone-gray">
      <rect class="shape" x="1.5" y="2.5" width="13" height="11" rx="1.5" />
      <path class="line" d="M4.5 6l2 2-2 2M8.5 10.5h3" />
    </g>
  ),
  javascript: () => (
    <g class="tone-yellow">
      <rect class="shape" x="1.5" y="1.5" width="13" height="13" rx="1.5" />
      <text class="solid label" x="8" y="11.5">
        JS
      </text>
    </g>
  ),
  typescript: () => (
    <g class="tone-blue">
      <rect class="shape" x="1.5" y="1.5" width="13" height="13" rx="1.5" />
      <text class="solid label" x="8" y="11.5">
        TS
      </text>
    </g>
  ),
  css: () => (
    <g class="tone-blue">
      <rect class="shape" x="1.5" y="1.5" width="13" height="13" rx="1.5" />
      <path class="line" d="M4.5 6.5h7M4.5 9.5h7M7 4.5l-1 7M10 4.5l-1 7" />
    </g>
  ),
  sql: () => (
    <g class="tone-yellow">
      <path class="shape" d="M2.5 3.5v9c0 1.1 2.5 2 5.5 2s5.5-.9 5.5-2v-9" />
      <ellipse class="shape" cx="8" cy="3.5" rx="5.5" ry="2" />
      <path class="line" d="M2.5 8c0 1.1 2.5 2 5.5 2s5.5-.9 5.5-2" />
    </g>
  ),
  file: () => (
    <g class="tone-gray">
      <path class="shape" d={page} />
      <path class="line" d="M9.5 1.5v3h3" />
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

/** Icon for a file, picked by its name. */
export function FileTypeIcon(props: { name: string }) {
  return <FileTypeSvg>{DRAWINGS[kindOf(props.name)]()}</FileTypeSvg>;
}

/**
 * Icon for a folder. A folder holding a project file is shown as that project,
 * as Rider's solution explorer does.
 */
export function FolderIcon(props: { project?: "fsharpProject" | "csharpProject" }) {
  return (
    <FileTypeSvg>
      {props.project ? (
        DRAWINGS[props.project]()
      ) : (
        <path
          class="tone-gray shape"
          d="M1.5 4A1.5 1.5 0 0 1 3 2.5h3l1.5 1.5H13A1.5 1.5 0 0 1 14.5 5.5V12A1.5 1.5 0 0 1 13 13.5H3A1.5 1.5 0 0 1 1.5 12z"
        />
      )}
    </FileTypeSvg>
  );
}
