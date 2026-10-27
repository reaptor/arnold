// Writes public/third-party-notices.json: Arnold's own LICENSE and every package built into
// Arnold (Rust crates for the two release targets, and the npm packages bundled into the
// webview), with the license files they ship. MIT, Apache-2.0, BSD and the font's OFL all ask for their notices to go along with
// the app; Arnold shows this file under "Open source licenses" (src/components/LicensesDialog.tsx).
//
// Runs as part of `npm run build`, so every release carries the list for exactly what it was
// built from. The output is generated, not committed.
//
//   node scripts/third-party-notices.mjs

import { execFileSync } from "node:child_process";
import { existsSync, mkdirSync, readdirSync, readFileSync, statSync, writeFileSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const root = join(dirname(fileURLToPath(import.meta.url)), "..");
const output = join(root, "public", "third-party-notices.json");

// The targets release.yml builds; crates only other platforms need are left out.
const TARGETS = ["aarch64-apple-darwin", "x86_64-pc-windows-msvc"];

// Not packages, but taken from elsewhere all the same.
const EXTRAS = [
  {
    name: "Rider Islands Dark color scheme (colors)",
    version: "",
    license: "Apache-2.0",
    url: "https://github.com/JetBrains/rider-theme-pack",
  },
  {
    name: "IntelliJ Platform Islands theme (colors)",
    version: "",
    license: "Apache-2.0",
    url: "https://github.com/JetBrains/intellij-community",
  },
  {
    name: "IntelliJ Platform new UI icons (file explorer)",
    version: "",
    license: "Apache-2.0",
    url: "https://github.com/JetBrains/intellij-community/tree/master/platform/icons/src/expui",
  },
  {
    name: "F# support for Rider icons (file explorer)",
    version: "",
    license: "Apache-2.0",
    url: "https://github.com/JetBrains/fsharp-support",
  },
];

const LICENSE_FILE = /^(licen[cs]e|copying|notice|copyright)([-._].*)?$/i;

/** The license and notice files at the top of a package folder, one text per file. */
function licenseTexts(dir, extraFile) {
  const files = existsSync(dir)
    ? readdirSync(dir)
        .filter((name) => LICENSE_FILE.test(name) && statSync(join(dir, name)).isFile())
        .sort()
    : [];
  if (extraFile && !files.includes(extraFile) && existsSync(join(dir, extraFile))) files.push(extraFile);
  return files.map((name) => readFileSync(join(dir, name), "utf8").replace(/\r\n/g, "\n").trim());
}

function rustPackages() {
  const found = new Map();
  for (const target of TARGETS) {
    const metadata = JSON.parse(
      execFileSync("cargo", ["metadata", "--format-version", "1", "--locked", "--filter-platform", target], {
        cwd: root,
        encoding: "utf8",
        maxBuffer: 256 * 1024 * 1024,
      }),
    );
    const byId = new Map(metadata.packages.map((p) => [p.id, p]));
    const nodes = new Map(metadata.resolve.nodes.map((n) => [n.id, n]));
    // Walk normal dependencies only: build scripts and tests are not part of the app.
    const seen = new Set();
    const queue = [...metadata.workspace_members];
    while (queue.length) {
      const id = queue.pop();
      if (seen.has(id)) continue;
      seen.add(id);
      for (const dep of nodes.get(id)?.deps ?? []) {
        if (dep.dep_kinds.some((kind) => kind.kind === null)) queue.push(dep.pkg);
      }
    }
    for (const id of seen) {
      const p = byId.get(id);
      if (!p.source) continue; // Arnold's own crates
      const key = `${p.name}@${p.version}`;
      if (found.has(key)) continue;
      found.set(key, {
        name: p.name,
        version: p.version,
        license: p.license ?? (p.license_file ? "see license text" : "unknown"),
        url: p.repository ?? p.homepage ?? `https://crates.io/crates/${p.name}`,
        texts: licenseTexts(dirname(p.manifest_path), p.license_file),
      });
    }
  }
  return [...found.values()];
}

function npmPackages() {
  const found = new Map();
  const pkg = JSON.parse(readFileSync(join(root, "package.json"), "utf8"));
  // Only `dependencies` end up in the webview bundle; `devDependencies` are build tools.
  const queue = Object.keys(pkg.dependencies ?? {}).map((name) => ({ name, from: root }));
  while (queue.length) {
    const { name, from } = queue.pop();
    // Node's lookup: the nearest node_modules up from the package that asked for it.
    let location = null;
    for (let dir = from; dir.startsWith(root); dir = dirname(dir)) {
      const candidate = join(dir, "node_modules", name);
      if (existsSync(join(candidate, "package.json"))) {
        location = candidate;
        break;
      }
      if (dir === root) break;
    }
    if (!location) continue; // an optional dependency for another platform
    const manifest = JSON.parse(readFileSync(join(location, "package.json"), "utf8"));
    const key = `${manifest.name}@${manifest.version}`;
    if (found.has(key)) continue;
    const repository = typeof manifest.repository === "string" ? manifest.repository : manifest.repository?.url;
    found.set(key, {
      name: manifest.name,
      version: manifest.version,
      license: typeof manifest.license === "string" ? manifest.license : (manifest.license?.type ?? "unknown"),
      url: (repository ?? manifest.homepage ?? `https://www.npmjs.com/package/${manifest.name}`)
        .replace(/^git\+/, "")
        .replace(/^git:\/\//, "https://")
        .replace(/\.git$/, "")
        .replace(/^github:/, "https://github.com/"),
      texts: licenseTexts(location),
    });
    for (const dep of Object.keys(manifest.dependencies ?? {})) queue.push({ name: dep, from: location });
  }
  return [...found.values()];
}

const packages = [...rustPackages(), ...npmPackages()];

// Each license file is stored once and referred to by index: most Apache-2.0 texts are
// identical, and dual-licensed packages often differ only in their MIT copyright line.
const texts = [];
const textIndex = new Map();
const indexOf = (text) => {
  if (!textIndex.has(text)) textIndex.set(text, texts.push(text) - 1);
  return textIndex.get(text);
};

// Packages with the same license files share one entry.
const groups = new Map();
for (const { texts: own, ...entry } of packages) {
  const ids = own.map(indexOf);
  const key = ids.length ? ids.join(",") : `(no license file) ${entry.license}`;
  if (!groups.has(key)) groups.set(key, { license: entry.license, texts: ids, packages: [] });
  groups.get(key).packages.push(entry);
}
const apache = [...groups.values()].find((group) => group.license === "Apache-2.0" && group.texts.length);
for (const extra of EXTRAS) {
  if (apache && extra.license === "Apache-2.0") apache.packages.push(extra);
  else groups.set(extra.name, { license: extra.license, texts: [], packages: [extra] });
}

const notices = [...groups.values()]
  .map((group) => ({ ...group, packages: group.packages.sort((a, b) => a.name.localeCompare(b.name)) }))
  .sort((a, b) => a.packages[0].name.localeCompare(b.packages[0].name));

const arnold = { license: "MIT", text: readFileSync(join(root, "LICENSE"), "utf8").replace(/\r\n/g, "\n").trim() };

mkdirSync(dirname(output), { recursive: true });
writeFileSync(output, JSON.stringify({ arnold, packageCount: packages.length + EXTRAS.length, texts, notices }));
console.log(
  `third-party-notices.json: ${packages.length + EXTRAS.length} packages, ${notices.length} entries, ${texts.length} license texts, ` +
    `${Math.round(statSync(output).size / 1024)} kB`,
);
const missing = packages.filter((p) => !p.texts.length);
if (missing.length) {
  console.log(`  without a license file: ${missing.map((p) => `${p.name} (${p.license})`).join(", ")}`);
}
