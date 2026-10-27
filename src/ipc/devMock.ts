// Stand-in backend for working on the UI in a plain browser (`npm run dev`
// without Tauri). It serves a small in-memory repository; nothing touches disk.
// Never loaded in the real app.

import type { Channel } from "@tauri-apps/api/core";
import { emit } from "@tauri-apps/api/event";
import { mockIPC, mockWindows } from "@tauri-apps/api/mocks";
import { isFavorite, toggleFavorite } from "../state/favorites";
import type {
  Branch,
  Fetched,
  PullOutcome,
  ChangedFile,
  Commit,
  CommitRange,
  Comparison,
  CompletionPolicies,
  FilePair,
  LspEvent,
  NewPullRequest,
  NewPullRequestPlan,
  NewThreadPlace,
  PrChange,
  PrRemote,
  Placement,
  PrThread,
  PullRequest,
  RepoInfo,
  Reviewer,
  SetupEvent,
  BuildEvent,
  Vote,
  WorkingSide,
  WorkingStatus,
  AiEvent,
  AskRequest,
  Patch,
  ContextFile,
  PatchOf,
} from "./commands";

interface FakeFile {
  base: string | null;
  working: string | null;
}

const ORDERS_BASE = `module Shop.Orders

open System
open System.IO

type OrderLine =
    { Sku: string
      Quantity: int
      UnitPrice: decimal }

type Order = { Id: int; Lines: OrderLine list }

/// Net total of all lines, before VAT.
let netTotal (order: Order) : decimal =
    order.Lines
    |> List.sumBy (fun line -> decimal line.Quantity * line.UnitPrice)

/// Total including VAT.
let total (order: Order) : decimal =
    order |> netTotal |> Pricing.withVat |> Pricing.roundMoney
`;

const ORDERS_WORKING =
  ORDERS_BASE.replace("/// Total including VAT.", "/// Total including VAT, rounded to two decimals.") +
  `
/// Most lines one order may have.
let maxLinesPerOrder: int = "ten"

/// Total for a returning customer, formatted for display.
let loyaltyTotalText (order: Order) : string =
    order
    |> total
    |> Pricing.applyLoyaltyDiscount
    |> Pricing.roundMoney
    |> Currency.format

/// Lines past which an order needs a manual check.
let reviewLimit = (List.length [ 1; 2; 3 ] * 4)
`;

const PRICING_BASE = `module Shop.Pricing

let vatRate = 0.25m

/// Adds VAT to a net amount.
let withVat (net: decimal) : decimal =
    net * (1m + vatRate)

/// Rounds to two decimals.
let roundMoney (amount: decimal) : decimal =
    System.Math.Round(amount, 2)
`;

const PRICING_WORKING =
  PRICING_BASE.replace("let vatRate = 0.25m", "let vatRate = 0.25m // standard rate") +
  `
let loyaltyRate = 0.10m // UNCOMMITTED edit

/// Discount for returning customers.
let applyLoyaltyDiscount (amount: decimal) : decimal =
    amount * (1m - loyaltyRate)
`;

const CSHARP_BASE = `using System;

namespace Shop.Api;

public sealed class OrderController
{
    private readonly IOrderService _orders;

    public OrderController(IOrderService orders) => _orders = orders;

    public string Describe(int id)
    {
        var order = _orders.Find(id);
        return $"Order {order.Id}";
    }
}
`;

const CSHARP_WORKING = CSHARP_BASE.replace(
  '        return $"Order {order.Id}";',
  '        if (order is null) throw new ArgumentException("Unknown order", nameof(id));\n        return $"Order {order.Id}: {order.Lines.Count} lines";',
);

const SQL_BASE = `-- Open orders per customer
CREATE PROCEDURE [dbo].[OpenOrders] @CustomerId INT
AS
BEGIN
    SET NOCOUNT ON;
    SELECT o.[Id], o.[Total], GETDATE() AS [Now]
    FROM [dbo].[Orders] o WITH (NOLOCK)
    WHERE o.CustomerId = @CustomerId AND o.Status = N'Open';
END
GO
`;

const SQL_WORKING = SQL_BASE.replace(
  "    SELECT o.[Id], o.[Total], GETDATE() AS [Now]",
  "    DECLARE @Since DATETIME2 = DATEADD(DAY, -30, SYSUTCDATETIME());\n    SELECT TOP (100) o.[Id], o.[Total], ISNULL(o.[Note], '') AS [Note]",
).replace("N'Open';", "N'Open' AND o.Created >= @Since;");

const QUERIES_BASE = `module Shop.Data.OrderQueries

/// Open orders of one customer.
let openOrders = "SELECT o.Id, o.Total FROM dbo.Orders o WHERE o.CustomerId = @customerId"
`;

const QUERIES_WORKING = `module Shop.Data.OrderQueries

/// Open orders of one customer, newest first.
let openOrders = """
    -- open orders only
    SELECT TOP (100) o.[Id], o.[Total], ISNULL(o.[Note], '') AS [Note]
    FROM [dbo].[Orders] o WITH (NOLOCK)
    WHERE o.CustomerId = @customerId AND o.Status = N'Open'
    ORDER BY o.Created DESC
    """

let closeOrder (id: int) =
    $"UPDATE dbo.Orders SET Status = 'Closed', Closed = SYSUTCDATETIME() WHERE Id = {id}"

let title = "Select an order from the list"
`;

const CSHARP_QUERIES = `namespace Shop.Api;

public static class OrderSql
{
    public const string Count = "select count(*) from dbo.Orders where Status = 'Open'";

    public static string ByCustomer(int customerId) =>
        $@"SELECT o.Id, o.Total
           FROM dbo.Orders o
           WHERE o.CustomerId = {customerId}";
}
`;

const files = new Map<string, FakeFile>([
  ["Orders.fs", { base: ORDERS_BASE, working: ORDERS_WORKING }],
  ["Pricing.fs", { base: PRICING_BASE, working: PRICING_WORKING }],
  ["src/Api/OrderController.cs", { base: CSHARP_BASE, working: CSHARP_WORKING }],
  ["Audit.fs", { base: null, working: 'module Shop.Audit\n\nlet log (message: string) : unit =\n    printfn "[audit] %s" message\n' }],
  ["db/Orders.sql", { base: SQL_BASE, working: SQL_WORKING }],
  ["src/Data/OrderQueries.fs", { base: QUERIES_BASE, working: QUERIES_WORKING }],
  ["src/Api/OrderSql.cs", { base: null, working: CSHARP_QUERIES }],
  ["docs/Legacy.md", { base: "# Legacy notes\n\nRemoved by the agent.\n", working: null }],
]);

// `?files=40` adds that many generated files, to try the all-files view on a large diff.
const extraFiles = Number(new URLSearchParams(location.search).get("files")) || 0;
for (let index = 1; index <= extraFiles; index++) {
  const name = `Generated${String(index).padStart(3, "0")}`;
  const base = Array.from({ length: 40 + index * 7 }, (_, line) => `let value${line} = ${line}`).join("\n") + "\n";
  const working = base
    .split("\n")
    .map((line, number) => (number % 13 === 5 ? `${line} // changed in ${name}` : line))
    .join("\n");
  files.set(`src/Generated/${name}.fs`, { base, working });
}

// A large file with a few small edits far apart: each should show as its own one-line change.
{
  let seed = 42;
  const rand = (n: number) => ((seed = (seed * 16807) % 2147483647), seed % n);
  const word = () => ["value", "count", "items", "result", "name", "index"][rand(6)] + rand(100);
  const lines = Array.from({ length: 3000 }, () =>
    rand(4) === 0 ? "" : `${"    ".repeat(rand(3))}let ${word()} = ${word()} + ${rand(1000)}`,
  );
  const base = lines.join("\n") + "\n";
  for (let at of [40, 700, 1400, 2100, 2900]) {
    while (!lines[at]) at++;
    lines[at] = lines[at].replace(/let \w+/, "let renamed");
  }
  files.set("src/Large/Inventory.fs", { base, working: lines.join("\n") + "\n" });
}

// Lines 101 to 500 moved into a nested module (indented, whitespace only) with one real edit: hiding
// whitespace changes leaves the module line and the edit.
{
  const base = Array.from({ length: 600 }, (_, line) => `let stock${line} = lookup "item-${line}" |> Option.defaultValue 0`);
  const working = base.map((line, index) => (index >= 100 && index < 500 ? `    ${line}` : line));
  working[300] = working[300].replace("defaultValue 0", "defaultValue -1");
  working.splice(100, 0, "module Reserved =");
  files.set("src/Large/Stock.fs", { base: base.join("\n") + "\n", working: working.join("\n") + "\n" });
}

const ROOT = "/src/shop-api";
const lineCount = (text: string | null) => (text ? text.split("\n").length - 1 : 0);

// `?lsp` starts a stand-in language server that answers go-to-definition,
// with the `let`/`type` declaring the symbol in any F# file (e.g. Pricing.fs
// for `Pricing.roundMoney` in Orders.fs), else the first whole-word
// occurrence of the symbol in the same file,
// hover, with a `let`'s declaration line, and completion (see `fakeCompletion`); it reports every `open` as unused
// and a string bound to an `int` as a type error, the way FsAutoComplete does.
const fakeServer = new URLSearchParams(location.search).has("lsp");

/** The open documents of the stand-in server, by URI. */
const serverDocs = new Map<string, string>();
/** The version of each document the stand-in server has. */
const serverVersions = new Map<string, number>();

let serverChannel: Channel<string> | null = null;

// The preparation of the F# workspace is simulated with pauses; `?setupfail`
// makes the build fail, `?setupslow` makes every step take 4 seconds.
// Every repository is trusted to run its build code, unless `?untrusted`
// (with `?buildfiles`, the branch also changes build files).
const searchParams = new URLSearchParams(location.search);
const pause = (ms: number) => new Promise((resolve) => setTimeout(resolve, searchParams.has("setupslow") ? 4000 : ms));
let preparation = 0;

// Projects the stand-in server "loads"; `?projectfail` makes the second fail
// the way FsAutoComplete reports a missing package.
const FAKE_PROJECTS = ["src/Core/Core.fsproj", "src/Api/Api.fsproj", "tests/Tests.fsproj"].map(
  (project) => `/src/shop-api/.arnold/worktrees/feature-agent-work-5f3a9c21/${project}`,
);

async function fakeProjectLoading(events: Channel<LspEvent>) {
  await pause(400);
  // `?workspacefail`: FsAutoComplete gives up on the workspace, as it does
  // over a project reference to a folder that does not exist.
  if (searchParams.has("workspacefail")) {
    const entry = "[11:27:31.042 ERR] [FsAutoComplete.Lsp.AdaptiveState] Reloading projects failed";
    const exception = "System.ArgumentException: The directory name '/src/shop-api/.arnold/worktrees/pr-1291/.elmish-land/Base' does not exist. (Parameter 'path')";
    events.onmessage({ kind: "workspaceFailed", message: exception.slice(exception.indexOf(": ") + 2), details: `${entry}\n${exception}` });
    return;
  }
  for (const project of FAKE_PROJECTS) events.onmessage({ kind: "projectLoading", project });
  events.onmessage({ kind: "status", text: `Loading ${FAKE_PROJECTS.length} Projects` });
  for (const [index, project] of FAKE_PROJECTS.entries()) {
    events.onmessage({ kind: "projectLoading", project });
    await pause(900);
    if (index === 1 && searchParams.has("projectfail")) {
      events.onmessage({
        kind: "projectFailed",
        project,
        message: "Package Giraffe, version 7.0.2 was not found. It might have been deleted since NuGet restore.",
        details: [
          "Build started.",
          "",
          'Project "Api.fsproj" (ResolveAssemblyReferencesDesignTime;CoreCompile target(s)):',
          'Task "ResolvePackageAssets"',
          "Package Giraffe, version 7.0.2 was not found. It might have been deleted since NuGet restore.",
          'Done executing task "ResolvePackageAssets" -- FAILED.',
          "Build FAILED.",
        ].join("\n"),
      });
    } else {
      events.onmessage({ kind: "projectLoaded", project });
    }
  }
  events.onmessage({ kind: "workspaceLoaded" });
  events.onmessage({ kind: "status", text: null });
}

async function fakePrepare(onEvent: Channel<SetupEvent>, root: string, inPlace: boolean, trusted: boolean) {
  const run = ++preparation;
  if (!trusted && searchParams.has("untrusted")) return { path: null, untrusted: true };
  const send = async (event: SetupEvent, ms = 0) => {
    if (ms) await pause(ms);
    if (run !== preparation) throw "cancelled";
    onEvent.onmessage(event);
  };
  const path = inPlace ? root : `${root}/.arnold/worktrees/feature-agent-work-5f3a9c21`;
  if (!inPlace) await send({ kind: "step", title: "Copying 7d3e9a1f to .arnold/worktrees/feature-agent-work-5f3a9c21" });
  await send({ kind: "skipped", title: "Restore packages", reason: "already restored for these project files" }, 600);
  await send({ kind: "step", title: "Building src/Api/Api.csproj (dotnet build): F# code uses its compiled assembly" });
  await send({ kind: "output", line: "  Determining projects to restore..." }, 500);
  if (searchParams.has("setupfail")) {
    await send({ kind: "output", line: "src/Api/Startup.cs(12,5): error CS1002: ; expected [src/Api/Api.csproj]" }, 800);
    await send({ kind: "warning", text: "dotnet build failed (exit status: 1)" });
  } else {
    await send({ kind: "output", line: `  Api -> ${path}/src/Api/bin/Debug/net10.0/Api.dll` }, 800);
  }
  return { path, untrusted: false };
}

// A build of the solution (Ctrl+Shift+B) takes a few seconds and reports two
// errors and two warnings; `?buildok` makes it succeed without any.
let building = 0;

async function fakeBuild(onEvent: Channel<BuildEvent>, root: string) {
  const run = ++building;
  const send = async (event: BuildEvent, ms = 0) => {
    if (ms) await new Promise((resolve) => setTimeout(resolve, ms));
    if (run !== building) throw "cancelled";
    onEvent.onmessage(event);
  };
  const ok = searchParams.has("buildok");
  await send({ kind: "started", target: "Shop.sln" });
  await send({ kind: "output", line: "  Determining projects to restore..." }, 300);
  await send({ kind: "output", line: "  All projects are up-to-date for restore." }, 500);
  await send({ kind: "output", line: `${root}/tests/Tests.fsproj : warning NU1603: Expecto 10.2.1 depends on FsCheck (>= 2.16.5) but FsCheck 2.16.5 was not found. FsCheck 2.16.6 was resolved instead. [${root}/Shop.sln]` }, 300);
  if (!ok) {
    await send({ kind: "output", line: `${root}/src/Api/OrderController.cs(15,27): warning CS8602: Dereference of a possibly null reference. [${root}/src/Api/Api.csproj]` }, 700);
    await send({ kind: "output", line: `${root}/Orders.fs(23,29): error FS0001: This expression was expected to have type    'int'    but here has type    'string' [${root}/Shop.fsproj]` }, 900);
    await send({ kind: "output", line: `${root}/Orders.fs(31,8): error FS0039: The value, namespace, type or module 'Currency' is not defined. [${root}/Shop.fsproj]` });
  }
  await send({ kind: "output", line: `  Api -> ${root}/src/Api/bin/Debug/net10.0/Api.dll` }, 600);
  return { succeeded: ok };
}

function publishUnusedOpens(uri: string, text: string) {
  const diagnostics = text.split("\n").flatMap((line, index) => {
    const name = /^\s*open\s+(\S+)/.exec(line);
    const mistyped = /^\s*let\s+\w+\s*:\s*int\s*=\s*("[^"]*")/.exec(line);
    if (mistyped) {
      const start = line.indexOf(mistyped[1]);
      const range = { start: { line: index, character: start }, end: { line: index, character: start + mistyped[1].length } };
      const message = "This expression was expected to have type\n    'int'    \nbut here has type\n    'string'    ";
      return [{ range, severity: 1, code: "FS0001", source: "F# Compiler", message }];
    }
    const parens = /=\s*(\([^()]*\))\s*$/.exec(line);
    if (parens) {
      const start = line.lastIndexOf(parens[1]);
      const range = { start: { line: index, character: start }, end: { line: index, character: start + parens[1].length } };
      return [{ range, severity: 4, code: "FSAC0004", source: "FSAC", message: "Parentheses can be removed.", tags: [1] }];
    }
    if (!name) return [];
    const start = line.indexOf(name[1], name[0].length - name[1].length);
    const range = { start: { line: index, character: start }, end: { line: index, character: start + name[1].length } };
    return [{ range, severity: 4, code: "FSAC0001", source: "FSAC", message: "Unused open statement", tags: [1] }];
  });
  const message = { jsonrpc: "2.0", method: "textDocument/publishDiagnostics", params: { uri, diagnostics } };
  setTimeout(() => serverChannel?.onmessage(JSON.stringify(message)), 300);
}

function fakeServerAnswer(raw: string): unknown {
  const message = JSON.parse(raw) as { id?: number; method: string; params?: any };
  const params = message.params;
  if (message.method === "textDocument/didOpen") {
    serverDocs.set(params.textDocument.uri, params.textDocument.text);
    serverVersions.set(params.textDocument.uri, params.textDocument.version);
    publishUnusedOpens(params.textDocument.uri, params.textDocument.text);
  }
  if (message.method === "textDocument/didChange") {
    serverDocs.set(params.textDocument.uri, params.contentChanges.at(-1).text);
    serverVersions.set(params.textDocument.uri, params.textDocument.version);
    publishUnusedOpens(params.textDocument.uri, params.contentChanges.at(-1).text);
  }
  if (message.id === undefined) return undefined;
  if (message.method === "initialize") {
    return {
      capabilities: {
        textDocumentSync: 1,
        definitionProvider: true,
        hoverProvider: true,
        completionProvider: { triggerCharacters: [".", "'", "{"], resolveProvider: true },
        codeActionProvider: true,
        renameProvider: { prepareProvider: true },
      },
    };
  }
  if (message.method === "textDocument/codeAction") return fakeCodeActions(params);
  if (message.method === "textDocument/prepareRename") return fakePrepareRename(params);
  if (message.method === "textDocument/rename") return fakeRename(params);
  if (message.method === "textDocument/completion") return fakeCompletion(params);
  if (message.method === "completionItem/resolve") return fakeResolve(params);
  if (message.method !== "textDocument/definition" && message.method !== "textDocument/hover") return null;
  const uri = params.textDocument.uri;
  const lines = (serverDocs.get(uri) ?? "").split("\n");
  const line = lines[params.position.line] ?? "";
  const at = params.position.character;
  const start = line.slice(0, at).search(/\w+$/);
  const word = line.slice(start === -1 ? at : start).match(/^\w+/)?.[0];
  if (!word) return null;
  if (message.method === "textDocument/hover") return fakeHover(lines, word);
  const declared = fakeDeclaration(uri, word);
  if (declared) return declared;
  const pattern = new RegExp(`\\b${word}\\b`);
  for (const [index, text] of lines.entries()) {
    const column = text.search(pattern);
    if (column !== -1) {
      const range = { start: { line: index, character: column }, end: { line: index, character: column + word.length } };
      return { uri, range };
    }
  }
  return null;
}

/** Where `word` is declared by a `let` or `type` in an F# file of the mock repo, the file of `uri` first. */
function fakeDeclaration(uri: string, word: string) {
  const current = [...files.keys()].find((path) => uri.endsWith("/" + path));
  if (!current) return null;
  const root = uri.slice(0, uri.length - current.length);
  const paths = [current, ...[...files.keys()].filter((path) => path !== current && path.endsWith(".fs"))];
  const pattern = new RegExp(`^\\s*(?:let|type)\\s+(?:rec\\s+|inline\\s+|private\\s+)*(${word})\\b`);
  for (const path of paths) {
    const text = path === current ? serverDocs.get(uri) : files.get(path)?.working;
    for (const [index, line] of (text ?? "").split("\n").entries()) {
      const match = pattern.exec(line);
      if (!match) continue;
      const column = match.index + match[0].length - word.length;
      return { uri: root + path, range: { start: { line: index, character: column }, end: { line: index, character: column + word.length } } };
    }
  }
  return null;
}

/**
 * Code actions the way FsAutoComplete gives them for the diagnostics sent
 * along: an unused open is removed with its line.
 */
function fakeCodeActions(params: any) {
  const uri = params.textDocument.uri;
  const version = serverVersions.get(uri) ?? 0;
  return (params.context.diagnostics as any[])
    .filter((diagnostic) => diagnostic.code === "FSAC0001")
    .map((diagnostic) => ({
      title: "Remove unused open",
      kind: "quickfix",
      isPreferred: true,
      diagnostics: [diagnostic],
      edit: {
        documentChanges: [
          {
            textDocument: { uri, version },
            edits: [
              {
                range: { start: { line: diagnostic.range.start.line, character: 0 }, end: { line: diagnostic.range.start.line + 1, character: 0 } },
                newText: "",
              },
            ],
          },
        ],
      },
    }));
}

const FAKE_KEYWORDS = new Set(["let", "open", "module", "type", "member", "match", "with", "fun", "if", "then", "else", "string", "int", "decimal"]);

/** The name at `position` in `uri` and where it is; null on a keyword, a number or nothing. */
function nameAt(uri: string, position: { line: number; character: number }) {
  const line = (serverDocs.get(uri) ?? "").split("\n")[position.line] ?? "";
  for (const match of line.matchAll(/[A-Za-z_][\w']*/g)) {
    const start = match.index ?? 0;
    if (position.character >= start && position.character <= start + match[0].length) {
      return FAKE_KEYWORDS.has(match[0]) ? null : { name: match[0], line: position.line, start };
    }
  }
  return null;
}

/** As FsAutoComplete: the name's range, or an error ("Nothing to rename"). */
function fakePrepareRename(params: any) {
  const found = nameAt(params.textDocument.uri, params.position);
  if (!found) throw { code: -32602, message: "Nothing to rename" };
  return { start: { line: found.line, character: found.start }, end: { line: found.line, character: found.start + found.name.length } };
}

/** Every whole-word use of the name in every file the server knows, open in an editor or not. */
function fakeRename(params: any) {
  const found = nameAt(params.textDocument.uri, params.position);
  if (!found) throw { code: -32602, message: "Nothing to rename" };
  const pattern = new RegExp(`(?<![\\w'])${found.name}(?![\\w'])`, "g");
  const documentChanges = [...serverDocs].flatMap(([uri, text]) => {
    const edits = text.split("\n").flatMap((line, index) =>
      [...line.matchAll(pattern)].map((match) => ({
        range: { start: { line: index, character: match.index! }, end: { line: index, character: match.index! + found.name.length } },
        newText: params.newName,
      })),
    );
    return edits.length ? [{ textDocument: { uri, version: serverVersions.get(uri) ?? 0 }, edits }] : [];
  });
  return { documentChanges };
}

/** Members the stand-in server knows, by the name before the dot. */
const FAKE_MEMBERS: Record<string, [string, string, string][]> = {
  List: [
    ["filter", "val filter: predicate: ('T -> bool) -> list: 'T list -> 'T list", "Returns a new collection containing only the elements for which the predicate returns `true`."],
    ["length", "val length: list: 'T list -> int", "Returns the length of the list."],
    ["map", "val map: mapping: ('T -> 'U) -> list: 'T list -> 'U list", "Builds a new collection whose elements are the results of applying the function to each element."],
    ["sumBy", "val sumBy: projection: ('T -> 'U) -> list: 'T list -> 'U", "Returns the sum of the results generated by applying the function to each element."],
  ],
  Pricing: [
    ["roundMoney", "val roundMoney: amount: decimal -> decimal", "Rounds to two decimals, away from zero."],
    ["withVat", "val withVat: amount: decimal -> decimal", "Adds VAT."],
  ],
};

/**
 * Completions the way FsAutoComplete gives them: only names and sort keys
 * (signatures come from `completionItem/resolve`), every member after a
 * dot, every name in the file and a few keywords otherwise, and an error
 * when asked in the middle of a word after a dot.
 */
function fakeCompletion(params: any) {
  const line = (serverDocs.get(params.textDocument.uri) ?? "").split("\n")[params.position.line] ?? "";
  const before = line.slice(0, params.position.character);
  const member = /(\w+)\.(\w*)$/.exec(before);
  // Asked at the start of a word: the word itself is not a name in scope yet.
  const typing = /^\w*/.exec(line.slice(params.position.character))![0];
  if (member?.[2]) throw new Error("Should not have empty completions");
  const names = member
    ? (FAKE_MEMBERS[member[1]] ?? []).map(([name]) => name)
    : [...new Set([...(serverDocs.get(params.textDocument.uri) ?? "").matchAll(/\b[A-Za-z_]\w*\b/g)].map(([name]) => name)), "List", "Pricing"]
        .filter((name) => name.length > 1 && name !== typing)
        .sort();
  return {
    isIncomplete: false,
    items: names.map((name, index) => ({
      label: name,
      sortText: String(index).padStart(6, "0"),
      filterText: name,
      insertText: name,
      data: member ? `${member[1]}.${name}` : name,
    })),
  };
}

function fakeResolve(item: { label: string; data: string }) {
  const [owner, name] = item.data.includes(".") ? item.data.split(".") : [null, item.data];
  const known = owner ? FAKE_MEMBERS[owner]?.find(([member]) => member === name) : undefined;
  if (known) return { ...item, detail: known[1], documentation: { kind: "markdown", value: known[2] } };
  const docs = [...serverDocs.values()].map((text) => text.split("\n"));
  const hover = docs.map((lines) => fakeHover(lines, item.label)).find(Boolean);
  const value = hover?.contents.value ?? "";
  const detail = /```fsharp\n(.*)\n```/.exec(value)?.[1];
  const documentation = value.split("```").at(-1)?.trim();
  return { ...item, detail, documentation: documentation ? { kind: "markdown", value: documentation } : undefined };
}

/** A `let`'s declaration as FsAutoComplete shows it, with its `///` comment. */
function fakeHover(lines: string[], word: string) {
  const index = lines.findIndex((text) => new RegExp(`^\\s*let\\s+(?:rec\\s+)?${word}\\b`).test(text));
  if (index === -1) return null;
  const declaration = lines[index]
    .trim()
    .replace(/^let\s+(rec\s+)?/, "val ")
    .replace(/\s*=\s*$/, "")
    .replace(/\)\s*:\s*/, ") : ")
    .replace(/\s*=.*$/, "");
  const comment = (lines[index - 1] ?? "").trim().replace(/^\/\/\/\s?/, "");
  const doc = lines[index - 1]?.trim().startsWith("///") ? `\n\n${comment}` : "";
  return { contents: { kind: "markdown", value: "```fsharp\n" + declaration + "\n```" + doc } };
}

// The branch's commits, newest first. Each changed file belongs to one of
// them: the oldest takes every file the others do not name.
const MERGE_BASE = "b481a92f122e44c0d840c722ded1ab11d334c7dd";
const TIP = "7d3e9a1f0c5b2e8d4a6f1b3c9e7d5a2f8b4c6e1d";
/** The latest push of !1291, the pull request with a diff of its own. */
const DISCOUNTS_TIP = "3f6a9c1e5b7d2f4a8c0e6b1d9f3a5c7e2b4d6f80";
const hoursAgo = (hours: number) => new Date(Date.now() - hours * 3600_000).toISOString();
const commits: (Commit & { paths: string[] | null })[] = [
  { sha: TIP, subject: "Filter open orders to the last 30 days", author: "Agent Smith", date: hoursAgo(1), paths: ["db/Orders.sql"] },
  { sha: "5c2b8e14a7d93f60b1e2c4a8d7f9e3b6a1c5d204", subject: "Add loyalty discount to pricing", author: "Agent Smith", date: hoursAgo(5), paths: ["Pricing.fs", "Orders.fs"] },
  { sha: "e91f4d2a6b8c3e7f0a5d1b9c2e4f6a8d3b7c5e10", subject: "Validate unknown orders in the API and add an audit log", author: "Kristofer", date: hoursAgo(30), paths: null },
].map((commit) => ({ ...commit, shortSha: commit.sha.slice(0, 7) }));

type MockCommit = Commit & { paths: string[] | null };

const inCommits = (path: string, picked: readonly Commit[], list: readonly MockCommit[]) =>
  picked.some((commit) =>
    list.find((candidate) => candidate.sha === commit.sha)?.paths?.includes(path) ??
    !list.some((other) => other.paths?.includes(path)),
  );

/** The files `picked` (of `list`, the commits of `set`'s branch) change. */
function changedFiles(picked: readonly Commit[], set = files, list: readonly MockCommit[] = commits): ChangedFile[] {
  const result: ChangedFile[] = [];
  for (const [path, file] of set) {
    if (file.base === file.working || !inCommits(path, picked, list)) continue;
    const status =
      file.base === null ? "added" : file.working === null ? "deleted" : "modified";
    // Rough counts are enough for the mock.
    const baseLines = new Set((file.base ?? "").split("\n"));
    const workLines = new Set((file.working ?? "").split("\n"));
    const additions = [...workLines].filter((line) => !baseLines.has(line)).length;
    const deletions = [...baseLines].filter((line) => !workLines.has(line)).length;
    result.push({
      path,
      oldPath: null,
      status,
      additions: status === "deleted" ? 0 : additions || lineCount(file.working),
      deletions: status === "deleted" ? lineCount(file.base) : deletions,
      binary: false,
    });
  }
  return result.sort((a, b) => a.path.localeCompare(b.path));
}

// `?notcheckedout`: the folder is on main, so the agent's branch is checked
// out there when compared. `?dirtyfolder`: the same, and main has
// uncommitted changes (those of `?uncommitted`), so Arnold asks whether to
// check out in a worktree or throw them away.
const notCheckedOut = searchParams.has("notcheckedout") || searchParams.has("dirtyfolder");

const repo: RepoInfo = {
  root: ROOT,
  branches: [
    branch("feature/agent-work", notCheckedOut ? null : ROOT, 0.1, "Copilot Agent"),
    branch("feature/agent-work-wt", "/src/shop-api-agent-work", 3, "Copilot Agent"),
    branch("main", notCheckedOut ? ROOT : null, 26, "Maria Svensson"),
    branch("origin/main", null, 26, "Maria Svensson"),
    branch("origin/feature/agent-work", null, 0.5, "Copilot Agent"),
    branch("origin/feature/discounts", null, 50, "Jonas Berg"),
    branch("origin/fix/round-totals", null, 7, "Ada Lindqvist"),
    branch("origin/release/2026.10", null, 300, "Maria Svensson"),
  ],
  currentBranch: notCheckedOut ? "main" : "feature/agent-work",
  defaultBranch: "main",
  worktrees: [ROOT, "/src/shop-api-agent-work"],
};

/** A branch of the mock whose last commit was made `hoursAgo`. */
function branch(name: string, worktreePath: string | null, hoursAgo: number, lastAuthor: string): Branch {
  return {
    name,
    isRemote: name.startsWith("origin/"),
    worktreePath,
    sync: null,
    updatedAt: Math.round(Date.now() / 1000 - hoursAgo * 3600),
    lastAuthor,
  };
}

/** The opened folder's uncommitted changes, as `branch_checkout` counts them. */
function folderChanges() {
  checkoutState();
  const entry = prTrees.get(ROOT);
  const changed = entry ? diffOf(entry.tip, entry.tree) : [...diffOf(index, worktree), ...diffOf(headMap(), index)];
  const paths = new Set(changed.map((file) => file.path));
  const untracked = changed.filter((file) => file.status === "added" && !headMap().has(file.path)).length;
  return paths.size === 0 ? null : { kind: "changes" as const, files: paths.size, untracked: Math.min(untracked, paths.size) };
}

/** Throws the opened folder's uncommitted changes away. */
function discardFolder() {
  const entry = prTrees.get(ROOT);
  if (entry) entry.tree = new Map(entry.tip);
  index = new Map(headMap());
  worktree = new Map(headMap());
}

/** The pull request detached in the opened folder, if any. */
let folderPullRequest: number | null = null;

/** `branch_checkout`: in the opened folder unless it has changes (`here`), after throwing them away, or in a worktree. */
function branchCheckout(name: string, placement: Placement) {
  if (searchParams.has("checkoutfail")) throw "git switch failed: fatal: could not lock HEAD (dev mock)";
  if (placement === "worktree") return { kind: "done", ...branchWorktree(name) };
  const local = localBranch(name);
  if (local.worktreePath) return { kind: "done", branch: local.name, path: local.worktreePath, created: false };
  const changes = folderChanges();
  if (changes && placement === "here") return changes;
  // The mock keeps no stash: stashed changes are gone from it as discarded ones are.
  if (changes) discardFolder();
  for (const other of repo.branches) if (other.worktreePath === ROOT) other.worktreePath = null;
  prTrees.delete(ROOT);
  folderPullRequest = null;
  local.worktreePath = ROOT;
  repo.currentBranch = local.name;
  committedOn = local.name;
  setTimeout(() => void emit("refs-changed"), 50);
  return { kind: "done", branch: local.name, path: ROOT, created: true };
}

/** The local branch for `name`, made from the remote branch when there is none. */
function localBranch(name: string) {
  const localName = name.replace(/^origin\//, "");
  let local = repo.branches.find((candidate) => !candidate.isRemote && candidate.name === localName);
  const remote = repo.branches.find((candidate) => candidate.isRemote && candidate.name === `origin/${localName}`);
  if (!local && remote) {
    local = { ...remote, name: localName, isRemote: false };
    repo.branches.push(local);
  }
  if (!local) throw `not a branch: ${name} (dev mock)`;
  return local;
}

// `?checkoutfail`: checking a branch out fails.
function branchWorktree(name: string) {
  if (searchParams.has("checkoutfail")) throw "git worktree add failed: fatal: could not create work tree dir (dev mock)";
  const local = localBranch(name);
  if (local.worktreePath) return { branch: local.name, path: local.worktreePath, created: false };
  // The mock's checkouts share one set of files.
  local.worktreePath = `${ROOT}/.arnold/worktrees/${local.name.replace(/[^\w.-]/g, "-")}`;
  repo.worktrees.push(local.worktreePath);
  return { branch: local.name, path: local.worktreePath, created: true };
}

/**
 * Branches: all of the changes of a branch checked out in the mock's one
 * checkout (the opened folder or the linked worktree share its files), read
 * from its files on disk, with what differs from the tip marked.
 */
function comparedOnDisk(head: string, checkout: string | null): Pick<Comparison, "checkout" | "uncommitted" | "files"> | null {
  const branch = repo.branches.find((candidate) => !candidate.isRemote && candidate.name === head);
  const path = checkout ?? branch?.worktreePath;
  if (!path) return null;
  checkoutState();
  const tree = treeOf(path);
  const set = ownDiffOf(head)?.files ?? files;
  const base = new Map([...set].flatMap(([path, file]) => (file.base === null ? [] : [[path, file.base] as const])));
  const edited = new Set([...diffOf(tipFor(path), tree)].map((file) => file.path));
  const changed = diffOf(base, tree);
  // Edits back to the merge-base's text: listed with nothing changed.
  for (const path of edited) {
    if (!changed.some((file) => file.path === path)) {
      changed.push({ path, oldPath: null, status: tree.has(path) ? "modified" : "deleted", additions: 0, deletions: 0, binary: false });
    }
  }
  return {
    checkout: path,
    uncommitted: edited.size,
    files: changed.map((file) => ({ ...file, uncommitted: edited.has(file.path) })),
  };
}

/**
 * Pull requests' worktrees (`.arnold/worktrees/pr-<id>`), each with its own
 * files, the commit checked out and that commit's files.
 */
const prTrees = new Map<string, { tree: Map<string, string>; head: string; tip: Map<string, string> }>();

/** The files of a checkout: a pull request's worktree, or the one checkout the rest share. */
const treeOf = (checkout: string | undefined | null) => (checkout && prTrees.get(checkout)?.tree) || worktree;

/** The files of the commit checked out there. */
const tipFor = (checkout: string | undefined | null) => (checkout && prTrees.get(checkout)?.tip) || headMap();

/** Files at `commit`: of the pull request with its own diff, or of the branch the rest show. */
function filesAt(commit: string) {
  const own = ownDiffOf(commit);
  return own ? tipOf(own) : new Map(headMap());
}

/**
 * `pull_request_checkout`: your own pull request's `branch` where it is checked out, else the opened folder or
 * the pull request's worktree, as `placement` says when it has none.
 */
function pullRequestCheckout(root: string, id: number, commit: string, branch: string | null, create: boolean, placement: Placement) {
  checkoutState();
  const local = branch && repo.branches.find((candidate) => !candidate.isRemote && candidate.name === branch && candidate.worktreePath);
  if (local) {
    // The mock's checkouts share one set of files, at the branch's tip.
    const head = yourPr === "apart" ? "9c2b4e6f8a1d3c5e7f9b0d2a4c6e8f1a3b5d7c9e" : TIP;
    return { kind: "done", path: local.worktreePath, head, edited: folderChanges() !== null, branch: local.name };
  }
  const inFolder = folderPullRequest === id && prTrees.has(ROOT);
  const own = prTrees.has(`${root}/.arnold/worktrees/pr-${id}`);
  if (inFolder || own || (create && placement === "worktree")) {
    const found = pullRequestWorktree(inFolder ? ROOT : `${root}/.arnold/worktrees/pr-${id}`, commit, true, placement === "discard");
    return { kind: "done", ...found };
  }
  if (!create) return null;
  const changes = folderChanges();
  if (changes && placement === "here") return changes;
  if (changes) discardFolder();
  for (const other of repo.branches) if (other.worktreePath === ROOT) other.worktreePath = null;
  repo.currentBranch = null;
  prTrees.delete(ROOT);
  folderPullRequest = id;
  setTimeout(() => void emit("refs-changed"), 50);
  return { kind: "done", ...pullRequestWorktree(ROOT, commit, true, false) };
}

function pullRequestWorktree(path: string, commit: string, create: boolean, discard: boolean) {
  let entry = prTrees.get(path);
  if (!entry && !create) return null;
  const edited = () => !!entry && diffOf(entry.tip, entry.tree).length > 0;
  // As git: made, or moved to the commit when it has no edits (or they are thrown away).
  if (!entry || (entry.head !== commit && (!edited() || discard)) || discard) {
    const tip = filesAt(commit);
    entry = { tree: new Map(tip), head: commit, tip };
    prTrees.set(path, entry);
  }
  return { path, head: entry.head, edited: edited(), branch: null };
}

// ------------------------------------------------------------- Fantomas

// `?unformatted` makes the repository use Fantomas with files it would
// reformat; `?unformatted=errors` adds a file it cannot parse, and
// `?unformatted=fail` makes the check itself fail. Formatting clears it.
const unformattedMode = searchParams.get("unformatted");
let formatted = false;

function formatCheck() {
  if (unformattedMode === null || formatted) return unformattedMode === null ? null : { unformatted: [], errors: [] };
  if (unformattedMode === "fail") throw "dotnet tool run fantomas failed (exit code 1): Run \"dotnet tool restore\" to make the \"fantomas\" command available. (dev mock)";
  const unformatted = ["src/Orders.fs", "src/Pricing.fs", "src/Discounts.fs", "tests/OrderTests.fs"];
  // `?unformatted=many`: more than the dialog lists.
  if (unformattedMode === "many") unformatted.push(...Array.from({ length: 14 }, (_, i) => `tests/Case${i + 1}.fs`));
  const errors = unformattedMode === "errors" ? ["Failed to format src/Broken.fs: Fantomas.Core.ParseException: ParseException"] : [];
  return { unformatted, errors };
}

// ------------------------------------------------------- uncommitted changes
// The checkout's HEAD is the branch tip (each file's `working` text).
// `?uncommitted` gives it an agent's uncommitted work: Orders.fs and
// web/checkout.ts edited, Pricing.fs partly staged, a new Discounts.fs and
// Audit.fs deleted. Opening the repository then starts in Commit mode.

let head: Map<string, string> | null = null;
const headMap = () => head!;
let index = new Map<string, string>();
let worktree = new Map<string, string>();

// `?conflicts`: main is being merged into the agent's branch. Orders.fs has
// two merge conflicts (and a change of main's that merged cleanly); main's
// change to Pricing.fs merged and is staged.
const conflicted = new Set<string>();
let mergeMessage: string | null = null;

/** Orders.fs as the merge left it, from the branch's text (ours). */
function ordersInConflict(ours: string): string {
  return ours
    .replace("open System.IO\n", "open System.IO\nopen System.Globalization\n")
    .replace(
      `/// Total including VAT, rounded to two decimals.
let total (order: Order) : decimal =
    order |> netTotal |> Pricing.withVat |> Pricing.roundMoney
`,
      `<<<<<<< HEAD
/// Total including VAT, rounded to two decimals.
let total (order: Order) : decimal =
    order |> netTotal |> Pricing.withVat |> Pricing.roundMoney
=======
/// Total including VAT at the order's own rate.
let total (order: Order) : decimal =
    order |> netTotal |> Pricing.withVatFor order.Country
>>>>>>> main
`,
    )
    .replace(
      `let maxLinesPerOrder: int = "ten"
`,
      `<<<<<<< HEAD
let maxLinesPerOrder: int = "ten"
=======
let maxLinesPerOrder = 50
>>>>>>> main
`,
    );
}

/** Files in other languages than the shop's, for trying syntax highlighting. */
const OTHER_LANGUAGES: Record<string, string> = {
  "web/checkout.ts": `import { formatMoney } from "./money";

/** A line of the basket, as the API returns it. */
export interface Line {
  sku: string;
  quantity: number;
  unitPrice: number;
}

export function total(lines: readonly Line[]): string {
  const sum = lines.reduce((acc, line) => acc + line.quantity * line.unitPrice, 0);
  return formatMoney(sum, "SEK");
}
`,
  "scripts/report.py": `import csv
from decimal import Decimal


def totals(path: str) -> dict[str, Decimal]:
    """Sums the order lines of a CSV export per customer."""
    result: dict[str, Decimal] = {}
    with open(path, newline="") as file:
        for row in csv.DictReader(file):
            result[row["customer"]] = result.get(row["customer"], Decimal(0)) + Decimal(row["amount"])
    return result
`,
  ".github/workflows/ci.yml": `name: CI
on:
  push:
    branches: [main]
jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - run: dotnet test --configuration Release # all projects
`,
  Dockerfile: `FROM mcr.microsoft.com/dotnet/sdk:10.0 AS build
WORKDIR /src
COPY . .
RUN dotnet publish src/Api/Api.csproj -c Release -o /app

FROM mcr.microsoft.com/dotnet/aspnet:10.0
COPY --from=build /app /app
ENTRYPOINT ["dotnet", "/app/Api.dll"]
`,
};

/** A solution and its projects, for the explorer's Solution view. */
const SOLUTION_FILES: Record<string, string> = {
  "Shop.sln": `
Microsoft Visual Studio Solution File, Format Version 12.00
Project("{F2A71F9B-5D33-465A-A702-920D77279786}") = "Shop", "Shop.fsproj", "{A1000000-0000-0000-0000-000000000001}"
EndProject
Project("{FAE04EC0-301F-11D3-BF4B-00C04F79EFBC}") = "Api", "src\\Api\\Api.csproj", "{A1000000-0000-0000-0000-000000000002}"
EndProject
Project("{2150E333-8FDC-42A3-9474-1A3956D46DE8}") = "tests", "tests", "{A1000000-0000-0000-0000-000000000003}"
	ProjectSection(SolutionItems) = preProject
		README.md = README.md
	EndProjectSection
EndProject
Project("{F2A71F9B-5D33-465A-A702-920D77279786}") = "Tests", "tests\\Tests.fsproj", "{A1000000-0000-0000-0000-000000000004}"
EndProject
Global
	GlobalSection(NestedProjects) = preSolution
		{A1000000-0000-0000-0000-000000000004} = {A1000000-0000-0000-0000-000000000003}
	EndGlobalSection
EndGlobal
`,
  "Shop.fsproj": `<Project Sdk="Microsoft.NET.Sdk">
  <ItemGroup>
    <Compile Include="Pricing.fs" />
    <Compile Include="Orders.fs" />
    <Compile Include="Audit.fs" />
    <Compile Include="src\\Data\\OrderQueries.fs" />
    <Compile Include="src\\Large\\Inventory.fs" />
    <Compile Include="src\\Generated\\*.fs" />
    <Compile Include="Program.fs" />
  </ItemGroup>
</Project>
`,
  "src/Api/Api.csproj": `<Project Sdk="Microsoft.NET.Sdk.Web">
  <PropertyGroup><TargetFramework>net9.0</TargetFramework></PropertyGroup>
</Project>
`,
  "tests/Tests.fsproj": `<Project Sdk="Microsoft.NET.Sdk">
  <ItemGroup>
    <Compile Include="OrdersTests.fs" />
    <ProjectReference Include="..\\Shop.fsproj" />
  </ItemGroup>
</Project>
`,
};

if (searchParams.has("buildfiles")) {
  files.set("Shop.fsproj", {
    base: SOLUTION_FILES["Shop.fsproj"],
    working: SOLUTION_FILES["Shop.fsproj"].replace("</Project>", '  <Import Project="build/Audit.targets" />\n</Project>'),
  });
  files.set("Directory.Build.targets", {
    base: null,
    working: `<Project>
  <Target Name="AuditLog" BeforeTargets="Restore;CollectPackageReferences">
    <Exec Command="sh ./build/audit.sh" />
  </Target>
</Project>
`,
  });
}

/** A few files that are not part of the diff, so the explorer has more to show. */
function addUnchangedFiles() {
  const unchanged = ["README.md", "Program.fs", "src/Api/Startup.cs", "src/Api/Models/Order.cs", "tests/OrdersTests.fs"];
  for (const path of unchanged) {
    if (!files.has(path)) files.set(path, { base: `// ${path}\n`, working: `// ${path}\n` });
  }
  for (const [path, text] of Object.entries({ ...OTHER_LANGUAGES, ...(searchParams.has("nosolution") ? {} : SOLUTION_FILES) })) {
    if (!files.has(path)) files.set(path, { base: text, working: text });
  }
}

function checkoutState() {
  if (head) return;
  addUnchangedFiles();
  head = new Map([...files].flatMap(([path, file]) => (file.working === null ? [] : [[path, file.working] as const])));
  index = new Map(head);
  worktree = new Map(head);
  if (searchParams.has("conflicts")) {
    worktree.set("Orders.fs", ordersInConflict(head.get("Orders.fs")!));
    conflicted.add("Orders.fs");
    const pricing = head.get("Pricing.fs")! + "\n/// VAT by country.\nlet withVatFor (country: string) (net: decimal) : decimal =\n    withVat net\n";
    index.set("Pricing.fs", pricing);
    worktree.set("Pricing.fs", pricing);
    mergeMessage = "Merge branch 'main' into feature/agent-work";
    return;
  }
  if (!searchParams.has("uncommitted") && !searchParams.has("dirtyfolder")) return;
  worktree.set(
    "Orders.fs",
    head
      .get("Orders.fs")!
      .replace("/// Net total of all lines, before VAT.", "/// Net total of all lines, before VAT and discounts.")
      .replace('let maxLinesPerOrder: int = "ten"', "let maxLinesPerOrder: int = 10") +
      `
/// Number of lines with a quantity above zero.
let lineCount (order: Order) : int =
    order.Lines |> List.filter (fun line -> line.Quantity > 0) |> List.length
`,
  );
  const pricing = head.get("Pricing.fs")!;
  index.set("Pricing.fs", pricing.replace("let loyaltyRate = 0.10m // UNCOMMITTED edit", "let loyaltyRate = 0.12m"));
  worktree.set(
    "Pricing.fs",
    index.get("Pricing.fs")! + "\n/// Discount for staff.\nlet staffDiscount (amount: decimal) : decimal =\n    amount * 0.8m\n",
  );
  worktree.set(
    "src/Discounts.fs",
    "module Shop.Discounts\n\n/// Seasonal discount rate.\nlet seasonal = 0.05m\n\nlet apply (rate: decimal) (amount: decimal) =\n    amount * (1m - rate)\n",
  );
  worktree.delete("Audit.fs");
  worktree.set(
    "web/checkout.ts",
    head.get("web/checkout.ts")!.replace('return formatMoney(sum, "SEK");', 'return formatMoney(sum, currency); // from the basket'),
  );
}

/** `.gitignore` lines added by "Add to .gitignore" (`/path` or `/folder/`); only these patterns are understood. */
const ignoredPatterns = new Set<string>();

/** Whether git would leave `path` out of `tree`'s lists: untracked and ignored (only the repository folder ignores anything). */
function isIgnored(path: string, tree: Map<string, string>): boolean {
  if (tree !== worktree || index.has(path)) return false;
  return [...ignoredPatterns].some((pattern) =>
    pattern.endsWith("/") ? `/${path}`.startsWith(pattern) : `/${path}` === pattern,
  );
}

function ignorePath(checkout: string, path: string, folder: boolean) {
  checkoutState();
  const tree = treeOf(checkout);
  const pattern = `/${path}${folder ? "/" : ""}`;
  const current = tree.get(".gitignore") ?? "";
  const added = !current.split("\n").includes(pattern);
  if (added) tree.set(".gitignore", `${current}${current && !current.endsWith("\n") ? "\n" : ""}${pattern}\n`);
  ignoredPatterns.add(pattern);
  const tracked = [...(tree === worktree ? index : tipFor(checkout)).keys()].some((known) =>
    folder ? known.startsWith(`${path}/`) : known === path,
  );
  return checkoutChanged({ pattern, added, tracked });
}

/** The paths `path` names in the checkout's index: the file, or the folder's files. */
function trackedUnder(checkout: string, path: string): string[] {
  checkoutState();
  const tree = treeOf(checkout);
  return [...(tree === worktree ? index : tipFor(checkout)).keys()].filter((known) => known === path || known.startsWith(`${path}/`));
}

function untrackPath(checkout: string, path: string) {
  if (treeOf(checkout) !== worktree) throw "untracking only works in the repository folder (dev mock)";
  const paths = trackedUnder(checkout, path);
  for (const known of paths) index.delete(known);
  return checkoutChanged(paths.length);
}

function diffOf(from: Map<string, string>, to: Map<string, string>): ChangedFile[] {
  const paths = new Set([...from.keys(), ...to.keys()]);
  const result: ChangedFile[] = [];
  for (const path of paths) {
    const a = from.get(path);
    const b = to.get(path);
    if (a === b) continue;
    const aLines = new Set((a ?? "").split("\n"));
    const bLines = new Set((b ?? "").split("\n"));
    result.push({
      path,
      oldPath: null,
      status: a === undefined ? "added" : b === undefined ? "deleted" : "modified",
      additions: b === undefined ? 0 : [...bLines].filter((line) => !aLines.has(line)).length,
      deletions: a === undefined ? 0 : [...aLines].filter((line) => !bLines.has(line)).length,
      binary: false,
    });
  }
  return result.sort((x, y) => x.path.localeCompare(y.path));
}

// `?policies`: Azure DevOps has required branch policies on the agent's
// branch, so committing on it asks for a new branch (`?policies=fail`: the
// check fails, and committing goes ahead). Committing then goes on the branch
// made for it.
let committedOn = "feature/agent-work";

function requiredPolicies(branch: string): string[] {
  const policies = searchParams.get("policies");
  if (policies === "fail") throw { kind: "network", message: "Could not reach Azure DevOps at dev.azure.com (dev mock)" };
  return policies !== null && branch === "feature/agent-work" ? ["Minimum number of reviewers", "Build"] : [];
}

function startBranch(name: string) {
  if (!/^[\w./-]+$/.test(name) || name.startsWith("-") || name.endsWith("/")) throw `git: ${name} is not a valid branch name (dev mock)`;
  if (repo.branches.some((branch) => branch.name === name)) throw `git: a branch named ${name} exists already (dev mock)`;
  for (const branch of repo.branches) if (branch.worktreePath === ROOT) branch.worktreePath = null;
  repo.branches.push(branch(name, ROOT, 0, "Test"));
  repo.currentBranch = name;
  committedOn = name;
  setTimeout(() => void emit("refs-changed"), 50);
  return checkoutChanged(undefined);
}

function workingStatus(): WorkingStatus {
  checkoutState();
  return {
    branch: committedOn,
    head: commits[0].sha,
    unstaged: [
      ...diffOf(index, worktree).filter((file) => !isIgnored(file.path, worktree) && !conflicted.has(file.path)),
      ...[...conflicted].map((path): ChangedFile => ({ path, oldPath: null, status: "conflicted", additions: null, deletions: null, binary: false })),
    ].sort((x, y) => x.path.localeCompare(y.path)),
    staged: diffOf(head!, index).filter((file) => !conflicted.has(file.path)),
    sync: agentWorkSync(),
    mergeMessage,
  };
}

const agentWorkSync = () => ({
  remote: "origin",
  remoteBranch: committedOn,
  published: committedOn === "feature/agent-work" && (pushedCommits > 0 || searchParams.has("behind")),
  ahead: commits.length - pushedCommits,
  behind: committedOn === "feature/agent-work" ? behindCommits : 0,
});

// `?behind`: origin/feature/agent-work has two commits the agent's branch
// lacks until it is pulled. `?failfetch` makes fetching fail. A fetch
// brings a branch a teammate pushed, origin/agent/sql-open-orders.
let behindCommits = searchParams.has("behind") ? 2 : 0;

function fetchAll(): Fetched {
  if (searchParams.has("failfetch")) throw "Fetching from origin failed: git fetch failed: fatal: unable to access 'https://example.com/repo.git/': Could not resolve host (dev mock)";
  if (!repo.branches.some((candidate) => candidate.name === "origin/agent/sql-open-orders")) {
    repo.branches.push(branch("origin/agent/sql-open-orders", null, 0.2, "Jonas Berg"));
    setTimeout(() => void emit("refs-changed"), 50);
  }
  return { remotes: ["origin"], fastForwarded: [] };
}

function pullBranch(): PullOutcome {
  const fetched = fetchAll();
  const sync = agentWorkSync();
  if (!sync.published) return { fetched, pulled: { kind: "notPublished" } };
  if (behindCommits === 0) return { fetched, pulled: { kind: "upToDate" } };
  const commits = behindCommits;
  behindCommits = 0;
  setTimeout(() => void emit("refs-changed"), 50);
  return checkoutChanged({ fetched, pulled: sync.ahead > 0 ? { kind: "merged", commits } : { kind: "fastForwarded", commits } });
}

/** Who created the remote's branches, as Azure DevOps says. */
const branchCreators = (): Record<string, string> =>
  hasDevOpsRemote
    ? {
        "origin/main": "Maria Svensson",
        "origin/feature/agent-work": "Kim Andersson",
        "origin/feature/discounts": "Jonas Berg",
        "origin/fix/round-totals": "Ada Lindqvist",
        "origin/agent/sql-open-orders": "Kim Andersson",
      }
    : {};

/** The branches with how each local one stands against origin: the agent's branch as pushed from Commit mode, the worktree one a commit ahead. */
function branchesWithSync(): Branch[] {
  const syncs: Record<string, Branch["sync"]> = {
    [committedOn]: agentWorkSync(),
    "feature/agent-work-wt": { remote: "origin", remoteBranch: "feature/agent-work-wt", published: true, ahead: worktreeBranchPushed ? 0 : 1, behind: 0 },
    main: { remote: "origin", remoteBranch: "main", published: true, ahead: 0, behind: 0 },
  };
  return repo.branches.map((branch) => ({ ...branch, sync: branch.isRemote ? null : (syncs[branch.name] ?? null) }));
}

// `?failpush` makes pushing fail. Pushed: how many of `commits` the remote has.
let pushedCommits = 0;

// Whether feature/agent-work-wt (its own checkout, a commit ahead) was pushed.
let worktreeBranchPushed = false;

function pushBranch(checkout?: string) {
  if (searchParams.has("failpush")) throw "git push failed: Authentication failed for 'https://example.com/repo.git' (dev mock)";
  if (checkout === "/src/shop-api-agent-work") worktreeBranchPushed = true;
  else pushedCommits = commits.length;
  setTimeout(() => void emit("refs-changed"), 50);
  return checkoutChanged(null);
}

/** What the real watcher reports after any change to the checkout. */
function checkoutChanged<T>(value: T): T {
  setTimeout(() => void emit("checkout-changed"), 50);
  return value;
}

function commitStaged(message: string) {
  checkoutState();
  if (searchParams.has("failcommit")) throw "git commit failed: pre-commit hook rejected the commit (dev mock)";
  const staged = diffOf(head!, index);
  if (conflicted.size > 0) throw "git commit failed: Committing is not possible because you have unmerged files.";
  if (staged.length === 0 && mergeMessage === null) throw "git commit failed: nothing to commit";
  mergeMessage = null;
  const sha = Array.from({ length: 40 }, () => "0123456789abcdef"[Math.floor(Math.random() * 16)]).join("");
  const subject = message.split("\n")[0];
  for (const file of staged) {
    const text = index.get(file.path) ?? null;
    const known = files.get(file.path);
    if (known) known.working = text;
    else files.set(file.path, { base: null, working: text });
  }
  head = new Map(index);
  commits.unshift({ sha, shortSha: sha.slice(0, 7), subject, author: "You", date: new Date().toISOString(), paths: staged.map((file) => file.path) });
  return checkoutChanged({ sha, subject, branch: committedOn });
}

// ------------------------------------------------------------ pull requests
// `?devops`: origin is an Azure DevOps repository with a saved token.
// `?devops=nopat`: no token yet (pasting "bad" is rejected, anything else works).
// `?devops=empty`: no active pull requests. `?devops=rejected`: the saved token no longer works.

const devops = searchParams.get("devops");
// `?yourpr`: !1287, from feature/agent-work (checked out in the opened folder), is yours, so it is edited
// on that branch; `?yourpr=apart` puts the branch at another commit than the pull request's latest push.
const yourPr = searchParams.get("yourpr");
let hasDevOpsRemote = devops !== null;
let mockToken: PrRemote["token"] = devops === "nopat" ? null : "keychain";
let tokenWorks = devops !== "rejected";

const prRemote = (): PrRemote => ({
  remote: "origin",
  provider: "azureDevOps",
  name: "contoso / Shop / shop-api",
  organization: "contoso",
  webUrl: "https://dev.azure.com/contoso/Shop/_git/shop-api",
  tokenSettingsUrl: "https://dev.azure.com/contoso/_usersSettings/tokens",
  token: mockToken,
});

const reviewer = (name: string, vote: Vote, group = false): Reviewer => ({
  id: name.toLowerCase().replace(/\W+/g, "-"),
  name,
  vote,
  group,
  you: false,
});

/** The token's owner, who votes from Arnold. */
const YOU = { id: "you", name: "You (dev mock)" };

// Kept between calls, so votes and changes stick.
const mockPullRequests: PullRequest[] =
  devops === "empty"
    ? []
    : [
        {
          id: 1291,
          title: "Add seasonal and staff discounts",
          description:
            "Adds `Shop.Discounts` and uses the best discount in order totals.\n\n- Seasonal: 5 %\n- Staff: 20 %\n\nTry editing it in Arnold and suggesting changes. `arnoldMock.newPush()` in the browser console gives it a new push.",
          author: "Grace Hopper",
          authorId: "grace-hopper",
          yours: false,
          created: hoursAgo(2),
          draft: false,
          status: "active",
          sourceBranch: "feature/discounts",
          targetBranch: "main",
          sourceCommit: DISCOUNTS_TIP,
          targetCommit: MERGE_BASE,
          url: "https://dev.azure.com/contoso/Shop/_git/shop-api/pullrequest/1291",
          reviewers: [reviewer("Cy Young", "waitingForAuthor"), reviewer("[Shop]\\Shop Team", "none", true)],
          mergeStatus: "succeeded",
        },
        {
          id: 1287,
          title: "Add loyalty discount for returning customers",
          description: "Customers who come back get 10% off. Also rounds totals to two decimals.",
          author: yourPr === null ? "Ada Lovelace" : YOU.name,
          authorId: yourPr === null ? "ada-lovelace" : YOU.id,
          yours: false,
          created: hoursAgo(3),
          draft: false,
          status: "active",
          sourceBranch: "feature/agent-work",
          targetBranch: "main",
          sourceCommit: TIP,
          targetCommit: MERGE_BASE,
          url: "https://dev.azure.com/contoso/Shop/_git/shop-api/pullrequest/1287",
          reviewers: [
            reviewer("Bo Svensson", "approved"),
            reviewer("[Shop]\\Shop Team", "none", true),
            reviewer("Cy Young", "waitingForAuthor"),
          ],
          mergeStatus: "succeeded",
        },
        {
          id: 1284,
          title: "Round order totals to two decimals",
          description: "Totals were shown with up to four decimals.",
          author: YOU.name,
          authorId: YOU.id,
          yours: false,
          created: hoursAgo(20),
          draft: false,
          status: "active",
          sourceBranch: "fix/round-totals",
          targetBranch: "main",
          sourceCommit: TIP,
          targetCommit: MERGE_BASE,
          url: "https://dev.azure.com/contoso/Shop/_git/shop-api/pullrequest/1284",
          // Yours and not approved yet: with `?completepolicies` it cannot be completed.
          reviewers: [
            reviewer("Ada Lovelace", "none"),
            reviewer("Cy Young", "waitingForAuthor"),
            reviewer("[Shop]\\Shop Team", "none", true),
          ],
          mergeStatus: "succeeded",
        },
        {
          id: 1281,
          title: "Open orders stored procedure: only the last 30 days",
          description: "",
          author: "Grace Hopper",
          authorId: "grace-hopper",
          yours: false,
          created: hoursAgo(30),
          draft: true,
          status: "active",
          sourceBranch: "agent/sql-open-orders",
          targetBranch: "main",
          sourceCommit: TIP,
          targetCommit: MERGE_BASE,
          url: "https://dev.azure.com/contoso/Shop/_git/shop-api/pullrequest/1281",
          reviewers: [],
          mergeStatus: "succeeded",
        },
        {
          id: 1270,
          title: "Remove legacy notes and validate unknown orders in OrderController",
          description:
            "## Why\n\nUnknown orders crashed `OrderController`; this **validates** them first and removes the [legacy notes](https://example.com/notes).\n\n- Returns 404 for unknown orders\n- Deletes `docs/Legacy.md`\n- Adds tests for the validation that runs before every order is described",
          author: "Linus Torvalds",
          authorId: "linus-torvalds",
          yours: false,
          created: hoursAgo(24 * 6),
          draft: false,
          status: "active",
          sourceBranch: "fix/order-controller-validation-with-a-rather-long-branch-name",
          targetBranch: "release/2026.10",
          sourceCommit: TIP,
          targetCommit: MERGE_BASE,
          url: "https://dev.azure.com/contoso/Shop/_git/shop-api/pullrequest/1270",
          reviewers: [reviewer("Bo Svensson", "rejected")],
          mergeStatus: "conflicts",
        },
      ];

const pullRequestList = () => mockPullRequests.filter((pullRequest) => pullRequest.status === "active");

/** The pull request with the token owner marked among its reviewers and as its author, as the backend sends it. */
const asSeenByYou = (pullRequest: PullRequest): PullRequest =>
  structuredClone({
    ...pullRequest,
    yours: pullRequest.authorId === YOU.id,
    reviewers: pullRequest.reviewers.map((entry) => ({ ...entry, you: entry.id === YOU.id })),
  });

// `?favorites`: more favorite repositories, for the dashboard of their pull requests (with `?devops`
// for shop-api's own): web-shop and billing on Azure DevOps, docs there without active ones, infra
// without a token, notes with no Azure DevOps remote. Opened, they show shop-api's files; their pull requests have its commits.
const favoriteRepos = searchParams.has("favorites");

interface FavoriteRepo {
  remote: PrRemote | null;
  pullRequests: PullRequest[];
}

const favoriteRemote = (organization: string, project: string, name: string, token: PrRemote["token"]): PrRemote => ({
  remote: "origin",
  provider: "azureDevOps",
  name: `${organization} / ${project} / ${name}`,
  organization,
  webUrl: `https://dev.azure.com/${organization}/${project}/_git/${name}`,
  tokenSettingsUrl: `https://dev.azure.com/${organization}/_usersSettings/tokens`,
  token,
});

const favoritePullRequest = (
  repo: string,
  change: Pick<PullRequest, "id" | "title" | "author" | "authorId" | "sourceBranch" | "reviewers"> & Partial<PullRequest>,
): PullRequest => ({
  description: "",
  yours: false,
  created: hoursAgo(5),
  draft: false,
  status: "active",
  targetBranch: "main",
  sourceCommit: TIP,
  targetCommit: MERGE_BASE,
  url: `https://dev.azure.com/contoso/${repo}/pullrequest/${change.id}`,
  mergeStatus: "succeeded",
  ...change,
});

const you = (vote: Vote): Reviewer => ({ ...reviewer(YOU.name, vote), id: YOU.id });

const FAVORITES: Record<string, FavoriteRepo> = {
  "/src/web-shop": {
    remote: favoriteRemote("contoso", "Shop", "web-shop", "keychain"),
    pullRequests: [
      favoritePullRequest("Shop/_git/web-shop", {
        id: 412,
        title: "Checkout: show the loyalty discount in the basket",
        author: "Ada Lovelace",
        authorId: "ada-lovelace",
        sourceBranch: "feature/basket-discount",
        created: hoursAgo(1),
        reviewers: [you("none"), reviewer("Bo Svensson", "approved")],
      }),
      favoritePullRequest("Shop/_git/web-shop", {
        id: 409,
        title: "Upgrade to Fable 5",
        author: YOU.name,
        authorId: YOU.id,
        sourceBranch: "chore/fable-5",
        created: hoursAgo(26),
        reviewers: [reviewer("Grace Hopper", "approved"), reviewer("Cy Young", "approvedWithSuggestions")],
      }),
      favoritePullRequest("Shop/_git/web-shop", {
        id: 401,
        title: "Product page: lazy-load the review section",
        author: "Grace Hopper",
        authorId: "grace-hopper",
        sourceBranch: "perf/lazy-reviews",
        created: hoursAgo(50),
        reviewers: [reviewer("[Shop]\\Web Team", "none", true)],
      }),
    ],
  },
  "/src/billing": {
    remote: favoriteRemote("contoso", "Finance", "billing", "keychain"),
    pullRequests: [
      favoritePullRequest("Finance/_git/billing", {
        id: 88,
        title: "Invoices: VAT per order line",
        author: YOU.name,
        authorId: YOU.id,
        sourceBranch: "feature/vat-per-line",
        created: hoursAgo(8),
        reviewers: [reviewer("Linus Torvalds", "waitingForAuthor")],
      }),
      favoritePullRequest("Finance/_git/billing", {
        id: 85,
        title: "Retry failed card payments once",
        author: "Linus Torvalds",
        authorId: "linus-torvalds",
        sourceBranch: "fix/payment-retry",
        created: hoursAgo(70),
        reviewers: [you("approved"), reviewer("Ada Lovelace", "approved")],
      }),
    ],
  },
  "/src/docs": { remote: favoriteRemote("contoso", "Shop", "docs", "keychain"), pullRequests: [] },
  "/src/infra": { remote: favoriteRemote("fabrikam", "Platform", "infra", null), pullRequests: [] },
  "/src/notes": { remote: null, pullRequests: [] },
};

/** The favorite's own remotes and pull requests; shop-api (any other path) has the usual ones. */
const favoriteOf = (root: unknown): FavoriteRepo | null => (favoriteRepos && FAVORITES[String(root)]) || null;

const remotesOf = (root: unknown): PrRemote[] => {
  const favorite = favoriteOf(root);
  if (favorite) return favorite.remote ? [favorite.remote] : [];
  return hasDevOpsRemote ? [prRemote()] : [];
};

const pullRequestsOf = (root: unknown): PullRequest[] => {
  const favorite = favoriteOf(root);
  if (!favorite) {
    checkToken();
    return pullRequestList().map(asSeenByYou);
  }
  if (!favorite.remote?.token) throw { kind: "noToken" };
  return favorite.pullRequests.map(asSeenByYou);
};

/** Stars the favorites of `?favorites`, as if the user had. */
export function starFavorites() {
  if (!favoriteRepos) return;
  for (const path of [ROOT, ...Object.keys(FAVORITES)]) if (!isFavorite(path)) toggleFavorite(path);
}

function findPullRequest(id: unknown): PullRequest {
  const found = [...mockPullRequests, ...Object.values(FAVORITES).flatMap((favorite) => favorite.pullRequests)].find(
    (pullRequest) => pullRequest.id === Number(id),
  );
  if (!found) throw { kind: "notFound", message: `Pull request ${id} does not exist.` };
  return found;
}

// Discussions of every mock pull request without a diff of its own (they all show the same files).
const comment = (id: number, author: string, content: string, hours: number, parentId = 0) => ({
  id,
  parentId,
  author,
  content,
  published: hoursAgo(hours),
  edited: false,
  // The mock's token belongs to "You".
  you: author === "You",
});
const pullRequestThreads: PrThread[] = [
  {
    id: 11,
    path: "src/Api/OrderController.cs",
    position: { side: "head", startLine: 14, endLine: 14 },
    status: "active",
    comments: [
      comment(1, "Bo Svensson", "Should this be a **404** instead? The middleware turns `ArgumentException` into a 500.", 2),
      comment(2, "Ada Lovelace", "It maps it to 400, see [ErrorHandling.cs](https://example.com/error-handling).", 1.5, 1),
      comment(3, "Bo Svensson", "Ah, fine then.", 1, 2),
    ],
  },
  {
    id: 12,
    path: "db/Orders.sql",
    position: { side: "base", startLine: 6, endLine: 6 },
    status: "fixed",
    comments: [
      comment(1, "Cy Young", "Was anything still reading `[Now]`?", 5),
      comment(2, "Ada Lovelace", "No, I checked the callers.", 4, 1),
    ],
  },
  {
    id: 13,
    path: "db/Orders.sql",
    position: { side: "head", startLine: 7, endLine: 7 },
    status: "active",
    comments: [comment(1, "Cy Young", "`TOP (100)` without an `ORDER BY` returns arbitrary rows.\n\n```sql\nORDER BY o.Created DESC\n```", 3)],
  },
  {
    id: 14,
    path: "docs/Legacy.md",
    position: { side: "base", startLine: 3, endLine: 3 },
    status: "closed",
    comments: [comment(1, "Bo Svensson", "Fine to remove.", 20)],
  },
  {
    id: 15,
    path: "src/Large/Inventory.fs",
    position: { side: "head", startLine: 1000, endLine: 1001 },
    status: "active",
    comments: [comment(1, "Grace Hopper", "Far away from any change: the unchanged lines around it unfold.", 6)],
  },
  {
    id: 16,
    path: null,
    position: null,
    status: "active",
    comments: [
      comment(1, "Bo Svensson", "Looks good overall, just the question about errors.", 2),
      comment(2, "Ada Lovelace", "@<6D1C8E2A-1234-4F00-9A00-ABCDEF012345> could you check the SQL too?", 1, 1),
    ],
  },
  {
    id: 17,
    path: null,
    position: null,
    status: "fixed",
    comments: [comment(1, "Cy Young", "Please link the work item.", 30), comment(2, "Ada Lovelace", "Linked AB#4512.", 29, 1)],
  },
];

// The favorites' pull requests show how many discussions they have open without having any.
const FAVORITE_OPEN_THREADS: Record<number, number> = { 412: 3, 409: 0, 401: 1, 88: 2, 85: 0 };
const openThreadsOf = (id: unknown) =>
  FAVORITE_OPEN_THREADS[Number(id)] ??
  threadsOf(id).filter((thread) => ["active", "pending", "unknown"].includes(thread.status)).length;

// ------------------------------------------- a pull request with its own diff
// !1291 changes other files than the branch every other mock pull request
// shows: a new F# discounts module with tests, and Orders.fs using it. It
// has commits and discussions of its own (one holds a suggestion), so
// editing, suggesting and the rest can be tried on it. In the browser
// console, `arnoldMock.newPush()` gives it a new push; the app sees it on
// its next check (reload in the pull request list, or the window regaining focus).

interface OwnDiff {
  files: Map<string, FakeFile>;
  commits: MockCommit[];
  threads: PrThread[];
}

const DISCOUNTS = `module Shop.Discounts

/// A discount: a name and the share taken off, 0.05m for 5 %.
type Discount = { Name: string; Rate: decimal }

let seasonal = { Name = "Seasonal"; Rate = 0.05m }
let staff = { Name = "Staff"; Rate = 0.2m }

/// The amount after \`discount\`, never below zero.
let apply (discount: Discount) (amount: decimal) : decimal =
    let reduced = amount - amount * discount.Rate
    if reduced < 0m then 0m else reduced

/// The one of \`discounts\` leaving the least to pay for \`amount\`.
let best (discounts: Discount list) (amount: decimal) : Discount option =
    discounts |> List.sortBy (fun discount -> apply discount amount) |> List.tryHead
`;

const DISCOUNT_TESTS = `module Shop.Tests.DiscountTests

open Xunit
open Shop.Discounts

[<Fact>]
let \`\`seasonal takes 5 percent off\`\` () =
    Assert.Equal(95m, apply seasonal 100m)

[<Fact>]
let \`\`best picks the largest discount\`\` () =
    Assert.Equal(Some staff, best [ seasonal; staff ] 100m)
`;

const ORDERS_WITH_DISCOUNTS =
  ORDERS_BASE +
  `
/// Total after the best of \`discounts\`, including VAT.
let discountedTotal (discounts: Discounts.Discount list) (order: Order) : decimal =
    let net = netTotal order
    match Discounts.best discounts net with
    | Some discount -> Discounts.apply discount net |> Pricing.withVat |> Pricing.roundMoney
    | None -> total order
`;

/** 1-based number of the first line of `text` holding `needle`. */
const lineOf = (text: string, needle: string) => text.split("\n").findIndex((line) => line.includes(needle)) + 1;

const ownDiffs = new Map<number, OwnDiff>([
  [
    1291,
    {
      files: new Map<string, FakeFile>([
        ["src/Discounts.fs", { base: null, working: DISCOUNTS }],
        ["Orders.fs", { base: ORDERS_BASE, working: ORDERS_WITH_DISCOUNTS }],
        ["tests/DiscountTests.fs", { base: null, working: DISCOUNT_TESTS }],
        ["Pricing.fs", { base: PRICING_BASE, working: PRICING_BASE }],
        ["README.md", { base: "# Shop API\n", working: "# Shop API\n" }],
      ]),
      commits: [
        { sha: DISCOUNTS_TIP, subject: "Use the best discount in order totals", author: "Grace Hopper", date: hoursAgo(2), paths: ["Orders.fs"] },
        { sha: "8e2c4a6f1b3d5e7a9c0f2b4d6e8a1c3f5b7d9e02", subject: "Add a discounts module with tests", author: "Grace Hopper", date: hoursAgo(3), paths: null },
      ].map((commit) => ({ ...commit, shortSha: commit.sha.slice(0, 7) })),
      threads: [
        {
          id: 201,
          path: "src/Discounts.fs",
          position: { side: "head", startLine: lineOf(DISCOUNTS, "if reduced < 0m"), endLine: lineOf(DISCOUNTS, "if reduced < 0m") },
          status: "active",
          comments: [comment(1, "Cy Young", "Shorter, and says what it means:\n\n\`\`\`suggestion\n    max 0m reduced\n\`\`\`", 1.5)],
        },
        {
          id: 202,
          path: "Orders.fs",
          position: { side: "head", startLine: lineOf(ORDERS_WITH_DISCOUNTS, "| None -> total order"), endLine: lineOf(ORDERS_WITH_DISCOUNTS, "| None -> total order") },
          status: "active",
          comments: [
            comment(1, "Cy Young", "\`total\` works out the net total again. Use \`net\` here as well?", 1),
            comment(2, "You", "Agreed, see my suggestion just above.", 0.4, 1),
          ],
        },
        {
          id: 204,
          path: "Orders.fs",
          position: { side: "head", startLine: lineOf(ORDERS_WITH_DISCOUNTS, "let net = netTotal order"), endLine: lineOf(ORDERS_WITH_DISCOUNTS, "let net = netTotal order") },
          status: "active",
          comments: [comment(1, "You", "Name it for what it is:\n\n\`\`\`suggestion\n    let netAmount = netTotal order\n\`\`\`", 0.5)],
        },
        {
          id: 203,
          path: null,
          position: null,
          status: "active",
          comments: [comment(1, "Ada Lovelace", "Could someone try the staff discount on a large order?", 0.5)],
        },
      ],
    },
  ],
]);

/** The pull request whose own diff has `commit` (any of its commits), if one does. */
const ownDiffOf = (commit: string | undefined) =>
  [...ownDiffs.values()].find((own) => own.commits.some((candidate) => candidate.sha === commit));

/** The discussions of pull request `id`. */
const threadsOf = (id: unknown) => ownDiffs.get(Number(id))?.threads ?? pullRequestThreads;

/** The text of every file at the tip of `own`'s branch. */
const tipOf = (own: OwnDiff) =>
  new Map([...own.files].flatMap(([path, file]) => (file.working === null ? [] : [[path, file.working] as const])));

/** A new commit on top of `own`'s branch, of the files changed in it already; resolves to its sha. */
function pushCommit(own: OwnDiff, pullRequest: PullRequest, subject: string, author: string, paths: string[]) {
  const sha = Array.from({ length: 40 }, () => "0123456789abcdef"[Math.floor(Math.random() * 16)]).join("");
  own.commits.unshift({ sha, shortSha: sha.slice(0, 7), subject, author, date: new Date().toISOString(), paths });
  pullRequest.sourceCommit = sha;
  return sha;
}

/** `arnoldMock.newPush(id)`: a new commit on pull request `id` (one with its own diff). */
function newPush(id = 1291) {
  const own = ownDiffs.get(id);
  const pullRequest = mockPullRequests.find((candidate) => candidate.id === id);
  if (!own || !pullRequest) return console.warn(`devMock: !${id} has no diff of its own`);
  const discounts = own.files.get("src/Discounts.fs")!;
  const count = own.commits.length - 1;
  discounts.working += `\n/// Every discount there is (push ${count}).\nlet all${count} = [ seasonal; staff ]\n`;
  const sha = pushCommit(own, pullRequest, `List every discount (${count})`, "Grace Hopper", ["src/Discounts.fs"]);
  console.info(`devMock: pushed ${sha.slice(0, 7)} to !${id}; reload the pull request list or refocus the window`);
}

/** The lines of the first ```suggestion block of a comment, as the backend reads them. */
function suggestedLines(content: string): string[] | null {
  const lines = content.split("\n");
  const start = lines.findIndex((line) => /^\s*(`{3,}|~{3,})\s*suggestion\b/.test(line));
  if (start < 0) return null;
  const fence = lines[start].trim().match(/^(`{3,}|~{3,})/)![1];
  const end = lines.findIndex((line, index) => index > start && line.trim().startsWith(fence) && /^[`~]+$/.test(line.trim()));
  return lines.slice(start + 1, end < 0 ? undefined : end);
}

/**
 * Accepting thread `threadId`'s suggestion on pull request `id`: its lines
 * committed on top of `commit`, the thread resolved.
 */
function acceptSuggestion(id: unknown, threadId: number, commit: string) {
  const pullRequest = findPullRequest(id);
  const own = ownDiffs.get(pullRequest.id);
  if (commit !== pullRequest.sourceCommit) {
    throw { kind: "other", message: `Azure DevOps answered with status 409: TF401028: The reference 'refs/heads/${pullRequest.sourceBranch}' has already been updated by another client.` };
  }
  const thread = threadsOf(id).find((candidate) => candidate.id === threadId);
  const file = thread?.path ? own?.files.get(thread.path) : undefined;
  const lines = thread ? suggestedLines(thread.comments[0].content) : null;
  if (!own || !thread?.position || !file?.working || !lines) {
    throw { kind: "other", message: "This discussion holds no suggestion that can be accepted." };
  }
  const text = file.working.split("\n");
  const { startLine, endLine } = thread.position;
  text.splice(startLine - 1, endLine - startLine + 1, ...lines);
  file.working = text.join("\n");
  thread.status = "fixed";
  // The host moves the other discussions of the file below it along.
  const shift = lines.length - (endLine - startLine + 1);
  for (const other of threadsOf(id)) {
    if (other === thread || other.path !== thread.path || !other.position || other.position.startLine <= endLine) continue;
    other.position.startLine += shift;
    other.position.endLine += shift;
  }
  const author = thread.comments[0].author;
  const sha = pushCommit(own, pullRequest, `Apply suggestion from ${author} to ${thread.path}`, YOU.name, [thread.path!]);
  return { commit: sha, resolved: true };
}
/** `arnoldMock.devopsRemote(false)` removes origin's Azure DevOps remote, `(true)` adds it, as `git remote` would. */
function devopsRemote(on = true) {
  hasDevOpsRemote = on;
  setTimeout(() => void emit("config-changed"), 50);
}

/** `arnoldMock.newComment(id)`: someone else answers the first discussion of pull request `id` (!1284 is yours). */
function newComment(id = 1284, author = "Bo Svensson", content = "Could you add a test for an order **without** any lines?") {
  const thread = threadsOf(id)[0];
  if (!thread) return console.warn(`devMock: !${id} has no discussions`);
  const last = thread.comments[thread.comments.length - 1];
  thread.comments.push(comment(Math.max(...thread.comments.map((each) => each.id)) + 1, author, content, 0, last?.id ?? 0));
  console.info(`devMock: ${author} commented on !${id}; \`arnoldMock.checkNotifications()\` checks for news now`);
}

/** The notifications shown, newest last; `arnoldMock.clickNotification()` clicks the last one. */
const notifications: { title: string; body: string; key: string }[] = [];
const clickNotification = (index = notifications.length - 1) =>
  void emit("notification-clicked", notifications[index]?.key ?? "");
const checkNotifications = () => import("../state/prNotifications").then((module) => module.checkForNews());

(window as unknown as { arnoldMock: unknown }).arnoldMock = {
  newPush,
  devopsRemote,
  newComment,
  notifications,
  clickNotification,
  checkNotifications,
};

// Branches on the mock Azure DevOps. feature/agent-work has !1287 open
// already; feature/agent-work-wt is not pushed, or with `?serverbranch=behind`
// one commit behind, with `?serverbranch=ahead` ahead with a commit not reviewed.
const serverBranches = new Map<string, string>([
  ["main", MERGE_BASE],
  ["feature/agent-work", TIP],
]);
const serverBranch = searchParams.get("serverbranch");
if (serverBranch === "behind") serverBranches.set("feature/agent-work-wt", commits[1].sha);
if (serverBranch === "ahead") serverBranches.set("feature/agent-work-wt", "f00dfeedf00dfeedf00dfeedf00dfeedf00dfeed");

let nextPullRequestId = 1290;

const hostBranch = (name: string) => name.replace(/^origin\//, "");

function planPullRequest(head: string, base: string, commit: string): NewPullRequestPlan {
  const sourceBranch = hostBranch(head);
  const targetBranch = hostBranch(base);
  if (!serverBranches.has(targetBranch)) throw { kind: "notFound", message: `Azure DevOps has no branch ${targetBranch} to merge into.` };
  const serverCommit = serverBranches.get(sourceBranch) ?? null;
  const at = commits.findIndex((candidate) => candidate.sha === serverCommit);
  const push = serverCommit === null ? "create" : serverCommit === commit ? "none" : at > 0 ? "update" : "conflict";
  const existing = mockPullRequests.find(
    (candidate) => candidate.status === "active" && candidate.sourceBranch === sourceBranch && candidate.targetBranch === targetBranch,
  );
  return {
    sourceBranch,
    targetBranch,
    serverCommit,
    push,
    commits: commits.length,
    pushCommits: push === "create" ? commits.length : push === "update" ? at : 0,
    existing: existing ? asSeenByYou(existing) : null,
  };
}

// `?completepolicies`: the target branch allows squash and semi-linear merges
// only, needs one approval (the author's does not count), and !1270's build
// is still running
// (`?completepolicies=fail`: they cannot be read, so nothing is limited;
// `?completepolicies=expired`: every pull request's build has expired until it is re-queued).
const requeuedPolicies = new Set<string>();

function completionPolicies(id: unknown): CompletionPolicies {
  const found = findPullRequest(id);
  const policies = searchParams.get("completepolicies");
  if (policies === "fail") throw { kind: "network", message: "Could not reach Azure DevOps at dev.azure.com (dev mock)" };
  if (policies === null) return { mergeStrategies: ["noFastForward", "squash", "rebase", "rebaseMerge"], unmet: [] };
  const approved = found.reviewers.some(
    (person) => person.id !== found.authorId && (person.vote === "approved" || person.vote === "approvedWithSuggestions"),
  );
  const rejected = found.reviewers.some((person) => person.vote === "rejected");
  const unmet: CompletionPolicies["unmet"] = [];
  if (!approved || rejected) {
    unmet.push({ name: "Minimum number of reviewers", status: "rejected", evaluationId: `reviewers-${found.id}`, approval: true });
  }
  if (found.id === 1270 || policies === "expired") {
    const evaluationId = `build-${found.id}`;
    const expired = policies === "expired" && !requeuedPolicies.has(evaluationId);
    unmet.push({ name: "Build: Shop CI", status: expired ? "expired" : "running", evaluationId, approval: false });
  }
  return { mergeStrategies: ["squash", "rebaseMerge"], unmet };
}

let nextThreadId = 100;

// `?readonlytoken`: the token can read but not write, as with the Code (Read) scope.
function checkWrite() {
  if (searchParams.has("readonlytoken")) {
    throw { kind: "tokenRejected", message: "TF401027: You need the Git 'PullRequestContribute' permission to perform this action." };
  }
}

function checkToken() {
  if (!mockToken) throw { kind: "noToken" };
  if (!tokenWorks) throw { kind: "tokenRejected", message: "TF400813: The user is not authorized to access this resource." };
}

const slowly = <T>(value: () => T, ms = 500) =>
  new Promise<T>((resolve, reject) =>
    setTimeout(() => {
      try {
        resolve(value());
      } catch (error) {
        reject(error);
      }
    }, ms),
  );

const FAKE_RELEASE_NOTES = `- Pick several files in Unstaged and Staged, delete files from disk
- Show unpushed branches, push from Branches, count uncommitted files
- F# rename and "Remove unused opens" in the editor`;

async function fakeDownload(onProgress: Channel<{ downloaded: number; total: number | null }>) {
  const total = 14_000_000;
  for (let downloaded = 0; downloaded < total; ) {
    await pause(150);
    downloaded = Math.min(total, downloaded + 700_000);
    onProgress.onmessage({ downloaded, total });
  }
}

// ------------------------------------------------------------------ AI (?ai)

/**
 * `?ai` (or `?ai=ready`): signed in to GitHub, Copilot downloaded; `?ai=new`:
 * signed in, Copilot still to download; `?ai=signedout` (also without the
 * flag): signing in shows a code and succeeds; `?ai=nosub`: Copilot cannot
 * start; `?ai=error`: questions fail.
 */
const aiMode = searchParams.get("ai") ?? (searchParams.has("ai") ? "ready" : "signedout");
let aiSignedIn = aiMode !== "signedout";
let aiInstalled = aiMode === "ready" || aiMode === "error";
const aiCancelled = new Set<string>();

function fakeAiStatus() {
  return {
    signedIn: aiSignedIn,
    login: aiSignedIn ? "octocat" : null,
    source: aiSignedIn ? { kind: "keychain" } : null,
    expired: false,
    installed: aiInstalled,
  };
}

async function fakeAiPrepare(onEvent: Channel<AiEvent>) {
  if (!aiSignedIn) throw "Sign in to GitHub to use Copilot.";
  if (!aiInstalled) {
    for (let percent = 0; percent <= 100; percent += 10) {
      onEvent.onmessage({ kind: "status", text: `Downloading GitHub Copilot CLI… ${percent}% of 114 MB` });
      await pause(150);
    }
    aiInstalled = true;
  }
  onEvent.onmessage({ kind: "status", text: "Starting GitHub Copilot…" });
  await pause(300);
  if (aiMode === "nosub") throw "GitHub did not accept the sign-in for Copilot. Sign out and sign in again.";
}

/** A canned answer that names what it was asked about, so the preview shows what was sent. */
function fakeAnswer(request: AskRequest): string {
  const files = [...new Set(request.context.flatMap((item) => [...item.text.matchAll(/^\+\+\+ b\/(.+)$/gm)].map((m) => m[1])))];
  const selection = request.context.find((item) => item.kind === "selection");
  const first = files[0] ?? selection?.label.split(",")[0] ?? "Orders.fs";
  const sent = request.context.map((item) => `- **${item.kind}** · ${item.label} (${item.text.length.toLocaleString()} characters)`).join("\n");
  return [
    `This is the browser preview's stand-in for GitHub Copilot, answering **${request.question}**`,
    "",
    selection
      ? `The selected code (\`${selection.label}\`) computes the order total and applies the discount from \`${first}:12\`.`
      : `The changes touch ${files.length || "no"} file${files.length === 1 ? "" : "s"}${files.length ? `: ${files.map((file) => `\`${file}\``).join(", ")}` : ""}. The main change is in \`${first}:12\`.`,
    "",
    "```fsharp",
    "let total (order: Order) =",
    "    order.Lines |> List.sumBy (fun line -> line.Price * decimal line.Quantity)",
    "```",
    "",
    "### Worth checking",
    "- Rounding happens before the discount is applied.",
    "",
    "<details><summary>What was sent</summary>",
    "",
    sent || "- nothing but the question",
    "",
    "</details>",
  ].join("\n");
}

/**
 * A stand-in review: suggestions for lines of the first changed files, and
 * one whose lines are not in the code (left out by the app). `?ai=noreview`
 * finds nothing, `?ai=badreview` answers with something that is not JSON.
 */
function fakeReview(request: AskRequest): string {
  if (aiMode === "badreview") return "Here are my suggestions: rename things.";
  if (aiMode === "noreview") return JSON.stringify({ summary: "The changes follow the repository's patterns.", suggestions: [] });
  const files = request.context
    .filter((item) => item.kind === "file" && item.label.endsWith("(new version, whole file)"))
    .map((item) => ({ path: item.label.replace(/ \(new version, whole file\)$/, ""), lines: item.text.split("\n").map((line) => line.replace(/^ *\d+\| /, "")) }));
  const suggestions = files.slice(0, 3).flatMap((file, index) => {
    const at = file.lines.findIndex((line, number) => number >= 2 && line.trim().length >= 8);
    if (at === -1) return [];
    const original = file.lines[at];
    const indent = /^\s*/.exec(original)![0];
    const pattern = index % 2 === 0;
    return [
      {
        path: file.path,
        line: at + 1,
        original,
        replacement: pattern ? `${original.trimEnd()}\n${indent}// As the other files of this folder do it.` : `${indent}// Guarded, as an empty list would fail here.\n${original}`,
        title: pattern ? "Follow the folder's naming pattern" : "Handle the empty case",
        why: pattern
          ? `The browser preview's stand-in: the other files of this folder name this the other way, as in \`${file.path}:1\`.`
          : "The browser preview's stand-in: an empty list is a well-known edge case that fails here.",
        basis: pattern ? "pattern" : "proven",
      },
    ];
  });
  suggestions.push({
    path: files[0]?.path ?? "src/Orders.fs",
    line: 3,
    original: "this line is not in the file",
    replacement: "nor is this",
    title: "A suggestion whose lines are not there",
    why: "Left out by the app.",
    basis: "proven",
  });
  return JSON.stringify(
    {
      summary: `The browser preview's stand-in for a review of ${files.length} changed file${files.length === 1 ? "" : "s"}. The changes look sound; ${suggestions.length - 1} thing${suggestions.length === 2 ? "" : "s"} could follow the repository more closely.`,
      suggestions,
    },
    null,
    2,
  );
}

async function fakeAiAsk(request: AskRequest, onEvent: Channel<AiEvent>) {
  aiCancelled.delete(request.conversation);
  await pause(400);
  if (aiMode === "error") throw "The signed-in GitHub account may have no Copilot plan.";
  onEvent.onmessage({ kind: "reasoning", text: "Reading the changes…" });
  await pause(300);
  const answer = request.purpose === "review" ? fakeReview(request) : fakeAnswer(request);
  for (let at = 0; at < answer.length; at += 14) {
    if (aiCancelled.has(request.conversation)) break;
    onEvent.onmessage({ kind: "delta", text: answer.slice(at, at + 14) });
    await pause(25);
  }
  onEvent.onmessage({ kind: "done", model: "GPT-6 Luna" });
}

/** A rough unified diff: the lines between the common start and end of the two texts. */
function fakeFileDiff(path: string, before: string | null, after: string | null): string {
  const a = before === null ? [] : before.split("\n");
  const b = after === null ? [] : after.split("\n");
  let start = 0;
  while (start < a.length && start < b.length && a[start] === b[start]) start++;
  let end = 0;
  while (end < a.length - start && end < b.length - start && a[a.length - 1 - end] === b[b.length - 1 - end]) end++;
  const removed = a.slice(start, a.length - end);
  const added = b.slice(start, b.length - end);
  return [
    `diff --git a/${path} b/${path}`,
    `--- ${before === null ? "/dev/null" : `a/${path}`}`,
    `+++ ${after === null ? "/dev/null" : `b/${path}`}`,
    `@@ -${start + 1},${removed.length} +${start + 1},${added.length} @@`,
    ...removed.map((line) => `-${line}`),
    ...added.map((line) => `+${line}`),
    "",
  ].join("\n");
}

/** The new version of `changed`, as `review_context` reads it; no neighbours. */
function fakeReviewContext(cwd: string, of: PatchOf, changed: string[]): ContextFile[] {
  return fakePairs(cwd, of)
    .filter(([path, , after]) => changed.includes(path) && after !== null)
    .map(([path, , after]) => ({ path, text: after!, changed: true }));
}

function fakePatch(cwd: string, of: PatchOf, paths: string[]): Patch {
  const pairs = fakePairs(cwd, of);
  const wanted = (path: string) => paths.length === 0 || paths.some((p) => path === p || path.startsWith(`${p.replace(/\/$/, "")}/`));
  const changed = pairs.filter(([path, before, after]) => before !== after && wanted(path)).sort(([a], [b]) => a.localeCompare(b));
  return {
    text: changed.map(([path, before, after]) => fakeFileDiff(path, before, after)).join(""),
    files: changed.map(([path]) => path),
    omitted: [],
  };
}

/** Each file's text before and after, as `of` compares them. */
function fakePairs(cwd: string, of: PatchOf): [string, string | null, string | null][] {
  let pairs: [string, string | null, string | null][];
  if (of.kind === "commits" || of.kind === "onDisk") {
    const set = ownDiffOf(of.kind === "commits" ? of.to : undefined)?.files ?? files;
    const tree = of.kind === "onDisk" ? treeOf(cwd) : null;
    const all = new Set([...set.keys(), ...(tree?.keys() ?? [])]);
    pairs = [...all].map((path) => [path, set.get(path)?.base ?? null, tree ? (tree.get(path) ?? null) : (set.get(path)?.working ?? null)]);
  } else {
    checkoutState();
    const [from, to] = of.kind === "unstaged" ? [index, worktree] : [head ?? new Map<string, string>(), index];
    pairs = [...new Set([...from.keys(), ...to.keys()])].map((path) => [path, from.get(path) ?? null, to.get(path) ?? null]);
  }
  return pairs;
}

// `?slow` (or `?slow=3000`): every git and Azure DevOps command takes that
// long (default 1500 ms), to see what the app shows while it waits.
const slowMs = searchParams.has("slow") ? Number(searchParams.get("slow")) || 1500 : 0;
const SLOW_SKIPS = /^(dev_trace|startup_path|plugin:|lsp_|ai_|save_worktree_file|updater_info|check_for_update|download_update)/;

function slowed(handle: (command: string, payload: unknown) => unknown) {
  return (command: string, payload: unknown) =>
    slowMs && !SLOW_SKIPS.test(command)
      ? new Promise((resolve) => setTimeout(resolve, slowMs)).then(() => handle(command, payload))
      : handle(command, payload);
}

export function install() {
  mockWindows("main");
  starFavorites();
  mockIPC(
    slowed((command, payload) => {
      const args = (payload ?? {}) as Record<string, string>;
      switch (command) {
        case "startup_path":
          return ROOT;
        case "dev_trace":
          return null;
        case "open_repo":
          // Any path opens the same files, under that path's name.
          return { ...repo, root: args.path, branches: branchesWithSync() };
        case "list_branches":
          return branchesWithSync();
        case "compare_branches": {
          const own = ownDiffOf(args.head);
          const list = own?.commits ?? commits;
          const wanted = (payload as { range: CommitRange | null }).range;
          const newest = list.findIndex((commit) => commit.sha === wanted?.newest);
          const oldest = list.findIndex((commit) => commit.sha === wanted?.oldest);
          const range = newest === -1 || oldest === -1 ? null : { newest: list[newest].sha, oldest: list[oldest].sha };
          return {
            base: args.base,
            head: args.head,
            mergeBase: range ? (list[oldest + 1]?.sha ?? MERGE_BASE) : MERGE_BASE,
            headCommit: range?.newest ?? list[0].sha,
            tipCommit: list[0].sha,
            range,
            commits: list.map(({ paths: _, ...commit }) => commit),
            files: changedFiles(range ? list.slice(newest, oldest + 1) : list, own?.files ?? files, list),
            checkout: null,
            uncommitted: 0,
            ...(range ? null : comparedOnDisk(args.head, args.checkout ?? null)),
          } satisfies Comparison;
        }
        case "pull_request_checkout": {
          const { id, branch, create, placement } = payload as { id: number; branch: string | null; create: boolean; placement: Placement };
          return slowly(() => pullRequestCheckout(args.root, id, args.commit, branch, create, placement), 400);
        }
        case "branch_checkout":
          return slowly(() => branchCheckout(args.branch, (payload as { placement: Placement }).placement), 300);
        case "list_files": {
          if (args.checkout) {
            checkoutState();
            const tree = treeOf(args.checkout);
            return [...tree.keys()].filter((path) => !isIgnored(path, tree)).sort();
          }
          addUnchangedFiles();
          return [...(ownDiffOf(args.commit)?.files ?? files)]
            .filter(([, file]) => file.working !== null)
            .map(([path]) => path)
            .sort();
        }
        case "read_texts": {
          const texts = args.checkout ? treeOf(args.checkout) : null;
          const set = ownDiffOf(args.commit)?.files ?? files;
          return (payload as { paths: string[] }).paths.map((path) => (texts ? (texts.get(path) ?? null) : (set.get(path)?.working ?? null)));
        }
        case "read_file_pair": {
          const set = ownDiffOf(args.commit)?.files ?? files;
          const file = set.get(args.path);
          if (args.checkout) checkoutState();
          return {
            base: set.get(args.oldPath ?? args.path)?.base ?? null,
            working: args.checkout ? (treeOf(args.checkout).get(args.path) ?? null) : (file?.working ?? null),
            committed: args.checkout && treeOf(args.checkout).has(args.path) ? (tipFor(args.checkout).get(args.path) ?? null) : null,
            binary: false,
            tooLarge: false,
          } satisfies FilePair;
        }
        case "working_status":
          return workingStatus();
        case "uncommitted_count": {
          const work = workingStatus();
          return new Set([...work.unstaged, ...work.staged].map((file) => file.path)).size;
        }
        case "stop_watching_checkout":
          return null;
        case "read_working_pair": {
          checkoutState();
          const side = (payload as { side: WorkingSide | "committed" | "conflicted" }).side;
          const [from, to] = side === "unstaged" ? [index, worktree] : side === "staged" ? [head!, index] : [head!, worktree];
          return {
            base: from.get(args.oldPath ?? args.path) ?? null,
            working: to.get(args.path) ?? null,
            binary: false,
            tooLarge: false,
          } satisfies FilePair;
        }
        case "worktree_files":
          checkoutState();
          return [...new Set([...worktree.keys(), ...index.keys()])].filter((path) => !isIgnored(path, worktree)).sort();
        case "stage_files":
          checkoutState();
          for (const path of (payload as { paths: string[] }).paths) {
            const text = worktree.get(path);
            if (text === undefined) index.delete(path);
            else index.set(path, text);
            conflicted.delete(path);
          }
          return checkoutChanged(null);
        case "unstage_files":
          checkoutState();
          for (const path of (payload as { paths: string[] }).paths) {
            const text = head!.get(path);
            if (text === undefined) index.delete(path);
            else index.set(path, text);
          }
          return checkoutChanged(null);
        case "discard_files":
          checkoutState();
          for (const path of (payload as { paths: string[] }).paths) {
            const text = index.get(path);
            if (conflicted.has(path)) worktree.set(path, ordersInConflict(head!.get(path)!));
            else if (text === undefined) worktree.delete(path);
            else worktree.set(path, text);
          }
          return checkoutChanged(null);
        case "delete_files":
          checkoutState();
          for (const path of (payload as { paths: string[] }).paths) worktree.delete(path);
          return checkoutChanged(null);
        case "ignore_path":
          return ignorePath(args.checkout, args.path, (payload as { folder: boolean }).folder);
        case "path_tracked":
          return trackedUnder(args.checkout, args.path).length > 0;
        case "untrack_path":
          return untrackPath(args.checkout, args.path);
        case "reveal_path":
          // No file manager in the browser preview.
          console.info("devMock: reveal", args.checkout, args.path);
          return null;
        case "undo_file_edits": {
          checkoutState();
          const tree = treeOf(args.checkout);
          const tip = tipFor(args.checkout);
          for (const path of (payload as { paths: string[] }).paths) {
            const text = tip.get(path);
            if (text === undefined) {
              if (tree === worktree) index.delete(path);
              tree.delete(path);
            } else {
              if (tree === worktree) index.set(path, text);
              tree.set(path, text);
            }
          }
          return checkoutChanged(null);
        }
        case "reset_files_to_head":
          checkoutState();
          for (const path of (payload as { paths: string[] }).paths) {
            const text = head!.get(path);
            if (text === undefined) {
              index.delete(path);
            } else {
              index.set(path, text);
              worktree.set(path, text);
            }
          }
          return checkoutChanged(null);
        case "set_index_text":
          checkoutState();
          index.set(args.path, args.text);
          return checkoutChanged(null);
        case "read_worktree_text":
          checkoutState();
          return treeOf(args.checkout).get(args.path) ?? null;
        case "save_worktree_file":
          checkoutState();
          treeOf(args.checkout).set(args.path, args.text);
          return checkoutChanged(null);
        case "commit_staged":
          return slowly(() => commitStaged(args.message), 500);
        case "required_branch_policies":
          return slowly(() => requiredPolicies(args.branch), 300);
        case "start_branch":
          return slowly(() => startBranch(args.name), 300);
        case "push_checkout_branch":
          return slowly(() => pushBranch(args.checkout), 700);
        case "fetch_all":
          return slowly(fetchAll, 900);
        case "pull_checkout_branch":
          return slowly(pullBranch, 1200);
        case "branch_creators":
          return slowly(branchCreators, 300);
        case "check_formatting":
          return slowly(formatCheck, 600);
        case "format_and_stage":
          return slowly(() => {
            formatted = true;
            return { staged: args.paths, failed: [] };
          }, 800);
        case "pull_request_remotes":
          return remotesOf(args.root);
        case "list_pull_requests":
          return slowly(() => pullRequestsOf(args.root));
        case "favorite_pull_request_remotes":
          return slowly(() => remotesOf(args.path), 200);
        case "list_favorite_pull_requests":
          return slowly(() => pullRequestsOf(args.path));
        case "favorite_pull_request_open_threads":
          return slowly(() => openThreadsOf(args.id), 300);
        case "set_pull_request_token":
          return slowly(() => {
            if (args.token.trim() === "bad") throw { kind: "tokenRejected", message: "" };
            mockToken = "keychain";
            tokenWorks = true;
            return pullRequestList().map(asSeenByYou);
          }, 800);
        case "remove_pull_request_token":
          mockToken = null;
          return null;
        case "open_pull_request":
          return slowly(() => {
            checkToken();
            return asSeenByYou(findPullRequest(args.id));
          }, 700);
        case "read_pull_request":
          return slowly(() => {
            checkToken();
            return asSeenByYou(findPullRequest(args.id));
          }, 300);
        case "notify":
          notifications.push({ title: args.title, body: args.body, key: args.key });
          console.info(`devMock: notification "${args.title}": ${args.body}`);
          return null;
        case "vote_on_pull_request":
          return slowly(() => {
            checkToken();
            checkWrite();
            const found = findPullRequest(args.id);
            const vote = (payload as { vote: Vote }).vote;
            const mine = found.reviewers.find((entry) => entry.id === YOU.id);
            if (mine) mine.vote = vote;
            else found.reviewers.push({ ...reviewer(YOU.name, vote), id: YOU.id });
            return asSeenByYou(found);
          }, 600);
        case "completion_policies":
          return slowly(() => {
            checkToken();
            return completionPolicies(args.id);
          }, 600);
        case "requeue_policy":
          return slowly(() => {
            checkToken();
            requeuedPolicies.add(String(args.evaluationId));
          }, 600);
        case "update_pull_request":
          return slowly(() => {
            checkToken();
            checkWrite();
            const found = findPullRequest(args.id);
            const change = (payload as { change: PrChange }).change;
            console.info("devMock: update pull request", found.id, change);
            if (change.kind === "complete") {
              if (found.draft) throw { kind: "other", message: "Azure DevOps answered with status 400: TF401395: Draft pull requests cannot be completed." };
              if (found.mergeStatus === "conflicts") {
                throw { kind: "other", message: `Azure DevOps answered with status 409: TF401181: The pull request cannot be completed because it has merge conflicts.` };
              }
              if (change.sourceCommit !== found.sourceCommit) {
                throw { kind: "other", message: "Azure DevOps answered with status 409: TF401192: The source branch has been updated since the changes were reviewed." };
              }
              found.status = "completed";
              found.mergeStatus = "queued";
              // The merge finishes a moment later.
              setTimeout(() => (found.mergeStatus = "succeeded"), 2000);
            } else if (change.kind === "abandon") found.status = "abandoned";
            else if (change.kind === "reactivate") found.status = "active";
            else found.draft = change.draft;
            return asSeenByYou(found);
          }, 900);
        case "plan_pull_request":
          return slowly(() => {
            checkToken();
            return planPullRequest(args.head, args.base, args.commit);
          }, 600);
        case "work_item":
          return slowly(() => {
            checkToken();
            const id = Number(args.id);
            // `?workitems=noaccess`: the token may not read work items. Numbers from 90000 do not exist.
            if (searchParams.get("workitems") === "noaccess") return { kind: "noAccess" };
            if (id >= 90000) return { kind: "notFound" };
            return {
              kind: "found",
              item: {
                id,
                title: id === 1234 ? "Loyalty discount for returning customers" : `Work item ${id} (dev mock)`,
                kind: id % 2 === 0 ? "User Story" : "Bug",
                state: "Active",
                url: `https://dev.azure.com/contoso/Shop/_workitems/edit/${id}`,
              },
            };
          }, 400);
        case "create_pull_request":
          return slowly(() => {
            checkToken();
            const { request } = payload as { request: NewPullRequest };
            const plan = planPullRequest(request.head, request.base, request.commit);
            if (plan.existing) throw { kind: "other", message: `Pull request !${plan.existing.id} is open already.` };
            if (plan.push === "conflict") throw { kind: "other", message: `${plan.sourceBranch} on Azure DevOps has commits that are not in what you reviewed.` };
            checkWrite();
            console.info("devMock: create pull request", plan.push === "none" ? "(no push)" : `after pushing ${request.commit}`, request);
            serverBranches.set(plan.sourceBranch, request.commit);
            const created: PullRequest = {
              id: nextPullRequestId++,
              title: request.title,
              description: request.description,
              author: YOU.name,
              authorId: YOU.id,
              yours: true,
              created: new Date().toISOString(),
              draft: request.draft,
              status: "active",
              sourceBranch: plan.sourceBranch,
              targetBranch: plan.targetBranch,
              sourceCommit: request.commit,
              targetCommit: MERGE_BASE,
              url: `https://dev.azure.com/contoso/Shop/_git/shop-api/pullrequest/${nextPullRequestId - 1}`,
              reviewers: [reviewer("[Shop]\\Shop Team", "none", true)],
              mergeStatus: "succeeded",
            };
            mockPullRequests.unshift(created);
            return asSeenByYou(created);
          }, 1500);
        case "create_pull_request_thread":
          return slowly(() => {
            checkToken();
            checkWrite();
            const { place, content } = payload as { place: NewThreadPlace | null; content: string };
            const thread: PrThread = {
              id: nextThreadId++,
              path: place?.path ?? null,
              position: place?.lines ? { side: place.lines.side, startLine: place.lines.startLine, endLine: place.lines.endLine } : null,
              status: "active",
              comments: [comment(1, "You", content, 0)],
            };
            threadsOf(args.id).push(thread);
            return thread;
          }, 600);
        case "reply_to_pull_request_thread":
          return slowly(() => {
            checkToken();
            checkWrite();
            const { thread, parent, content } = payload as { thread: number; parent: number; content: string };
            const found = threadsOf(args.id).find((candidate) => candidate.id === thread);
            if (!found) throw { kind: "notFound", message: `Thread ${thread} does not exist.` };
            found.comments.push(comment(found.comments.length + 1, "You", content, 0, parent));
            return null;
          }, 600);
        case "edit_pull_request_comment":
          return slowly(() => {
            checkToken();
            checkWrite();
            const { thread, comment: id, content } = payload as { thread: number; comment: number; content: string };
            const found = threadsOf(args.id)
              .find((candidate) => candidate.id === thread)
              ?.comments.find((candidate) => candidate.id === id);
            if (!found) throw { kind: "notFound", message: `Comment ${id} does not exist.` };
            if (!found.you) throw { kind: "other", message: "Only its author may edit a comment." };
            found.content = content;
            found.edited = true;
            return null;
          }, 600);
        case "delete_pull_request_comment":
          return slowly(() => {
            checkToken();
            checkWrite();
            const { thread, comment: id } = payload as { thread: number; comment: number };
            const threads = threadsOf(args.id);
            const found = threads.find((candidate) => candidate.id === thread);
            const index = found?.comments.findIndex((candidate) => candidate.id === id) ?? -1;
            if (!found || index < 0) throw { kind: "notFound", message: `Comment ${id} does not exist.` };
            if (!found.comments[index].you) throw { kind: "other", message: "Only its author may delete a comment." };
            found.comments.splice(index, 1);
            // A thread without comments is gone.
            if (found.comments.length === 0) threads.splice(threads.indexOf(found), 1);
            return null;
          }, 500);
        case "accept_pull_request_suggestion":
          return slowly(() => {
            checkToken();
            checkWrite();
            const { thread, commit } = payload as { thread: number; commit: string };
            return acceptSuggestion(args.id, thread, commit);
          }, 900);
        case "pull_request_threads":
          return slowly(() => {
            checkToken();
            // Copies, as from the real backend.
            return structuredClone(threadsOf(args.id));
          }, 400);
        case "plugin:opener|open_url":
          console.info("devMock: would open", args.url);
          return null;
        // Updates: ?update=none (up to date), ?update=offline (the check fails), else 0.1.80 is found.
        case "ai_status":
          return slowly(fakeAiStatus, 200);
        case "ai_sign_in": {
          const onEvent = (payload as { onEvent: Channel<{ kind: "code"; userCode: string; verificationUri: string }> }).onEvent;
          return (async () => {
            await pause(400);
            onEvent.onmessage({ kind: "code", userCode: "WDJB-MJHT", verificationUri: "https://github.com/login/device" });
            await pause(3000);
            aiSignedIn = true;
            return "octocat";
          })();
        }
        case "ai_cancel_sign_in":
          return null;
        case "ai_sign_out":
          aiSignedIn = false;
          return null;
        case "ai_prepare":
          return fakeAiPrepare((payload as { onEvent: Channel<AiEvent> }).onEvent);
        case "ai_ask":
          return fakeAiAsk((payload as { request: AskRequest }).request, (payload as { onEvent: Channel<AiEvent> }).onEvent);
        case "ai_cancel":
          aiCancelled.add(args.conversation);
          return null;
        case "ai_end":
          return null;
        case "ai_models":
          return [
            { id: "gpt-6-luna", name: "GPT-6 Luna" },
            { id: "claude-sonnet-5-5", name: "Claude Sonnet 5.5" },
          ];
        case "review_patch":
          return slowly(() => fakePatch(args.cwd, (payload as { of: PatchOf }).of, (payload as { paths: string[] }).paths), 150);
        case "review_context":
          return slowly(() => fakeReviewContext(args.cwd, (payload as { of: PatchOf }).of, (payload as { changed: string[] }).changed), 150);
        case "updater_info":
          return { version: "0.1.77", canInstall: true };
        case "check_for_update":
          return slowly(() => {
            if (searchParams.get("update") === "offline") throw "error sending request for url (https://github.com/reaptor/arnold/releases/latest/download/latest.json)";
            if (searchParams.get("update") === "none") return null;
            return { version: "0.1.80", notes: FAKE_RELEASE_NOTES };
          }, 800);
        case "download_update":
          return fakeDownload((payload as { onProgress: Channel<{ downloaded: number; total: number | null }> }).onProgress);
        case "install_update":
          return slowly(() => {
            throw "The browser preview cannot restart.";
          }, 800);
        case "prepare_workspace":
          return fakePrepare(
            (payload as { onEvent: Channel<SetupEvent> }).onEvent,
            args.root,
            (payload as { inPlace: boolean }).inPlace,
            (payload as { trusted: boolean }).trusted,
          );
        case "build_solution":
          return fakeBuild((payload as { onEvent: Channel<BuildEvent> }).onEvent, args.root);
        case "cancel_build":
          building++;
          return null;
        case "lsp_start": {
          if (!fakeServer) throw new Error("No language server in the browser preview (add ?lsp for a stand-in).");
          serverDocs.clear();
          const channels = payload as { onMessage: Channel<string>; onEvent: Channel<LspEvent> };
          serverChannel = channels.onMessage;
          const events = channels.onEvent;
          void fakeProjectLoading(events);
          return 1;
        }
        case "lsp_send": {
          const message = JSON.parse(args.message) as { id?: number };
          let result: unknown;
          let error: { code: number; message: string } | undefined;
          try {
            result = fakeServer ? fakeServerAnswer(args.message) : undefined;
          } catch (thrown) {
            error = { code: -32603, message: String((thrown as Error).message) };
          }
          if (message.id !== undefined) {
            const reply = JSON.stringify({ jsonrpc: "2.0", id: message.id, ...(error ? { error } : { result: result ?? null }) });
            setTimeout(() => serverChannel?.onmessage(reply));
          }
          return null;
        }
        case "lsp_open_background":
          // Known to the stand-in server, so a rename reaches it too.
          serverDocs.set(args.uri, args.text);
          return null;
        case "lsp_stop":
          return null;
        case "plugin:dialog|message": {
          // Always press the confirming button: a blocking browser dialog
          // would stall automated UI checks.
          const buttons = (payload as { buttons?: unknown }).buttons;
          if (buttons && typeof buttons === "object" && "OkCancelCustom" in buttons) {
            return (buttons as { OkCancelCustom: string[] }).OkCancelCustom[0];
          }
          return buttons === "YesNo" ? "Yes" : "Ok";
        }
        case "plugin:window|is_maximized":
        case "plugin:window|is_fullscreen":
          return false;
        case "plugin:window|minimize":
        case "plugin:window|toggle_maximize":
        case "plugin:window|close":
          return null;
        default:
          console.warn("devMock: unhandled command", command, payload);
          return null;
      }
    }),
    { shouldMockEvents: true },
  );
}
