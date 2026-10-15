// Stand-in backend for working on the UI in a plain browser (`npm run dev`
// without Tauri). It serves a small in-memory repository; nothing touches disk.
// Never loaded in the real app.

import type { Channel } from "@tauri-apps/api/core";
import { mockIPC, mockWindows } from "@tauri-apps/api/mocks";
import type { ChangedFile, Commit, CommitRange, Comparison, FilePair, LspEvent, RepoInfo, SetupEvent } from "./commands";

interface FakeFile {
  base: string | null;
  working: string | null;
}

const ORDERS_BASE = `module ZedTrial.Orders

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
// PLANTED ERROR (check 4): a string where an int is expected.
let maxLinesPerOrder: int = "ten"

/// Total for a returning customer, formatted for display.
let loyaltyTotalText (order: Order) : string =
    order
    |> total
    |> Pricing.applyLoyaltyDiscount
    |> Pricing.roundMoney
    |> Currency.format
`;

const PRICING_BASE = `module ZedTrial.Pricing

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

const files = new Map<string, FakeFile>([
  ["Orders.fs", { base: ORDERS_BASE, working: ORDERS_WORKING }],
  ["Pricing.fs", { base: PRICING_BASE, working: PRICING_WORKING }],
  ["src/Api/OrderController.cs", { base: CSHARP_BASE, working: CSHARP_WORKING }],
  ["Audit.fs", { base: null, working: 'module ZedTrial.Audit\n\nlet log (message: string) : unit =\n    printfn "[audit] %s" message\n' }],
  ["db/Orders.sql", { base: SQL_BASE, working: SQL_WORKING }],
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

const ROOT = "/fake/zed-trial/repo";
const lineCount = (text: string | null) => (text ? text.split("\n").length - 1 : 0);

// `?lsp` starts a stand-in language server that only answers go-to-definition,
// with the first whole-word occurrence of the symbol in the same file, and
// reports every `open` as unused the way FsAutoComplete does.
const fakeServer = new URLSearchParams(location.search).has("lsp");

/** The open documents of the stand-in server, by URI. */
const serverDocs = new Map<string, string>();

let serverChannel: Channel<string> | null = null;

// The preparation of the F# workspace is simulated with pauses; `?setupfail`
// makes the build fail, `?setupslow` makes every step take 4 seconds.
const searchParams = new URLSearchParams(location.search);
const pause = (ms: number) => new Promise((resolve) => setTimeout(resolve, searchParams.has("setupslow") ? 4000 : ms));
let preparation = 0;

// Projects the stand-in server "loads"; `?projectfail` makes the second fail
// the way FsAutoComplete reports a missing package.
const FAKE_PROJECTS = ["src/Core/Core.fsproj", "src/Api/Api.fsproj", "tests/Tests.fsproj"].map(
  (project) => `/fake/arnold-tmp/repo/${project}`,
);

async function fakeProjectLoading(events: Channel<LspEvent>) {
  await pause(400);
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

async function fakePrepare(onEvent: Channel<SetupEvent>) {
  const run = ++preparation;
  const send = async (event: SetupEvent, ms = 0) => {
    if (ms) await pause(ms);
    if (run !== preparation) throw "cancelled";
    onEvent.onmessage(event);
  };
  await send({ kind: "step", title: "Copying 7d3e9a1f out of the repository" });
  await send({ kind: "skipped", title: "Restore packages", reason: "already restored for these project files" }, 600);
  await send({ kind: "step", title: "Building src/Api/Api.csproj (dotnet build): F# code uses its compiled assembly" });
  await send({ kind: "output", line: "  Determining projects to restore..." }, 500);
  if (searchParams.has("setupfail")) {
    await send({ kind: "output", line: "src/Api/Startup.cs(12,5): error CS1002: ; expected [src/Api/Api.csproj]" }, 800);
    await send({ kind: "warning", text: "dotnet build failed (exit status: 1)" });
  } else {
    await send({ kind: "output", line: "  Api -> /fake/arnold-tmp/repo/src/Api/bin/Debug/net10.0/Api.dll" }, 800);
  }
  return { path: "/fake/arnold-tmp/repo" };
}

function publishUnusedOpens(uri: string, text: string) {
  const diagnostics = text.split("\n").flatMap((line, index) => {
    const name = /^\s*open\s+(\S+)/.exec(line);
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
    publishUnusedOpens(params.textDocument.uri, params.textDocument.text);
  }
  if (message.method === "textDocument/didChange") {
    serverDocs.set(params.textDocument.uri, params.contentChanges.at(-1).text);
  }
  if (message.id === undefined) return undefined;
  if (message.method === "initialize") {
    return { capabilities: { textDocumentSync: 1, definitionProvider: true } };
  }
  if (message.method !== "textDocument/definition") return null;
  const uri = params.textDocument.uri;
  const lines = (serverDocs.get(uri) ?? "").split("\n");
  const line = lines[params.position.line] ?? "";
  const at = params.position.character;
  const start = line.slice(0, at).search(/\w+$/);
  const word = line.slice(start === -1 ? at : start).match(/^\w+/)?.[0];
  if (!word) return null;
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

// The branch's commits, newest first. Each changed file belongs to one of
// them: the oldest takes every file the others do not name.
const MERGE_BASE = "b481a92f122e44c0d840c722ded1ab11d334c7dd";
const TIP = "7d3e9a1f0c5b2e8d4a6f1b3c9e7d5a2f8b4c6e1d";
const hoursAgo = (hours: number) => new Date(Date.now() - hours * 3600_000).toISOString();
const commits: (Commit & { paths: string[] | null })[] = [
  { sha: TIP, subject: "Filter open orders to the last 30 days", author: "Agent Smith", date: hoursAgo(1), paths: ["db/Orders.sql"] },
  { sha: "5c2b8e14a7d93f60b1e2c4a8d7f9e3b6a1c5d204", subject: "Add loyalty discount to pricing", author: "Agent Smith", date: hoursAgo(5), paths: ["Pricing.fs", "Orders.fs"] },
  { sha: "e91f4d2a6b8c3e7f0a5d1b9c2e4f6a8d3b7c5e10", subject: "Validate unknown orders in the API and add an audit log", author: "Kristofer", date: hoursAgo(30), paths: null },
].map((commit) => ({ ...commit, shortSha: commit.sha.slice(0, 7) }));

const inCommits = (path: string, picked: readonly Commit[]) =>
  picked.some((commit) =>
    commits.find((candidate) => candidate.sha === commit.sha)?.paths?.includes(path) ??
    !commits.some((other) => other.paths?.includes(path)),
  );

function changedFiles(picked: readonly Commit[]): ChangedFile[] {
  const result: ChangedFile[] = [];
  for (const [path, file] of files) {
    if (file.base === file.working || !inCommits(path, picked)) continue;
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

const repo: RepoInfo = {
  root: ROOT,
  branches: [
    { name: "feature/agent-work", isRemote: false, worktreePath: ROOT },
    { name: "feature/agent-work-wt", isRemote: false, worktreePath: "/fake/zed-trial/repo-worktree" },
    { name: "main", isRemote: false, worktreePath: null },
    { name: "origin/main", isRemote: true, worktreePath: null },
  ],
  currentBranch: "feature/agent-work",
  defaultBranch: "main",
  worktrees: [ROOT, "/fake/zed-trial/repo-worktree"],
};

export function install() {
  mockWindows("main");
  mockIPC(
    (command, payload) => {
      const args = (payload ?? {}) as Record<string, string>;
      switch (command) {
        case "startup_path":
          return ROOT;
        case "dev_trace":
          return null;
        case "open_repo":
          // Any path opens the same files, under that path's name.
          return { ...repo, root: args.path };
        case "compare_branches": {
          const wanted = (payload as { range: CommitRange | null }).range;
          const newest = commits.findIndex((commit) => commit.sha === wanted?.newest);
          const oldest = commits.findIndex((commit) => commit.sha === wanted?.oldest);
          const range = newest === -1 || oldest === -1 ? null : { newest: commits[newest].sha, oldest: commits[oldest].sha };
          return {
            base: args.base,
            head: args.head,
            mergeBase: range ? (commits[oldest + 1]?.sha ?? MERGE_BASE) : MERGE_BASE,
            headCommit: range?.newest ?? TIP,
            tipCommit: TIP,
            range,
            commits: commits.map(({ paths: _, ...commit }) => commit),
            files: changedFiles(range ? commits.slice(newest, oldest + 1) : commits),
          } satisfies Comparison;
        }
        case "list_files": {
          // A few files that are not part of the diff, so the explorer has more to show.
          const unchanged = ["README.md", "Program.fs", "src/Api/Startup.cs", "src/Api/Models/Order.cs", "tests/OrdersTests.fs"];
          for (const path of unchanged) {
            if (!files.has(path)) files.set(path, { base: `// ${path}\n`, working: `// ${path}\n` });
          }
          return [...files]
            .filter(([, file]) => file.working !== null)
            .map(([path]) => path)
            .sort();
        }
        case "read_file_pair": {
          const file = files.get(args.path);
          return {
            base: file?.base ?? null,
            working: file?.working ?? null,
            binary: false,
            tooLarge: false,
          } satisfies FilePair;
        }
        case "prepare_workspace":
          return fakePrepare((payload as { onEvent: Channel<SetupEvent> }).onEvent);
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
          const result = fakeServer ? fakeServerAnswer(args.message) : undefined;
          if (message.id !== undefined) {
            const reply = JSON.stringify({ jsonrpc: "2.0", id: message.id, result: result ?? null });
            setTimeout(() => serverChannel?.onmessage(reply));
          }
          return null;
        }
        case "lsp_open_background":
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
    },
    { shouldMockEvents: true },
  );
}
