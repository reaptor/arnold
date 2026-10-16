// Stand-in backend for working on the UI in a plain browser (`npm run dev`
// without Tauri). It serves a small in-memory repository; nothing touches disk.
// Never loaded in the real app.

import type { Channel } from "@tauri-apps/api/core";
import { emit } from "@tauri-apps/api/event";
import { mockIPC, mockWindows } from "@tauri-apps/api/mocks";
import type {
  ChangedFile,
  Commit,
  CommitRange,
  Comparison,
  FilePair,
  LspEvent,
  NewPullRequest,
  NewPullRequestPlan,
  NewThreadPlace,
  PrChange,
  PrRemote,
  PrThread,
  PullRequest,
  RepoInfo,
  Reviewer,
  SetupEvent,
  Vote,
  WorkingSide,
  WorkingStatus,
} from "./commands";

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
  (project) => `/src/shop-api/.arnold/worktrees/feature-agent-work-5f3a9c21/${project}`,
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

async function fakePrepare(onEvent: Channel<SetupEvent>, root: string, inPlace: boolean) {
  const run = ++preparation;
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
  return { path };
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

// ------------------------------------------------------- uncommitted changes
// The checkout's HEAD is the branch tip (each file's `working` text).
// `?uncommitted` gives it an agent's uncommitted work: Orders.fs edited,
// Pricing.fs partly staged, a new Discounts.fs and Audit.fs deleted. Opening
// the repository then starts in Commit mode.

let head: Map<string, string> | null = null;
let index = new Map<string, string>();
let worktree = new Map<string, string>();

/** A few files that are not part of the diff, so the explorer has more to show. */
function addUnchangedFiles() {
  const unchanged = ["README.md", "Program.fs", "src/Api/Startup.cs", "src/Api/Models/Order.cs", "tests/OrdersTests.fs"];
  for (const path of unchanged) {
    if (!files.has(path)) files.set(path, { base: `// ${path}\n`, working: `// ${path}\n` });
  }
}

function checkoutState() {
  if (head) return;
  addUnchangedFiles();
  head = new Map([...files].flatMap(([path, file]) => (file.working === null ? [] : [[path, file.working] as const])));
  index = new Map(head);
  worktree = new Map(head);
  if (!searchParams.has("uncommitted")) return;
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
    "module ZedTrial.Discounts\n\n/// Seasonal discount rate.\nlet seasonal = 0.05m\n\nlet apply (rate: decimal) (amount: decimal) =\n    amount * (1m - rate)\n",
  );
  worktree.delete("Audit.fs");
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

function workingStatus(): WorkingStatus {
  checkoutState();
  return {
    branch: "feature/agent-work",
    head: commits[0].sha,
    unstaged: diffOf(index, worktree),
    staged: diffOf(head!, index),
  };
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
  if (staged.length === 0) throw "git commit failed: nothing to commit";
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
  return checkoutChanged({ sha, subject, branch: "feature/agent-work" });
}

// ------------------------------------------------------------ pull requests
// `?devops`: origin is an Azure DevOps repository with a saved token.
// `?devops=nopat`: no token yet (pasting "bad" is rejected, anything else works).
// `?devops=empty`: no active pull requests. `?devops=rejected`: the saved token no longer works.

const devops = searchParams.get("devops");
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
          id: 1287,
          title: "Add loyalty discount for returning customers",
          description: "Customers who come back get 10% off. Also rounds totals to two decimals.",
          author: "Ada Lovelace",
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
          author: "Bo Svensson",
          created: hoursAgo(20),
          draft: false,
          status: "active",
          sourceBranch: "fix/round-totals",
          targetBranch: "main",
          sourceCommit: TIP,
          targetCommit: MERGE_BASE,
          url: "https://dev.azure.com/contoso/Shop/_git/shop-api/pullrequest/1284",
          reviewers: [
            reviewer("Ada Lovelace", "approved"),
            reviewer("Cy Young", "approvedWithSuggestions"),
            reviewer("[Shop]\\Shop Team", "none", true),
          ],
          mergeStatus: "succeeded",
        },
        {
          id: 1281,
          title: "Open orders stored procedure: only the last 30 days",
          description: "",
          author: "Grace Hopper",
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

/** The pull request with the token owner marked among its reviewers, as the backend sends it. */
const asSeenByYou = (pullRequest: PullRequest): PullRequest =>
  structuredClone({ ...pullRequest, reviewers: pullRequest.reviewers.map((entry) => ({ ...entry, you: entry.id === YOU.id })) });

function findPullRequest(id: unknown): PullRequest {
  const found = mockPullRequests.find((pullRequest) => pullRequest.id === Number(id));
  if (!found) throw { kind: "notFound", message: `Pull request ${id} does not exist.` };
  return found;
}

// Discussions of every mock pull request (they all show the same files).
const comment = (id: number, author: string, content: string, hours: number, parentId = 0) => ({
  id,
  parentId,
  author,
  content,
  published: hoursAgo(hours),
  edited: false,
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
        case "list_branches":
          return repo.branches;
        case "compare_branches": {
          const wanted = (payload as { range: CommitRange | null }).range;
          const newest = commits.findIndex((commit) => commit.sha === wanted?.newest);
          const oldest = commits.findIndex((commit) => commit.sha === wanted?.oldest);
          const range = newest === -1 || oldest === -1 ? null : { newest: commits[newest].sha, oldest: commits[oldest].sha };
          return {
            base: args.base,
            head: args.head,
            mergeBase: range ? (commits[oldest + 1]?.sha ?? MERGE_BASE) : MERGE_BASE,
            headCommit: range?.newest ?? commits[0].sha,
            tipCommit: commits[0].sha,
            range,
            commits: commits.map(({ paths: _, ...commit }) => commit),
            files: changedFiles(range ? commits.slice(newest, oldest + 1) : commits),
          } satisfies Comparison;
        }
        case "list_files": {
          addUnchangedFiles();
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
        case "working_status":
          return workingStatus();
        case "stop_watching_checkout":
          return null;
        case "read_working_pair": {
          checkoutState();
          const side = (payload as { side: WorkingSide | "committed" }).side;
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
          return [...new Set([...worktree.keys(), ...index.keys()])].sort();
        case "stage_files":
          checkoutState();
          for (const path of (payload as { paths: string[] }).paths) {
            const text = worktree.get(path);
            if (text === undefined) index.delete(path);
            else index.set(path, text);
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
            if (text === undefined) worktree.delete(path);
            else worktree.set(path, text);
          }
          return checkoutChanged(null);
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
        case "save_worktree_file":
          checkoutState();
          worktree.set(args.path, args.text);
          return checkoutChanged(null);
        case "commit_staged":
          return slowly(() => commitStaged(args.message), 500);
        case "pull_request_remotes":
          return devops === null ? [] : [prRemote()];
        case "list_pull_requests":
          return slowly(() => {
            checkToken();
            return pullRequestList().map(asSeenByYou);
          });
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
            pullRequestThreads.push(thread);
            return thread;
          }, 600);
        case "reply_to_pull_request_thread":
          return slowly(() => {
            checkToken();
            checkWrite();
            const { thread, parent, content } = payload as { thread: number; parent: number; content: string };
            const found = pullRequestThreads.find((candidate) => candidate.id === thread);
            if (!found) throw { kind: "notFound", message: `Thread ${thread} does not exist.` };
            found.comments.push(comment(found.comments.length + 1, "You", content, 0, parent));
            return null;
          }, 600);
        case "pull_request_threads":
          return slowly(() => {
            checkToken();
            // Copies, as from the real backend.
            return structuredClone(pullRequestThreads);
          }, 400);
        case "plugin:opener|open_url":
          console.info("devMock: would open", args.url);
          return null;
        case "prepare_workspace":
          return fakePrepare(
            (payload as { onEvent: Channel<SetupEvent> }).onEvent,
            args.root,
            (payload as { inPlace: boolean }).inPlace,
          );
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
