// Stand-in backend for working on the UI in a plain browser (`npm run dev`
// without Tauri). It serves a small in-memory repository; nothing touches disk.
// Never loaded in the real app.

import { mockIPC, mockWindows } from "@tauri-apps/api/mocks";
import type { ChangedFile, Comparison, FilePair, RepoInfo } from "./commands";

interface FakeFile {
  base: string | null;
  working: string | null;
  untracked?: boolean;
}

const ORDERS_BASE = `module ZedTrial.Orders

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

const files = new Map<string, FakeFile>([
  ["Orders.fs", { base: ORDERS_BASE, working: ORDERS_WORKING }],
  ["Pricing.fs", { base: PRICING_BASE, working: PRICING_WORKING }],
  ["src/Api/OrderController.cs", { base: CSHARP_BASE, working: CSHARP_WORKING }],
  ["Audit.fs", { base: null, working: 'module ZedTrial.Audit\n\nlet log (message: string) : unit =\n    printfn "[audit] %s" message\n', untracked: true }],
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

function changedFiles(): ChangedFile[] {
  const result: ChangedFile[] = [];
  for (const [path, file] of files) {
    if (file.base === file.working) continue;
    const status =
      file.base === null ? (file.untracked ? "untracked" : "added") : file.working === null ? "deleted" : "modified";
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
          return repo;
        case "compare_branches":
          return {
            base: args.base,
            head: args.head,
            mergeBase: "b481a92f122e44c0d840c722ded1ab11d334c7dd",
            worktreePath: repo.branches.find((branch) => branch.name === args.head)?.worktreePath ?? null,
            files: changedFiles(),
          } satisfies Comparison;
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
            eol: "lf",
            bom: false,
            binary: false,
            tooLarge: false,
          } satisfies FilePair;
        }
        case "write_file": {
          const file = files.get(args.path);
          if (file) file.working = args.content;
          return null;
        }
        case "reset_file": {
          const file = files.get(args.path);
          if (file) file.working = file.base;
          return null;
        }
        case "lsp_start":
          throw new Error("No language server in the browser preview.");
        case "lsp_send":
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
        default:
          console.warn("devMock: unhandled command", command, payload);
          return null;
      }
    },
    { shouldMockEvents: true },
  );
}
