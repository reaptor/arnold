#!/usr/bin/env bash
# Creates (or resets) the Zed trial fixture next to this script:
#   repo/           main checkout, on feature/agent-work, with uncommitted + untracked changes
#   repo-worktree/  linked worktree, on feature/agent-work-wt, with a different planted error
# Re-run at any time to get back to the pristine state. Only these two folders are touched.
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo="$here/repo"
worktree="$here/repo-worktree"

rm -rf "$repo" "$worktree"
mkdir -p "$repo"
cd "$repo"
git init --quiet --initial-branch=main

commit() {
  git add --all
  git commit --quiet -m "$1" -m "Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
}

# ---------------------------------------------------------------- base (main)
cat > .gitignore <<'EOF'
bin/
obj/
EOF

cat > ZedTrial.fsproj <<'EOF'
<Project Sdk="Microsoft.NET.Sdk">

  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>net10.0</TargetFramework>
  </PropertyGroup>

  <ItemGroup>
    <Compile Include="Currency.fs" />
    <Compile Include="Pricing.fs" />
    <Compile Include="Orders.fs" />
    <Compile Include="Program.fs" />
  </ItemGroup>

</Project>
EOF

cat > Currency.fs <<'EOF'
module ZedTrial.Currency

/// Formats an amount as a price string, e.g. "12.50 SEK".
let format (amount: decimal) : string =
    sprintf "%.2f SEK" amount
EOF

cat > Pricing.fs <<'EOF'
module ZedTrial.Pricing

let vatRate = 0.25m

/// Adds VAT to a net amount.
let withVat (net: decimal) : decimal =
    net * (1m + vatRate)

/// Rounds to two decimals.
let roundMoney (amount: decimal) : decimal =
    System.Math.Round(amount, 2)
EOF

cat > Orders.fs <<'EOF'
module ZedTrial.Orders

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
EOF

cat > Program.fs <<'EOF'
module ZedTrial.Program

open ZedTrial.Orders

[<EntryPoint>]
let main _ =
    let order =
        { Id = 1
          Lines =
            [ { Sku = "A-100"; Quantity = 2; UnitPrice = 40m }
              { Sku = "B-200"; Quantity = 1; UnitPrice = 19.90m } ] }

    printfn "Order %d total: %M" order.Id (total order)
    0
EOF

commit "Initial order pricing app"
git branch feature/agent-work

# ------------------------------------- main moves on AFTER the branch point
# Neither change below may show up in the branch diff (check 1).
sed -i.bak 's/Order %d total: %M/Order %d total (incl. VAT): %M/' Program.fs && rm Program.fs.bak
cat > MAIN-ONLY.md <<'EOF'
This file was committed to main after feature/agent-work branched off.
It must NOT appear in the branch diff.
EOF
commit "Clarify total label (main only, after branch point)"

# ------------------------------------------- the agent's committed work
git checkout --quiet feature/agent-work

cat >> Pricing.fs <<'EOF'

let loyaltyRate = 0.05m

/// Discount for returning customers.
let applyLoyaltyDiscount (amount: decimal) : decimal =
    amount * (1m - loyaltyRate)
EOF

sed -i.bak 's|^/// Total including VAT\.$|/// Total including VAT, rounded to two decimals.|' Orders.fs && rm Orders.fs.bak
cat >> Orders.fs <<'EOF'

// PLANTED ERROR (check 4): a string where an int is expected.
let maxLinesPerOrder: int = "ten"

/// Total for a returning customer, formatted for display.
/// Go-to-definition targets (check 5): applyLoyaltyDiscount and Currency.format.
let loyaltyTotalText (order: Order) : string =
    order
    |> total
    |> Pricing.applyLoyaltyDiscount
    |> Pricing.roundMoney
    |> Currency.format
EOF

commit "Add loyalty discount"

# ------------------------------------------------- worktree copy (check 6)
git branch feature/agent-work-wt
git worktree add --quiet "$worktree" feature/agent-work-wt

# Uncommitted in the worktree: the Orders.fs error is fixed, a different one is planted.
(
  cd "$worktree"
  sed -i.bak 's|let maxLinesPerOrder: int = "ten"|let maxLinesPerOrder: int = 10|' Orders.fs && rm Orders.fs.bak
  sed -i.bak 's|^let loyaltyRate = 0.05m$|let loyaltyRate: decimal = "5%" // WORKTREE-ONLY planted error (check 6)|' Pricing.fs && rm Pricing.fs.bak
)

# ------------------------- uncommitted + untracked in the main checkout (check 2)
sed -i.bak 's|^let loyaltyRate = 0.05m$|let loyaltyRate = 0.10m // UNCOMMITTED edit (check 2)|' Pricing.fs && rm Pricing.fs.bak

cat > Audit.fs <<'EOF'
module ZedTrial.Audit

// UNTRACKED file (check 2): never added to git.
let log (message: string) : unit =
    printfn "[audit] %s" message
EOF

sed -i.bak 's|    <Compile Include="Pricing.fs" />|    <Compile Include="Pricing.fs" />\
    <Compile Include="Audit.fs" />|' ZedTrial.fsproj && rm ZedTrial.fsproj.bak

echo "Trial repo ready:"
echo "  $repo           (feature/agent-work, uncommitted + untracked changes)"
echo "  $worktree  (feature/agent-work-wt)"
