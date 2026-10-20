// Naming a new branch for a commit that cannot go on the branch checked out.

/** A branch name from the commit message's first line: `fix-the-order-total`. */
export function branchNameFor(message: string): string {
  const words = (message.split("\n")[0] ?? "")
    .toLowerCase()
    .normalize("NFKD")
    .replace(/[\u0300-\u036f]/g, "")
    .split(/[^a-z0-9]+/)
    .filter(Boolean);
  let name = "";
  for (const word of words) {
    if (name && name.length + word.length + 1 > 40) break;
    name = name ? `${name}-${word}` : word.slice(0, 40);
  }
  return name;
}
