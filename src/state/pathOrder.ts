// The order files are listed in: the order of the file explorer.

/** Names compare case-insensitively, with numbers by value ("File2" before "File10"). */
export const collator = new Intl.Collator(undefined, { numeric: true, sensitivity: "base" });

/**
 * Orders paths as the file explorer shows them: within a folder, its folders
 * come before its files, and each by name.
 */
export function comparePaths(a: string, b: string): number {
  const left = a.split("/");
  const right = b.split("/");
  for (let index = 0; index < Math.min(left.length, right.length); index++) {
    if (left[index] === right[index]) continue;
    const leftIsFolder = index < left.length - 1;
    const rightIsFolder = index < right.length - 1;
    if (leftIsFolder !== rightIsFolder) return leftIsFolder ? -1 : 1;
    return collator.compare(left[index], right[index]) || (left[index] < right[index] ? -1 : 1);
  }
  return left.length - right.length;
}
