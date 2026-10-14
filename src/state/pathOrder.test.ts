import { describe, expect, it } from "vitest";
import { comparePaths } from "./pathOrder";

describe("file order", () => {
  it("puts the folders of a folder before its files", () => {
    const paths = ["README.md", "src/main.fs", "src/Core/Types.fs", "build.fsx", "src/App.fs", "docs/a.md"];
    expect(paths.sort(comparePaths)).toEqual([
      "docs/a.md",
      "src/Core/Types.fs",
      "src/App.fs",
      "src/main.fs",
      "build.fsx",
      "README.md",
    ]);
  });

  it("orders names ignoring case and with numbers by value", () => {
    expect(["b/File10.fs", "b/file2.fs", "B2/x.fs", "a/x.fs"].sort(comparePaths)).toEqual([
      "a/x.fs",
      "b/file2.fs",
      "b/File10.fs",
      "B2/x.fs",
    ]);
  });
});
