import { beforeEach, describe, expect, it } from "vitest";
import type { ChangedFile } from "../ipc/commands";
import { changedBuildFiles, isApproved, isBuildFile, isTrusted, trust } from "./trust";

const stored = new Map<string, string>();
globalThis.localStorage = {
  getItem: (key: string) => stored.get(key) ?? null,
  setItem: (key: string, value: string) => void stored.set(key, value),
} as Storage;

const changed = (path: string, oldPath: string | null = null): ChangedFile => ({
  path,
  oldPath,
  status: oldPath ? "renamed" : "modified",
  additions: 1,
  deletions: 1,
  binary: false,
});

describe("build files", () => {
  it("knows the files restoring and building run code from", () => {
    for (const path of [
      "src/Api/Api.fsproj",
      "src/Web/Web.csproj",
      "Directory.Build.props",
      "src/Directory.Build.targets",
      "build/Custom.targets",
      "Directory.Packages.props",
      ".config/dotnet-tools.json",
      "paket.dependencies",
      "src/Api/paket.references",
      ".paket/Paket.Restore.targets",
      "global.json",
      "NuGet.Config",
      "Shop.sln",
      "elmish-land.json",
    ]) {
      expect(isBuildFile(path), path).toBe(true);
    }
  });

  it("leaves code and other files alone", () => {
    for (const path of ["src/Orders.fs", "build.fsx", "README.md", "src/Api/appsettings.json", "db/Orders.sql"]) {
      expect(isBuildFile(path), path).toBe(false);
    }
  });

  it("lists changed build files, also renamed from one", () => {
    expect(
      changedBuildFiles([changed("Orders.fs"), changed("src/Api/Api.fsproj"), changed("build/notes.txt", "build/Old.targets")]),
    ).toEqual(["src/Api/Api.fsproj", "build/notes.txt"]);
  });
});

describe("trust", () => {
  beforeEach(() => stored.clear());

  it("remembers trusted repositories", () => {
    expect(isTrusted("/src/shop")).toBe(false);
    trust("/src/shop");
    expect(isTrusted("/src/shop")).toBe(true);
    expect(isTrusted("/src/other")).toBe(false);
  });

  it("approves a branch's build files for one commit only", () => {
    trust("/src/shop", { name: "feature/x", commit: "abc" });
    expect(isTrusted("/src/shop")).toBe(true);
    expect(isApproved("/src/shop", "feature/x", "abc")).toBe(true);
    expect(isApproved("/src/shop", "feature/x", "def")).toBe(false);
    expect(isApproved("/src/shop", "feature/y", "abc")).toBe(false);
  });

  it("survives a broken store", () => {
    stored.set("arnold.trust", "not json");
    expect(isTrusted("/src/shop")).toBe(false);
    trust("/src/shop");
    expect(isTrusted("/src/shop")).toBe(true);
  });
});
