import { describe, expect, it } from "vitest";
import { languageDescription, languageLoading } from "./languages";

const nameOf = (path: string) => languageDescription(path)?.name ?? null;

describe("languages by file name", () => {
  it("knows the popular languages by extension", () => {
    expect(nameOf("web/app.ts")).toBe("TypeScript");
    expect(nameOf("web/App.tsx")).toBe("TSX");
    expect(nameOf("web/app.js")).toBe("JavaScript");
    expect(nameOf("scripts/report.py")).toBe("Python");
    expect(nameOf("src/main.rs")).toBe("Rust");
    expect(nameOf("cmd/server.go")).toBe("Go");
    expect(nameOf("App.java")).toBe("Java");
    expect(nameOf("Build.kt")).toBe("Kotlin");
    expect(nameOf("native/lib.cpp")).toBe("C++");
    expect(nameOf("deploy.sh")).toBe("Shell");
    expect(nameOf("build.ps1")).toBe("PowerShell");
    expect(nameOf(".github/workflows/ci.yml")).toBe("YAML");
    expect(nameOf("Cargo.toml")).toBe("TOML");
    expect(nameOf("site/index.html")).toBe("HTML");
    expect(nameOf("site/style.scss")).toBe("SCSS");
  });

  it("knows files by name and by Arnold's aliases", () => {
    expect(nameOf("Dockerfile")).toBe("Dockerfile");
    expect(nameOf("docker/Dockerfile.dev")).toBe("Dockerfile");
    expect(nameOf("CMakeLists.txt")).toBe("CMake");
    expect(nameOf(".zsh")).toBe("Shell");
    expect(nameOf("tsconfig.jsonc")).toBe("JSON");
    expect(nameOf("Pages/Index.cshtml")).toBe("HTML");
    expect(nameOf("Shared/Nav.razor")).toBe("HTML");
    expect(nameOf(".editorconfig")).toBe("Properties files");
  });

  it("leaves unknown files plain", () => {
    expect(nameOf("notes.txt")).toBeNull();
    expect(nameOf("LICENSE")).toBeNull();
  });

  it("loads nothing for the languages Arnold picks itself", () => {
    for (const path of ["Orders.fs", "Api.cs", "Shop.fsproj", "db/Orders.sql", "README.md", "package.json"]) {
      expect(languageLoading(path)).toBeNull();
    }
  });
});
