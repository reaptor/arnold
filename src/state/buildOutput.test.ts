import { describe, expect, it } from "vitest";
import { parseBuildLine } from "./buildOutput";
import { mergeProblems, type Problem } from "./problems";

const ROOT = "/src/shop";

describe("parseBuildLine", () => {
  it("reads an error with its place, relative to the folder built", () => {
    expect(
      parseBuildLine(
        "/src/shop/src/Orders.fs(12,5): error FS0039: The value or constructor 'total' is not defined. [/src/shop/src/Shop.fsproj]",
        ROOT,
      ),
    ).toEqual({
      path: "src/Orders.fs",
      problem: { severity: "error", message: "The value or constructor 'total' is not defined.", code: "FS0039", line: 11, character: 4 },
    });
  });

  it("reads warnings with a range, and Windows paths", () => {
    expect(parseBuildLine("C:\\src\\shop\\Api\\Startup.cs(3,1,3,9): warning CS8618: Non-nullable field. [C:\\src\\shop\\Api\\Api.csproj::TargetFramework=net8.0]", "C:\\src\\shop")).toEqual({
      path: "Api/Startup.cs",
      problem: { severity: "warning", message: "Non-nullable field.", code: "CS8618", line: 2, character: 0 },
    });
  });

  it("lists a problem without a place under the project file, or the tool when there is none", () => {
    expect(parseBuildLine("CSC : error CS5001: Program does not contain a static 'Main' method [/src/shop/Api/Api.csproj]", ROOT)).toEqual({
      path: "Api/Api.csproj",
      problem: { severity: "error", message: "Program does not contain a static 'Main' method", code: "CS5001", line: -1, character: 0 },
    });
    expect(parseBuildLine("/src/shop/Shop.fsproj : warning NU1603: Giraffe 7.0.0 was not found. [/src/shop/Shop.sln]", ROOT)?.path).toBe("Shop.fsproj");
    expect(parseBuildLine("MSBUILD : error MSB1009: Project file does not exist.", ROOT)).toMatchObject({ path: "MSBUILD", problem: { code: "MSB1009" } });
  });

  it("leaves other lines alone", () => {
    for (const line of ["  Determining projects to restore...", "  Shop -> /src/shop/bin/Debug/net8.0/Shop.dll", "Build FAILED.", "    0 Error(s)"]) {
      expect(parseBuildLine(line, ROOT)).toBeNull();
    }
  });
});

describe("mergeProblems", () => {
  const problem = (line: number, message: string, code: string | null = null): Problem => ({ severity: "error", message, code, line, character: 0 });

  it("lists the build's problems with the server's, each once", () => {
    const built = { "src/Orders.fs": [problem(4, "not defined", "FS0039")], "Shop.fsproj": [problem(-1, "package missing")] };
    const server = { "src/Orders.fs": [problem(4, "not\n defined", "39"), problem(1, "other")] };
    expect(mergeProblems(server, built)).toEqual({
      "src/Orders.fs": [problem(1, "other"), problem(4, "not defined", "FS0039")],
      "Shop.fsproj": [problem(-1, "package missing")],
    });
  });
});
