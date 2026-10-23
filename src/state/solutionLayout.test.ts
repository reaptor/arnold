import { describe, expect, it } from "vitest";
import {
  buildSolutionTree,
  mainSolution,
  parseProject,
  parseSolution,
  resolvePath,
  solutionOrder,
  type TreeNode,
} from "./solutionLayout";

const SLN = `
Microsoft Visual Studio Solution File, Format Version 12.00
Project("{2150E333-8FDC-42A3-9474-1A3956D46DE8}") = "build", "build", "{11111111-0000-0000-0000-000000000000}"
	ProjectSection(SolutionItems) = preProject
		build.fsx = build.fsx
	EndProjectSection
EndProject
Project("{F2A71F9B-5D33-465A-A702-920D77279786}") = "Web", "src\\Web\\Web.fsproj", "{22222222-0000-0000-0000-000000000000}"
EndProject
Project("{F2A71F9B-5D33-465A-A702-920D77279786}") = "Core", "src\\Core\\Core.fsproj", "{33333333-0000-0000-0000-000000000000}"
EndProject
Project("{FAE04EC0-301F-11D3-BF4B-00C04F79EFBC}") = "Tools", "tools\\Tools.csproj", "{44444444-0000-0000-0000-000000000000}"
EndProject
Global
	GlobalSection(NestedProjects) = preSolution
		{44444444-0000-0000-0000-000000000000} = {11111111-0000-0000-0000-000000000000}
	EndGlobalSection
EndGlobal
`;

const CORE = `<Project Sdk="Microsoft.NET.Sdk">
  <ItemGroup>
    <Compile Include="Types.fs" />
    <!-- <Compile Include="Old.fs" /> -->
    <Compile Include="Domain\\Order.fs" />
    <Compile Include="Domain\\Price.fs" />
    <Compile Include="Helpers.fs" />
    <Compile Include="Domain\\Rules.fs" />
    <Compile Include="..\\Shared\\Shared.fs">
      <Link>Shared/Shared.fs</Link>
    </Compile>
    <None Include="paket.references" />
    <ProjectReference Include="..\\Web\\Web.fsproj" />
  </ItemGroup>
</Project>`;

const WEB = `<Project Sdk="Microsoft.NET.Sdk.Web">
  <ItemGroup>
    <Compile Include="Pages/*.fs" />
    <Compile Include="Program.fs" />
  </ItemGroup>
</Project>`;

const FILES = [
  "Shop.sln",
  "build.fsx",
  "README.md",
  "src/Core/Core.fsproj",
  "src/Core/Types.fs",
  "src/Core/Domain/Order.fs",
  "src/Core/Domain/Price.fs",
  "src/Core/Domain/Rules.fs",
  "src/Core/Helpers.fs",
  "src/Core/Forgotten.fs",
  "src/Core/paket.references",
  "src/Shared/Shared.fs",
  "src/Web/Web.fsproj",
  "src/Web/Program.fs",
  "src/Web/Pages/B.fs",
  "src/Web/Pages/A.fs",
  "tools/Tools.csproj",
  "tools/Run.cs",
  "tools/obj/Generated.cs",
];

const TEXTS = new Map([
  ["Shop.sln", SLN],
  ["src/Core/Core.fsproj", CORE],
  ["src/Web/Web.fsproj", WEB],
]);

/** The tree as indented lines, with every group open. */
function outline(nodes: TreeNode[], depth = 0): string[] {
  return nodes.flatMap((node) => {
    const pad = "  ".repeat(depth);
    if (node.kind === "file") return [`${pad}${node.label}${node.unlisted ? " (unlisted)" : ""}`];
    if (node.kind === "folders") return [`${pad}[${node.files.join(", ")}]`];
    return [`${pad}${node.label}/`, ...outline(node.children, depth + 1)];
  });
}

describe("solution view", () => {
  it("resolves relative paths with backslashes", () => {
    expect(resolvePath("src/Core", "..\\Shared\\A.fs")).toBe("src/Shared/A.fs");
    expect(resolvePath("", "./a/b.fs")).toBe("a/b.fs");
    expect(resolvePath("", "../a.fs")).toBeNull();
  });

  it("picks the solution nearest the top", () => {
    expect(mainSolution(["tests/data/X.sln", "Shop.slnx", "a.fs"])).toBe("Shop.slnx");
    expect(mainSolution(["a.fs"])).toBeNull();
  });

  it("reads folders, nesting and solution items of a .sln", () => {
    expect(parseSolution("Shop.sln", SLN)).toEqual([
      {
        kind: "folder",
        name: "build",
        entries: [{ kind: "project", path: "tools/Tools.csproj" }],
        files: ["build.fsx"],
      },
      { kind: "project", path: "src/Web/Web.fsproj" },
      { kind: "project", path: "src/Core/Core.fsproj" },
    ]);
  });

  it("reads a .slnx", () => {
    const slnx = `<Solution>
  <Folder Name="/src/">
    <Project Path="src/Core/Core.fsproj" />
    <File Path="README.md" />
  </Folder>
  <Folder Name="/src/inner/">
    <Project Path="src\\Web\\Web.fsproj"><BuildType Project="Debug" /></Project>
  </Folder>
  <Project Path="tools/Tools.csproj" />
</Solution>`;
    expect(parseSolution("Shop.slnx", slnx)).toEqual([
      {
        kind: "folder",
        name: "src",
        entries: [
          { kind: "project", path: "src/Core/Core.fsproj" },
          { kind: "folder", name: "inner", entries: [{ kind: "project", path: "src/Web/Web.fsproj" }], files: [] },
        ],
        files: ["README.md"],
      },
      { kind: "project", path: "tools/Tools.csproj" },
    ]);
  });

  it("lists a project's files in its order, with links and globs", () => {
    expect(parseProject("src/Core/Core.fsproj", CORE, FILES)).toEqual([
      { path: "src/Core/Types.fs", shownAs: "Types.fs" },
      { path: "src/Core/Domain/Order.fs", shownAs: "Domain/Order.fs" },
      { path: "src/Core/Domain/Price.fs", shownAs: "Domain/Price.fs" },
      { path: "src/Core/Helpers.fs", shownAs: "Helpers.fs" },
      { path: "src/Core/Domain/Rules.fs", shownAs: "Domain/Rules.fs" },
      { path: "src/Shared/Shared.fs", shownAs: "Shared/Shared.fs" },
      { path: "src/Core/paket.references", shownAs: "paket.references" },
    ]);
    expect(parseProject("src/Web/Web.fsproj", WEB, FILES).map((item) => item.path)).toEqual([
      "src/Web/Pages/A.fs",
      "src/Web/Pages/B.fs",
      "src/Web/Program.fs",
    ]);
  });

  it("shows projects in solution order and F# files in compile order", () => {
    const tree = buildSolutionTree(FILES, "Shop.sln", TEXTS);
    expect(outline(tree.nodes)).toEqual([
      "Shop/",
      "  Shop.sln",
      "  build/",
      "    Tools/",
      "      Tools.csproj",
      "      [tools/Run.cs]",
      "    build.fsx",
      "  Web/",
      "    Web.fsproj",
      "    Pages/",
      "      A.fs",
      "      B.fs",
      "    Program.fs",
      "  Core/",
      "    Core.fsproj",
      "    Types.fs",
      "    Domain/",
      "      Order.fs",
      "      Price.fs",
      "    Helpers.fs",
      "    Domain/",
      "      Rules.fs",
      "    Shared/",
      "      Shared.fs",
      "    paket.references",
      "    Forgotten.fs (unlisted)",
      "Files outside projects/",
      "  [README.md, tools/obj/Generated.cs]",
    ]);
  });

  it("knows the file of the solution and of each project, for Reveal in Finder", () => {
    const groups: string[] = [];
    const visit = (node: TreeNode) => {
      if (node.kind !== "group") return;
      if (node.projectFile) groups.push(`${node.label}: ${node.projectFile}`);
      node.children.forEach(visit);
    };
    buildSolutionTree(FILES, "Shop.sln", TEXTS).nodes.forEach(visit);
    expect(groups).toEqual(["Shop: Shop.sln", "Tools: tools/Tools.csproj", "Web: src/Web/Web.fsproj", "Core: src/Core/Core.fsproj"]);
  });

  it("orders changed files as the view shows them", () => {
    const order = solutionOrder(buildSolutionTree(FILES, "Shop.sln", TEXTS));
    const changed = ["README.md", "src/Core/Helpers.fs", "src/Gone.fs", "src/Web/Program.fs", "src/Core/Types.fs", "tools/Run.cs"];
    expect(changed.sort(order)).toEqual([
      "tools/Run.cs",
      "src/Web/Program.fs",
      "src/Core/Types.fs",
      "src/Core/Helpers.fs",
      // Not in the view (deleted): among the files outside projects.
      "src/Gone.fs",
      "README.md",
    ]);
  });

  it("shows every project without a solution", () => {
    const files = FILES.filter((path) => path !== "Shop.sln");
    const tree = buildSolutionTree(files, null, TEXTS);
    expect(tree.nodes.map((node) => (node.kind === "group" ? node.label : node.key))).toEqual([
      "Core",
      "Web",
      "Tools",
      "Files outside projects",
    ]);
  });
});
