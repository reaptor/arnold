import { describe, expect, it } from "vitest";
import { canonicalUri, joinPath, pathToUri, relativeTo, uriToPath } from "./uri";

describe("file URIs", () => {
  it("converts POSIX paths both ways", () => {
    expect(pathToUri("/work/my repo/src/Å.fs")).toBe("file:///work/my%20repo/src/%C3%85.fs");
    expect(uriToPath("file:///work/my%20repo/src/%C3%85.fs")).toBe("/work/my repo/src/Å.fs");
  });

  it("converts Windows paths both ways", () => {
    expect(pathToUri("C:\\Users\\me\\repo\\A.fs")).toBe("file:///C:/Users/me/repo/A.fs");
    expect(pathToUri("c:/Users/me/repo/A.fs")).toBe("file:///C:/Users/me/repo/A.fs");
    expect(uriToPath("file:///C:/Users/me/repo/A.fs")).toBe("C:/Users/me/repo/A.fs");
  });

  it("gives every spelling of a URI the same form", () => {
    const expected = "file:///C:/Users/me/my%20repo/A.fs";
    expect(canonicalUri("file:///c%3A/Users/me/my%20repo/A.fs")).toBe(expected);
    expect(canonicalUri("file:///C:/Users/me/my repo/A.fs")).toBe(expected);
    expect(canonicalUri(expected)).toBe(expected);
    expect(canonicalUri("untitled:Untitled-1")).toBe("untitled:Untitled-1");
  });

  it("rejects URIs that are not files or are malformed", () => {
    expect(uriToPath("fsharp://metadata/FSharp.Core")).toBeNull();
    expect(uriToPath("file:///bad%ZZ")).toBeNull();
  });
});

describe("relativeTo", () => {
  it("finds paths inside a folder", () => {
    expect(relativeTo("/work/repo", "/work/repo/src/A.fs")).toBe("src/A.fs");
    expect(relativeTo("/work/repo/", "/work/repo/A.fs")).toBe("A.fs");
    expect(relativeTo("/work/repo", "/work/repo-other/A.fs")).toBeNull();
    expect(relativeTo("/work/repo", "/elsewhere/A.fs")).toBeNull();
  });

  it("ignores case and slash direction on Windows", () => {
    expect(relativeTo("C:/Users/me/repo", "c:\\users\\me\\repo\\src\\A.fs")).toBe("src/A.fs");
  });

  it("round-trips with joinPath", () => {
    expect(relativeTo("/work/repo", joinPath("/work/repo/", "src/A.fs"))).toBe("src/A.fs");
  });
});
