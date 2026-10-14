import { describe, expect, it } from "vitest";
import { folderName, sortFavorites } from "./favorites";

describe("favorites", () => {
  it("names a repository after its folder on either platform", () => {
    expect(folderName("/Users/me/Projects/arnold")).toBe("arnold");
    expect(folderName("C:\\src\\Shop\\")).toBe("Shop");
  });

  it("sorts by folder name ignoring case, then by path", () => {
    expect(sortFavorites(["/b/zed", "/a/Shop", "/c/api2", "/a/api10", "/z/shop"])).toEqual([
      "/c/api2",
      "/a/api10",
      "/a/Shop",
      "/z/shop",
      "/b/zed",
    ]);
  });
});
