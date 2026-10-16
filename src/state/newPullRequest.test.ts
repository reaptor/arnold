import { describe, expect, it } from "vitest";
import { parseWorkItem } from "./newPullRequest";

describe("parseWorkItem", () => {
  it("reads a number, a #number or the work item's address", () => {
    expect(parseWorkItem(" 1234 ")).toBe(1234);
    expect(parseWorkItem("#1234")).toBe(1234);
    expect(parseWorkItem("https://dev.azure.com/contoso/Shop/_workitems/edit/1234")).toBe(1234);
    expect(parseWorkItem("https://dev.azure.com/contoso/Shop/_workitems/edit/1234/?view=full")).toBe(1234);
  });

  it("is null when empty and invalid otherwise", () => {
    expect(parseWorkItem("  ")).toBeNull();
    expect(parseWorkItem("0")).toBe("invalid");
    expect(parseWorkItem("12a")).toBe("invalid");
    expect(parseWorkItem("AB#1234")).toBe("invalid");
    expect(parseWorkItem("99999999999999999999")).toBe("invalid");
  });
});
