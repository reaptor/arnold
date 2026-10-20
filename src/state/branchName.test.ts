import { describe, expect, it } from "vitest";
import { branchNameFor } from "./branchName";

describe("branchNameFor", () => {
  it("makes a name of the first line's words", () => {
    expect(branchNameFor("Fix the order total (VAT)\n\nBody text.")).toBe("fix-the-order-total-vat");
  });

  it("drops accents and stops at a word before 40 characters", () => {
    expect(branchNameFor("Lägg till rabatt för återkommande kunder i kassan och i appen")).toBe(
      "lagg-till-rabatt-for-aterkommande-kunder",
    );
  });

  it("is empty without words", () => {
    expect(branchNameFor("")).toBe("");
    expect(branchNameFor("  !! ")).toBe("");
  });
});
