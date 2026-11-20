import { describe, expect, it } from "vitest";
import { cleanMessage } from "./suggestedMessage";

describe("cleanMessage", () => {
  it("keeps a plain message, trimmed", () => {
    expect(cleanMessage("\n Fix the build\n\nIt failed on Windows.\n")).toBe("Fix the build\n\nIt failed on Windows.");
  });

  it("drops a code fence around the message", () => {
    expect(cleanMessage("```text\nAdd retries\n\nWith backoff.\n```")).toBe("Add retries\n\nWith backoff.");
    expect(cleanMessage("```\nAdd ret")).toBe("Add ret");
  });

  it("drops quotes around the message, not inside it", () => {
    expect(cleanMessage('"Add retries"')).toBe("Add retries");
    expect(cleanMessage('Rename "Foo" to "Bar"')).toBe('Rename "Foo" to "Bar"');
  });
});
