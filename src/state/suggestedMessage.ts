// A suggested commit message, as Copilot wrote it and as the message box gets it.

/** The model's answer as a commit message: without a code fence or quotes around it. */
export function cleanMessage(text: string): string {
  let message = text.trim();
  const fence = /^```[^\n]*\n([\s\S]*?)\n?```$/.exec(message);
  if (fence) message = fence[1].trim();
  // Still being written: the opening fence alone.
  else if (/^```[^\n]*\n/.test(message)) message = message.replace(/^```[^\n]*\n/, "").trim();
  const quoted = /^(["'“])([\s\S]*)(["'”])$/.exec(message);
  if (quoted && !/["'“”]/.test(quoted[2])) message = quoted[2].trim();
  return message;
}
