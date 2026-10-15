// A deleted file is shown whole, read-only, as it was at the merge-base. So it
// does not pass for an unchanged file, every line is tinted like a removed line.

import { EditorView } from "@codemirror/view";

export const deletedFile = EditorView.editorAttributes.of({ class: "cm-deletedFile" });
