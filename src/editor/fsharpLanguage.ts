// F# highlighting without a language server. The legacy ML parser only knows
// keywords, strings, numbers and comments; this wraps it with the patterns
// Rider colors by symbol kind, so a file looks right before (or without) the
// server's semantic tokens:
//
// - function bindings (`let total (order: Order) =`) and earlier-defined
//   functions where they are used: function color
// - `member x.Name(...)` and calls like `s.ToUpper()` or `List.map`: function
//   color; `member x.Name =` and `order.Lines`: field color
// - type annotations (`: decimal`, `: OrderLine list`) and capitalized names
//   (types, modules, union cases, attributes): type color
// - record fields (`{ Id: int; Name = "x" }`): field color

import { StreamLanguage, type StreamParser, type StringStream } from "@codemirror/language";
import { fSharp } from "@codemirror/legacy-modes/mode/mllike";
import { tags as t } from "@lezer/highlight";

type Pending = "let" | "member" | null;

interface FSharpState {
  inner: unknown;
  /** The binding keyword waiting for its name. */
  pending: Pending;
  /** Inside a type annotation, until the end of it or of the line. */
  inType: boolean;
  braceDepth: number;
  /** Functions defined so far in the file; F# defines before use. Copied on write. */
  functions: readonly string[];
  /** The last word and whether a dot came after it, for `x.Name`. */
  previousWord: string | null;
  afterDot: boolean;
}

/** Keywords that may stand between `let`/`member` and the name. */
const BINDING_MODIFIERS = new Set(["rec", "inline", "mutable", "private", "internal", "public", "static", "val"]);
/** F# keywords the legacy parser misses. */
const MISSING_KEYWORDS = new Set(["and", "sig"]);
const MEMBER_KEYWORDS = new Set(["member", "override", "default"]);
/** Tokens that keep a type annotation going: `int -> string`, `Map<string, int>`, `'T list`. */
const TYPE_PUNCTUATION = new Set([".", "<", ">", "*", "-", "'"]);
/** The legacy parser's modules ("builtin"); Rider colors them as types. */
const MODULES = new Set(["List", "Seq", "Map", "Set", "Option"]);
/** Common FSharp.Core functions, colored as functions like Rider does. */
const CORE_FUNCTIONS = new Set([
  "printf", "printfn", "sprintf", "eprintf", "eprintfn", "failwithf", "invalidArg", "invalidOp", "nullArg",
  "ignore", "id", "fst", "snd", "box", "unbox", "ref", "incr", "decr", "defaultArg", "async", "task", "seq",
  "decimal", "float", "float32", "double", "int64", "int16", "uint", "uint64", "byte", "char", "bool",
]);

const isCapitalized = (word: string) => /^[A-Z]/.test(word);
const isWord = (text: string) => /^[\w¡-￿]+$/.test(text);

/**
 * After a binding name: `=`, `:` or `with get` means a value or property,
 * anything else is a parameter. `= fun` binds a function.
 */
const valueFollows = (stream: StringStream) =>
  !!stream.match(/^\s*(=|:(?!:)|with\b|$)/, false) && !stream.match(/^\s*=\s*fun\b/, false);

const base = fSharp as StreamParser<unknown>;

const parser: StreamParser<FSharpState> = {
  name: "fsharp",
  startState: (indentUnit) => ({
    inner: base.startState!(indentUnit),
    pending: null,
    inType: false,
    braceDepth: 0,
    functions: [],
    previousWord: null,
    afterDot: false,
  }),
  copyState: (state) => ({ ...state, inner: { ...(state.inner as object) } }),
  languageData: base.languageData,
  tokenTable: {
    functionName: t.function(t.variableName),
    functionDefinition: t.function(t.definition(t.variableName)),
    fieldName: t.propertyName,
  },
  token(stream, state) {
    if (stream.sol()) state.inType = false;
    let style = base.token(stream, state.inner);
    const text = stream.current();
    if (MISSING_KEYWORDS.has(text)) style = "keyword";
    if (!text.trim()) return style;
    if (style === "comment" || style === "string" || style === "number") {
      state.pending = null;
      state.inType = false;
      state.previousWord = null;
      state.afterDot = false;
      return style;
    }
    if (!isWord(text)) return punctuation(stream, state, text, style);

    const afterDot = state.afterDot;
    const previous = state.previousWord;
    state.previousWord = text;
    state.afterDot = false;

    if (style === "keyword") {
      if (text === "let" || text === "and" || text === "use") state.pending = "let";
      else if (MEMBER_KEYWORDS.has(text)) state.pending = "member";
      else if (!BINDING_MODIFIERS.has(text)) state.pending = null;
      if (text === "true" || text === "false") return "keyword";
      if (text === "of" || text === "inherit") state.inType = true;
      else state.inType = false;
      return style;
    }
    if (text === "true" || text === "false") return "keyword";

    const pending = state.pending;
    state.pending = null;

    if (pending === "member") {
      // `member this.Name`: the self name comes first.
      if (stream.match(/^\s*\./, false)) {
        state.pending = "member";
        return "variable";
      }
      return valueFollows(stream) ? "fieldName" : "functionDefinition";
    }
    if (pending === "let" && !afterDot) {
      if (valueFollows(stream)) return "variable";
      if (!state.functions.includes(text)) state.functions = [...state.functions, text];
      return "functionDefinition";
    }
    if (afterDot) {
      if (stream.match(/^\(/, false)) return "functionName";
      const qualifier = previous !== null && isCapitalized(previous);
      if (!isCapitalized(text)) return qualifier ? "functionName" : "variable";
      // `System.Math` is a path; `Math.PI`, `order.Lines` and `i.Name.Length` are members.
      return qualifier && stream.match(/^\./, false) ? "type" : "fieldName";
    }
    if (state.inType) {
      // A named union field: `Circle of radius: float`.
      return stream.match(/^\s*:(?!:)/, false) ? "variable" : "type";
    }
    if (isCapitalized(text)) {
      if (state.braceDepth > 0 && stream.match(/^\s*(=(?!=)|:(?!:))/, false)) return "fieldName";
      return "type";
    }
    if (MODULES.has(text)) return "type";
    if (state.functions.includes(text) || CORE_FUNCTIONS.has(text)) return "functionName";
    return style;
  },
};

function punctuation(stream: StringStream, state: FSharpState, text: string, style: string | null) {
  const afterWord = state.previousWord !== null && !state.afterDot;
  state.afterDot = text === "." && afterWord;
  if (!state.afterDot) state.previousWord = null;
  // `member this.Name`: keep waiting for the name.
  if (!(state.pending === "member" && text === ".")) state.pending = null;

  if (text === "{") state.braceDepth++;
  else if (text === "}") state.braceDepth = Math.max(0, state.braceDepth - 1);

  if (text === ":") {
    // `:` starts a type, `::` is cons; `:>` and `:?>` cast to a type.
    state.inType = !stream.match(/^:/, false);
  } else if (state.inType && !TYPE_PUNCTUATION.has(text)) {
    state.inType = false;
  }
  return style;
}

export const fSharpLanguage = StreamLanguage.define(parser);
export { parser as fSharpParser };
