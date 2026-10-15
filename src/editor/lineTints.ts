// The diff tints and the caret row are painted in a layer under the selection
// instead of on the lines themselves. CodeMirror draws the selection behind the
// text, so tints on the lines covered it: a partial selection was hidden on the
// caret row and on changed lines. In a layer underneath, the selection has one
// color everywhere and the text keeps its own.
//
// styles.css stays the source of truth: it sets the colors as --line-bg on
// lines and --text-bg on changed words, and clears their real backgrounds.

import { Prec, type Extension } from "@codemirror/state";
import { layer, type EditorView, type LayerMarker } from "@codemirror/view";

class Tint implements LayerMarker {
  constructor(
    readonly left: number,
    readonly top: number,
    readonly width: number,
    readonly height: number,
    readonly color: string,
  ) {}

  draw() {
    const element = document.createElement("div");
    this.adjust(element);
    return element;
  }

  update(element: HTMLElement) {
    this.adjust(element);
    return true;
  }

  eq(other: LayerMarker): boolean {
    return (
      other instanceof Tint &&
      other.left === this.left &&
      other.top === this.top &&
      other.width === this.width &&
      other.height === this.height &&
      other.color === this.color
    );
  }

  private adjust(element: HTMLElement) {
    element.style.left = `${this.left}px`;
    element.style.top = `${this.top}px`;
    element.style.width = `${this.width}px`;
    element.style.height = `${this.height}px`;
    element.style.background = this.color;
  }
}

function tints(view: EditorView): Tint[] {
  // Layer positions are relative to the scrolled content, as in CodeMirror's
  // own RectangleMarker.
  const scroller = view.scrollDOM.getBoundingClientRect();
  const baseLeft = scroller.left - view.scrollDOM.scrollLeft * view.scaleX;
  const baseTop = scroller.top - view.scrollDOM.scrollTop * view.scaleY;
  const result: Tint[] = [];
  const add = (elements: Iterable<Element>, property: string) => {
    for (const element of elements) {
      const color = getComputedStyle(element).getPropertyValue(property).trim();
      if (!color) continue;
      const rect = element.getBoundingClientRect();
      result.push(
        new Tint(
          (rect.left - baseLeft) / view.scaleX,
          (rect.top - baseTop) / view.scaleY,
          rect.width / view.scaleX,
          rect.height / view.scaleY,
          color,
        ),
      );
    }
  };
  // Lines first: the changed words are drawn over their line.
  add(view.contentDOM.querySelectorAll(":scope > .cm-line"), "--line-bg");
  add(view.contentDOM.querySelectorAll(":scope > .cm-line .cm-changedText"), "--text-bg");
  return result;
}

/** Lowest precedence, so the layer sorts after (and under) the selection layer. */
export const lineTints: Extension = Prec.lowest(
  layer({
    above: false,
    class: "cm-tintLayer",
    markers: tints,
    // Diff decorations, the caret row and semantic colors all arrive in
    // transactions; heights move with fonts and folding.
    update: (update) => update.transactions.length > 0 || update.viewportChanged || update.geometryChanged,
  }),
);
