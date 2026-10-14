/** The fold arrow of a collapsible row: points right when closed, down when open. */
export function Chevron(props: { open: boolean; class?: string }) {
  return (
    <svg
      class={["chevron", props.class, props.open && "open"].filter(Boolean).join(" ")}
      viewBox="0 0 16 16"
      aria-hidden="true"
    >
      <path d="M6 3.5 10.5 8 6 12.5" />
    </svg>
  );
}
