// Line icons in the style of the JetBrains "new UI" icon set: 16×16, 1px strokes.

type IconProps = { class?: string };

function Icon(props: IconProps & { children: unknown }) {
  return (
    <svg class={["icon", props.class].filter(Boolean).join(" ")} viewBox="0 0 16 16" aria-hidden="true">
      {props.children as never}
    </svg>
  );
}

export const BranchIcon = (props: IconProps) => (
  <Icon {...props}>
    <circle cx="4.5" cy="3.5" r="1.5" />
    <circle cx="4.5" cy="12.5" r="1.5" />
    <circle cx="11.5" cy="4.5" r="1.5" />
    <path d="M4.5 5v6M11.5 6v1.5a2 2 0 0 1-2 2h-3a2 2 0 0 0-2 2" />
  </Icon>
);

export const CommitIcon = (props: IconProps) => (
  <Icon {...props}>
    <circle cx="8" cy="8" r="2.5" />
    <path d="M1.5 8h4M10.5 8h4" />
  </Icon>
);

export const ChevronDownIcon = (props: IconProps) => (
  <Icon class={["chevron-down", props.class].filter(Boolean).join(" ")}>
    <path d="M4.5 6.5 8 10l3.5-3.5" />
  </Icon>
);

export const ArrowUpIcon = (props: IconProps) => (
  <Icon {...props}>
    <path d="M8 13V3.5M3.5 7.5 8 3l4.5 4.5" />
  </Icon>
);

export const ArrowDownIcon = (props: IconProps) => (
  <Icon {...props}>
    <path d="M8 3v9.5M3.5 8.5 8 13l4.5-4.5" />
  </Icon>
);

export const BackIcon = (props: IconProps) => (
  <Icon {...props}>
    <path d="M13 8H3.5M7.5 3.5 3 8l4.5 4.5" />
  </Icon>
);

export const RefreshIcon = (props: IconProps) => (
  <Icon {...props}>
    <path d="M13 8a5 5 0 1 1-1.5-3.6M12 1.5v3.2H8.8" />
  </Icon>
);

/** Side-by-side diff: two panes. */
export const SplitIcon = (props: IconProps) => (
  <Icon {...props}>
    <rect x="2.5" y="2.5" width="11" height="11" rx="1.5" />
    <path d="M8 2.5v11" />
  </Icon>
);

/** Unified diff: one pane. */
export const UnifiedIcon = (props: IconProps) => (
  <Icon {...props}>
    <rect x="2.5" y="2.5" width="11" height="11" rx="1.5" />
    <path d="M5 6h6M5 8h6M5 10h4" />
  </Icon>
);

/** One file of the diff. */
export const FileIcon = (props: IconProps) => (
  <Icon {...props}>
    <path d="M4.5 1.5h4.8l3.2 3.2v9.8h-8z" />
    <path d="M9 1.5v3.5h3.5" />
  </Icon>
);

/** Every file of the diff in one list. */
export const FilesIcon = (props: IconProps) => (
  <Icon {...props}>
    <path d="M6.5 1.5h4l3 3v7h-7z" />
    <path d="M4.5 4.5h-2v10h7v-1" />
  </Icon>
);

/** A pull request: a branch merged back into another. */
export const PullRequestIcon = (props: IconProps) => (
  <Icon {...props}>
    <circle cx="4.5" cy="3.5" r="1.5" />
    <circle cx="4.5" cy="12.5" r="1.5" />
    <circle cx="11.5" cy="12.5" r="1.5" />
    <path d="M4.5 5v6M11.5 11V6.5a2 2 0 0 0-2-2H7M8.5 3 7 4.5 8.5 6" />
  </Icon>
);

/** A comment: a speech bubble. */
export const CommentIcon = (props: IconProps) => (
  <Icon {...props}>
    <path d="M2.5 3.5a1 1 0 0 1 1-1h9a1 1 0 0 1 1 1v6a1 1 0 0 1-1 1H7l-3 3v-3h-.5a1 1 0 0 1-1-1z" />
  </Icon>
);

/** Opens a web page in the browser. */
export const ExternalIcon = (props: IconProps) => (
  <Icon {...props}>
    <path d="M9.5 2.5h4v4M13.5 2.5 7.5 8.5M11.5 9.5v3a1 1 0 0 1-1 1h-7a1 1 0 0 1-1-1v-7a1 1 0 0 1 1-1h3" />
  </Icon>
);

/** A personal access token. */
export const KeyIcon = (props: IconProps) => (
  <Icon {...props}>
    <circle cx="5" cy="11" r="2.5" />
    <path d="M6.8 9.2 13 3M11 5l1.5 1.5M9.5 6.5 11 8" />
  </Icon>
);

/** Done, approved. */
export const CheckIcon = (props: IconProps) => (
  <Icon {...props}>
    <path d="M3 8.5 6.5 12 13 4.5" />
  </Icon>
);

/** Merging a branch into another. */
export const MergeIcon = (props: IconProps) => (
  <Icon {...props}>
    <circle cx="4.5" cy="3.5" r="1.5" />
    <circle cx="4.5" cy="12.5" r="1.5" />
    <circle cx="11.5" cy="8" r="1.5" />
    <path d="M4.5 5v6M4.5 5a5 3 0 0 0 5.5 3" />
  </Icon>
);
