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

/** Two arrows pointing apart: open folders. */
export const ExpandIcon = (props: IconProps) => (
  <Icon {...props}>
    <path d="M4.5 6 8 2.5 11.5 6M4.5 10 8 13.5l3.5-3.5" />
  </Icon>
);

/** A solution's grid: the explorer's Solution view. */
export const SolutionViewIcon = (props: IconProps) => (
  <Icon {...props}>
    <path d="M4.5 2.5h7a2 2 0 0 1 2 2v7a2 2 0 0 1-2 2h-7a2 2 0 0 1-2-2v-7a2 2 0 0 1 2-2ZM8 2.5v11M2.5 8h11" />
  </Icon>
);

/** Two arrows pointing together: close folders. */
export const CollapseAllIcon = (props: IconProps) => (
  <Icon {...props}>
    <path d="M4.5 2.5 8 6l3.5-3.5M4.5 13.5 8 10l3.5 3.5" />
  </Icon>
);

/** An arrow down onto a bar: stage, into the Staged list below. */
export const StageIcon = (props: IconProps) => (
  <Icon {...props}>
    <path d="M8 2.5v8M4.5 7 8 10.5 11.5 7M3 13.5h10" />
  </Icon>
);

/** Two arrows down onto a bar: stage everything. */
export const StageAllIcon = (props: IconProps) => (
  <Icon {...props}>
    <path d="M4.5 3 8 6.5 11.5 3M4.5 7 8 10.5 11.5 7M3 13.5h10" />
  </Icon>
);

/** An arrow up onto a bar: unstage, back to the Unstaged list above. */
export const UnstageIcon = (props: IconProps) => (
  <Icon {...props}>
    <path d="M8 13.5v-8M4.5 9 8 5.5 11.5 9M3 2.5h10" />
  </Icon>
);

/** Two arrows up onto a bar: unstage everything. */
export const UnstageAllIcon = (props: IconProps) => (
  <Icon {...props}>
    <path d="M4.5 13 8 9.5 11.5 13M4.5 9 8 5.5 11.5 9M3 2.5h10" />
  </Icon>
);

export const BackIcon = (props: IconProps) => (
  <Icon {...props}>
    <path d="M13 8H3.5M7.5 3.5 3 8l4.5 4.5" />
  </Icon>
);

export const ForwardIcon = (props: IconProps) => (
  <Icon {...props}>
    <path d="M3 8h9.5M8.5 3.5 13 8l-4.5 4.5" />
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

/** Whitespace: a tab over a space. */
export const WhitespaceIcon = (props: IconProps) => (
  <Icon {...props}>
    <path d="M4 5.5h7M9 3.5l2 2-2 2M2.5 9.5v3h11v-3" />
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

/** A page with a title and lines of text: a pull request's overview. */
export const OverviewIcon = (props: IconProps) => (
  <Icon {...props}>
    <path d="M3.5 2.5h9a1 1 0 0 1 1 1v9a1 1 0 0 1-1 1h-9a1 1 0 0 1-1-1v-9a1 1 0 0 1 1-1Z" />
    <path d="M5 5.5h6M5 8h6M5 10.5h3.5" />
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

/** Four tiles: the dashboard of every favorite's pull requests. */
export const DashboardIcon = (props: IconProps) => (
  <Icon {...props}>
    <rect x="2.5" y="2.5" width="4.5" height="4.5" rx="1" />
    <rect x="9" y="2.5" width="4.5" height="4.5" rx="1" />
    <rect x="2.5" y="9" width="4.5" height="4.5" rx="1" />
    <rect x="9" y="9" width="4.5" height="4.5" rx="1" />
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

/** Notifications. */
export const BellIcon = (props: IconProps) => (
  <Icon {...props}>
    <path d="M4 11.5V7a4 4 0 0 1 8 0v4.5l1.5 1.5h-11ZM6.5 13.5a1.5 1.5 0 0 0 3 0" />
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

/** Formatting code: ragged lines made even. */
export const FormatIcon = (props: IconProps) => (
  <Icon {...props}>
    <path d="M2.5 4h11M5.5 8h8M2.5 12h11" />
  </Icon>
);

/** Closing, cancelling. */
export const CloseIcon = (props: IconProps) => (
  <Icon {...props}>
    <path d="M4 4l8 8M12 4l-8 8" />
  </Icon>
);

/** AI: explaining code. */
export const SparkleIcon = (props: IconProps) => (
  <Icon {...props}>
    <path d="M7 2.5l1.2 3.3 3.3 1.2-3.3 1.2L7 11.5 5.8 8.2 2.5 7l3.3-1.2z" />
    <path d="M12 10.5l.6 1.4 1.4.6-1.4.6-.6 1.4-.6-1.4-1.4-.6 1.4-.6z" />
  </Icon>
);

/** Sending a question. */
export const SendIcon = (props: IconProps) => (
  <Icon {...props}>
    <path d="M8 13V3M3.5 7.5L8 3l4.5 4.5" />
  </Icon>
);

/** Stopping an answer. */
/** Build, as Rider's hammer. */
export const BuildIcon = (props: IconProps) => (
  <Icon {...props}>
    <path d="M8.5 7.5l-5.2 5.2a1.06 1.06 0 0 0 1.5 1.5L10 9" />
    <path d="M6.5 4.5L9 2h2.5l2.5 2.5L12.5 6l-1-1L9 7.5 7.5 6z" />
  </Icon>
);

export const StopIcon = (props: IconProps) => (
  <Icon {...props}>
    <rect x="4.5" y="4.5" width="7" height="7" rx="1" />
  </Icon>
);

/** Copying text. */
export const CopyIcon = (props: IconProps) => (
  <Icon {...props}>
    <rect x="5.5" y="5.5" width="8" height="8" rx="1.5" />
    <path d="M10.5 3.5v-.5a1 1 0 0 0-1-1h-6a1 1 0 0 0-1 1v6a1 1 0 0 0 1 1h.5" />
  </Icon>
);

/** Starting over. */
export const NewChatIcon = (props: IconProps) => (
  <Icon {...props}>
    <path d="M13.5 8.5v3a2 2 0 0 1-2 2h-7a2 2 0 0 1-2-2v-7a2 2 0 0 1 2-2h3M12 1.5v5M9.5 4h5" />
  </Icon>
);

/** A worktree: a second checkout beside the first. */
export const WorktreeIcon = (props: IconProps) => (
  <Icon {...props}>
    <rect x="1.5" y="2.5" width="8" height="8" rx="1.5" />
    <path d="M12.5 5.5h1v8h-8v-1" />
  </Icon>
);

/** Throwing something away for good. */
export const TrashIcon = (props: IconProps) => (
  <Icon {...props}>
    <path d="M2.5 4.5h11M6.5 4.5v-2h3v2M4 4.5l.7 9h6.6l.7-9M6.75 7v4.5M9.25 7v4.5" />
  </Icon>
);

/** The git stash: changes put away in a box, to take out later. */
export const StashIcon = (props: IconProps) => (
  <Icon {...props}>
    <path d="M2.5 6.5h11v6a1 1 0 0 1-1 1h-9a1 1 0 0 1-1-1zM1.5 3.5h13v3h-13zM6.5 9h3" />
  </Icon>
);

/** A problem of the language server: an error. */
export const ErrorIcon = (props: IconProps) => (
  <Icon {...props}>
    <circle cx="8" cy="8" r="6" />
    <path d="M5.75 5.75l4.5 4.5M10.25 5.75l-4.5 4.5" />
  </Icon>
);

/** A problem of the language server: a warning. */
export const WarningIcon = (props: IconProps) => (
  <Icon {...props}>
    <path d="M8 2l6.5 11.5h-13z" />
    <path d="M8 6.5v3.5M8 11.75v.25" />
  </Icon>
);

/** A problem of the language server: a message. */
export const InfoIcon = (props: IconProps) => (
  <Icon {...props}>
    <circle cx="8" cy="8" r="6" />
    <path d="M8 7v4.5M8 4.75v.25" />
  </Icon>
);
