namespace Shared

type GitStatus =
    | NotUpdated
    | ModifiedInIndex
    | TypeChangedInIndex
    | AddedToIndex
    | DeletedFromIndex
    | RenamedInIndex
    | CopiedInIndex
    | ModifiedInWorkTreeSinceIndex
    | TypeChangedInWorkTreeSinceIndex
    | DeletedInWorkTree
    | RenamedInWorkTree
    | CopiedInWorkTree
    | UnmergedBothDeleted
    | UnmergedAddedByUs
    | UnmergedDeletedByThem
    | UnmergedAddedByThem
    | UnmergedDeletedByUs
    | UnmergedBothAdded
    | UnmergedBothModified
    | Untracked
    | Ignored

module GitStatus =
    let isStaged =
        function
        | ModifiedInIndex
        | TypeChangedInIndex
        | AddedToIndex
        | DeletedFromIndex
        | RenamedInIndex
        | CopiedInIndex -> true
        | _ -> false

type GitStatusEntry = { Filename: string; Status: GitStatus }

type ClientMessage =
    | GitStatus
    | GetFileContent of GitStatusEntry

type ServerMessage =
    | GitStatusResponse of Result<GitStatusEntry array, string>
    | GetFileContentResponse of Result<string option, string>
    | FileChanged
