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
type FileData = { Name: string; Content: string }

type RepositoryPath = RepositoryPath of string

module RepositoryPath =
    let value (RepositoryPath p) = p

type ClientMessage =
    | ChangeRepository of RepositoryPath
    | GitStatus of RepositoryPath
    | GetFile of RepositoryPath * GitStatusEntry
    | SaveFile of RepositoryPath * FileData

type ServerMessage =
    | GitStatusResponse of Result<GitStatusEntry array, string>
    | GetFileResponse of Result<FileData option, string>
    | SaveFileResponse of Result<unit, string>
    | FileChanged of RepositoryPath
    | RepositoryChanged of RepositoryPath
    | UnknownServerError of string
