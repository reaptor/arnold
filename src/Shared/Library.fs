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

type TextFile = { Name: string; Content: string }

type BinaryFile = { Name: string }

type File =
    | TextFile of TextFile
    | BinaryFile of BinaryFile

type RepositoryPath = RepositoryPath of string

module RepositoryPath =
    let value (RepositoryPath p) = p

type ClientMessage =
    | ChangeRepository of RepositoryPath
    | GitStatus of RepositoryPath
    | GetFile of RepositoryPath * GitStatusEntry
    | SaveFile of RepositoryPath * TextFile

type ServerMessage =
    | GitStatusResponse of Result<GitStatusEntry array, string>
    | GetFileResponse of Result<File option, string>
    | SaveFileResponse of Result<unit, string>
    | FileChanged of RepositoryPath
    | RepositoryChanged of RepositoryPath
    | UnknownServerError of string
