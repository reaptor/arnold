module Git

open Browser
open System
open FSharp.Core

type GitLogEntry =
    {
        Commit: string
        AbbreviatedCommit: string
        Tree: string
        AbbreviatedTree: string
        Parent: string
        AbbreviatedParent: string
        Refs: string
        Encoding: string
        Subject: string
        SanitizedSubjectLine: string
        Body: string
        CommitNotes: string
        VerificationFlags: string
        Signer: string
        SignerKey: string
        Author:
            {|
                Name: string
                Email: string
                Date: string
            |}
        Commiter:
            {|
                Name: string
                Email: string
                Date: string
            |}
    }


let (|X|_|) (x: string) (input: string) =
    if String.exists (fun ch -> ch = input[0]) x then
        Some()
    else
        None

let (|Y|_|) (y: string) (input: string) =
    if String.exists (fun ch -> ch = input[1]) y then
        Some()
    else
        None

let (|Filename|) (input: string) = input[3..].Trim(' ').Trim('"')

type GitStatus =
    | NotUpdated // Not staged, modified
    | UpdatedInIndex // Staged, modified
    | TypeChangedInIndex
    | AddedToIndex // Staged, new file
    | DeletedFromIndex
    | RenamedInIndex
    | CopiedInIndex
    | IndexAndWorkTreeMatches
    | WorkTreeChangedSinceIndex
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

type GitStatusEntry = { Filename: string; Status: GitStatus }

module GitStatus =
    let parsePorcelain (s: string) =
        s.Split([| '\n' |], StringSplitOptions.RemoveEmptyEntries)
        |> fun x ->
            console.log (x)
            x
        |> Array.map (
            function
            // https://git-scm.com/docs/git-status#_short_format
            // https://git-scm.com/docs/git-status#_porcelain_format_version_1
            | X " MTARC" & Y "D" & Filename filename -> DeletedInWorkTree, filename
            | X " " & Y "AMD" & Filename filename -> NotUpdated, filename
            | X "M" & Y " MTD" & Filename filename -> UpdatedInIndex, filename
            | X "T" & Y " MTD" & Filename filename -> TypeChangedInIndex, filename
            | X "A" & Y " MTD" & Filename filename -> AddedToIndex, filename
            | X "D" & Y " " & Filename filename -> DeletedFromIndex, filename
            | X "R" & Y " MTD" & Filename filename -> RenamedInIndex, filename
            | X "C" & Y " MTD" & Filename filename -> CopiedInIndex, filename
            | X "MTARC" & Y " " & Filename filename -> IndexAndWorkTreeMatches, filename
            | X " MTARC" & Y "M" & Filename filename -> WorkTreeChangedSinceIndex, filename
            | X " MTARC" & Y "T" & Filename filename -> TypeChangedInWorkTreeSinceIndex, filename
            | X " " & Y "R" & Filename filename -> RenamedInWorkTree, filename
            | X " " & Y "C" & Filename filename -> CopiedInWorkTree, filename
            | X "D" & Y "D" & Filename filename -> UnmergedBothDeleted, filename
            | X "A" & Y "U" & Filename filename -> UnmergedAddedByUs, filename
            | X "U" & Y "D" & Filename filename -> UnmergedDeletedByThem, filename
            | X "U" & Y "A" & Filename filename -> UnmergedAddedByThem, filename
            | X "D" & Y "U" & Filename filename -> UnmergedDeletedByUs, filename
            | X "A" & Y "A" & Filename filename -> UnmergedBothAdded, filename
            | X "U" & Y "U" & Filename filename -> UnmergedBothModified, filename
            | X "?" & Y "?" & Filename filename -> Untracked, filename
            | X "!" & Y "!" & Filename filename -> Ignored, filename
            | unsupported -> failwithf "Unsupported git status output %s" unsupported
            >> fun (status, filename) -> { Filename = filename; Status = status }
        )