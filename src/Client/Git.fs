module Git

open Browser
open System
open FSharp.Core
open Thoth.Json
open Thoth.Fetch

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
    | ModifiedInIndex
    | TypeChangedInIndex
    | AddedToIndex
    | DeletedFromIndex
    | RenamedInIndex
    | CopiedInIndex
    | ModifiedInWorkTree
    | TypeChangedInWorkTree
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
    let isStaged =
        function
        | ModifiedInIndex
        | TypeChangedInIndex
        | AddedToIndex
        | DeletedFromIndex
        | RenamedInIndex
        | CopiedInIndex -> true
        | _ -> false
        
    let parsePorcelain (s: string) =
        s.Split([| '\n' |], StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (
            function
            // https://git-scm.com/docs/git-status#_short_format
            // https://git-scm.com/docs/git-status#_porcelain_format_version_1
            // | X " " & Y "AMD" & Filename filename -> NotUpdated, filename
            | X "M" & Y " " & Filename filename -> ModifiedInIndex, filename
            | X "T" & Y " " & Filename filename -> TypeChangedInIndex, filename
            | X "A" & Y " " & Filename filename -> AddedToIndex, filename
            | X "D" & Y " " & Filename filename -> DeletedFromIndex, filename
            | X "R" & Y " " & Filename filename -> RenamedInIndex, filename
            | X "C" & Y " " & Filename filename -> CopiedInIndex, filename
            // | X "MTARC" & Y " " & Filename filename -> IndexAndWorkTreeMatches, filename
            | X " " & Y "M" & Filename filename -> ModifiedInWorkTree, filename
            | X " " & Y "T" & Filename filename -> TypeChangedInWorkTree, filename
            | X " " & Y "D" & Filename filename -> DeletedInWorkTree, filename
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
        

type GitRequest = { Path: string }

type GitResponse<'a> =
    { Error: string option; Data: 'a array }

module GitResponse =
    // let LogDecoder =
    //     Decode.object (fun get ->
    //         {
    //             Error = get.Optional.Field "error" Decode.string
    //             Data =
    //                 get.Optional.Field
    //                     "data"
    //                     (Decode.array (
    //                         Decode.object (fun get ->
    //                             {
    //                                 Commit = get.Required.Field "commit" Decode.string
    //                                 AbbreviatedCommit = get.Required.Field "abbreviated_commit" Decode.string
    //                                 Tree = get.Required.Field "tree" Decode.string
    //                                 AbbreviatedTree = get.Required.Field "abbreviated_tree" Decode.string
    //                                 Parent = get.Required.Field "parent" Decode.string
    //                                 AbbreviatedParent = get.Required.Field "abbreviated_parent" Decode.string
    //                                 Refs = get.Required.Field "refs" Decode.string
    //                                 Encoding = get.Required.Field "encoding" Decode.string
    //                                 Subject = get.Required.Field "subject" Decode.string
    //                                 SanitizedSubjectLine = get.Required.Field "sanitized_subject_line" Decode.string
    //                                 Body = get.Required.Field "body" Decode.string
    //                                 CommitNotes = get.Required.Field "commit_notes" Decode.string
    //                                 VerificationFlags = get.Required.Field "verification_flags" Decode.string
    //                                 Signer = get.Required.Field "signer" Decode.string
    //                                 SignerKey = get.Required.Field "signer_key" Decode.string
    //                                 Author =
    //                                     get.Required.Field
    //                                         "author"
    //                                         (Decode.object (fun get ->
    //                                             {|
    //                                                 Name = get.Required.Field "name" Decode.string
    //                                                 Email = get.Required.Field "email" Decode.string
    //                                                 Date = get.Required.Field "date" Decode.string
    //                                             |}
    //                                         ))
    //                                 Commiter =
    //                                     get.Required.Field
    //                                         "commiter"
    //                                         (Decode.object (fun get ->
    //                                             {|
    //                                                 Name = get.Required.Field "name" Decode.string
    //                                                 Email = get.Required.Field "email" Decode.string
    //                                                 Date = get.Required.Field "date" Decode.string
    //                                             |}
    //                                         ))
    //                             }
    //                         )
    //                     ))
    //                 |> Option.toArray
    //                 |> Array.concat
    //         }
    //     )

    let StatusDecoder =
        Decode.object (fun get ->
            {
                Error = get.Optional.Field "error" Decode.string
                Data =
                    get.Optional.Field "data" Decode.string
                    |> Option.map GitStatus.parsePorcelain
                    |> Option.toArray
                    |> Array.concat
            }
        )

let loadStatusEntries () =
    promise {
        let! response =
            Fetch.post (
                url = "http://localhost:5000/git/status",
                data =
                    {
                        Path = "/Users/kristofferlofberg/Projects/arnold"
                    },
                decoder = GitResponse.StatusDecoder
            )
        return
            match response.Error with
            | None -> response.Data
            | Some e -> failwith e
    } 