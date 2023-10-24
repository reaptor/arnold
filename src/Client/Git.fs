module Git

open System
open FSharp.Core
open Shared

type GitLogEntry = {
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
    Author: {|
        Name: string
        Email: string
        Date: string
    |}
    Commiter: {|
        Name: string
        Email: string
        Date: string
    |}
}

module RepositoryPath =
    let name (RepositoryPath path) =
        path.Substring(path.Replace('\\', '/').LastIndexOf('/') + 1)

// module GitResponse =
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
