module App

open Feliz
open Browser
open Thoth.Json
open Thoth.Fetch
open Elmish
open Elmish.React
open FSharp.Core
open UI
open Git
open GitComponents

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
                        Path = "/Users/kristofferlofberg/Projects/foo"
                    },
                decoder = GitResponse.StatusDecoder
            )
        return
            match response.Error with
            | None -> response.Data
            | Some e -> failwith e
    }

type Model =
    {
        Status: GitStatusEntry array
    }

type Msg =
    | LoadStatusEntries
    | LoadStatusEntriesSucceeded of GitStatusEntry array
    | LoadStatusEntriesFailed of exn

let init () =
    { Status = [||] }, Cmd.ofMsg LoadStatusEntries
    
let update (msg: Msg) (model: Model) =
    match msg with
    | LoadStatusEntries -> model, Cmd.OfPromise.either loadStatusEntries () LoadStatusEntriesSucceeded LoadStatusEntriesFailed
    | LoadStatusEntriesSucceeded status -> { model with Status = status }, Cmd.none
    | LoadStatusEntriesFailed ex ->
        console.log(ex)
        model, Cmd.none
    
let view (model: Model) _dispatch =
    // let _entries, _setEntries = React.useState ([||])
    // let status, setStatus = React.useState ([||])

    // React.useEffectOnce (fun () ->
    //     promise {
    //         // let! response =
    //         //     Fetch.post (
    //         //         url = "http://localhost:5000/git/log",
    //         //         data =
    //         //             {
    //         //                 Path = "/Users/kristofferlofberg/Projects/foo"
    //         //             },
    //         //         decoder = GitResponse.LogDecoder
    //         //     )
    //
    //         let! response =
    //             Fetch.post (
    //                 url = "http://localhost:5000/git/status",
    //                 data =
    //                     {
    //                         Path = "/Users/kristofferlofberg/Projects/foo"
    //                     },
    //                 decoder = GitResponse.StatusDecoder
    //             )
    //
    //         setStatus (response.Data)
    //     }
    //     |> ignore<Promise<unit>>
    // )

    Html.div
        [
            UI.StatusEntries(
                entires =
                    Array.filter
                        (fun entry ->
                            match entry.Status with
                            | NotUpdated
                            | Untracked
                            | DeletedInWorkTree -> true
                            | _ -> false
                        )
                        model.Status
            )
            UI.StatusEntries(
                entires =
                    Array.filter
                        (fun entry ->
                            match entry.Status with
                            | AddedToIndex
                            | UpdatedInIndex
                            | DeletedFromIndex -> true
                            | _ -> false
                        )
                        model.Status
            )
        ]

Program.mkProgram init update view
|> Program.withReactBatched "root"
|> Program.run

// let root = ReactDOM.createRoot (document.getElementById "root")
//
// root.render (App())
