module App

open Feliz
open Browser
open Elmish
open Elmish.React
open FSharp.Core
open UI
open Git
open GitComponents
  
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
            prop.className "select-none"
            prop.children [                
                UI.StatusEntries(
                    entries =
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
                    entries =
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
        ]

Program.mkProgram init update view
|> Program.withReactBatched "root"
|> Program.run

// let root = ReactDOM.createRoot (document.getElementById "root")
//
// root.render (App())
