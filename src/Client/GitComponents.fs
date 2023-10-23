module GitComponents

open Feliz
open Browser
open FSharp.Core
open Git
open UI
open Shared

type UI with

    // [<ReactComponent>]
    // static member CommitLog(entries: GitLogEntry array) =
    //     UI.List(
    //         items = entries,
    //         selectionMode = Multi ignore,
    //         itemTemplate =
    //             (fun entry -> [
    //                 Html.span [ prop.className "select-none"; prop.text entry.Subject ]
    //                 Html.span [ prop.className "select-none"; prop.text entry.AbbreviatedCommit ]
    //             ])
    //     )

    [<ReactComponent>]
    static member StatusEntries
        (
            entries: GitStatusEntry array,
            selectionChanged: GitStatusEntry * GitStatusEntry array -> unit
        ) =
        UI.List(
            items = Array.sortBy (fun x -> x.Filename) entries,
            selectionMode = Multi selectionChanged,
            itemTemplate =
                (fun entry -> [
                    let icon =
                        match entry.Status with
                        | ModifiedInWorkTreeSinceIndex
                        | ModifiedInIndex -> Icon.Pencil
                        | AddedToIndex
                        | Untracked -> Icon.Plus
                        | DeletedInWorkTree
                        | DeletedFromIndex -> Icon.Minus
                        | _ -> Icon.QuestionMarkCircle

                    Html.div [
                        prop.className "flex"
                        prop.children [
                            Html.img [
                                prop.className "inline-block text-white mx-1 w-4"
                                prop.src (Icon.asFilepath icon)
                            ]

                            Html.span [ prop.className "select-none mr-2"; prop.text entry.Filename ]
                        ]
                    ]
                ])
        )
