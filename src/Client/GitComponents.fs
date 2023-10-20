module GitComponents

open Feliz
open Browser
open FSharp.Core
open Git
open UI

type UI with        
    [<ReactComponent>]
    static member CommitLog(entries: GitLogEntry array) =
        UI.List(
            items = entries,
            multiSelect = true,
            itemTemplate =
                (fun entry ->
                    [
                        Html.span [ prop.className "select-none"; prop.text entry.Subject ]
                        Html.span [ prop.className "select-none"; prop.text entry.AbbreviatedCommit ]
                    ]
                )
            )

    [<ReactComponent>]
    static member StatusEntries(entries: GitStatusEntry array) =
        UI.List(
            items = Array.sortBy (fun x -> x.Filename) entries,
            multiSelect = true,
            itemTemplate =
                (fun entry ->
                    [
                        let icon =
                            match entry.Status with
                            | ModifiedInWorkTreeSinceIndex
                            | ModifiedInIndex -> Icon.Pencil
                            | AddedToIndex
                            | Untracked -> Icon.Plus
                            | DeletedInWorkTree
                            | DeletedFromIndex -> Icon.Minus
                            | _ -> Icon.QuestionMarkCircle

                        Html.img
                            [
                                prop.className "inline-block text-white mx-1 w-4"
                                prop.src (Icon.asFilepath icon)
                            ]

                        Html.span [ prop.className "select-none"; prop.text entry.Filename ]
                    ]
                )
            )
        