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
            items = entries,
            multiSelect = true,
            itemTemplate =
                (fun entry ->
                    [
                        let imgName =
                            match entry.Status with
                            | ModifiedInWorkTree
                            | ModifiedInIndex -> "pencil"
                            | AddedToIndex
                            | Untracked -> "plus"
                            | DeletedInWorkTree
                            | DeletedFromIndex -> "minus"
                            | _ -> ""

                        Html.img
                            [
                                prop.className "inline-block text-white mx-1 w-4"
                                prop.src $"svg/%s{imgName}.svg"
                            ]

                        Html.span [ prop.className "select-none"; prop.text entry.Filename ]
                    ]
                )
            )
        