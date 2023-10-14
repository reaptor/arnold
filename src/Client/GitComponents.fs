module GitComponents

open Feliz
open Browser
open FSharp.Core
open Git
open UI

type UI with
    [<ReactComponent>]
    static member CommitLog(entires: GitLogEntry array) =
        let keyboardNavigation = useKeyboardNavigation (entires.Length)

        Html.div
            [
                prop.className "text-neutral-300 m-5 border border-black"
                prop.children (
                    entires
                    |> Array.mapi (fun i entry ->
                        Html.div
                            [
                                prop.ref (fun e ->
                                    e :?> Types.HTMLElement
                                    |> Option.ofObj
                                    |> Option.iter (fun elem ->
                                        if not (keyboardNavigation.Refs.current.Contains(Some elem)) then
                                            keyboardNavigation.Refs.current.Add(Some elem)
                                    )
                                )
                                prop.classes
                                    [
                                        "outline-none border-black p-0.5"
                                        if i > 0 then
                                            "border-t"
                                        if keyboardNavigation.SelectedIndex = i then
                                            "bg-neutral-700"
                                    ]
                                prop.onKeyDown (fun e -> keyboardNavigation.KeyDown i e.key)
                                prop.onMouseDown (fun e -> keyboardNavigation.MouseDown e i)
                                prop.children
                                    [
                                        Html.span [ prop.className "select-none"; prop.text entry.Subject ]
                                        Html.span [ prop.className "select-none"; prop.text entry.AbbreviatedCommit ]
                                    ]
                            ]
                    )
                )
            ]

    [<ReactComponent>]
    static member StatusEntries(entires: GitStatusEntry array) =
        let keyboardNavigation = useKeyboardNavigation (entires.Length)

        Html.div
            [
                prop.className "text-neutral-300 m-5 border border-black"
                prop.children (
                    entires
                    |> Array.mapi (fun i entry ->
                        Html.div
                            [
                                prop.ref (fun e ->
                                    e :?> Types.HTMLElement
                                    |> Option.ofObj
                                    |> Option.iter (fun elem ->
                                        if not (keyboardNavigation.Refs.current.Contains(Some elem)) then
                                            keyboardNavigation.Refs.current.Add(Some elem)
                                    )
                                )
                                prop.classes
                                    [
                                        "outline-none border-black p-0.5"
                                        if i > 0 then
                                            "border-t"
                                        if keyboardNavigation.SelectedIndex = i then
                                            "bg-neutral-700"
                                    ]
                                prop.onKeyDown (fun e -> keyboardNavigation.KeyDown i e.key)
                                prop.onMouseDown (fun e -> keyboardNavigation.MouseDown e i)
                                prop.children
                                    [
                                        let imgName =
                                            match entry.Status with
                                            | NotUpdated
                                            | UpdatedInIndex -> "pencil"
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
                            ]
                    )
                )
            ]