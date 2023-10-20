module App

open Feliz
open Browser
open Elmish
open Elmish.React
open FSharp.Core
open UI
open Git
open GitComponents

type Model = { Status: GitStatusEntry array }

type Msg =
    | LoadStatusEntries
    | LoadStatusEntriesSucceeded of GitStatusEntry array
    | LoadStatusEntriesFailed of exn

let init () =
    { Status = [||] }, Cmd.ofMsg LoadStatusEntries

let update (msg: Msg) (model: Model) =
    let searchParams = URLSearchParams.Create(window.location.search)
    let gitFilepath = searchParams.get ("path")

    match msg with
    | LoadStatusEntries ->
        model,
        match gitFilepath with
        | Some gitFilepath' ->
            let repoName =
                gitFilepath'.Substring(gitFilepath'.Replace('\\', '/').LastIndexOf('/') + 1)

            document.title <- $"Git %s{repoName}"
            Cmd.OfPromise.either loadStatusEntries gitFilepath' LoadStatusEntriesSucceeded LoadStatusEntriesFailed
        | None ->
            document.title <- "Arnold"
            Cmd.none
    | LoadStatusEntriesSucceeded status -> { model with Status = status }, Cmd.none
    | LoadStatusEntriesFailed ex ->
        console.log (ex)
        model, Cmd.none

let view (model: Model) _dispatch =
    let unstaged =
        model.Status
        |> Array.filter (fun entry -> not (GitStatus.isStaged entry.Status))

    let staged =
        model.Status |> Array.filter (fun entry -> GitStatus.isStaged entry.Status)

    Html.div
        [
            prop.className "select-none m-2 text-sm"
            prop.children
                [
                    Html.div
                        [
                            prop.className "flex gap-2"
                            prop.children
                                [
                                    Html.div
                                        [
                                            prop.className ""
                                            prop.children
                                                [
                                                    if unstaged.Length > 0 then
                                                        UI.StatusEntries(entries = unstaged)
                                                    Html.div
                                                        [
                                                            prop.children
                                                                [
                                                                    Html.div
                                                                        [
                                                                            prop.className "my-2 flex gap-1"
                                                                            prop.children
                                                                                [
                                                                                    UI.Button(
                                                                                        icon = Icon.ArrowUpOnSquareStack
                                                                                    )
                                                                                    UI.Button(
                                                                                        "Unstage",
                                                                                        Icon.ArrowUpOnSquare
                                                                                    )
                                                                                    UI.Button(
                                                                                        "Stage",
                                                                                        Icon.ArrowDownOnSquare
                                                                                    )
                                                                                    UI.Button(
                                                                                        icon =
                                                                                            Icon.ArrowDownOnSquareStack
                                                                                    )
                                                                                ]
                                                                        ]
                                                                    if staged.Length > 0 then
                                                                        UI.StatusEntries(entries = staged)
                                                                ]
                                                        ]
                                                ]
                                        ]
                                    Html.div
                                        [
                                            prop.className
                                                "text-neutral-300 grow border border-black bg-neutral-800 rounded p-2"
                                            prop.children [ Html.div "asdasd" ]
                                        ]
                                ]
                        ]
                ]
        ]

Program.mkProgram init update view
|> Program.withReactBatched "root"
|> Program.run

// let root = ReactDOM.createRoot (document.getElementById "root")
//
// root.render (App())
