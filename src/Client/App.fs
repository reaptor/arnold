module App

open Browser.Types
open Feliz
open Browser
open Elmish
open Elmish.React
open FSharp.Core
open UI
open GitComponents
open Shared
open Thoth.Json

// Warning icon
// Do you want to save the changes you made to {filename}?
// Your changes will be lost if you don't save them.
// Save
// Don't save
// Cancel

let repoInfo =
    let searchParams = URLSearchParams.Create(window.location.search)
    let gitFilepath = searchParams.get "repoPath"

    gitFilepath
    |> Option.map (fun gitFilepath' -> {|
        Directory = gitFilepath'
        Name = gitFilepath'.Substring(gitFilepath'.Replace('\\', '/').LastIndexOf('/') + 1)
    |})

let socket =
    repoInfo
    |> Option.map (fun x -> WebSocket.Create($"ws://localhost:5000/?repoPath={x.Directory}"))

type Model = {
    Status: GitStatusEntry array
    CurrentFile: FileData option
}

type Msg =
    | WebSocketOpened of Event
    | WebSocketErrored of Event
    | SendClientMessage of ClientMessage
    | SendClientMessageSucceeded of unit
    | SendClientMessageFailed of exn
    | ServerMessageReceived of MessageEvent
    | SaveCurrentFile
    | UpdateModel of Model

let sendClientMessage (msg: ClientMessage) =
    promise {
        try
            match socket with
            | Some socket' -> msg |> Encode.Auto.toString |> socket'.send
            | None -> ()
        with ex ->
            console.log ($"Encode error: {ex}")
    }

let init () =
    { Status = [||]; CurrentFile = None }, Cmd.none

let update (msg: Msg) (model: Model) =
    match msg with
    | WebSocketOpened _ -> model, Cmd.ofMsg (SendClientMessage GitStatus)
    | WebSocketErrored _ -> model, []
    | SendClientMessage cmd ->
        model, Cmd.OfPromise.either sendClientMessage cmd SendClientMessageSucceeded SendClientMessageFailed
    | SendClientMessageSucceeded() -> model, Cmd.none
    | SendClientMessageFailed ex ->
        console.log ex
        model, Cmd.none
    | ServerMessageReceived e ->
        try
            Decode.Auto.fromString<ServerMessage> (string e.data)
            |> function
                | Ok(GitStatusResponse(Ok entries)) -> model, Cmd.ofMsg (UpdateModel({ model with Status = entries }))
                | Ok(GetFileResponse(Ok content)) -> { model with CurrentFile = content }, Cmd.none
                | Ok FileChanged ->
                    console.log ("file changed.")
                    model, Cmd.ofMsg (SendClientMessage GitStatus)
                | Ok(SaveFileResponse(Ok())) -> model, Cmd.Empty
                | Ok(GitStatusResponse(Error e))
                | Ok(GetFileResponse(Error e))
                | Ok(SaveFileResponse(Error e))
                | Ok(UnknownServerError(e)) ->
                    console.log ($"Response error: {e}")

                    {
                        model with
                            CurrentFile = Some { Name = ""; Content = e }
                    },
                    Cmd.Empty
                | Error e ->
                    console.log e
                    model, Cmd.Empty
        with ex ->
            console.log ($"Decode error: {ex}")
            model, Cmd.none
    | UpdateModel m -> m, Cmd.none
    | SaveCurrentFile ->
        match model.CurrentFile with
        | Some data ->
            console.log ("Saving")
            model, Cmd.ofMsg (data |> SaveFile |> SendClientMessage)
        | None -> model, Cmd.Empty

let view (model: Model) dispatch =
    let unstaged =
        model.Status
        |> Array.filter (fun entry -> not (GitStatus.isStaged entry.Status))

    let staged =
        model.Status |> Array.filter (fun entry -> GitStatus.isStaged entry.Status)

    Html.div [
        prop.className "select-none m-2 text-sm flex flex-col grow"
        prop.children [
            Html.div [
                prop.className "flex grow gap-2"
                prop.children [
                    Html.div [
                        prop.children [
                            if unstaged.Length > 0 then
                                Html.div [
                                    prop.className "mb-2"
                                    prop.children [
                                        UI.StatusEntries(
                                            entries = unstaged,
                                            selectionChanged =
                                                (fun (entry, _) -> GetFile entry |> SendClientMessage |> dispatch)
                                        )
                                    ]
                                ]
                            Html.div [
                                prop.className "flex flex-col gap-2"
                                prop.children [
                                    Html.div [
                                        prop.className "flex gap-1"
                                        prop.children [
                                            UI.Button(icon = Icon.ArrowUpOnSquareStack)
                                            UI.Button("Unstage", Icon.ArrowUpOnSquare)
                                            UI.Button("Stage", Icon.ArrowDownOnSquare)
                                            UI.Button(icon = Icon.ArrowDownOnSquareStack)
                                        ]
                                    ]
                                    if staged.Length > 0 then
                                        UI.StatusEntries(
                                            entries = staged,
                                            selectionChanged =
                                                (fun (entry, _) -> entry |> GetFile |> SendClientMessage |> dispatch)
                                        )
                                ]
                            ]
                        ]
                    ]
                    match model.CurrentFile with
                    | Some fileData ->
                        UI.CodeEditor(
                            fileData,
                            onChange =
                                (fun content ->
                                    {
                                        model with
                                            CurrentFile = Some { fileData with Content = content }
                                    }
                                    |> UpdateModel
                                    |> dispatch
                                )
                        )
                    | None -> Html.none
                ]
            ]
        ]
    ]

let connectWs dispatch =
    match socket with
    | Some ws' ->
        ws'.onopen <- fun e -> dispatch (WebSocketOpened e)
        ws'.onerror <- fun e -> dispatch (WebSocketErrored e)
        ws'.onmessage <- fun e -> dispatch (ServerMessageReceived e)
        React.createDisposable (fun () -> ws'.close ())
    | None -> React.createDisposable ignore

let keyDownSubscription dispatch =
    let handler (e: Event) =
        let e = e :?> KeyboardEvent

        if (e.metaKey || e.ctrlKey) && e.key = "s" then
            dispatch SaveCurrentFile
            e.preventDefault ()
        else
            ()

    window.addEventListener ("keydown", handler)
    React.createDisposable (fun () -> window.removeEventListener ("keydown", handler))

Program.mkProgram init update view
|> Program.withReactBatched "root"
|> Program.withSubscription (fun _model -> [
    [ "connectWs" ], connectWs
    [ "keyDownSubscription" ], keyDownSubscription
])
|> Program.run
