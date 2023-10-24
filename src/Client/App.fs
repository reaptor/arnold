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
open Git
open Util

// Warning icon
// Do you want to save the changes you made to {filename}?
// Your changes will be lost if you don't save them.
// Save
// Don't save
// Cancel

type Model = {
    Status: GitStatusEntry array
    CurrentFile: FileData option
    Socket: WebSocket option
    CurrentRepository: RepositoryPath option
}

type Msg =
    | ChangeRepository of RepositoryPath option
    | WebSocketOpened of WebSocket
    | WebSocketErrored of WebSocket
    | ServerMessageReceived of WebSocket * MessageEvent
    | SendClientMessage of ClientMessage
    | SendClientMessageSucceeded of unit
    | SendClientMessageFailed of exn
    | SaveCurrentFile
    | UpdateModel of Model

let getCurrentRepositoryPath () =
    let searchParams = URLSearchParams.Create(window.location.search)
    let gitFilepath = searchParams.get "repoPath"
    gitFilepath |> Option.map RepositoryPath.ofUriEncoded

let setCurrentRepositoryPath (repositoryPath: RepositoryPath option) =
    let searchParams = URLSearchParams.Create(window.location.search)

    console.log ("Settings repoPath=", repositoryPath)

    searchParams.set (
        "repoPath",
        match repositoryPath with
        | Some repositoryPath' -> RepositoryPath.asUriEncoded repositoryPath'
        | None -> null
    )

    window.history.replaceState (null, url = searchParams.ToString())

let sendClientMessage (socket: WebSocket) (msg: ClientMessage) =
    promise {
        try
            msg |> Encode.Auto.toString |> socket.send
        with ex ->
            console.log ($"Encode error: {ex}")
    }

let init () =
    {
        Status = [||]
        CurrentFile = None
        Socket = None
        CurrentRepository = None
    },
    Cmd.none

let update (msg: Msg) (model: Model) =
    match msg with
    | WebSocketOpened socket ->
        console.log "Socket opened"

        { model with Socket = Some socket },
        match getCurrentRepositoryPath () with
        | Some repositoryPath -> Cmd.ofMsg (SendClientMessage(ClientMessage.ChangeRepository repositoryPath))
        | None -> Cmd.none
    | WebSocketErrored _ -> model, Cmd.Empty
    | ChangeRepository repositoryPath ->
        setCurrentRepositoryPath repositoryPath

        {
            model with
                CurrentRepository = repositoryPath
                CurrentFile = None
        },
        match repositoryPath with
        | Some repositoryPath' ->
            console.log ("sending ChangeRepository")
            Cmd.ofMsg (SendClientMessage(ClientMessage.ChangeRepository repositoryPath'))
        | None -> Cmd.none
    | SendClientMessage cmd ->
        console.log ("SendClientMessage", cmd)

        match model.Socket with
        | Some socket ->
            model,
            Cmd.OfPromise.either (sendClientMessage socket) cmd SendClientMessageSucceeded SendClientMessageFailed
        | None ->
            console.log ("Skipping SendClientMessage due to socket is not connected")
            model, Cmd.Empty
    | SendClientMessageSucceeded() -> model, Cmd.none
    | SendClientMessageFailed ex ->
        console.log ex
        model, Cmd.none
    | ServerMessageReceived(_, e) ->
        console.log ($"Received server message {string e.data}")

        try
            Decode.Auto.fromString<ServerMessage> (string e.data)
            |> function
                | Ok(GitStatusResponse(Ok entries)) -> model, Cmd.ofMsg (UpdateModel({ model with Status = entries }))
                | Ok(GetFileResponse(Ok content)) -> { model with CurrentFile = content }, Cmd.none
                | Ok(FileChanged repositoryPath) ->
                    console.log ("file changed.")
                    model, Cmd.ofMsg (SendClientMessage(GitStatus repositoryPath))
                | Ok(RepositoryChanged repositoryPath) ->
                    {
                        model with
                            CurrentRepository = Some repositoryPath
                    },
                    Cmd.ofMsg (SendClientMessage(GitStatus repositoryPath))
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
        match model.CurrentRepository, model.CurrentFile with
        | Some repositoryPath, Some data ->
            console.log ("Saving")
            model, Cmd.ofMsg (SaveFile(repositoryPath, data) |> SendClientMessage)
        | _ -> model, Cmd.Empty

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
                        prop.className "flex flex-col justify-stretch gap-2"
                        prop.children [
                            UI.RecentRepositories(model.CurrentRepository, Some >> ChangeRepository >> dispatch)
                            if unstaged.Length > 0 then
                                Html.div [
                                    prop.children [
                                        UI.StatusEntries(
                                            entries = unstaged,
                                            selectionChanged =
                                                (fun (entry, _) ->
                                                    match model.CurrentRepository with
                                                    | Some r -> GetFile(r, entry) |> SendClientMessage |> dispatch
                                                    | None -> ()
                                                )
                                        )
                                    ]
                                ]
                            Html.div [
                                prop.className "flex gap-2"
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
                                        (fun (entry, _) ->
                                            match model.CurrentRepository with
                                            | Some r -> GetFile(r, entry) |> SendClientMessage |> dispatch
                                            | None -> ()
                                        )
                                )
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

let connectWs _ dispatch =
    console.log "Connecting socket..."
    let socket = WebSocket.Create("ws://localhost:5000/")
    socket.onopen <- fun e -> dispatch (WebSocketOpened socket)
    socket.onerror <- fun e -> dispatch (WebSocketErrored socket)
    socket.onmessage <- fun e -> dispatch (ServerMessageReceived(socket, e))
    React.createDisposable (fun () -> socket.close ())

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
|> Program.withSubscription (fun model -> [
    [ "connectWs" ], connectWs model
    [ "keyDownSubscription" ], keyDownSubscription
])
|> Program.run
