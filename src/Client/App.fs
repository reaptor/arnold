module App

open System
open Browser.Types
open Feliz
open Browser
open Elmish
open Elmish.React
open FSharp.Core
open UI
open Git
open GitComponents
open Shared
open Thoth.Json
open Fable.Core.JsInterop

let monaco: obj = importAll "monaco-editor"

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
    FileContent: string option
}

type Msg =
    | WebSocketOpened of Event
    | WebSocketErrored of Event
    | SendClientMessage of ClientMessage
    | SendClientMessageSucceeded of unit
    | SendClientMessageFailed of exn
    | ServerMessageReceived of MessageEvent
    | StatusEntriesLoaded of GitStatusEntry array

let sendClientMessage (msg: ClientMessage) =
    promise {
        try
            match socket with
            | Some socket' -> msg |> Encode.Auto.toString |> socket'.send
            | None -> ()
        with ex ->
            console.log (ex)
    }

let init () =
    { Status = [||]; FileContent = None }, Cmd.none

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
        Decode.Auto.fromString<ServerMessage> (string e.data)
        |> function
            | Ok(GitStatusResponse(Ok entries)) -> model, Cmd.ofMsg (StatusEntriesLoaded(entries))
            | Ok(GetFileContentResponse(Ok content)) -> { model with FileContent = content }, Cmd.none
            | Ok FileChanged ->
                console.log ("file changed")
                model, Cmd.ofMsg (SendClientMessage GitStatus)
            | Ok(GitStatusResponse(Error e))
            | Ok(GetFileContentResponse(Error e)) ->
                console.log e
                { model with FileContent = Some e }, Cmd.Empty
            | Error e ->
                console.log e
                model, Cmd.Empty
    | StatusEntriesLoaded status -> { model with Status = status }, Cmd.none

[<ReactComponent>]
let CodeEditor (language: string) (content: string) =
    let editor, setEditor = React.useStateWithUpdater None
    let monacoEl = React.useInputRef ()

    React.useEffect (
        (fun () ->
            let resizeEvent =
                (fun _ ->
                    match editor, monacoEl.current with
                    | Some editor', Some el ->
                        editor'?layout (
                            {|
                                height = el.offsetHeight - 5.
                                width = el.offsetWidth - 2.
                            |}
                        )
                    | _ -> ()
                )

            match monacoEl.current with
            | Some monacoEl' ->
                window.addEventListener ("resize", resizeEvent)

                setEditor (fun e ->
                    if e.IsSome then
                        e
                    else
                        monaco?editor?create (
                            monacoEl',
                            {|
                                value = content
                                language = language
                                theme = "vs-dark"
                            |}
                        )
                        |> Some
                )
            | None -> ()

            React.createDisposable (fun () ->
                window.removeEventListener ("resize", resizeEvent)

                match editor with
                | Some editor' -> editor'?dispose ()
                | None -> ()
            )
        ),
        [| monacoEl.current |> Option.toObj |> box |]
    )

    match editor with
    | Some editor' -> editor'?setValue (content)
    | None -> ()

    Html.div [
        prop.ref monacoEl
        prop.className "shadow-lg font-mono text-neutral-300 grow outline-none border border-black bg-[#1e1e1e] rounded"
    ]

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
                                                (fun (entry, _) ->
                                                    GetFileContent entry |> SendClientMessage |> dispatch
                                                )
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
                                                (fun (entry, _) ->
                                                    entry |> GetFileContent |> SendClientMessage |> dispatch
                                                )
                                        )
                                ]
                            ]
                        ]
                    ]
                    match model.FileContent with
                    | Some fileContent -> fileContent
                    | None -> ""
                    |> CodeEditor "fsharp"
                // Html.textarea [
                //     prop.className
                //         "shadow-lg font-mono text-neutral-300 grow outline-none border border-black bg-neutral-800 rounded p-2"
                //     prop.value (
                //         match model.FileContent with
                //         | Some fileContent -> fileContent
                //         | None -> ""
                //     )
                // ]
                ]
            ]
        ]
    ]

let mkDisposable f =
    { new IDisposable with
        member _.Dispose() = f ()
    }

let connectWs dispatch =
    match socket with
    | Some ws' ->
        ws'.onopen <- fun e -> dispatch (WebSocketOpened e)
        ws'.onerror <- fun e -> dispatch (WebSocketErrored e)
        ws'.onmessage <- fun e -> dispatch (ServerMessageReceived e)
        mkDisposable (fun () -> ws'.close ())
    | None -> mkDisposable ignore

Program.mkProgram init update view
|> Program.withReactBatched "root"
|> Program.withSubscription (fun _model -> [ [ "ws" ], connectWs ])
|> Program.run
