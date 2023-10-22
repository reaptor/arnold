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

type Model = { Status: GitStatusEntry array }

type Msg =
    | WebSocketOpened of Event
    | WebSocketErrored of Event
    | ProcessRequestMsg of ProcessRequestType
    | ProcessRequestSucceeded of unit
    | ProcessRequestFailed of exn
    | ProcessResponseMsg of MessageEvent
    | StatusEntriesLoaded of GitStatusEntry array

let processRequest type' fileName workingDir args = {
    Type = type'
    FileName = fileName
    Args = args
    WorkingDir = workingDir
}

let sendProcessRequest (type': ProcessRequestType) =
    promise {
        match socket with
        | Some socket' ->
            match type' with
            | Git command ->
                match repoInfo with
                | Some repoInfo' ->
                    GitCommand.asCommandArgs command
                    |> processRequest type' "git" repoInfo'.Directory
                    |> Encode.Auto.toString
                    |> socket'.send
                | None -> ()
        | None -> ()
    }

let handleProcessResponse (req: ProcessRequest, response: string) =
    match req.Type with
    | Git Status -> Cmd.ofMsg (StatusEntriesLoaded(GitStatus.parsePorcelain response))

let init () = { Status = [||] }, Cmd.none

let update (msg: Msg) (model: Model) =
    match msg with
    | WebSocketOpened _ -> model, Cmd.ofMsg (ProcessRequestMsg(Git Status))
    | WebSocketErrored _ -> model, []
    | ProcessRequestMsg cmd ->
        model, Cmd.OfPromise.either sendProcessRequest cmd ProcessRequestSucceeded ProcessRequestFailed
    | ProcessRequestSucceeded() -> model, Cmd.none
    | ProcessRequestFailed ex ->
        console.log (ex)
        model, Cmd.none
    | ProcessResponseMsg e ->
        let cmd =
            Decode.Auto.fromString<Result<ProcessRequest * string, string>> (string e.data)
            |> Result.bind id
            |> function
                | Ok x -> handleProcessResponse x
                | Error e ->
                    console.log (e)
                    []

        model, cmd
    | StatusEntriesLoaded status -> { model with Status = status }, Cmd.none

let view (model: Model) _dispatch =
    let unstaged =
        model.Status
        |> Array.filter (fun entry -> not (GitStatus.isStaged entry.Status))

    let staged =
        model.Status |> Array.filter (fun entry -> GitStatus.isStaged entry.Status)

    Html.div [
        prop.className "select-none m-2 text-sm"
        prop.children [
            Html.div [
                prop.className "flex gap-2"
                prop.children [
                    Html.div [
                        prop.className ""
                        prop.children [
                            if unstaged.Length > 0 then
                                Html.div [
                                    prop.className "mb-2"
                                    prop.children [ UI.StatusEntries(entries = unstaged) ]
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
                                        UI.StatusEntries(entries = staged)
                                ]
                            ]
                        ]
                    ]
                    Html.div [
                        prop.className "text-neutral-300 grow border border-black bg-neutral-800 rounded p-2"
                        prop.children [ Html.div "asdasd" ]
                    ]
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
        ws'.onmessage <- fun e -> dispatch (ProcessResponseMsg e)
        mkDisposable (fun () -> ws'.close ())
    | None -> mkDisposable ignore

Program.mkProgram init update view
|> Program.withReactBatched "root"
|> Program.withSubscription (fun _model -> [ [ "ws" ], connectWs ])
|> Program.run
