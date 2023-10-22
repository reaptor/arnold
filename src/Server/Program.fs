open System
open System.Collections.Concurrent
open System.Net.WebSockets
open System.Text
open System.Threading
open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.DependencyInjection
open System.Diagnostics
open Microsoft.AspNetCore.Http
open System.Threading.Tasks
open Microsoft.AspNetCore.Cors.Infrastructure
open Microsoft.FSharp.Core
open Shared
open Thoth.Json.Net
open System.IO

let startProcess fileName args workingDir =
    try
        let p =
            ProcessStartInfo(
                fileName,
                args,
                RedirectStandardOutput = true,
                RedirectStandardError = true,
                WorkingDirectory = workingDir
            )
            |> Process.Start

        p.WaitForExit()

        p.StandardOutput.ReadToEnd()
        |> fun output -> if p.ExitCode <> 0 then Error output else Ok output
    with ex ->
        Error ex.Message

let repositories =
    ConcurrentDictionary<string, FileSystemWatcher * ResizeArray<WebSocket>>()

let removeSocket socket =
    let respositoriesToRemove =
        repositories
        |> Seq.choose (
            function
            | KeyValue(repoPath, (watcher, sockets)) ->
                if sockets.Remove socket && sockets.Count = 0 then
                    Some(repoPath, watcher)
                else
                    None
        )

    for repoPath, watcher in respositoriesToRemove do
        printfn "Disposing watcher"
        watcher.Dispose()
        repositories.TryRemove(repoPath) |> ignore

let removeClosedSockets () =
    let closedSockets =
        repositories
        |> Seq.collect (
            function
            | KeyValue(_, (_, sockets)) ->
                sockets
                |> Seq.filter (fun socket ->
                    socket.State = WebSocketState.Closed || socket.State = WebSocketState.Aborted
                )
        )
        |> Seq.toArray

    for socket in closedSockets do
        removeSocket socket

let sendServerMessage repoPath (msg: ServerMessage) =
    task {
        removeClosedSockets ()

        let sockets =
            repositories
            |> Seq.collect (
                function
                | KeyValue(p, (_, sockets)) -> if p = repoPath then Array.ofSeq sockets else Array.empty
            )

        for socket in sockets do
            let json = msg |> Encode.Auto.toString
            let buffer = json |> Encoding.UTF8.GetBytes

            do!
                socket.SendAsync(
                    ArraySegment<byte>(buffer, 0, buffer.Length),
                    WebSocketMessageType.Text,
                    true,
                    CancellationToken.None
                )
    }

[<EntryPoint>]
let main args =
    let builder = WebApplication.CreateBuilder(args)

    builder.Services.AddCors(fun options ->
        options.AddDefaultPolicy(fun policy ->
            policy.AllowAnyOrigin().AllowAnyMethod().AllowAnyHeader()
            |> ignore<CorsPolicyBuilder>
        )
    )
    |> ignore<IServiceCollection>

    let app = builder.Build()

    let webSocketOptions = WebSocketOptions(KeepAliveInterval = TimeSpan.FromMinutes(2))

    app.UseCors().UseWebSockets(webSocketOptions) |> ignore<IApplicationBuilder>

    app.Map(
        "/",
        Func<HttpContext, Task>(fun ctx ->
            match ctx.Request.Query.TryGetValue("repoPath") with
            | true, repoPath ->
                if ctx.WebSockets.IsWebSocketRequest then
                    task {
                        let repoPath = string repoPath
                        printfn "Web socket connect requested"
                        let! socket = ctx.WebSockets.AcceptWebSocketAsync()

                        let setupWatcher () =
                            let watcher =
                                new FileSystemWatcher(
                                    repoPath,
                                    IncludeSubdirectories = true,
                                    EnableRaisingEvents = true
                                )

                            let f (e: FileSystemEventArgs) =
                                if
                                    not (
                                        e.FullPath.Contains(
                                            $"{Path.DirectorySeparatorChar}.git{Path.DirectorySeparatorChar}"
                                        )
                                    )
                                then
                                    printfn $"file changed {e.FullPath}"
                                    sendServerMessage repoPath FileChanged |> ignore<Task<unit>>

                            watcher.Changed.Add(f)
                            watcher.Created.Add(f)
                            watcher.Deleted.Add(f)
                            watcher.Renamed.Add(f)
                            watcher

                        let _, sockets = repositories.GetOrAdd(repoPath, (setupWatcher (), ResizeArray()))
                        sockets.Add(socket)

                        let buffer = Array.zeroCreate<byte> (4096)

                        try
                            while not socket.CloseStatus.HasValue do
                                let! receiveResult =
                                    socket.ReceiveAsync(ArraySegment<byte>(buffer), ctx.RequestAborted)

                                do!
                                    match
                                        Decode.Auto.fromString<ClientMessage> (
                                            Encoding.UTF8.GetString(buffer, 0, receiveResult.Count)
                                        )
                                    with
                                    | Ok GitStatus ->
                                        startProcess "git" "status --porcelain -z" repoPath
                                        |> GitStatusResponse
                                        |> sendServerMessage repoPath
                                    | Ok(GetFileContent fileName) ->
                                        try
                                            Ok(File.ReadAllText(Path.Combine(repoPath, fileName)))
                                        with ex ->
                                            Error(ex.ToString())
                                        |> GetFileContentResponse
                                        |> sendServerMessage repoPath
                                    | Error e -> sendServerMessage repoPath (GitStatusResponse(Error e))

                            removeSocket socket

                            printfn $"Closing socket for {repoPath}"

                            do!
                                socket.CloseAsync(
                                    socket.CloseStatus.Value,
                                    socket.CloseStatusDescription,
                                    CancellationToken.None
                                )
                        with
                        | :? TaskCanceledException -> ()
                        | ex ->
                            printfn "%A" ex
                            removeSocket socket
                    }
                else
                    ctx.Response.StatusCode <- 400
                    Task.CompletedTask
            | _ ->
                ctx.Response.StatusCode <- 400
                Task.CompletedTask
        )
    )
    |> ignore

    // app.MapPost(
    //     "/git/log",
    //     Func<GitRequest, HttpContext, Task>(fun req ctx ->
    //         git
    //             """log --pretty=format:{\"commit\":\"%H\",\"abbreviated_commit\":\"%h\",\"tree\":\"%T\",\"abbreviated_tree\":\"%t\",\"parent\":\"%P\",\"abbreviated_parent\":\"%p\",\"refs\":\"%D\",\"encoding\":\"%e\",\"subject\":\"%s\",\"sanitized_subject_line\":\"%f\",\"body\":\"%b\",\"commit_notes\":\"%N\",\"verification_flags\":\"%G?\",\"signer\":\"%GS\",\"signer_key\":\"%GK\",\"author\":{\"name\":\"%aN\",\"email\":\"%aE\",\"date\":\"%aD\"},\"commiter\":{\"name\":\"%cN\",\"email\":\"%cE\",\"date\":\"%cD\"}},"""
    //             req.Path
    //         |> function
    //             | Ok output ->
    //                 ctx.Response.ContentType <- "application/json"
    //
    //                 let data =
    //                     match output with
    //                     | data when data.EndsWith(",") -> data.Substring(0, data.Length - 1)
    //                     | data -> data
    //
    //                 """{ "error": null, "data": [{{data}}] }""".Replace("{{data}}", data)
    //                 |> ctx.Response.WriteAsync
    //
    //             | Error output -> { Error = output; Data = null } |> ctx.Response.WriteAsJsonAsync
    //     )
    // )
    // |> ignore

    app.Run()

    0
