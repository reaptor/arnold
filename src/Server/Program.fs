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

module GitStatus =
    let (|X|_|) (x: string) (input: string) =
        if String.exists (fun ch -> ch = input[0]) x then
            Some()
        else
            None

    let (|Y|_|) (y: string) (input: string) =
        if String.exists (fun ch -> ch = input[1]) y then
            Some()
        else
            None

    let (|Filename|) (input: string) = input[3..].Trim(' ').Trim('"')

    let parsePorcelain (s: string) =
        s.Split([| char 0 |], StringSplitOptions.RemoveEmptyEntries)
        |> Array.collect (fun s -> [|
            // https://git-scm.com/docs/git-status#_short_format
            // https://git-scm.com/docs/git-status#_porcelain_format_version_1
            match s with
            | X "M" & Y " MTD" & Filename filename -> ModifiedInIndex, filename
            | X "T" & Y " MTD" & Filename filename -> TypeChangedInIndex, filename
            | X "A" & Y " MTD" & Filename filename -> AddedToIndex, filename
            | X "D" & Y " " & Filename filename -> DeletedFromIndex, filename
            | X "R" & Y " MTD" & Filename filename -> RenamedInIndex, filename
            | X "C" & Y " MTD" & Filename filename -> CopiedInIndex, filename
            | _ -> ()
            match s with
            | X " MTARC" & Y "M" & Filename filename -> ModifiedInWorkTreeSinceIndex, filename
            | X " MTARC" & Y "T" & Filename filename -> TypeChangedInWorkTreeSinceIndex, filename
            | X " MTARC" & Y "D" & Filename filename -> DeletedInWorkTree, filename
            | X " " & Y "R" & Filename filename -> RenamedInWorkTree, filename
            | X " " & Y "C" & Filename filename -> CopiedInWorkTree, filename
            | X "D" & Y "D" & Filename filename -> UnmergedBothDeleted, filename
            | X "A" & Y "U" & Filename filename -> UnmergedAddedByUs, filename
            | X "U" & Y "D" & Filename filename -> UnmergedDeletedByThem, filename
            | X "U" & Y "A" & Filename filename -> UnmergedAddedByThem, filename
            | X "D" & Y "U" & Filename filename -> UnmergedDeletedByUs, filename
            | X "A" & Y "A" & Filename filename -> UnmergedBothAdded, filename
            | X "U" & Y "U" & Filename filename -> UnmergedBothModified, filename
            | X "?" & Y "?" & Filename filename -> Untracked, filename
            | X "!" & Y "!" & Filename filename -> Ignored, filename
            | _ -> ()
        |])
        |> Array.map (fun (status, filename) -> { Filename = filename; Status = status })

let sockets = ResizeArray<WebSocket>()
let watchers = ConcurrentDictionary<RepositoryPath, FileSystemWatcher>()

let removeSocket socket =
    let watchersToRemove =
        watchers
        |> Seq.choose (
            function
            | KeyValue(repoPath, watcher) ->
                if sockets.Remove socket && sockets.Count = 0 then
                    Some(repoPath, watcher)
                else
                    None
        )

    for repoPath, watcher in watchersToRemove do
        printfn "Disposing watcher"
        watcher.Dispose()
        watchers.TryRemove(repoPath) |> ignore

let removeClosedSockets () =
    let closedSockets =
        sockets
        |> Seq.filter (fun socket -> socket.State = WebSocketState.Closed || socket.State = WebSocketState.Aborted)

    for socket in closedSockets do
        removeSocket socket

let sendServerMessage (socket: WebSocket) (msg: ServerMessage) =
    task {
        removeClosedSockets ()

        let json = msg |> Encode.Auto.toString
        let buffer = json |> Encoding.UTF8.GetBytes

        printfn $"Sending server message {json}"

        do!
            socket.SendAsync(
                ArraySegment<byte>(buffer, 0, buffer.Length),
                WebSocketMessageType.Text,
                true,
                CancellationToken.None
            )
    }

let broadcastServerMessage (msg: ServerMessage) =
    task {
        removeClosedSockets ()

        let json = msg |> Encode.Auto.toString
        let buffer = json |> Encoding.UTF8.GetBytes

        printfn $"Broadcasting server message {json}"

        for socket in sockets do
            do!
                socket.SendAsync(
                    ArraySegment<byte>(buffer, 0, buffer.Length),
                    WebSocketMessageType.Text,
                    true,
                    CancellationToken.None
                )
    }

let receiveAllText (socket: WebSocket) cancellationToken =
    task {
        let all = ResizeArray()
        let buffer = Array.zeroCreate<byte> (4096)
        let mutable result = null
        let! res = socket.ReceiveAsync(ArraySegment<byte>(buffer), cancellationToken)
        result <- res
        all.AddRange(buffer[0 .. result.Count - 1])

        while not result.EndOfMessage do
            let! res = socket.ReceiveAsync(ArraySegment<byte>(buffer), cancellationToken)
            result <- res
            all.AddRange(buffer[0 .. result.Count - 1])

        return Encoding.UTF8.GetString(all.ToArray())
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
            if
                ctx.WebSockets.IsWebSocketRequest
                && not (ctx.Request.Headers["Sec-WebSocket-Extensions"] |> Seq.contains "vite-hmr")
            then
                task {
                    let! socket = ctx.WebSockets.AcceptWebSocketAsync()
                    sockets.Add(socket)
                    printfn "Web socket accepted"

                    try
                        while not socket.CloseStatus.HasValue do
                            let! text = receiveAllText socket ctx.RequestAborted

                            printfn $"Received client message {text}"

                            do!
                                match Decode.Auto.fromString<ClientMessage> text with
                                | Ok(ChangeRepository repositoryPath) ->
                                    if not (watchers.ContainsKey(repositoryPath)) then
                                        printfn $"Adding watcher for {repositoryPath}"

                                        let watcher =
                                            new FileSystemWatcher(
                                                RepositoryPath.value repositoryPath,
                                                IncludeSubdirectories = true,
                                                EnableRaisingEvents = true
                                            )

                                        let f (e: FileSystemEventArgs) =
                                            let pathParts =
                                                e.FullPath.Split(
                                                    Path.DirectorySeparatorChar,
                                                    StringSplitOptions.RemoveEmptyEntries
                                                )

                                            if Array.contains ".git" pathParts then
                                                broadcastServerMessage (RepositoryChanged repositoryPath)
                                            else
                                                broadcastServerMessage (FileChanged repositoryPath)
                                            |> ignore<Task<unit>>

                                        watcher.Changed.Add(f)
                                        watcher.Created.Add(f)
                                        watcher.Deleted.Add(f)
                                        watcher.Renamed.Add(f)

                                        watchers.TryAdd(repositoryPath, watcher) |> ignore<bool>

                                    sendServerMessage socket (RepositoryChanged repositoryPath)
                                | Ok(GitStatus repositoryPath) ->
                                    match watchers.TryGetValue(repositoryPath) with
                                    | true, watcher ->
                                        try
                                            watcher.EnableRaisingEvents <- false

                                            startProcess
                                                "git"
                                                "status --porcelain -z"
                                                (RepositoryPath.value repositoryPath)
                                            |> Result.map GitStatus.parsePorcelain
                                            |> GitStatusResponse
                                            |> sendServerMessage socket
                                        finally
                                            watcher.EnableRaisingEvents <- true
                                    | _ -> task { () }
                                | Ok(GetFile(RepositoryPath repositoryPath, entry)) ->
                                    printfn "%A" entry

                                    try
                                        {
                                            Name = entry.Filename
                                            Content = File.ReadAllText(Path.Combine(repositoryPath, entry.Filename))
                                        }
                                        |> Some
                                        |> Ok
                                    with ex ->
                                        Error(ex.ToString())
                                    |> GetFileResponse
                                    |> sendServerMessage socket
                                | Ok(SaveFile(RepositoryPath repositoryPath, data)) ->
                                    try
                                        File.WriteAllText(Path.Combine(repositoryPath, data.Name), data.Content)
                                        Ok()
                                    with ex ->
                                        Error(ex.ToString())
                                    |> SaveFileResponse
                                    |> sendServerMessage socket
                                | Error e -> sendServerMessage socket (UnknownServerError e)

                        removeSocket socket

                        printfn "Closing socket"

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
