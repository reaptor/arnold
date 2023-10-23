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

        let json = msg |> Encode.Auto.toString
        let buffer = json |> Encoding.UTF8.GetBytes

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
                                let pathParts =
                                    e.FullPath.Split(
                                        Path.DirectorySeparatorChar,
                                        StringSplitOptions.RemoveEmptyEntries
                                    )

                                if not (Array.contains ".git" pathParts) then
                                    sendServerMessage repoPath FileChanged |> ignore<Task<unit>>

                            watcher.Changed.Add(f)
                            watcher.Created.Add(f)
                            watcher.Deleted.Add(f)
                            watcher.Renamed.Add(f)
                            watcher

                        let _, sockets = repositories.GetOrAdd(repoPath, (setupWatcher (), ResizeArray()))
                        sockets.Add(socket)

                        try
                            while not socket.CloseStatus.HasValue do
                                let! text = receiveAllText socket ctx.RequestAborted

                                do!
                                    match Decode.Auto.fromString<ClientMessage> text with
                                    | Ok GitStatus ->
                                        startProcess "git" "status --porcelain -z" repoPath
                                        |> Result.map GitStatus.parsePorcelain
                                        |> GitStatusResponse
                                        |> sendServerMessage repoPath
                                    | Ok(GetFile entry) ->
                                        try
                                            {
                                                Name = entry.Filename
                                                Content = File.ReadAllText(Path.Combine(repoPath, entry.Filename))
                                            }
                                            |> Some
                                            |> Ok
                                        with ex ->
                                            Error(ex.ToString())
                                        |> GetFileResponse
                                        |> sendServerMessage repoPath
                                    | Ok(SaveFile data) ->
                                        try
                                            File.WriteAllText(Path.Combine(repoPath, data.Name), data.Content)
                                            Ok()
                                        with ex ->
                                            Error(ex.ToString())
                                        |> SaveFileResponse
                                        |> sendServerMessage repoPath
                                    | Error e -> sendServerMessage repoPath (UnknownServerError e)

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
