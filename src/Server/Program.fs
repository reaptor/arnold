open System
open System.Collections.Concurrent
open System.Net.WebSockets
open System.Text
open System.Threading
open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.DependencyInjection
open System.IO
open System.Diagnostics
open Microsoft.AspNetCore.Http
open System.Threading.Tasks
open Microsoft.AspNetCore.Cors.Infrastructure
open Microsoft.FSharp.Core
open Shared
open Thoth.Json
open Thoth.Json.Net

type GitRequest = { Path: string }
type GitResponse = { Error: string; Data: string }

let startProcess (req: ProcessRequest) =
    try
        let p =
            ProcessStartInfo(
                req.FileName,
                req.Args,
                RedirectStandardOutput = true,
                RedirectStandardError = true,
                WorkingDirectory = req.WorkingDir
            )
            |> Process.Start

        p.WaitForExit()

        p.StandardOutput.ReadToEnd()
        |> fun output -> if p.ExitCode <> 0 then Error output else Ok output
    with ex ->
        Error ex.Message

// let git commandWithArgs path = runCommand "git" commandWithArgs path

let sockets = ConcurrentDictionary<WebSocket, string>()

let sendProccessResponse repoPath (response: Result<ProcessRequest * string, string>) =
    task {
        for KeyValue(socket, p) in
            sockets
            |> Seq.filter (
                function
                | KeyValue(_, p) -> p = repoPath
            ) do
            if socket.State = WebSocketState.Closed then
                sockets.TryRemove(socket, ref p) |> ignore
            else
                let json = response |> Encode.Auto.toString
                let buffer = json |> Encoding.UTF8.GetBytes

                do!
                    socket.SendAsync(
                        ArraySegment<byte>(buffer, 0, buffer.Length),
                        WebSocketMessageType.Text,
                        true,
                        CancellationToken.None
                    )
    }

type Worker() =
    inherit BackgroundService()

    override _.ExecuteAsync(stoppingToken) =
        task {
            while not stoppingToken.IsCancellationRequested do
                do! Task.Delay(5000)
        }

[<EntryPoint>]
let main args =
    let builder = WebApplication.CreateBuilder(args)

    builder.Services
        .AddHostedService<Worker>()
        .AddCors(fun options ->
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

                        sockets.AddOrUpdate(socket, (fun _ -> repoPath), (fun _ _ -> repoPath))
                        |> ignore

                        let buffer = Array.zeroCreate<byte> (4096)
                        printfn "Receiving from socket..."

                        try
                            while not socket.CloseStatus.HasValue do
                                let! receiveResult =
                                    socket.ReceiveAsync(ArraySegment<byte>(buffer), ctx.RequestAborted)

                                do!
                                    match
                                        Decode.Auto.fromString<ProcessRequest> (
                                            Encoding.UTF8.GetString(buffer, 0, receiveResult.Count)
                                        )
                                    with
                                    | Ok req ->
                                        startProcess req
                                        |> Result.map (fun response -> req, response)
                                        |> sendProccessResponse repoPath
                                    | Error e -> sendProccessResponse repoPath (Error e)

                            do!
                                socket.CloseAsync(
                                    socket.CloseStatus.Value,
                                    socket.CloseStatusDescription,
                                    CancellationToken.None
                                )
                        with
                        | :? TaskCanceledException -> ()
                        | ex ->
                            sockets.TryRemove(socket, ref (string repoPath)) |> ignore
                            printfn $"Socket receive exception. %A{ex}"
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
    //
    // app.MapPost(
    //     "/git/status",
    //     Func<GitRequest, HttpContext, Task>(fun req ctx ->
    //         if not (Directory.Exists req.Path) then
    //             Error $"Path not found. Got '%s{req.Path}'"
    //         else
    //             git "status --porcelain -z" req.Path
    //         |> function
    //             | Ok output ->
    //                 ctx.Response.ContentType <- "application/json"
    //                 let data = output.Replace(char 0, '\n').Replace("\n", "\\n").Replace("\"", "\\\"")
    //
    //                 """{ "error": null, "data": "{{data}}" }""".Replace("{{data}}", data)
    //                 |> ctx.Response.WriteAsync
    //             | Error output -> { Error = output; Data = null } |> ctx.Response.WriteAsJsonAsync
    //     )
    // )
    // |> ignore

    app.Run()

    0 // Exit code
