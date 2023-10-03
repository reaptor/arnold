open System
open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.DependencyInjection
open System.IO
open System.Diagnostics
open Microsoft.AspNetCore.Http
open System.Threading.Tasks
open Microsoft.AspNetCore.Cors.Infrastructure

type GitRequest = { Path: string }
type GitResponse = { Error: string; Data: string }

let git commandWithArgs path =
    try
        let p =
            ProcessStartInfo(
                "git",
                commandWithArgs,
                RedirectStandardOutput = true,
                RedirectStandardError = true,
                WorkingDirectory = path
            )
            |> Process.Start

        p.WaitForExit()

        p.StandardOutput.ReadToEnd()
        |> fun output -> if p.ExitCode <> 0 then Error output else Ok output
    with ex ->
        Error ex.Message

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

    app.UseCors() |> ignore<IApplicationBuilder>

    app.MapPost(
        "/git/log",
        Func<GitRequest, HttpContext, Task>(fun req ctx ->
            git
                """log --pretty=format:{\"commit\":\"%H\",\"abbreviated_commit\":\"%h\",\"tree\":\"%T\",\"abbreviated_tree\":\"%t\",\"parent\":\"%P\",\"abbreviated_parent\":\"%p\",\"refs\":\"%D\",\"encoding\":\"%e\",\"subject\":\"%s\",\"sanitized_subject_line\":\"%f\",\"body\":\"%b\",\"commit_notes\":\"%N\",\"verification_flags\":\"%G?\",\"signer\":\"%GS\",\"signer_key\":\"%GK\",\"author\":{\"name\":\"%aN\",\"email\":\"%aE\",\"date\":\"%aD\"},\"commiter\":{\"name\":\"%cN\",\"email\":\"%cE\",\"date\":\"%cD\"}},"""
                req.Path
            |> function
                | Ok output ->
                    ctx.Response.ContentType <- "application/json"

                    let data =
                        match output with
                        | data when data.EndsWith(",") -> data.Substring(0, data.Length - 1)
                        | data -> data

                    """{ "error": null, "data": [{{data}}] }""".Replace("{{data}}", data)
                    |> ctx.Response.WriteAsync

                | Error output -> { Error = output; Data = null } |> ctx.Response.WriteAsJsonAsync
        )
    )
    |> ignore

    app.MapPost(
        "/git/status",
        Func<GitRequest, HttpContext, Task>(fun req ctx ->
            git "status --porcelain" req.Path
            |> function
                | Ok output ->
                    ctx.Response.ContentType <- "application/json"
                    let data = output.ReplaceLineEndings("\\n").Replace("\"", "\\\"")

                    // let data =
                    //     GitStatus.parsePorcelain output
                    //     |> Array.map (fun (status, filename) ->
                    //         {|
                    //             status = GitStatus.asString status
                    //             filename = filename
                    //         |}
                    //     )
                    //     |> System.Text.Json.JsonSerializer.Serialize

                    """{ "error": null, "data": "{{data}}" }""".Replace("{{data}}", data)
                    |> ctx.Response.WriteAsync
                | Error output -> { Error = output; Data = null } |> ctx.Response.WriteAsJsonAsync
        )
    )
    |> ignore

    app.Run()

    0 // Exit code
