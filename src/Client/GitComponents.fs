module GitComponents

open Feliz
open Browser
open FSharp.Core
open Git
open UI
open Shared
open Thoth.Json
open Util

type UI with

    // [<ReactComponent>]
    // static member CommitLog(entries: GitLogEntry array) =
    //     UI.List(
    //         items = entries,
    //         selectionMode = Multi ignore,
    //         itemTemplate =
    //             (fun entry -> [
    //                 Html.span [ prop.className "select-none"; prop.text entry.Subject ]
    //                 Html.span [ prop.className "select-none"; prop.text entry.AbbreviatedCommit ]
    //             ])
    //     )

    [<ReactComponent>]
    static member StatusEntries
        (
            entries: GitStatusEntry array,
            selectionChanged: GitStatusEntry * GitStatusEntry array -> unit
        ) =
        UI.List(
            items = Array.sortBy (fun x -> x.Filename) entries,
            selectionMode = Multi selectionChanged,
            itemTemplate =
                (fun entry -> [
                    let icon =
                        match entry.Status with
                        | ModifiedInWorkTreeSinceIndex
                        | ModifiedInIndex -> Icon.Pencil
                        | AddedToIndex
                        | Untracked -> Icon.Plus
                        | DeletedInWorkTree
                        | DeletedFromIndex -> Icon.Minus
                        | _ -> Icon.QuestionMarkCircle

                    Html.div [
                        prop.className "flex"
                        prop.children [
                            Html.img [
                                prop.className "inline-block text-white mx-1 w-4"
                                prop.src (Icon.asFilepath icon)
                            ]

                            Html.span [ prop.className "select-none mr-2"; prop.text entry.Filename ]
                        ]
                    ]
                ])
        )


    [<ReactComponent>]
    static member RecentRepositories(repositoryPath: RepositoryPath option, onChange: RepositoryPath -> unit) =
        console.log ("RecentRepositories", repositoryPath)

        let recentRepositores () =
            window.localStorage.getItem ("recentRepositores")
            |> Option.ofObj
            |> Option.map Decode.Auto.fromString<RepositoryPath list>
            |> Option.map (
                function
                | Ok x -> x
                | Error e -> failwith $"Failed to decode recentRepositores from local storage. {e}"
            )
            |> Option.toList
            |> List.concat

        match repositoryPath with
        | Some repoInfo ->
            let repos = recentRepositores ()
            let uriEncodedPath = RepositoryPath.asUriEncoded repoInfo

            if not (repos |> List.exists (fun rp -> RepositoryPath.asUriEncoded rp = uriEncodedPath)) then
                window.localStorage.setItem ("recentRepositores", Encode.Auto.toString (repoInfo :: repos))
        | None -> ()

        let repos = recentRepositores ()

        Html.select [
            prop.className
                "shadow-lg border border-black bg-neutral-800 text-neutral-300 text-sm outline-none rounded h-7 px-2"
            match repositoryPath with
            | Some repositoryPath -> prop.value (RepositoryPath.asUriEncoded repositoryPath)
            | None -> ()
            prop.onChange (fun (uriEncodedPath: string) ->
                repos
                |> List.map RepositoryPath.asUriEncoded
                |> List.find (fun repositoryPath -> repositoryPath = uriEncodedPath)
                |> RepositoryPath.ofUriEncoded
                |> onChange
            )
            prop.children (
                repos
                |> List.map (fun p ->
                    Html.option [
                        prop.value (RepositoryPath.asUriEncoded p)
                        prop.text (RepositoryPath.name p)
                    ]
                )
            )
        ]
