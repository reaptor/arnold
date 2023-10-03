module App

open Feliz
open Browser
open Thoth.Json
open Thoth.Fetch
open System

type Promise<'a> = Fable.Core.JS.Promise<'a>

type GitLogEntry =
    {
        Commit: string
        AbbreviatedCommit: string
        Tree: string
        AbbreviatedTree: string
        Parent: string
        AbbreviatedParent: string
        Refs: string
        Encoding: string
        Subject: string
        SanitizedSubjectLine: string
        Body: string
        CommitNotes: string
        VerificationFlags: string
        Signer: string
        SignerKey: string
        Author:
            {|
                Name: string
                Email: string
                Date: string
            |}
        Commiter:
            {|
                Name: string
                Email: string
                Date: string
            |}
    }


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

type GitStatus =
    | NotUpdated // Not staged, modified
    | UpdatedInIndex // Staged, modified
    | TypeChangedInIndex
    | AddedToIndex // Staged, new file
    | DeletedFromIndex
    | RenamedInIndex
    | CopiedInIndex
    | IndexAndWorkTreeMatches
    | WorkTreeChangedSinceIndex
    | TypeChangedInWorkTreeSinceIndex
    | DeletedInWorkTree
    | RenamedInWorkTree
    | CopiedInWorkTree
    | UnmergedBothDeleted
    | UnmergedAddedByUs
    | UnmergedDeletedByThem
    | UnmergedAddedByThem
    | UnmergedDeletedByUs
    | UnmergedBothAdded
    | UnmergedBothModified
    | Untracked
    | Ignored

type GitStatusEntry = { Filename: string; Status: GitStatus }

module GitStatus =
    let parsePorcelain (s: string) =
        s.Split([| '\n' |], StringSplitOptions.RemoveEmptyEntries)
        |> fun x ->
            console.log (x)
            x
        |> Array.map (
            function
            // https://git-scm.com/docs/git-status#_short_format
            // https://git-scm.com/docs/git-status#_porcelain_format_version_1
            | X " MTARC" & Y "D" & Filename filename -> DeletedInWorkTree, filename
            | X " " & Y "AMD" & Filename filename -> NotUpdated, filename
            | X "M" & Y " MTD" & Filename filename -> UpdatedInIndex, filename
            | X "T" & Y " MTD" & Filename filename -> TypeChangedInIndex, filename
            | X "A" & Y " MTD" & Filename filename -> AddedToIndex, filename
            | X "D" & Y " " & Filename filename -> DeletedFromIndex, filename
            | X "R" & Y " MTD" & Filename filename -> RenamedInIndex, filename
            | X "C" & Y " MTD" & Filename filename -> CopiedInIndex, filename
            | X "MTARC" & Y " " & Filename filename -> IndexAndWorkTreeMatches, filename
            | X " MTARC" & Y "M" & Filename filename -> WorkTreeChangedSinceIndex, filename
            | X " MTARC" & Y "T" & Filename filename -> TypeChangedInWorkTreeSinceIndex, filename
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
            | unsupported -> failwithf "Unsupported git status output %s" unsupported
            >> fun (status, filename) -> { Filename = filename; Status = status }
        )

[<AbstractClass>]
type UI =
    class
    end

let useKeyboardNavigation (elementCount) =
    let refs = React.useRef (ResizeArray<Types.HTMLElement option>())
    let selectedIndex, setSelectedIndex = React.useState (-1)

    let onKeyDown index key =
        match key with
        | "ArrowUp" ->
            if index > 0 then
                refs.current[index].Value.tabIndex <- -1
                let newSelectedIndex = index - 1
                setSelectedIndex newSelectedIndex
                refs.current[newSelectedIndex].Value.tabIndex <- 0
                refs.current[newSelectedIndex].Value.focus ()
        | "ArrowDown" ->
            if index < elementCount - 1 then
                refs.current[index].Value.tabIndex <- -1
                let newSelectedIndex = index + 1
                setSelectedIndex newSelectedIndex
                refs.current[newSelectedIndex].Value.tabIndex <- 0
                refs.current[newSelectedIndex].Value.focus ()
        | _ -> ()

    let onMouseDown (e: Types.MouseEvent) index =
        if e.button = 0 then
            for ref in refs.current do
                ref |> Option.iter (fun ref -> ref.tabIndex <- -1)

            setSelectedIndex index
            refs.current[index].Value.tabIndex <- 0
            refs.current[index].Value.focus ()

        else
            e.preventDefault ()

    {|
        Refs = refs
        KeyDown = onKeyDown
        MouseDown = onMouseDown
        SelectedIndex = selectedIndex
    |}

type UI with

    [<ReactComponent>]
    static member CommitLog(entires: GitLogEntry array) =
        let keyboardNavigation = useKeyboardNavigation (entires.Length)

        Html.div
            [
                prop.className "text-neutral-300 m-5 border border-black"
                prop.children (
                    entires
                    |> Array.mapi (fun i entry ->
                        Html.div
                            [
                                prop.ref (fun e ->
                                    e :?> Types.HTMLElement
                                    |> Option.ofObj
                                    |> Option.iter (fun elem ->
                                        if not (keyboardNavigation.Refs.current.Contains(Some elem)) then
                                            keyboardNavigation.Refs.current.Add(Some elem)
                                    )
                                )
                                prop.classes
                                    [
                                        "outline-none border-black p-0.5"
                                        if i > 0 then
                                            "border-t"
                                        if keyboardNavigation.SelectedIndex = i then
                                            "bg-neutral-700"
                                    ]
                                prop.onKeyDown (fun e -> keyboardNavigation.KeyDown i e.key)
                                prop.onMouseDown (fun e -> keyboardNavigation.MouseDown e i)
                                prop.children
                                    [
                                        Html.span [ prop.className "select-none"; prop.text entry.Subject ]
                                        Html.span [ prop.className "select-none"; prop.text entry.AbbreviatedCommit ]
                                    ]
                            ]
                    )
                )
            ]

    [<ReactComponent>]
    static member StatusEntries(entires: GitStatusEntry array) =
        let keyboardNavigation = useKeyboardNavigation (entires.Length)

        Html.div
            [
                prop.className "text-neutral-300 m-5 border border-black"
                prop.children (
                    entires
                    |> Array.mapi (fun i entry ->
                        Html.div
                            [
                                prop.ref (fun e ->
                                    e :?> Types.HTMLElement
                                    |> Option.ofObj
                                    |> Option.iter (fun elem ->
                                        if not (keyboardNavigation.Refs.current.Contains(Some elem)) then
                                            keyboardNavigation.Refs.current.Add(Some elem)
                                    )
                                )
                                prop.classes
                                    [
                                        "outline-none border-black p-0.5"
                                        if i > 0 then
                                            "border-t"
                                        if keyboardNavigation.SelectedIndex = i then
                                            "bg-neutral-700"
                                    ]
                                prop.onKeyDown (fun e -> keyboardNavigation.KeyDown i e.key)
                                prop.onMouseDown (fun e -> keyboardNavigation.MouseDown e i)
                                prop.children
                                    [
                                        let imgName =
                                            match entry.Status with
                                            | NotUpdated
                                            | UpdatedInIndex -> "pencil"
                                            | AddedToIndex
                                            | Untracked -> "plus"
                                            | DeletedInWorkTree
                                            | DeletedFromIndex -> "minus"
                                            | _ -> ""

                                        Html.img
                                            [
                                                prop.className "inline-block text-white mx-1 w-4"
                                                prop.src $"svg/%s{imgName}.svg"
                                            ]

                                        Html.span [ prop.className "select-none"; prop.text entry.Filename ]
                                    ]
                            ]
                    )
                )
            ]

type GitRequest = { Path: string }

type GitResponse<'a> =
    { Error: string option; Data: 'a array }

module GitResponse =
    // let LogDecoder =
    //     Decode.object (fun get ->
    //         {
    //             Error = get.Optional.Field "error" Decode.string
    //             Data =
    //                 get.Optional.Field
    //                     "data"
    //                     (Decode.array (
    //                         Decode.object (fun get ->
    //                             {
    //                                 Commit = get.Required.Field "commit" Decode.string
    //                                 AbbreviatedCommit = get.Required.Field "abbreviated_commit" Decode.string
    //                                 Tree = get.Required.Field "tree" Decode.string
    //                                 AbbreviatedTree = get.Required.Field "abbreviated_tree" Decode.string
    //                                 Parent = get.Required.Field "parent" Decode.string
    //                                 AbbreviatedParent = get.Required.Field "abbreviated_parent" Decode.string
    //                                 Refs = get.Required.Field "refs" Decode.string
    //                                 Encoding = get.Required.Field "encoding" Decode.string
    //                                 Subject = get.Required.Field "subject" Decode.string
    //                                 SanitizedSubjectLine = get.Required.Field "sanitized_subject_line" Decode.string
    //                                 Body = get.Required.Field "body" Decode.string
    //                                 CommitNotes = get.Required.Field "commit_notes" Decode.string
    //                                 VerificationFlags = get.Required.Field "verification_flags" Decode.string
    //                                 Signer = get.Required.Field "signer" Decode.string
    //                                 SignerKey = get.Required.Field "signer_key" Decode.string
    //                                 Author =
    //                                     get.Required.Field
    //                                         "author"
    //                                         (Decode.object (fun get ->
    //                                             {|
    //                                                 Name = get.Required.Field "name" Decode.string
    //                                                 Email = get.Required.Field "email" Decode.string
    //                                                 Date = get.Required.Field "date" Decode.string
    //                                             |}
    //                                         ))
    //                                 Commiter =
    //                                     get.Required.Field
    //                                         "commiter"
    //                                         (Decode.object (fun get ->
    //                                             {|
    //                                                 Name = get.Required.Field "name" Decode.string
    //                                                 Email = get.Required.Field "email" Decode.string
    //                                                 Date = get.Required.Field "date" Decode.string
    //                                             |}
    //                                         ))
    //                             }
    //                         )
    //                     ))
    //                 |> Option.toArray
    //                 |> Array.concat
    //         }
    //     )

    let StatusDecoder =
        Decode.object (fun get ->
            {
                Error = get.Optional.Field "error" Decode.string
                Data =
                    get.Optional.Field "data" Decode.string
                    |> Option.map GitStatus.parsePorcelain
                    |> Option.toArray
                    |> Array.concat
            }
        )

[<ReactComponent>]
let App () =
    let entries, setEntries = React.useState ([||])
    let status, setStatus = React.useState ([||])

    React.useEffectOnce (fun () ->
        promise {
            // let! response =
            //     Fetch.post (
            //         url = "http://localhost:5000/git/log",
            //         data =
            //             {
            //                 Path = "/Users/kristofferlofberg/Projects/foo"
            //             },
            //         decoder = GitResponse.LogDecoder
            //     )

            let! response =
                Fetch.post (
                    url = "http://localhost:5000/git/status",
                    data =
                        {
                            Path = "/Users/kristofferlofberg/Projects/foo"
                        },
                    decoder = GitResponse.StatusDecoder
                )

            setStatus (response.Data)
        }
        |> ignore<Promise<unit>>
    )

    Html.div
        [
            UI.StatusEntries(
                entires =
                    Array.filter
                        (fun entry ->
                            match entry.Status with
                            | NotUpdated
                            | Untracked
                            | DeletedInWorkTree -> true
                            | _ -> false
                        )
                        status
            )
            UI.StatusEntries(
                entires =
                    Array.filter
                        (fun entry ->
                            match entry.Status with
                            | AddedToIndex
                            | UpdatedInIndex
                            | DeletedFromIndex -> true
                            | _ -> false
                        )
                        status
            )
        ]

let root = ReactDOM.createRoot (document.getElementById "root")

root.render (App())
