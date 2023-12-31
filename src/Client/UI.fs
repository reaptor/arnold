module UI

open Fable.React
open Feliz
open Browser
open Elmish
open FSharp.Core
open Feliz.style
open Util
open Shared
open Monaco

type Promise<'a> = Fable.Core.JS.Promise<'a>

[<AbstractClass>]
type UI =
    class
    end

type SelectionMode<'a> =
    | Single of selectionChanged: ('a -> unit)
    | Multi of selectionChanged: ('a * 'a array -> unit)

module SelectionMode =
    let isMulti =
        function
        | Single _ -> false
        | Multi _ -> true

let useKeyboardNavigation elementCount (selectionMode: SelectionMode<int>) =
    let refs = React.useRef (ResizeArray<Types.HTMLElement option>())
    let selectedIndexes, setSelectedIndexes = React.useState Array.empty
    let selectedIndex, setSelectedIndex = React.useState None
    let isShiftActive, setIsShiftActive = React.useState false
    let isCtrlActive, setIsCtrlActive = React.useState false

    let isCtrlKey (e: Types.KeyboardEvent) = e.metaKey || e.ctrlKey

    let updateSelectedIndexes selectedIndex selectedIndexes =
        match selectionMode with
        | Single selectionChanged ->
            selectionChanged selectedIndex
            setSelectedIndexes selectedIndexes
        | Multi selectionChanged ->
            selectionChanged (selectedIndex, selectedIndexes)
            setSelectedIndexes selectedIndexes

    let addSorted x xs =
        xs |> Array.append [| x |] |> Array.sort

    let removeSorted x xs =
        xs |> Array.filter (fun y -> x <> y) |> Array.sort

    let onKeyDown (e: Types.KeyboardEvent) index =
        setIsShiftActive e.shiftKey
        setIsCtrlActive (isCtrlKey e)

        let setSelectedIndex newIndex =
            if not e.shiftKey then
                setSelectedIndex (Some newIndex)

        let step addIndex =
            refs.current[index].Value.tabIndex <- -1
            let newSelectedIndex = index + addIndex
            setSelectedIndex newSelectedIndex
            refs.current[newSelectedIndex].Value.tabIndex <- 0
            refs.current[newSelectedIndex].Value.focus ()

            let newSelectedIndexes =
                if SelectionMode.isMulti selectionMode && isShiftActive then
                    match selectedIndex with
                    | Some x when newSelectedIndex <= x -> [| newSelectedIndex..x |]
                    | Some x -> [| x..newSelectedIndex |]
                    | None -> [| newSelectedIndex |]
                else
                    [| newSelectedIndex |]

            updateSelectedIndexes newSelectedIndex newSelectedIndexes

        match isCtrlKey e, e.key with
        | _, "ArrowUp" ->
            if index > 0 then
                step -1
        | _, "ArrowDown" ->
            if index < elementCount - 1 then
                step 1
        | true, "a" when SelectionMode.isMulti selectionMode ->
            let newSelectedIndexes = [|
                for i = 0 to elementCount do
                    i
            |]

            updateSelectedIndexes 0 newSelectedIndexes
        | _ -> ()

    let onKeyUp (e: Types.KeyboardEvent) =
        setIsCtrlActive (isCtrlKey e)
        setIsShiftActive e.shiftKey

    let onMouseDown (_e: Types.MouseEvent) index =
        if not (isCtrlActive || isShiftActive) then
            setSelectedIndex (Some index)

        for ref in refs.current do
            ref |> Option.iter (fun ref -> ref.tabIndex <- -1)

        refs.current[index].Value.tabIndex <- 0
        refs.current[index].Value.focus ()

        if SelectionMode.isMulti selectionMode && isCtrlActive then
            if Array.contains index selectedIndexes then
                removeSorted index selectedIndexes
            else
                addSorted index selectedIndexes
        elif SelectionMode.isMulti selectionMode && isShiftActive then
            match selectedIndex with
            | Some x when index < x -> [| index..x |]
            | Some x -> [| x..index |]
            | None -> [| index |]
        else
            [| index |]
        |> updateSelectedIndexes index

    {|
        Refs = refs
        SetupEvents =
            fun i -> [
                prop.onKeyDown (fun e -> onKeyDown e i)
                prop.onKeyUp (fun e -> onKeyUp e)
                prop.onMouseDown (fun e -> onMouseDown e i)
            ]
        SelectedIndexes = selectedIndexes
    |}

type Icon =
    | ArrowDownOnSquare
    | ArrowDownOnSquareStack
    | ArrowUpOnSquare
    | ArrowUpOnSquareStack
    | Minus
    | Plus
    | Pencil
    | QuestionMarkCircle

module Icon =
    let asFilepath =
        function
        | ArrowDownOnSquare -> "svg/arrow-down-on-square.svg"
        | ArrowDownOnSquareStack -> "svg/arrow-down-on-square-stack.svg"
        | ArrowUpOnSquare -> "svg/arrow-up-on-square.svg"
        | ArrowUpOnSquareStack -> "svg/arrow-up-on-square-stack.svg"
        | Minus -> "svg/minus.svg"
        | Plus -> "svg/plus.svg"
        | Pencil -> "svg/pencil.svg"
        | QuestionMarkCircle -> "svg/question-mark-circle.svg"

type UI with

    static member Button(?text: string, ?icon: Icon) =
        Html.button [
            prop.className
                "shadow-lg border border-black bg-neutral-800 hover:bg-neutral-700 active:bg-neutral-900 focus:bg-neutral-700 text-neutral-300 text-sm outline-none rounded h-7 px-2 whitespace-nowrap"
            prop.children [
                match icon with
                | Some i -> Html.img [ prop.className "inline-block w-5 h-5 pb-[3px]"; prop.src (Icon.asFilepath i) ]
                | None -> ()
                match text with
                | Some t -> Html.text t
                | None -> ()
            ]
        ]

    [<ReactComponent>]
    static member List<'a>(items: 'a array, itemTemplate: 'a -> ReactElement list, ?selectionMode: SelectionMode<'a>) =
        let keyboardNavigation =
            useKeyboardNavigation
                items.Length
                (match selectionMode with
                 | Some(Single selectionChanged) -> Single(fun ix -> selectionChanged items[ix])
                 | Some(Multi selectionChanged) ->
                     Multi(fun (ix, ixs) -> selectionChanged (items[ix], (Array.map (fun i -> items[i]) ixs)))
                 | None -> Single ignore)

        let isActiveElement, setIsActiveElement = React.useState false
        let mouseIndex, setMouseIndex = React.useState None

        Html.div [
            prop.className "shadow-lg bg-neutral-900 text-neutral-300 border border-black rounded"
            prop.children (
                items
                |> Array.mapi (fun i item ->
                    Html.div [
                        prop.ref (fun e ->
                            e :?> Types.HTMLElement
                            |> Option.ofObj
                            |> Option.iter (fun elem ->
                                if not (keyboardNavigation.Refs.current.Contains(Some elem)) then
                                    keyboardNavigation.Refs.current.Add(Some elem)
                            )
                        )
                        prop.classes [
                            "outline-none border-black p-[3px] cursor-pointer"
                            if i > 0 then
                                "border-t"
                            if Array.contains i keyboardNavigation.SelectedIndexes && isActiveElement then
                                "bg-neutral-700"
                            elif mouseIndex = Some i then
                                "bg-neutral-800"
                        ]
                        prop.onBlur (fun _ -> setIsActiveElement false)
                        prop.onFocus (fun _ -> setIsActiveElement true)
                        prop.onMouseEnter (fun _ -> setMouseIndex (Some i))
                        prop.onMouseLeave (fun _ -> setMouseIndex None)
                        yield! keyboardNavigation.SetupEvents i
                        prop.children (itemTemplate item)
                    ]
                )
            )
        ]

    [<ReactComponent>]
    static member CodeEditor(textFile: TextFile, ?onChange: string -> unit) =
        let (editor: IStandaloneCodeEditor option), setEditor =
            React.useStateWithUpdater None

        let monacoEl = React.useInputRef ()

        let createModel () =
            let model =
                monaco.editor.createModel (textFile.Content, uri = Uri.parse textFile.Name)

            match onChange with
            | Some f -> model.onDidChangeContent (fun _ -> f (model.getValue ())) |> ignore<IDisposable>
            | None -> ()

            model

        React.useEffect (
            (fun () ->
                let resizeEvent =
                    (fun _ ->
                        match editor, monacoEl.current with
                        | Some editor', Some el ->
                            editor'.layout (Dimension(height = el.offsetHeight - 5., width = el.offsetWidth - 2.))
                        | _ -> ()
                    )

                match monacoEl.current with
                | Some monacoEl' ->
                    window.addEventListener ("resize", resizeEvent)

                    setEditor (fun e ->
                        if e.IsSome then
                            e
                        else
                            monaco.editor.create (
                                monacoEl',
                                StandaloneEditorConstructionOptions(model = createModel (), theme = "vs-dark")
                            )
                            |> Some
                    )
                | None -> ()

                React.createDisposable (fun () ->
                    window.removeEventListener ("resize", resizeEvent)

                    match editor with
                    | Some editor' ->
                        editor'.getModel().dispose ()
                        editor'.dispose ()
                    | None -> ()
                )
            ),
            [| monacoEl.current |> Option.toObj |> box |]
        )

        match editor with
        | Some editor' ->
            let model = editor'.getModel ()

            if model.uri.path[1..] <> textFile.Name then
                model.dispose ()
                editor'.setModel (createModel ())
        | None -> ()

        Html.div [ prop.ref monacoEl; prop.className "grow" ]
