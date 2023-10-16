module UI

open Feliz
open Browser
open Elmish
open FSharp.Core
open Util

type Promise<'a> = Fable.Core.JS.Promise<'a>

[<AbstractClass>]
type UI =
    class
    end

let useKeyboardNavigation elementCount selectionChanged multiSelection =
    let refs = React.useRef (ResizeArray<Types.HTMLElement option>())
    let selectedIndexes, setSelectedIndexes = React.useState Array.empty
    let selectedIndex, setSelectedIndex = React.useState None
    let isShiftActive, setIsShiftActive = React.useState false
    let isCtrlActive, setIsCtrlActive = React.useState false

    let isCtrlKey (e: Types.KeyboardEvent) = e.metaKey || e.ctrlKey
    
    let updateSelectedIndexes newSelectedIndexes =       
        selectionChanged newSelectedIndexes
        setSelectedIndexes newSelectedIndexes
        
    let addSorted x xs = xs |> Array.append [| x |] |> Array.sort
    let removeSorted x xs = xs |> Array.filter (fun y -> x <> y) |> Array.sort   
            
    let onKeyDown (e: Types.KeyboardEvent) index =        
        setIsShiftActive e.shiftKey
        setIsCtrlActive (isCtrlKey e)
        let setSelectedIndex newIndex =
            if not (e.shiftKey || isCtrlKey e) then
                setSelectedIndex (Some newIndex)
        
        let step addIndex =
            refs.current[index].Value.tabIndex <- -1
            let newSelectedIndex = index + addIndex
            setSelectedIndex newSelectedIndex
            refs.current[newSelectedIndex].Value.tabIndex <- 0
            refs.current[newSelectedIndex].Value.focus ()
            let newSelectedIndexes =
                if multiSelection && isShiftActive then
                    match selectedIndex with
                    | Some x when newSelectedIndex <= x ->                                       
                        [| newSelectedIndex..x |]
                    | Some x -> 
                        [| x..newSelectedIndex |]
                    | None ->
                        [| newSelectedIndex |]
                else
                    [| newSelectedIndex |]
            updateSelectedIndexes newSelectedIndexes
            
        match isCtrlKey e, e.key with
        | _, "ArrowUp" ->
            if index > 0 then
                step -1
        | _, "ArrowDown" ->
            if index < elementCount - 1 then
                step 1
        | true, "a" when multiSelection ->
            let newSelectedIndexes = [| for i = 0 to elementCount do i |]
            updateSelectedIndexes newSelectedIndexes
        | _ -> ()
    
    let onKeyUp (e: Types.KeyboardEvent) =
        setIsCtrlActive (isCtrlKey e)
        setIsShiftActive e.shiftKey
        
    let onMouseDown (e: Types.MouseEvent) index =
        if e.button = 0 then
            if not (isCtrlActive || isShiftActive) then
                setSelectedIndex (Some index)
                
            for ref in refs.current do
                ref |> Option.iter (fun ref -> ref.tabIndex <- -1)

            refs.current[index].Value.tabIndex <- 0
            refs.current[index].Value.focus ()
            if multiSelection && isCtrlActive then
                if Array.contains index selectedIndexes then 
                    removeSorted index selectedIndexes
                else
                    addSorted index selectedIndexes
            elif multiSelection && isShiftActive then
                match selectedIndex with
                | Some x when index < x ->                                       
                    [| index..x |]
                | Some x -> 
                    [| x..index |]
                | None ->
                    [| index |]
            else
                [| index |]
            |> updateSelectedIndexes
        else
            e.preventDefault ()  
        
    {|
        Refs = refs
        SetupEvents = fun i -> [
            prop.onKeyDown (fun e -> onKeyDown e i)
            prop.onKeyUp (fun e -> onKeyUp e)
            prop.onMouseDown (fun e -> onMouseDown e i)
        ]
        SelectedIndexes = selectedIndexes
    |}
    
type UI with
    [<ReactComponent>]
    static member List<'a>
        (
            items: 'a array,
            itemTemplate: 'a -> ReactElement list,
            ?itemsSelected: 'a array -> unit,
            ?multiSelect: bool
        ) =
        let keyboardNavigation =
            useKeyboardNavigation
                items.Length
                (fun indexes ->
                    itemsSelected
                    |> Option.iter (fun f ->                        
                        indexes
                        |> Array.map (fun i -> items[i])
                        |> f
                    )
                )
                (defaultArg multiSelect false)
        let isActiveElement, setIsActiveElement = React.useState false
        
        Html.div
            [
                prop.className "text-neutral-300 m-5 border border-black"
                prop.children (
                    items
                    |> Array.mapi (fun i item ->
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
                                        if  Array.contains i keyboardNavigation.SelectedIndexes && isActiveElement then
                                            "bg-neutral-700"
                                    ]
                                prop.onBlur (fun _ -> setIsActiveElement false)
                                prop.onFocus (fun _ -> setIsActiveElement true)
                                yield! keyboardNavigation.SetupEvents i
                                prop.children (itemTemplate item)
                            ]
                    )
                )
            ] 