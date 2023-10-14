module UI

open Feliz
open Browser
open Elmish
open FSharp.Core

type Promise<'a> = Fable.Core.JS.Promise<'a>

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