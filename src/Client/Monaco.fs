module Monaco

open Browser.Types
open Fable.Core
open FSharp.Core

type IDisposable =
    abstract member dispose: unit -> unit

[<AbstractClass>]
[<Import("Uri", "monaco-editor")>]
type Uri =
    static member parse: string -> Uri = jsNative
    abstract member path: string

type IModelContentChange =
    abstract member text: string

type IModelContentChangedEvent =
    abstract member changes: IModelContentChange array

type ITextModel =
    inherit IDisposable
    abstract member onDidChangeContent: listener: (IModelContentChangedEvent -> unit) -> IDisposable
    abstract member getValue: unit -> string
    abstract member uri: Uri

type StandaloneEditorConstructionOptions
    [<ParamObject; Emit("$0")>]
    (?value: string, ?language: string, ?model: ITextModel, ?theme: string) =
    class
    end

type Dimension [<ParamObject; Emit("$0")>] (?height: float, ?width: float) =
    class
    end

type IStandaloneCodeEditor =
    inherit IDisposable
    abstract member layout: ?dimension: Dimension -> unit
    abstract member getModel: model: unit -> ITextModel
    abstract member setModel: model: ITextModel -> unit

type Editor =
    abstract member create:
        domElement: HTMLElement * ?options: StandaloneEditorConstructionOptions -> IStandaloneCodeEditor

    abstract member createModel: value: string * ?language: string * ?uri: Uri -> ITextModel

type Monaco =
    abstract member editor: Editor

[<ImportAll("monaco-editor")>]
let monaco: Monaco = jsNative
