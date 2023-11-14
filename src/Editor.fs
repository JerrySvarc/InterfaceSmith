module Editor

open Elmish
open Feliz
open Feliz.Bulma
open Types
open Fable.SimpleJson
open DataLoading
open DataRecognition
open FileUpload
open System
open CodeGeneration
open Browser
open Fable.React
open Fable.Core.JsInterop
type Model =
    { CurrentComponent : Component
      FileUploadError : bool
      EditingName : bool
      NameInput : string
      RenderingCodes : RenderingCode list
    }

type Msg =
    | UploadData of string
    | ChangeName of string
    | SetInput of string
    | ChangeNameEditMode of bool
    | SaveComponent of Component

let init() =
    {CurrentComponent = {Name = "New component"; Code = Hole JNull; Id = Guid.Empty};
        FileUploadError = false; EditingName = false; NameInput = "";RenderingCodes = []}

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | UploadData data ->
        let loadedDataOption = loadJson data
        match loadedDataOption with
        | Some(data)  ->
            match data with
            | JObject obj ->
                let codes = List.map (fun (key , json)-> recognizeJson json) (obj |> Map.toList)
                let newComponent = {Name = "New component"; Code = Hole data; Id = Guid.NewGuid()}
                {model with CurrentComponent = newComponent; FileUploadError = false; RenderingCodes = codes }, Cmd.none
            | _ ->
                {model with FileUploadError = true}, Cmd.none
        | None ->
            {model with FileUploadError = true}, Cmd.none
    | ChangeName newName->
        {model with CurrentComponent = {model.CurrentComponent with Name = newName}; NameInput = ""; EditingName = false}, Cmd.none
    | SetInput input->
        {model with NameInput = input}, Cmd.none
    | ChangeNameEditMode value ->
        {model with EditingName = value; NameInput = ""}, Cmd.none
    | SaveComponent _ ->
        model, Cmd.none


type PathItem =
    | InList
    | InSeq of int

let rec replace currentCode path replacementCode =
    match path with
    | [] -> replacementCode
    | InList::path ->
        match currentCode with
        | HtmlList(lt, n, d, c) ->
            HtmlList(lt, n, d, replace c path replacementCode)
        | HtmlElement(tag, attrs, innerText) -> failwith "Not Implemented"
        | Sequence(_) -> failwith "Not Implemented"
        | Hole(_) -> failwith "Not Implemented"
    | InSeq(i)::path ->
        match currentCode with
        | HtmlList(lt, n, d, c) -> failwith "Not Implemented"
        | HtmlElement(tag, attrs, innerText) -> failwith "Not Implemented"
        | Sequence(items) ->
            let newItems = List.mapi (fun index item -> if index = i then replace item path replacementCode else item) items
            Sequence(newItems)
        | Hole(_) -> failwith "Not Implemented"

let rec render path rc =
    match rc with
    | Sequence items ->
        for i, item in Seq.indexed items do
            render (path @ [InSeq i]) rc
    | HtmlList(_, _, _, rc) ->
        render (path @ [InList]) rc
    | _ -> ()

let view (model: Model) (dispatch: Msg -> unit) =

    let upploadButtonView onLoad =
        Bulma.block[
            Bulma.file[
                file.isNormal
                prop.classes ["is-centered"]
                prop.children [
                    Bulma.fileLabel.label [
                        Bulma.fileInput [
                            prop.type' "file"
                            prop.name "component-data"
                            prop.onChange ( handleFileEvent onLoad)
                        ]
                        Bulma.fileCta [
                            Bulma.fileLabel.span [
                                prop.text "Choose a file…"
                            ]
                        ]
                    ]
                ]
            ]
            if  model.FileUploadError then
                Html.text "The selected file could not be used for creation."
            else
                Html.text ""
        ]

    let uploadButton = upploadButtonView (UploadData >> dispatch)

    let nameEditView =
        Bulma.box[
                if model.EditingName then
                    Bulma.block[
                        Bulma.input.text[
                        prop.onTextChange (fun text -> dispatch (SetInput text))
                        ]
                    ]
                else
                    Bulma.block[
                        let nameText = "Component name: " + model.CurrentComponent.Name
                        Html.h1 (nameText)
                    ]
                Bulma.block[
                    Bulma.buttons[
                        Bulma.button.button[
                            color.isPrimary
                            if model.EditingName then
                                if model.NameInput.Length > 0 then
                                    prop.text "Save"
                                    prop.onClick (fun _ -> dispatch (ChangeName model.NameInput))
                                else
                                    color.isWarning
                                    prop.text "Name must be at least one character"
                            else
                                prop.text "Edit name"
                                prop.onClick (fun _ -> dispatch (ChangeNameEditMode (model.EditingName |> not )))
                        ]
                        if model.EditingName then
                            Bulma.button.button[
                                prop.text "Cancel"
                                color.isDanger
                                prop.onClick (fun _ -> dispatch (ChangeNameEditMode false))
                            ]
                        else
                             Bulma.button.button[
                                if model.CurrentComponent.Code = Hole JNull then
                                    color.isWarning
                                    prop.text "Upload data first to save a component"
                                else
                                    prop.text "Save component"
                                    color.isSuccess
                                    prop.onClick (fun _ -> dispatch (SaveComponent model.CurrentComponent))
                            ]
                    ]
                ]
            ]
    let codeToHtml code  =
        let htmlString =  generateCode code
        let props = createObj ["dangerouslySetInnerHTML" ==> createObj ["__html" ==> htmlString]]
        ReactBindings.React.createElement("div", props, children = [])


    let editorView  =
        Bulma.box[
            if model.CurrentComponent.Code <> Hole JNull then
                nameEditView
            Bulma.box[
                if model.CurrentComponent.Code = Hole JNull then
                    uploadButton
                else
                    codeToHtml (Sequence model.RenderingCodes)

            ]
        ]
    editorView
