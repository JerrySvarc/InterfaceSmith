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
    {CurrentComponent = {Name = "New component"; JsonData = JNull; Code = Sequence([Hole]); Id = Guid.Empty  };
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
                let newComponent = {Name = "New component"; JsonData = data ; Code= Hole; Id = Guid.NewGuid()}
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
    | SaveComponent newComponent ->
        {model with CurrentComponent = {model.CurrentComponent with  Code = Sequence(model.RenderingCodes)}}, Cmd.none


let changeTag (code : RenderingCode)(tag: string)  =
    match code with
    | HtmlElement(_, attrs, data) ->
        HtmlElement(tag, attrs, data)
    | _ -> code

let addAttr (code : RenderingCode)(attr: (string * string) list)  =
    match code with
    | HtmlElement(tag, attrs, data) ->
        HtmlElement(tag, attrs @ attr, data)
    | _ -> code

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
                        text.hasTextLeft
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
                                    button.isText
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
                                if model.CurrentComponent.JsonData = JNull then
                                    color.isWarning
                                    button.isText
                                    prop.text "Upload data first to save a component"
                                else
                                    prop.text "Save component"
                                    color.isSuccess
                                    prop.onClick (fun _ -> dispatch (SaveComponent model.CurrentComponent))
                            ]
                    ]
                ]
            ]

    let listMenu (code : RenderingCode) =
        match code with
        | HtmlList(listType, numbered, data, code) ->
            Bulma.box[]
        | _ -> Html.div[]

    let elementMenu (code : RenderingCode) =
        match code with
        | HtmlElement (tag, attrs, data) ->
            Bulma.box[]
        | _ -> Html.div[]

    let rec codeMenu  (code : RenderingCode) : Fable.React.ReactElement =
        match code with
        | Sequence seq ->
            Bulma.box[
                Html.h3 [
                    prop.text "Sequence"
                ]
                Bulma.block[
                    List.map (fun item -> Bulma.menuItem.a[codeMenu item]) seq |> Html.div
                ]
            ]
        | HtmlElement(tag, attrs, innerText) -> elementMenu code
        | HtmlList(listType, numbered, data, code) -> listMenu code
        | Hole -> Html.div[]

    let codePreview  (code : RenderingCode) =
        let htmlString = code |> generateCode
        let script = "var newWindow = window.open(); newWindow.document.body.innerHTML = `" + htmlString + "`;"
        Bulma.block[
            Bulma.button.a [
                prop.onClick (fun _ -> Browser.Dom.window.setTimeout(script, 100) |> ignore )
                prop.text "Preview"
            ]
        ]

    let editorView  =
        Bulma.box[
            if model.CurrentComponent.JsonData <> JNull then
                nameEditView
            Bulma.box[
                if model.CurrentComponent.JsonData = JNull then
                    uploadButton
                else
                    List.map (fun code ->
                        Bulma.block[
                            codeMenu code
                            codePreview code
                        ]
                        ) model.RenderingCodes
                        |> Html.div
            ]
        ]
    editorView
