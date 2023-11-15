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

type PathItem =
    | InList
    | InSeq of int
    | InElement


let rec replace currentCode path replacementCode =
    match path with
    | [] -> replacementCode
    | InList::path ->
        match currentCode with
        | HtmlList(lt, n, d, c) ->
            HtmlList(lt, n, d, replace c path replacementCode)
        | _ -> failwith "Invalid path"
    | InSeq(i)::path ->
        match currentCode with
        | Sequence(items) ->
            let newItems = List.mapi (fun index item -> if index = i then replace item path replacementCode else item) items
            Sequence(newItems)
        | _-> failwith "Invalid path"
    | InElement::path ->
        match currentCode with
        | HtmlElement(tag, attrs, innerText) ->
            match innerText with
            | Data(code) ->
                HtmlElement(tag, attrs, Data(replace code path replacementCode))
            | _ -> HtmlElement(tag, attrs, Data(replacementCode))
        | _ -> failwith "Invalid path"
let rec getPath path rc =
    match rc with
    | Sequence items ->
        for i, item in Seq.indexed items do
            getPath (path @ [InSeq i]) rc
    | HtmlList(_, _, _, rc) ->
        getPath (path @ [InList]) rc
    | HtmlElement(_, _, _) -> getPath (path @ [InElement]) rc
    | _ -> ()

let j = JNull;
let rc =
  Sequence
    [
      HtmlElement("h1", [], Constant "hi")
      Hole j
      HtmlElement("h1", [], Constant "hi")
      Hole j
    ]
type Model =
    { CurrentComponent : Component
      FileUploadError : bool
      EditingName : bool
      EditingCode : bool
      NameInput : string
    }

type Msg =
    | UploadData of string
    | ChangeName of string
    | SetInput of string
    | ChangeNameEditMode of bool
    | SaveComponent of Component
    | EditCode of RenderingCode

let init() =
    {CurrentComponent = {Name = "New component"; Code = Hole JNull; Id = Guid.Empty};
        FileUploadError = false; EditingName = false; EditingCode=false; NameInput = "";}

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | UploadData data ->
        let loadedDataOption = loadJson data
        match loadedDataOption with
        | Some(data)  ->
            match data with
            | JObject obj ->
                let codes = List.map (fun (key , json)-> recognizeJson json) (obj |> Map.toList)
                let newComponent = {Name = "New component"; Code = Sequence codes; Id = Guid.NewGuid()}
                {model with CurrentComponent = newComponent; FileUploadError = false;  }, Cmd.none
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
    | SaveComponent comp ->
        model, Cmd.none
    | EditCode(_) -> model, Cmd.none




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

    let elementMenu code =
        match code with
        | HtmlElement (tag, attrs, innerText) ->
            Bulma.box[
                Bulma.block[codeToHtml code]
                Bulma.block[
                    Bulma.buttons[
                        Bulma.button.button[
                            prop.text "Edit"
                            prop.onClick (fun _ -> dispatch (EditCode code))
                        ]
                    ]
                ]
            ]
        | _ -> failwith "Not an element"

    let sequenceMenu code =
        match code with
        | Sequence elements ->
            Bulma.box[
                Bulma.block[codeToHtml code]
                Bulma.block[
                    Bulma.buttons[
                        Bulma.button.button[
                            prop.text "Edit"
                            prop.onClick (fun _ -> dispatch (EditCode code))
                        ]
                    ]
                ]
            ]
        | _ -> failwith "Not a sequence"

    let listMenu code =
        match code with
        | HtmlList (listType, numbered, data, code) ->
            Bulma.box[
                Bulma.block[codeToHtml code]
                Bulma.block[
                    Bulma.buttons[
                        Bulma.button.button[
                            prop.text "Edit"
                            prop.onClick (fun _ -> dispatch (EditCode code))
                        ]
                    ]
                ]
            ]
        | _ -> failwith "Not a sequence"

    let option code =
        match code with
        | Hole json ->
            recognizeJson json
        | _-> failwith "No options"

    let rec options code =
        match code with
        | Hole json -> options ( option code)
        | HtmlElement(tag, attrs, innerText) -> elementMenu code
        | HtmlList _->
            listMenu code
        | Sequence seq ->seq |> List.map (fun code -> options code) |> Html.div

    let editorView  =
        Bulma.box[
            if model.CurrentComponent.Code <> Hole JNull then
                nameEditView
            Bulma.box[
                if model.CurrentComponent.Code = Hole JNull then
                    uploadButton
                else
                    options model.CurrentComponent.Code
            ]
        ]
    editorView
