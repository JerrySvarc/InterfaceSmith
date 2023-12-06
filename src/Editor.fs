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

open Fable.React


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
    | EditCode of RenderingCode * RenderingCode list

let rec replace path replacementElement (currentCode : RenderingCode) =
    match path with
    | [] -> replacementElement
    | head::tail ->
        match currentCode with
        | HtmlList(lt, n, item) ->
            let newItems =
                if HashIdentity.Reference.Equals(item, head) then
                    replacementElement
                else
                    replace tail replacementElement item
            HtmlList(lt, n, newItems)
        | Sequence(items) ->
            let newItems =
                items
                |> List.map (fun item -> if HashIdentity.Reference.Equals(item, head) then replacementElement else replace tail replacementElement item)
            Sequence(newItems)
        | _ -> currentCode

let init() =
    {CurrentComponent = {Name = "New component"; Code = Hole (UnNamed); Id = Guid.NewGuid(); Data = JNull};
        FileUploadError = false; EditingName = false; EditingCode=false; NameInput = ""}

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | UploadData data ->
        let loadedDataOption = loadJson data
        match loadedDataOption with
        | Some(data)  ->
            match data with
            | JObject obj ->
                let newComponent = {model.CurrentComponent with Code = recognizeJson data; Data = data;}
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
    | EditCode (code, path) ->
        let newcodes = replace path code model.CurrentComponent.Code
        let newComponent = {model.CurrentComponent with Code = newcodes}
        {model with CurrentComponent = newComponent}, Cmd.none


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
                Bulma.hero[prop.text "The selected file could not be used for creation."]
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
                                if model.CurrentComponent.Data = JNull then
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

    let elementOptions (element : RenderingCode) path (name : string)  : ReactElement =
        match element with
        | HtmlElement (tag, attrs, innerText) ->
            Bulma.box[
                Bulma.title[
                    prop.text name
                ]
                Bulma.block[
                    Bulma.buttons[
                        Bulma.button.button[
                            prop.text "Add element"
                            prop.onClick (fun _ -> dispatch (EditCode (element, path)))
                        ]
                    ]
                ]
            ]
        | _ -> failwith "Not an element"

    let listOptions list  path : ReactElement=
        match list with
        | HtmlList (listType, numbered, code) ->
            Bulma.box[]
        | _ -> failwith "Not a list"

    let rec sequenceOptions sequence path (name : string) : ReactElement=
        match sequence with
        | Sequence(_) ->
            Bulma.box[
                Bulma.title[
                    prop.text name
                ]
                Bulma.block[
                    Bulma.buttons[
                        Bulma.button.button[
                            prop.text "Edit"
                            prop.onClick (fun _ -> dispatch (EditCode (sequence, path)))
                        ]
                    ]
                ]
            ]
        | _ -> failwith "Not a sequence"

    let options (code : RenderingCode) (path : RenderingCode list) (name : string) =
        match code with
        | HtmlElement _ ->
            elementOptions code path name
        | HtmlList _->
            listOptions code path
        | Sequence(_) ->
            sequenceOptions code path name
        | Hole _ -> failwith "Hole cannot contain another hole"

    let rec renderingCodeToReactElement (code: RenderingCode) (path : RenderingCode list) (json : Json) : ReactElement =
        match code with
        | HtmlElement (tag, attrs, innerText) ->
            let props = attrs |> List.toSeq |> dict
            match innerText with
                | Data ->
                    let selectedFields = json
                    let jsonStr = selectedFields |> Json.convertFromJsonAs<String>
                    Bulma.block[ReactBindings.React.createElement(tag, props, [str jsonStr])]
                | Value.Empty -> Bulma.block[ReactBindings.React.createElement(tag, props, [])]
                | Constant s -> Bulma.block[ReactBindings.React.createElement(tag, props, [str s])]
        | HtmlList (listType, numbered, code) ->
            Bulma.block[options code (path @ [code]) "List"]

        | Sequence codes ->
            let jsonList =
                match json with
                | JObject object ->
                    object |> Map.toList |> List.map (fun (key, value) -> value)
                | _ -> failwith "Not a sequence"
            let renderedElements =
                List.mapi (fun index code -> renderingCodeToReactElement code (path @ [code]) (List.item index jsonList)) codes
            Bulma.block[ReactBindings.React.createElement("div", dict [], renderedElements)]
        | Hole named ->
            let name =
                match named with
                | UnNamed -> "Unnamed"
                | Named name -> name
            let fieldType = json |> recognizeJson
            let optionPane = options fieldType (path @ [code]) name
            Bulma.block[optionPane]

    let editorView  =
        Bulma.box[
            if model.CurrentComponent.Data <> JNull then
                nameEditView
            Bulma.box[
                prop.children[
                if model.CurrentComponent.Data = JNull then
                    uploadButton
                else
                    Bulma.columns[
                        prop.classes ["is-centered"]
                        prop.children[
                            Bulma.column[
                                prop.children[
                                    renderingCodeToReactElement model.CurrentComponent.Code [] model.CurrentComponent.Data
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
    editorView