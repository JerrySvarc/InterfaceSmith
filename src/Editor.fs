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
    | EditCode of (RenderingCode * Json list ) * RenderingCode  list

let rec replace path replacementElement (currentCode : RenderingCode * Json)  =
    match path with
    | [] -> replacementElement
    | head::tail ->
        match fst currentCode with
        | HtmlElement _ -> currentCode
        | HtmlList(lt, n, code) ->
            if HashIdentity.Reference.Equals(code, head) then
                (HtmlList(lt, n, fst replacementElement), snd replacementElement)
            else
                (HtmlList(lt, n, fst (replace tail replacementElement (code, snd currentCode))), snd currentCode)
        | Sequence(items) ->
            let newItems =
                items
                |> List.map (fun item ->
                    if HashIdentity.Reference.Equals(item, head) then
                        replacementElement
                    else
                        (fst (replace tail replacementElement (item, snd currentCode)), snd currentCode))
            let newRenderingCodes = newItems |> List.map fst
            (Sequence(newRenderingCodes), snd currentCode)
        | Hole _ -> currentCode

let init() =
    {CurrentComponent = {Name = "New component"; Code = Hole (UnNamed), JNull; Id = Guid.NewGuid(); Data = JNull};
        FileUploadError = false; EditingName = false; EditingCode=false; NameInput = ""}

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | UploadData data ->
        let loadedDataOption = loadJson data
        match loadedDataOption with
        | Some(data)  ->
            match data with
            | JObject obj ->
                let newComponent = {model.CurrentComponent with Code = List.head (recognizeJson data)}
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
        let newcodes = replace path (code) model.CurrentComponent.Code
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
            Bulma.box[
                Bulma.title[
                    prop.text "List"
                ]
                Bulma.block[
                    Bulma.buttons[
                        Bulma.button.button[
                            prop.text "Edit"
                            prop.onClick (fun _ -> dispatch (EditCode (list, path)))
                        ]
                    ]
                ]
            ]
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
                    let selectedField =
                        match json with
                        | JString str ->
                            str
                        | JNumber _
                        | JBool _ ->
                            json |> Json.convertFromJsonAs<String>
                        | JNull ->
                            "null"
                        | _ -> failwith "Not a primitive"
                    Bulma.block[ReactBindings.React.createElement(tag, props, [str selectedField])]
                | Value.Empty -> Bulma.block[ReactBindings.React.createElement(tag, props, [])]
                | Constant s -> Bulma.block[ReactBindings.React.createElement(tag, props, [str s])]
        | HtmlList (listType, numbered, code) ->
            let topTag =
                match listType with
                | ListType.List -> if numbered then "ol" else "ul"
                | Table -> "table"

            let itemTag =
                match listType with
                | ListType.List  -> "li"
                | Table -> "td"

            let optionalTableTag =
                match listType with
                | ListType.List -> ""
                | Table -> "tr"

            let jsonList =
                match json with
                | JArray array -> array
                | _ -> failwith "Not a list"

            let renderedElements = Html.none
            Bulma.block[ReactBindings.React.createElement(topTag, [], [])]
        | Sequence codes ->
            let jsonList =
                match json with
                | JObject object ->
                    object |> Map.toList |> List.map (fun (key, value) -> value)
                | _ -> failwith "Not a sequence"
            let renderedElements =
                codes
                |> List.collect (fun code ->
                    match List.tryFind (fun (c, _) -> c = code) jsonList with
                    | Some (_, json) -> [renderingCodeToReactElement code (path @ [code]) json]
                    | None -> [])
            Bulma.block[ReactBindings.React.createElement("div", dict [], renderedElements)]
        | Hole named ->
            let name =
                match named with
                | UnNamed -> "Unnamed"
                | Named name -> name
            let fieldTypes = json |> recognizeJson
            let optionPane =
                fieldTypes
                |> List.collect (fun (code, json) -> [options code (path @ [code]) name])
            Bulma.block[ReactBindings.React.createElement("div", dict [], optionPane)]


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