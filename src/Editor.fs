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
open Fable.React
open Fable.Core.JsInterop
open Selector
open Fable.Core.JS


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
    {CurrentComponent = {Name = "New component"; Code = Hole []; Id = Guid.NewGuid(); Data = JNull};
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
                                if model.CurrentComponent.Code = Hole [] then
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

    let elementOptions (element : RenderingCode) path  : ReactElement =
        match element with
        | HtmlElement (tag, attrs, innerText) ->
            Bulma.box[

            ]
        | _ -> failwith "Not an element"

    let listOptions list  path : ReactElement=
        match list with
        | HtmlList (listType, numbered, code, _) ->
            Bulma.box[]
        | _ -> failwith "Not a list"

    let rec sequenceOptions sequence path : ReactElement=
        match sequence with
        | Sequence(_) ->
            Bulma.box[]
        | _ -> failwith "Not a sequence"


    let options (code : RenderingCode) (path : RenderingCode list) =
        match code with
        | HtmlElement _ ->
            elementOptions code path
        | HtmlList _->
            listOptions code path
        | Sequence(_) ->
            sequenceOptions code path
        | Hole _ -> failwith "Hole cannot contain another hole"

    let rec renderingCodeToReactElement (code: RenderingCode) (path : RenderingCode list)  =
        match code with
        | HtmlElement (tag, attrs, innerText) ->
            let props = attrs |> List.toSeq |> dict
            match innerText with
                | Data data ->
                    let selectedFields = (select model.CurrentComponent.Data [data])
                    let jsonStr = selectedFields.Head |> Json.convertFromJsonAs<String>
                    ReactBindings.React.createElement(tag, props, [str jsonStr])
                | Value.Empty -> ReactBindings.React.createElement(tag, props, [])
                | Constant s -> ReactBindings.React.createElement(tag, props, [str s])
        | HtmlList (listType, numbered, code, _) ->
            let tag = if numbered then "ol" else "ul"
            let children = [path @ [code] |> renderingCodeToReactElement code]
            ReactBindings.React.createElement(tag, null, children)
        | Sequence codes ->
            let children = List.map (fun code -> path @ [code] |> renderingCodeToReactElement code) codes |> List.toArray
            ReactBindings.React.createElement("div", null, children)
        | Hole selectors ->
            //let selectedFields = (select model.CurrentComponent.Data selectors)
           // let optionPanes = List.map (fun field -> options (recognizeJson field)) selectedFields |> List.toArray
           // ReactBindings.React.createElement("div", null, optionPanes)
            ReactBindings.React.createElement("div", null, [str "Hole"])


    let rec replace path replacementElement (currentCode : RenderingCode) =
        match path with
        | [] -> replacementElement
        | head::tail ->
            match currentCode with
            | HtmlList(lt, n, item, c) ->
                let newItems =
                    item
                    |> fun item ->
                        if HashIdentity.Reference.Equals(item, head) then
                            replace tail replacementElement item
                        else
                            item
                HtmlList(lt, n, newItems, c)
            | Sequence(items) ->
                let newItems =
                    items
                    |> List.map (fun item -> if HashIdentity.Reference.Equals(item, head) then replace tail replacementElement item else item)
                Sequence(newItems)
            | _ -> currentCode

    let x = Sequence([HtmlElement("div", [], Data(FieldSelector ("name"))); Hole([FieldSelector ("type")])])

    let editorView  =
        Bulma.box[
            if model.CurrentComponent.Code <> Hole [] then
                nameEditView
            Bulma.box[
                prop.children[
                if model.CurrentComponent.Code = Hole [] then
                    uploadButton
                else
                    Bulma.columns[
                        prop.classes ["is-centered"]
                        prop.children[
                            Bulma.column[
                                column.isHalf
                                prop.children[
                                    renderingCodeToReactElement x []
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
    editorView