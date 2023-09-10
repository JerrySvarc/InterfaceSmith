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
open FSharp.Data

type Model =
    { CurrentComponent : Component
      FileUploadError : bool
      EditingName : bool
      NameInput : string
      ElementChoices : RenderingCode list
      CurrentField : Json * RenderingCode}

type Msg =
    | UploadData of string
    | ChangeName of string
    | SetInput of string
    | ChangeNameEditMode of bool
    | SaveComponent of Component
    | EditField of Json

let init() =
    {CurrentComponent = {Name = "New component"; JsonData = JNull; Code = Sequence([Hole]); Id = Guid.Empty  }; FileUploadError = false; EditingName = false; NameInput = "";
     ElementChoices = List.Empty; CurrentField = (JNull, Hole)}

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | UploadData data ->
        let loadedDataOption = loadJson data
        match loadedDataOption with
        | Some(data)  ->
            let newComponent = {Name = "New component"; JsonData = data ; Code= Hole; Id = Guid.NewGuid()}
            {model with CurrentComponent = newComponent; FileUploadError = false; CurrentField = (newComponent.JsonData, Hole)}, Cmd.none
        | None ->
            {model with FileUploadError = true}, Cmd.none
    | ChangeName newName->
        {model with CurrentComponent = {model.CurrentComponent with Name = newName}; NameInput = ""; EditingName = false}, Cmd.none
    | SetInput input->
        {model with NameInput = input}, Cmd.none
    | ChangeNameEditMode value ->
        {model with EditingName = value; NameInput = ""}, Cmd.none
    | EditField field ->
        {model with CurrentField = (field, fieldToCode field)}, Cmd.none
    | SaveComponent newComponent ->
        model, Cmd.none

let view (model: Model) (dispatch: Msg -> unit) =

    let upploadButtonView onLoad =
        Bulma.block[
            Bulma.file[
                file.isNormal
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
    let menuView (label: string) (menuItems : ReactElement)  =
        Bulma.menu [
            Bulma.menuLabel [
                Html.text label
            ]
            Bulma.menuList [menuItems]
        ]

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

    let editButton (name:string) (data : Json) =
        Bulma.block[
            Bulma.button.a [
                prop.text name
                prop.onClick (fun _ -> dispatch (EditField data))
            ]
        ]
    let elementSelectionView =
        Bulma.columns[
                Bulma.column[
                    column.is3
                    prop.children[
                        match model.CurrentField with
                        | JNull, _ ->
                            Html.text "No field to edit"
                        | json,code  ->
                            match json with
                            | JObject object ->
                                let sequenceElementsButtons =
                                    Map.map (fun key item ->editButton key item) object
                                    |> Map.toSeq
                                    |> Seq.map snd
                                    |> List.ofSeq
                                menuView "Select field to edit" (sequenceElementsButtons|> Html.div)
                            | JArray array ->
                                let childEditButton  =
                                    array.Head |> editButton "Array element"
                                menuView "Select field to edit" childEditButton
                            | _ ->
                                Html.text "No children to edit"
                    ]
                ]

                Bulma.column[
                    Bulma.box[
                        Html.h1 "Selected field"
                        Html.div[
                            Html.text (model.CurrentField.ToString())
                        ]
                    ]
                ]
        ]

    let editorView  =
        Bulma.box[
            if model.CurrentComponent.JsonData <> JNull then
                nameEditView
            Bulma.box[
                if model.CurrentComponent.JsonData = JNull then
                    //center
                    prop.className "is-centered"
                    prop.children [
                        menuView "Upload data" uploadButton
                    ]
                else
                    prop.children[
                        elementSelectionView
                    ]
            ]
        ]
    editorView
