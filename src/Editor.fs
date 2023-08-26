module Editor

open Elmish
open Feliz
open Feliz.Bulma
open Types
open Fable.SimpleJson
open DataLoading
open FileUpload

type Model =
    { CurrentComponent : Component
      FileUploadError : bool
      EditingName : bool
      Input : string }

type Msg =
    | UploadData of string
    | ChangeName of string
    | SetInput of string
    | NameEditMode of bool

let init() =
    {CurrentComponent = {Name = "New component"; JsonData = JNull; Code = Hole   }; FileUploadError = false; EditingName = false; Input = ""}

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | UploadData data ->
        let loadedDataOption = loadJson data
        match loadedDataOption with
        | Some(data)  ->
            let newComponent = {Name = "New component"; JsonData = data ; Code= Hole}
            {model with CurrentComponent = newComponent; FileUploadError = false}, Cmd.none
        | None ->
            {model with FileUploadError = true}, Cmd.none
    | ChangeName newName->
        {model with CurrentComponent = {model.CurrentComponent with Name = newName}; Input = ""; EditingName = false}, Cmd.none
    | SetInput input->
        {model with Input = input}, Cmd.none
    | NameEditMode value ->
        {model with EditingName = value}, Cmd.none

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


    let sideMenuView =
        Bulma.block[
            Bulma.menu [
                Bulma.menuLabel [
                    Html.text "Upload data to start"
                ]
                Bulma.menuList [ upploadButtonView (UploadData >> dispatch)]
            ]
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
                        Html.text (nameText)
                    ]
                Bulma.block[
                    Bulma.buttons[
                        Bulma.button.button[
                            color.isPrimary
                            if model.EditingName then
                                prop.text "Save"
                                if model.Input.Length > 0 then
                                    prop.onClick (fun _ -> dispatch (ChangeName model.Input))
                                else
                                    color.isWarning
                                    prop.text "Name must be at least one character"
                            else
                                prop.text "Edit"
                                prop.onClick (fun _ -> dispatch (NameEditMode (model.EditingName |> not )))
                        ]
                        if model.EditingName then
                            Bulma.button.button[
                                prop.text "Cancel"
                                color.isDanger
                                prop.onClick (fun _ -> dispatch (NameEditMode false))
                            ]
                    ]
                ]
            ]
    let editorView  =
        Bulma.box[
            nameEditView
            Bulma.box[
                Bulma.columns[
                    if model.CurrentComponent.JsonData = JNull then
                        Bulma.column[
                            sideMenuView
                        ]
                    else
                        Bulma.column[ Html.text ( model.CurrentComponent.JsonData.ToString())]
                ]
            ]
        ]
    editorView