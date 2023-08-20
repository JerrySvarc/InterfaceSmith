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
      FileUploadError : bool  }

type Msg =
    | UploadData of string

let init() =
    {CurrentComponent = {Name = "New component"; JsonData = JNull; Code = Hole   }; FileUploadError = false }

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | UploadData data ->
        let loadedDataOption = loadJson data
        match loadedDataOption with
        | Some(data)  ->
            let newComponent = {Name = ""; JsonData = data ; Code= Hole}
            let newEditorModel = {model with CurrentComponent = newComponent}
            {model with CurrentComponent = newComponent; FileUploadError = false}, Cmd.none
        | None ->
            {model with FileUploadError = true}, Cmd.none

let view (model: Model) (dispatch: Msg -> unit) =
    let upploadButtonView onLoad =
        Html.div[
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
        Bulma.box[
            Bulma.menu [
                Bulma.menuLabel [
                    Html.text "Menu"
                ]
                Bulma.menuList [ upploadButtonView (UploadData >> dispatch)]
            ]
        ]
    let editorView  =
        Html.div[
            Bulma.columns[
                Bulma.column[ Html.text (Json.stringify (model.CurrentComponent.JsonData))]
                Bulma.column[
                    sideMenuView
                ]
            ]
        ]
    editorView