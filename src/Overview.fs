module Overview

open Elmish
open Feliz
open Feliz.Bulma
open Types
open Fable.SimpleJson
open DataLoading

type Model =
    {
        FileUploadError : bool
        CreatedComponents: Component list
    }

type Msg =
    | DeleteComponent of string
    | UploadData of string

let init() =
    {  CreatedComponents = []; FileUploadError  = false}

let update (msg: Msg) (model: Model) : Model * Cmd<Msg>=
    match msg  with
    | UploadData data ->
        let loadedDataOption = loadJson data
        match loadedDataOption with
        | Some(data)  -> failwith "k"
            //let newComponent = {Name = ""; JsonData =data ; Code= Hole}
            //let newEditorModel = {model.EditorModel with CurrentComponent = newComponent}
            //{model with CurrentPage = Editor; EditorModel = newEditorModel; FileUploadError = false}, Cmd.none
        | None ->
            {model with FileUploadError = true}, Cmd.none
    | DeleteComponent(_) -> failwith "Not Implemented"

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
                           // prop.onChange ( handleFileEvent onLoad)
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

    let componentCards (createdComponent : Component)=
        Bulma.card [
        Bulma.cardContent [
            Bulma.media [
                Bulma.mediaContent [
                    Bulma.title.p [
                        Bulma.title.is4
                        prop.text createdComponent.Name
                    ]
                ]
            ]
            Bulma.cardFooter [
            Bulma.cardFooterItem.a [
                prop.text "Save"
            ]
            Bulma.cardFooterItem.a [
                prop.text "Edit"
               // prop.onClick (fun _ -> dispatch (ChangePage Editor))
            ]
            Bulma.cardFooterItem.a [
                prop.text "Delete"
                prop.onClick (fun _ -> dispatch (DeleteComponent createdComponent.Name))
            ]
            ]
        ]
    ]



    let createdComponentView  =
        List.map componentCards model.CreatedComponents

    createdComponentView |> Html.div