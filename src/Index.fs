module Index

open Elmish
open Fable.Remoting.Client
open Types

type Page =
    | Main
    | Editor
    | Preview

type EditorModel =
    {   CurrentComponent : Component }

type Model =
    {   CurrentPage : Page
        CreatedComponents: Component list
        EditorModel : EditorModel }


type Msg =
    | ChangePage of Page
    | DeleteComponent of string
    | UploadData of string


//placeholder for the data that will be uploaded
let example =
    Sequence [ HtmlElement("h1", [], Constant("TODO list"))
               HtmlList(false, Field("tasks"), Hole) ]

let exampleEditor = { CurrentComponent = {Name = "example";Code =example ; JsonData = ""} }


let init () : Model * Cmd<Msg> =
    {CurrentPage = Main;  CreatedComponents = [ exampleEditor.CurrentComponent; exampleEditor.CurrentComponent]; EditorModel = exampleEditor }, Cmd.none


let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | ChangePage page -> { model with CurrentPage = page }, Cmd.none
    | UploadData data ->
        let newComponent = {Name = ""; JsonData = data; Code = Hole}
        let newEditorModel = {model.EditorModel with CurrentComponent = newComponent}
        {model with CurrentPage = Editor; EditorModel = newEditorModel}, Cmd.none
    | DeleteComponent(_) -> failwith "Not Implemented"

open FileUpload
open Feliz
open Feliz.Bulma

let view (model: Model) (dispatch: Msg -> unit) =

    //Definition of different UI elements used in the application

    let upploadButtonView onLoad =
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
                prop.onClick (fun _ -> dispatch (ChangePage Editor))
            ]
            Bulma.cardFooterItem.a [
                prop.text "Delete"
                prop.onClick (fun _ -> dispatch (DeleteComponent createdComponent.Name))
            ]
            ]
        ]
    ]

    let navBar =
        Bulma.navbar[
            Bulma.navbarBrand.a [
                Bulma.navbarItem.a [
                    prop.text "Value driven UI editor"
                ]
        ]
    ]

    let createdComponentView (model: Model) =
        List.map componentCards model.CreatedComponents

    let editorView (createdComponent : Component) =
        Html.div[
            Bulma.columns[
                Bulma.column[ Html.text createdComponent.JsonData]
                Bulma.column[]
            ]
        ]

    let mainView =
        Html.div [
            navBar
            match model.CurrentPage with
            | Main ->
                sideMenuView
                Bulma.column [createdComponentView model |> Html.div]

            | Editor ->
                Feliz.Html.button [
                    prop.text "Back"
                    prop.onClick (fun _ -> dispatch (ChangePage Main))
                ]
                editorView model.EditorModel.CurrentComponent
            | Preview ->
                Bulma.columns [
                    prop.children [
                        Bulma.column[
                            column.is2
                            prop.children [
                                sideMenuView
                            ]
                        ]
                        Bulma.column [
                            Bulma.box [
                            ]
                        ]
                    ]
                ]
        ]

    mainView
