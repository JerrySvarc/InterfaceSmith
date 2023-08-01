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
        CreatedComponents: RenderingCode list
        Editor : EditorModel }


type Msg =
    | ChangePage of Page

let example =
    Sequence [ HtmlElement("h1", [], Constant("TODO list"))
               HtmlList(false, Field("tasks"), Hole) ]

let exampleEditor = { CurrentComponent = {Name = "example";Code =example } }


let init () : Model * Cmd<Msg> =
    {CurrentPage = Main;  CreatedComponents = [ example; ]; Editor = exampleEditor }, Cmd.none


let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | ChangePage page -> { model with CurrentPage = page }, Cmd.none


open Feliz
open Feliz.Bulma


let view (model: Model) (dispatch: Msg -> unit) =
    //Definition of different UI elements used in the application
    let upploadButtonView =
        Bulma.file[
                file.isNormal
                prop.children [
                    Bulma.fileLabel.label [
                        Bulma.fileInput [
                            prop.type' "file"
                            prop.name "component-data"
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
                    Bulma.menuList [ upploadButtonView]
                ]
            ]

    let componentCards renderingCode =
        Bulma.card [
        Bulma.cardContent [
            Bulma.media [
                Bulma.mediaContent [
                    Bulma.title.p [
                        Bulma.title.is4
                        prop.text "Component name"
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

    Html.div [
        match model.CurrentPage with
        | Main ->
            navBar
            Bulma.columns [
                prop.children [
                    Bulma.column[
                        column.is2
                        prop.children [
                            sideMenuView
                        ]
                    ]
                    Bulma.column [createdComponentView model |> Html.div]
                ]
            ]
        | Editor ->
            navBar
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
        | Preview ->
            navBar
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