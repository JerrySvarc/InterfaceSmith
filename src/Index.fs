module Index

open Elmish
open Fable.Remoting.Client
open Types

type Model =
    { CreatedComponents: RenderingCode list }


type Msg =
    | SetData of Value
    | SetRenderingCode of RenderingCode

let example =
    Sequence [ HtmlElement("h1", [], Constant("TODO list"))
               HtmlList(false, Field("tasks"), Hole) ]

let init () : Model * Cmd<Msg> =
    { CreatedComponents = [ example;  ] }, Cmd.none


let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    { model with CreatedComponents = [ example; ] }, Cmd.none


open Feliz
open Feliz.Bulma
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
                    prop.text "Component structure"
                ]
            ]
        ]
        Bulma.cardFooter [
        Bulma.cardFooterItem.a [
            prop.text "Save"
        ]
        Bulma.cardFooterItem.a [
            prop.text "Edit"
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

let view (model: Model) (dispatch: Msg -> unit) =
    Html.div [
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
    ]