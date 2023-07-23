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
    { CreatedComponents = [ example ] }, Cmd.none


let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    { model with CreatedComponents = [ example ] }, Cmd.none


open Feliz
open Feliz.Bulma

let upploadButtonView =
   Bulma.file[
        file.isNormal
        prop.children [
            Bulma.fileLabel.label [
                Bulma.fileInput [
                    prop.type' "file"
                    prop.name "resume"
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

let componentCard RenderingCode =
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

let createdComponentView (model: Model) =
    Bulma.box [ componentCard example; componentCard example ]

let view (model: Model) (dispatch: Msg -> unit) =

        Bulma.columns [
            prop.children [
                Bulma.column[
                    column.is2
                    prop.children [
                        sideMenuView
                    ]
                ]
                Bulma.column[
                    column.is10
                    prop.children [
                        createdComponentView model
                       //Created components will be shown here
                    ]
                ]
            ]
        ]