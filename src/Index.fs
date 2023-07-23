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

let upploadButtonView = Html.button [ Html.text "Upload" ]

let sideMenuView =
    Bulma.box[
        Bulma.menu [
            Bulma.menuLabel [
                Html.text "Menu"
            ]
            Bulma.menuList [ upploadButtonView]
        ]
    ]

let createdComponentView (model: Model) =
    Bulma.box [  Html.text "Created Components"  ]
let componentCard = Html.div [ Bulma.card [ Html.text "Component Card" ] ]
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
                       //Created components will be shown here
                    ]
                ]
            ]
        ]