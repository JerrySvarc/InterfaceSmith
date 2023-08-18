module Editor

open Elmish
open Feliz
open Feliz.Bulma
open Types
open Fable.SimpleJson

type Model =
    { CurrentComponent : Component }

type Msg =
    | UploadData

let init() =
    {CurrentComponent = {Name = "New component"; JsonData = JNull; Code = Hole   } }

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | UploadData -> failwith ":"

let view (model: Model) (dispatch: Msg -> unit) =
    let editorView (createdComponent : Component) =
        Html.div[
            Bulma.columns[
                Bulma.column[ Html.text (SimpleJson.stringify (createdComponent.JsonData))]
                Bulma.column[

                ]
            ]
        ]
    editorView