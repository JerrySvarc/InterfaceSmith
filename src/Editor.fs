module Editor

open Elmish
open Feliz
open Feliz.Bulma
open Types
open Fable.SimpleJson

type Model =
    {  }

type Msg =


let init() =
    { }

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    |

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