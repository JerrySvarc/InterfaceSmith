module Index

open Elmish
open Fable.Remoting.Client
open Types

type Model =
    { Data: Value
      RenderingCode: RenderingCode }

type Msg =
    | SetData of Value
    | SetRenderingCode of RenderingCode

let example =
    Sequence [
        HtmlElement("h1", [], Constant("TODO list"))
        HtmlList(false, Field("tasks"), Hole)
    ]



let ex2 =
    Sequence [
        HtmlElement("h1", [], Constant("TODO list"))
        HtmlList(
            false,
            Field("tasks"),
            Sequence [
                HtmlElement("input", [ "checked", Reference(Field("completed")) ], Empty)
                HtmlElement("label", [], Reference(Field("task")))
            ]
        )
    ]

let init () : Model * Cmd<Msg> =
    { Data = Empty
      RenderingCode = example },
    Cmd.none


let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    { Data = Empty
      RenderingCode = example },
    Cmd.none


open Feliz
open Feliz.Bulma



let view (model: Model) (dispatch: Msg -> unit) =
    Html.div [
        Html.h1 [ Html.text "TODO list" ]
        Html.ul [
            Html.li [ Html.text "Buy milk" ]
            Html.li [ Html.text "Buy eggs" ]
            Html.li [ Html.text "Buy bread" ]
        ]
    ]