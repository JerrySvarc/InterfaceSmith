module Overview

open Elmish
open Feliz
open Feliz.Bulma
open Types
open Fable.SimpleJson
open DataLoading
open System
type Model =
    {
        FileUploadError : bool
        CreatedComponents: Map<Guid, Component>
    }

type Msg =
    | DeleteComponent of Guid

let code = Sequence [
    HtmlElement("h1", [], Constant("TODO list"))
    HtmlList(false, Field("tasks"), Hole) ]
let newComp = {Name = "example";Code =code ; JsonData = JNull}

let init() =
    {  CreatedComponents = Map.empty; FileUploadError  = false}

let update (msg: Msg) (model: Model) : Model * Cmd<Msg>=
    match msg  with
    | DeleteComponent guid ->
        {model with CreatedComponents = model.CreatedComponents |> Map.remove guid}, Cmd.none

let view (model: Model) (dispatch: Msg -> unit) =

    let componentCards id createdComponent =
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
                    prop.onClick (fun _ -> dispatch (DeleteComponent id))
                ]
            ]
        ]
    ]


    let createdComponentView  =
        let cards = Map.map  (fun key value -> componentCards key value)  model.CreatedComponents
        cards


    createdComponentView.Values |> Html.div
