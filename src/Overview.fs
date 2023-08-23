module Overview

open Elmish
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
    | EditComponent of Guid

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
    | EditComponent guid ->
        model, Cmd.none

open Feliz
open Feliz.Bulma

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
                        prop.onClick (fun _ -> dispatch (EditComponent id))
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


    if model.CreatedComponents.Count = 0 then
        Bulma.card[
            color.isInfo
            prop.children[
                Bulma.cardContent[
                    Html.text "No components to display."
                ]
            ]
        ]

    else
        createdComponentView.Values |> Html.div
