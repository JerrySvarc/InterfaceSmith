module Overview

open Elmish
open Types
open System

type Model = {
    FileUploadError: bool
    CreatedComponents: Map<Guid, Component>
}

type Msg =
    | DeleteComponent of Guid
    | EditComponent of Guid


let init () = {
    CreatedComponents = Map.empty
    FileUploadError = false
}

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | DeleteComponent guid ->
        {
            model with
                CreatedComponents = model.CreatedComponents |> Map.remove guid
        },
        Cmd.none
    | EditComponent guid -> model, Cmd.none

open Feliz
open Feliz.Bulma

let view (model: Model) (dispatch: Msg -> unit) =

    let componentCards id createdComponent =
        Html.div [
            prop.className "block bg-white rounded-lg shadow-md overflow-hidden"
            prop.children [
                Html.div [
                    prop.className "px-4 py-5 sm:p-6"
                    prop.children [
                        Html.div [
                            prop.className "media-content"
                            prop.children [ Html.h4 [ prop.className "title is-4"; prop.text createdComponent.Name ] ]
                        ]
                    ]
                ]
                Html.div [
                    prop.className "px-4 py-4 sm:px-6 bg-gray-50"
                    prop.children [
                        Html.a [ prop.className "text-indigo-600 hover:text-indigo-900"; prop.text "Save" ]
                        Html.a [
                            prop.className "text-indigo-600 hover:text-indigo-900 ml-4"
                            prop.text "Edit"
                            prop.onClick (fun _ -> dispatch (EditComponent id))
                        ]
                        Html.a [
                            prop.className "text-indigo-600 hover:text-indigo-900 ml-4"
                            prop.text "Delete"
                            prop.onClick (fun _ -> dispatch (DeleteComponent id))
                        ]
                    ]
                ]
            ]
        ]

    let createdComponentView =
        let cards =
            Map.map (fun key value -> componentCards key value) model.CreatedComponents

        cards


    if model.CreatedComponents.Count = 0 then
        Html.div [
            prop.className "mt-16 block bg-white rounded-lg shadow-md overflow-hidden mx-auto text-center"
            prop.children [
                Html.div [
                    prop.className "px-4 py-5 sm:p-6 justify-center items-center flex"
                    prop.children [
                        Html.div [
                            prop.children [ Html.h4 [ prop.className "text-lg font-medium"; prop.text "No components created" ] ]
                        ]
                    ]
                ]
            ]
        ]
    else
        Html.div [
            prop.className "mt-16 grid grid-cols-1 gap-4 sm:grid-cols-2 lg:grid-cols-3"
            prop.children [ createdComponentView.Values |> Html.div ]
        ]