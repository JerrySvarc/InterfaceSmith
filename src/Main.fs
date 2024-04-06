module Index

open Elmish
open Types
open Overview
open Editor
open Feliz

type Page =
    | Overview
    | Editor

type Model = {
    CurrentPage: Page
    OverviewModel: Overview.Model
    EditorModel: Editor.Model
}

type Msg =
    | ChangePage of Page
    | OverviewMsg of Overview.Msg
    | EditorMsg of Editor.Msg

let init () : Model * Cmd<Msg> =
    {
        CurrentPage = Overview
        OverviewModel = Overview.init ()
        EditorModel = Editor.init ()
    },
    Cmd.none

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | ChangePage page -> { model with CurrentPage = page }, Cmd.none
    | OverviewMsg msg ->
        match msg with
        | EditComponent guid ->
            let found, chosenComponent = model.OverviewModel.CreatedComponents.TryGetValue(guid)

            if found then
                let updatedEditor = {
                    model.EditorModel with
                        CurrentComponent = chosenComponent
                        FileUploadError = false
                        EditingName = false
                        NameInput = ""
                }

                {
                    model with
                        EditorModel = updatedEditor
                        CurrentPage = Editor
                },
                Cmd.none
            else
                model, Cmd.none
        | _ ->
            let updatedOverview, overviewCmd = Overview.update msg model.OverviewModel

            {
                model with
                    OverviewModel = updatedOverview
            },
            Cmd.none
    | EditorMsg msg ->
        match msg with
        | SaveComponent comp ->
            let newComponent = comp

            let updatedMap =
                model.OverviewModel.CreatedComponents.Add(newComponent.Id, newComponent)

            {
                model with
                    OverviewModel = {
                        model.OverviewModel with
                            CreatedComponents = updatedMap
                    }
                    EditorModel = Editor.init ()
                    CurrentPage = Overview
            },
            Cmd.none
        | _ ->
            let updatedEditor, editorCmd = Editor.update msg model.EditorModel

            {
                model with
                    EditorModel = updatedEditor
            },
            Cmd.none



let view (model: Model) (dispatch: Msg -> unit) =

    let navButton (page: Page) (text: string) =
        Html.button [
            if model.CurrentPage = page then
                prop.className
                    "text-gray-300 hover:bg-gray-700 hover:text-white px-3 py-2 rounded-md text-sm font-medium bg-gray-900"
            else
                prop.className
                    "text-gray-300 hover:bg-gray-700 hover:text-white px-3 py-2 rounded-md text-sm font-medium"
            prop.text text
            prop.onClick (fun _ -> dispatch (ChangePage page))
        ]

    let navButtons =
        Html.div [
            prop.className "flex space-x-4"
            prop.children [ navButton Overview "Overview"; navButton Editor "Editor" ]
        ]

    let navBar =
        Html.div [
            prop.className "fixed top-0 w-full"
            prop.children [
                Html.nav [
                    prop.className "bg-gray-800"
                    prop.children [
                        Html.div [
                            prop.className "max-w-7xl px-2 sm:px-6 lg:px-8"
                            prop.children [
                                Html.div [
                                    prop.className "relative flex h-16 items-center"
                                    prop.children [
                                        Html.div [
                                            prop.className
                                                "flex items-center justify-start sm:items-stretch sm:justify-start"
                                            prop.children [
                                                Html.div [
                                                    prop.className "sm:ml-6 sm:block"
                                                    prop.children [ navButtons ]
                                                ]
                                            ]
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]

    let mainView =
        Html.div [
            prop.className "flex"
            prop.children [
                navBar
                match model.CurrentPage with
                | Overview -> Overview.view model.OverviewModel (OverviewMsg >> dispatch)
                | Editor -> Editor.view model.EditorModel (EditorMsg >> dispatch)
            ]
        ]

    mainView