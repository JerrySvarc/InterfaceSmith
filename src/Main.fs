module Main

open Elmish
open Feliz
open Editor.Types.EditorDomain
open Editor.Types.PageEditorDomain
open CoreLogic.Types.RenderingTypes
open Fable.SimpleJson
open System
open Editor.UIComponents.EditorComponents
open Editor.UIComponents.PageEditorComponents


let init () : Model * Cmd<Msg> =
    let newEditorPage, cmd = pageEditorInit ()

    let newModel = {
        Pages = Map []
        IsSidebarOpen = true
        ActivePageId = Some(newEditorPage.PageData.Id)
        CurrentPageEditor = Some(newEditorPage)
    }

    newModel, Cmd.none

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | CreatePage ->
        let newEditorPage, cmd = pageEditorInit ()

        {
            model with
                Pages = model.Pages |> Map.add newEditorPage.PageData.Id newEditorPage
                ActivePageId = Some newEditorPage.PageData.Id
        },
        Cmd.none

    | UpdatePage page ->
        {
            model with
                Pages = model.Pages |> Map.add page.PageData.Id page
        },
        Cmd.none

    | DeletePage pageId ->
        let newPages = model.Pages |> Map.remove pageId

        {
            model with
                Pages = newPages
                ActivePageId = None
        },
        Cmd.none

    | ToggleSidebar ->
        {
            model with
                IsSidebarOpen = not model.IsSidebarOpen
        },
        Cmd.none
    | OpenPage pageId ->
        match model.Pages |> Map.tryFind pageId with
        | Some pageModel ->
            {
                model with
                    ActivePageId = Some pageId
                    CurrentPageEditor = Some pageModel

            },
            Cmd.none
        | None ->

            model, Cmd.none
    | PageEditorMsg(pageId, pageEditorMsg) ->
        let updatedPages, editorCmd =
            match model.Pages |> Map.tryFind pageId with
            | Some pageEditorModel ->
                let updatedModel, cmd = pageEditorUpdate pageEditorMsg pageEditorModel
                model.Pages |> Map.add pageId updatedModel, cmd
            | None -> model.Pages, Cmd.none

        let newCmd = Cmd.map (fun msg -> PageEditorMsg(pageId, msg)) editorCmd

        { model with Pages = updatedPages }, newCmd


// View function that renders the Main application
let view (model: Model) (dispatch: Msg -> unit) =

    Html.div [
        prop.className "flex h-screen bg-gray-700 text-gray-800"
        prop.children [
            Sidebar model dispatch
            Html.div [
                prop.className "flex-1 flex flex-col overflow-hidden"
                prop.children [
                    match model.ActivePageId with
                    | Some pageId ->
                        match Map.tryFind pageId model.Pages with
                        | Some page ->
                            PageEditorView page (fun editorMsg -> dispatch (PageEditorMsg(pageId, editorMsg)))
                        | None ->
                            Html.div [
                                prop.className "flex-1 flex items-center justify-center"
                                prop.children [ Html.text "Page not found" ]
                            ]
                    | None ->
                        Html.div [
                            prop.className "flex-1 flex items-center justify-center"
                            prop.children [ Html.text "No active tab" ]
                        ]
                ]
            ]
        ]
    ]