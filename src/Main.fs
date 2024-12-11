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
    let newPageEditorModel, cmd = pageEditorInit ()

    let newModel = {
        Pages = Map [ (newPageEditorModel.PageData.Id, newPageEditorModel) ]
        PageOrder = [ newPageEditorModel.PageData.Id ]
        IsSidebarOpen = true
        ActivePageId = Some(newPageEditorModel.PageData.Id)
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
                PageOrder = model.PageOrder @ [ newEditorPage.PageData.Id ]
        },
        Cmd.none

    | UpdatePage page ->
        {
            model with
                Pages = model.Pages |> Map.add page.PageData.Id page
        },
        Cmd.none

    | DeletePage pageId ->
        let newPages =
            match Map.tryFind pageId model.Pages with
            | Some page -> Map.remove pageId model.Pages
            | None -> model.Pages

        let newPageOrder =
            match List.tryFindIndex (fun x -> x = pageId) model.PageOrder with
            | Some index -> List.removeAt index model.PageOrder
            | None -> model.PageOrder

        let activePageId =
            match model.ActivePageId with
            | Some id -> if id = pageId then None else model.ActivePageId
            | None -> None

        {
            model with
                Pages = newPages
                ActivePageId = activePageId
                PageOrder = newPageOrder
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