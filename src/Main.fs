module Main

open Elmish
open Feliz
open Editor.Types.EditorDomain
open CoreLogic.Types.RenderingTypes
open Fable.SimpleJson
open System
open Editor.UIComponents.EditorComponents
open Editor.UIComponents.PageEditorComponents
open Feliz.UseElmish


let newPage = {
    Name = "New page"
    Id = Guid.NewGuid()
    ParsedJson = JNull
    CurrentTree = Hole(UnNamed)
    JsonString = ""
    CustomHandlers = Map([])
}

let init () : Model * Cmd<Msg> =

    let newModel = {
        Pages = Map[newPage.Id, newPage]
        IsSidebarOpen = true
        ActivePageId = Some(newPage.Id)
    }

    newModel, Cmd.none

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with


    | CreatePage ->
        let newPageId = Guid.NewGuid()
        let newPage = { newPage with Id = newPageId }

        {
            model with
                Pages = model.Pages |> Map.add newPageId newPage
                ActivePageId = Some newPageId
        },
        Cmd.none

    | UpdatePage page ->
        {
            model with
                Pages = model.Pages |> Map.add page.Id page
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
        let existingTab = model.Pages |> Map.tryFind pageId

        match existingTab with
        | Some tab ->
            {
                model with
                    ActivePageId = Some tab.Id
            },
            Cmd.none
        | None ->

            model, Cmd.none

// View function that renders the Main application
let view (model: Model) (dispatch: Msg -> unit) =
    let LowCodeEditor =

        Html.div [
            prop.className "flex h-screen bg-gray-100 text-gray-800"
            prop.children [
                Sidebar model dispatch
                Html.div [
                    prop.className "flex-1 flex flex-col overflow-hidden"
                    prop.children [
                        match model.ActivePageId with
                        | Some pageId ->
                            match Map.tryFind pageId model.Pages with
                            | Some page -> PageEditor page dispatch
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

    LowCodeEditor