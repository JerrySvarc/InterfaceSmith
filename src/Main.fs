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
    let newTab = {
        Id = Guid.NewGuid()
        PageId = newPage.Id
    }

    let newModel = {
        Pages = Map[newPage.Id, newPage]
        IsSidebarOpen = true
        OpenTabs = [ newTab ]
        ActiveTabId = Some(newTab.Id)
    }

    newModel, Cmd.none

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | OpenTab pageId ->
        let existingTab = model.OpenTabs |> List.tryFind (fun t -> t.PageId = pageId)

        match existingTab with
        | Some tab -> { model with ActiveTabId = Some tab.Id }, Cmd.none
        | None ->
            let newTab = { Id = Guid.NewGuid(); PageId = pageId }

            {
                model with
                    OpenTabs = model.OpenTabs @ [ newTab ]
                    ActiveTabId = Some newTab.Id
            },
            Cmd.none

    | CloseTab tabId ->
        let newTabs = model.OpenTabs |> List.filter (fun t -> t.Id <> tabId)

        let newActiveTabId =
            if model.ActiveTabId = Some tabId then
                newTabs |> List.tryLast |> Option.map (fun t -> t.Id)
            else
                model.ActiveTabId

        {
            model with
                OpenTabs = newTabs
                ActiveTabId = newActiveTabId
        },
        Cmd.none

    | SetActiveTab tabId -> { model with ActiveTabId = Some tabId }, Cmd.none

    | ReorderTabs newOrder -> { model with OpenTabs = newOrder }, Cmd.none

    | CreatePage ->
        let newPageId = Guid.NewGuid()
        let newPage = { newPage with Id = newPageId }

        let newTab = {
            Id = Guid.NewGuid()
            PageId = newPageId
        }

        {
            model with
                Pages = model.Pages |> Map.add newPageId newPage
                OpenTabs = model.OpenTabs @ [ newTab ]
                ActiveTabId = Some newTab.Id
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
        let newTabs = model.OpenTabs |> List.filter (fun t -> t.PageId <> pageId)

        let newActiveTabId =
            if
                model.ActiveTabId
                |> Option.exists (fun activeTabId ->
                    model.OpenTabs |> List.exists (fun t -> t.Id = activeTabId && t.PageId = pageId))
            then
                newTabs |> List.tryLast |> Option.map (fun t -> t.Id)
            else
                model.ActiveTabId

        {
            model with
                Pages = newPages
                OpenTabs = newTabs
                ActiveTabId = newActiveTabId
        },
        Cmd.none

    | ToggleSidebar ->
        {
            model with
                IsSidebarOpen = not model.IsSidebarOpen
        },
        Cmd.none

    | OpenOrSelectTab pageId ->
        let existingTab = model.OpenTabs |> List.tryFind (fun tab -> tab.PageId = pageId)

        match existingTab with
        | Some tab -> { model with ActiveTabId = Some tab.Id }, Cmd.none
        | None ->
            let newTab = { Id = Guid.NewGuid(); PageId = pageId }

            {
                model with
                    OpenTabs = model.OpenTabs @ [ newTab ]
                    ActiveTabId = Some newTab.Id
            },
            Cmd.none

// View function that renders the Main application
let view (model: Model) (dispatch: Msg -> unit) =
    let LowCodeEditor =
        let activePageId =
            model.ActiveTabId
            |> Option.bind (fun tabId ->
                model.OpenTabs
                |> List.tryFind (fun tab -> tab.Id = tabId)
                |> Option.map (fun tab -> tab.PageId))

        Html.div [
            prop.className "flex h-screen bg-gray-100 text-gray-800"
            prop.children [
                Sidebar model dispatch
                Html.div [
                    prop.className "flex-1 flex flex-col overflow-hidden"
                    prop.children [
                        Tabs model dispatch
                        match activePageId with
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