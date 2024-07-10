module Main

open Elmish
open Feliz
open Types.EditorDomain
open Types.RenderingTypes
open Fable.SimpleJson
open System
open DataProcessing.DataLoading
open UIComponents.MainPageComponents
open UIComponents.EditorComponents
open Feliz.UseElmish


let newPage =  {
    Name = "New page"
    Id = Guid.NewGuid()
    Data = JNull
    Code =  Hole(UnNamed)
    FileUploadError = false
}
let init () : Model * Cmd<Msg> =
    let newTab = {Id = Guid.NewGuid(); PageId = newPage.Id}
    let newModel = {
        Pages = Map[newPage.Id, newPage]
        IsSidebarOpen = true
        OpenTabs = [newTab]
        ActiveTabId = Some(newTab.Id)}
    newModel,
    Cmd.none

let update (msg: Msg) (model: Model): Model * Cmd<Msg> =
    match msg with
    | OpenTab pageId ->
        let newTab = { Id = Guid.NewGuid(); PageId = pageId }
        { model with OpenTabs = model.OpenTabs @ [newTab]; ActiveTabId = Some newTab.Id }, Cmd.none
    | CloseTab tabId ->
        { model with OpenTabs = model.OpenTabs |> List.filter (fun t -> t.Id <> tabId) }, Cmd.none
    | SetActiveTab tabId ->
        { model with ActiveTabId = Some tabId }, Cmd.none
    | ReorderTabs newOrder ->
        { model with OpenTabs = newOrder }, Cmd.none
    | CreatePage  ->
        let newPageId = Guid.NewGuid()
        let newPage = newPage
        let newTab = { Id = Guid.NewGuid(); PageId = newPageId }
        { model with
            Pages = model.Pages |> Map.add newPageId newPage
            OpenTabs = model.OpenTabs @ [newTab]
            ActiveTabId = Some newTab.Id }, Cmd.none
    | UpdatePage page ->
        { model with Pages = model.Pages |> Map.add page.Id page }, Cmd.none
    | DeletePage pageId ->
        let newPages = model.Pages |> Map.remove pageId
        let newTabs = model.OpenTabs |> List.filter (fun t -> t.PageId <> pageId)
        { model with
            Pages = newPages
            OpenTabs = newTabs
            ActiveTabId = if newTabs.IsEmpty then None else model.ActiveTabId }, Cmd.none
    | ToggleSidebar -> {model with IsSidebarOpen = not model.IsSidebarOpen }, Cmd.none
    | OpenOrSelectTab pageId ->
        match model.OpenTabs |> List.tryFind (fun tab -> tab.PageId = pageId) with
        | Some existingTab ->
            { model with ActiveTabId = Some existingTab.Id }, Cmd.none
        | None ->
            let newTab = { Id = Guid.NewGuid(); PageId = pageId }
            { model with
                OpenTabs = model.OpenTabs @ [newTab]
                ActiveTabId = Some newTab.Id
            }, Cmd.none

let view (model: Model) (dispatch: Msg -> unit) =
    let LowCodeEditor =
        Html.div [
        prop.className "flex h-screen bg-gray-100 text-gray-800"
        prop.children [
            Sidebar model dispatch
            Html.div [
                prop.className "flex-1 flex flex-col overflow-hidden"
                prop.children [
                    Tabs model dispatch
                    match model.ActiveTabId with
                    | Some tabId ->
                        match model.OpenTabs |> List.tryFind (fun tab -> tab.Id = tabId) with
                        | Some activeTab ->
                            match Map.tryFind activeTab.PageId model.Pages with
                            | Some page ->
                                PageEditor page dispatch
                            | None ->
                                Html.div [
                                    prop.className "flex-1 flex items-center justify-center"
                                    prop.children [ Html.text "Page not found" ]
                                ]
                        | None ->
                            Html.div [
                                prop.className "flex-1 flex items-center justify-center"
                                prop.children [ Html.text "No tab selected" ]
                            ]
                    | None ->
                        Html.div [
                            prop.className "flex-1 flex items-center justify-center"
                            prop.children [ Html.text "No tab selected" ]
                        ]
                ]
            ]
        ]
    ]
    LowCodeEditor