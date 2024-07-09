module Main

open Elmish
open Feliz
open Types.EditorDomain
open Types.RenderingTypes
open Fable.SimpleJson
open System
open DataProcessing.DataLoading
open UIComponents.MainPageComponents
open Feliz.UseElmish

let newPage =  {
    Name = "New page"
    Id = Guid.NewGuid()
    Data = JNull
    Code =  Hole(UnNamed)
    FileUploadError = false
}
let init () : Model * Cmd<Msg> =
    {
        Pages = Map[newPage.Id, newPage]
        IsSidebarOpen = true
        ActivePageId = Some newPage.Id
        TabOrder = [newPage.Id]
    },
    Cmd.none

let update (msg: Msg) (model: Model): Model * Cmd<Msg> =
    match msg with
    | ToggleSidebar ->
        { model with IsSidebarOpen = not model.IsSidebarOpen }, Cmd.none
    | SetActivePage id ->
        { model with ActivePageId = Some id }, Cmd.none
    | ClosePage id ->
        let newPages = model.Pages |> Map.remove id
        let newTabOrder = model.TabOrder |> List.filter ((<>) id)
        let newActiveId =
            if model.ActivePageId = Some id then
                newTabOrder |> List.tryHead
            else model.ActivePageId
        { model with Pages = newPages; TabOrder = newTabOrder; ActivePageId = newActiveId }, Cmd.none
    | CreateNewPage ->
        let newId = Guid.NewGuid()
        let newPage = { Name = "New page"
                        Id = Guid.NewGuid()
                        Data = JNull
                        Code = Hole(UnNamed)
                        FileUploadError = false }
        { model with
            Pages = model.Pages.Add(newId, newPage)
            TabOrder = model.TabOrder @ [newId]
            ActivePageId = Some newId }, Cmd.none
    | ReorderTabs newOrder ->
        { model with TabOrder = newOrder }, Cmd.none




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
                        match model.ActivePageId with
                        | Some id when model.Pages.ContainsKey id ->
                            PageEditor (model.Pages.[id])
                        | _ ->
                            Html.div [
                                prop.className "flex-1 flex items-center justify-center"
                                prop.children [ Html.text "No page selected" ]
                            ]
                    ]
                ]
            ]
        ]
    LowCodeEditor