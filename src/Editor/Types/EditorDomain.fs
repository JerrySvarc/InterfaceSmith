module Editor.Types.EditorDomain

open Fable.SimpleJson
open System
open CoreLogic.Types.RenderingTypes

//Represents a single created page
type Page = {
    Name: string
    Id: Guid
    ParsedJson: Json
    CurrentTree: RenderingCode
    JsonString: string
    CustomHandlers: Map<string, Javascript>
}



// Application state
type Model = {
    Pages: Map<Guid, Page>
    ActivePageId: Guid option
    IsSidebarOpen: bool
}

// Application operations
// General operation such as tab management or opening new pages
type Msg =
    | CreatePage
    | UpdatePage of Page
    | DeletePage of Guid
    | ToggleSidebar
    | OpenPage of Guid