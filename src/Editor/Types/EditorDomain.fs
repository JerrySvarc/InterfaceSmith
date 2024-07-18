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

type Tab = { Id: Guid; PageId: Guid }

// Application state
type Model = {
    Pages: Map<Guid, Page>
    OpenTabs: Tab list
    ActiveTabId: Guid option
    IsSidebarOpen: bool
}
// Application operations
// General operation such as tab management or opening new pages
type Msg =
    | OpenTab of Guid
    | CloseTab of Guid
    | SetActiveTab of Guid
    | ReorderTabs of Tab list
    | CreatePage
    | UpdatePage of Page
    | DeletePage of Guid
    | ToggleSidebar
    | OpenOrSelectTab of Guid