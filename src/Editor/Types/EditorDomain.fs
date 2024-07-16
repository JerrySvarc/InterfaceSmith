module Editor.Types.EditorDomain

open Fable.SimpleJson
open System
open CoreLogic.Types.RenderingTypes

type Page = {
    Name: string
    Id: Guid
    ParsedJson: Json
    CurrentTree: RenderingCode
    JsonString: string
    CustomHandlers: Map<string, Javascript>
}
type Tab = { Id: Guid; PageId: Guid }

//Application state
type Model = {
    Pages: Map<Guid, Page>
    OpenTabs: Tab list
    ActiveTabId: Guid option
    IsSidebarOpen: bool
}
//Application operations
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