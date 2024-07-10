module Types.EditorDomain

open Fable.SimpleJson
open System
open RenderingTypes


type Page = {
    Name: string
    Id: Guid
    Data: Json
    Code: RenderingCode
    FileUploadError: bool
}

//Application state


type Tab = { Id: Guid; PageId: Guid }

type Model =
    { Pages: Map<Guid, Page>
      OpenTabs: Tab list
      ActiveTabId: Guid option
      IsSidebarOpen : bool }

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