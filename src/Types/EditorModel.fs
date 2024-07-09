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

type Model = {
    IsSidebarOpen: bool
    ActivePageId: Guid option
    Pages: Map<Guid, Page>
    TabOrder: Guid list
}

type Msg =
    | ToggleSidebar
    | SetActivePage of Guid
    | ClosePage of Guid
    | CreateNewPage
    | ReorderTabs of Guid list