module Editor.Types.EditorDomain

open Fable.SimpleJson
open System
open CoreLogic.Types.RenderingTypes
open PageEditorDomain


// Application state
type Model = {
    Pages: Map<Guid, PageEditorModel>
    PageOrder: Guid list
    ActivePageId: Guid option
    IsSidebarOpen: bool
}

// Application operations
// General operation such as tab management or opening new pages
type Msg =
    | CreatePage
    | UpdatePage of PageEditorModel
    | DeletePage of Guid
    | ToggleSidebar
    | OpenPage of Guid
    | PageEditorMsg of Guid * PageEditorMsg