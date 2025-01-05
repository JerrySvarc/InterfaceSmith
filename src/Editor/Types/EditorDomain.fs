module Editor.Types.EditorDomain

open Fable.SimpleJson
open System
open CoreLogic.Types.RenderingTypes
open PageEditorDomain


/// <summary> The model of the main Elmish application. </summary>
type Model = {
    Pages: Map<Guid, PageEditorModel>
    PageOrder: Guid list
    ActivePageId: Guid option
    IsSidebarOpen: bool
}

/// <summary> The messages that can be sent to main application's update function. </summary>
type Msg =
    | CreatePage
    | UpdatePage of PageEditorModel
    | DeletePage of Guid
    | ToggleSidebar
    | OpenPage of Guid
    | PageEditorMsg of Guid * PageEditorMsg