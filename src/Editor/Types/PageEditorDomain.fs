module Editor.Types.PageEditorDomain

open CoreLogic.Operations
open CoreLogic.Types.RenderingTypes
open Editor.Types.EditorDomain
open Fable.SimpleJson

type RightPaneTab =
    | JavaScriptEditor
    | SandboxPreview
    | CustomHandlerEditorTab

type PageEditorModel = {
    PageData: Page
    FileUploadError: bool
    ActiveRightTab: RightPaneTab
}

type PageEditorMsg =
    | SyncWithMain of Page
    | TogglePreview
    | UploadData of string
    | SetActiveRightTab of RightPaneTab
    | ReplaceCode of RenderingCode * path: int list
    | AddHandler of string * Javascript
    | RemoveHandler of name: string