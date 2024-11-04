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
    | UploadData of string
    | ReplaceCode of RenderingCode * path: int list