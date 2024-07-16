module Editor.Types.PageEditorDomain

open CoreLogic.Operations
open CoreLogic.Types.RenderingTypes
open Editor.Types.EditorDomain
open Fable.SimpleJson

type RightPaneTab =
    | GeneratedCode
    | JavaScriptEditor
    | SandboxPreview

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
    | UpdateJavaScriptCode of string
    | ReplaceCode of RenderingCode * path: int list
    | WrapCode of path: int list * tag: string
    | AddHandler of string * Javascript
    | ModifyHandler of name: string * code: Javascript
    | RemoveHandler of name: string
    | ReplaceHole of path: int list
    | ReplaceElement of path: int list * replacementElement: RenderingCode * currentCode: RenderingCode
    | DeleteElement of int list
    | AddCustomElement of path: int list * customElement: CustomElement
    | ReorderObjectKeys of path: int list * newOrder: string list

