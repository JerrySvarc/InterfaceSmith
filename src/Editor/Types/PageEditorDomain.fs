module Editor.Types.PageEditorDomain

open CoreLogic.Types.RenderingTypes
open System
open Fable.SimpleJson


//Represents a single created page
type Page = {
    Name: string
    Id: Guid
    ParsedJson: Json
    CurrentTree: RenderingCode
    JsonString: string
    CustomHandlers: Map<string, Javascript>
}


type Position = { X: float; Y: float }


type ElementType =
    | Message
    | View of RenderingCode
    | Model

type Element = {
    Id: int
    Position: Position
    Content: ElementType
}

type RightPaneTab =
    | JavaScriptEditor
    | SandboxPreview
    | CustomHandlerEditorTab

type PageEditorModel = {
    PageData: Page
    FileUploadError: bool
    ViewportPosition: Position
    Scale: float
    Elements: Element list
    DraggingElementId: int option
    IsPanning: bool
    LastMousePosition: Position option
    IsPreviewOpen: bool
}



type PageEditorMsg =
    | SyncWithMain of PageEditorModel
    | UploadData of string
    | ReplaceCode of RenderingCode * path: int list
    | StartPanning of Position
    | UpdatePanning of Position
    | EndPanning
    | StartDraggingItem of int * Position
    | UpdateDraggingItem of Position
    | EndDraggingItem
    | Zoom of float
    | AddItem of Position
    | TogglePreview