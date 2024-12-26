module Editor.Types.PageEditorDomain

open CoreLogic.Types.RenderingTypes
open System
open Fable.SimpleJson
open Fable.React

type UserMessage = string
type UpdateFunction = Map<UserMessage, string>


/// <summary></summary>
type Page = {
    Name: string
    Id: Guid
    ParsedJson: Json
    CurrentTree: RenderingCode
    JsonString: string
    UserMessages: UserMessage list
    UpdateFunction: UpdateFunction
    CustomFunctions: Map<string, Javascript>
}

/// <summary></summary>
type RenderContext<'Msg> = {
    Options:
        ('Msg -> unit)
            -> RenderingCode
            -> list<int>
            -> string
            -> Map<string, Javascript>
            -> UserMessage list
            -> ReactElement
    Dispatch: 'Msg -> unit
    Json: Json
    Path: int list
    Name: string
    CustomFunctions: Map<string, Javascript>
    ShowOptions: bool
    UserMessages: UserMessage list
}

/// <summary></summary>
type Position = { X: float; Y: float }

/// <summary></summary>
type Element = {
    Id: int
    Position: Position
    Render: PageEditorModel -> (PageEditorMsg -> unit) -> ReactElement
}

/// <summary></summary>
and PageEditorModel = {
    PageData: Page
    FileUploadError: bool
    ViewportPosition: Position
    Scale: float
    Elements: Element list
    DraggingElementId: int option
    IsPanning: bool
    LastMousePosition: Position option
    IsPreviewOpen: bool
    ContextMenuVisible: bool
    ContextMenuPosition: Position option
}


and PageEditorMsg =
    | SyncWithMain of PageEditorModel
    | UploadData of string * (PageEditorMsg -> unit)
    | ReplaceCode of RenderingCode * path: int list
    | StartPanning of Position
    | UpdatePanning of Position
    | EndPanning
    | StartDraggingItem of int * Position
    | UpdateDraggingItem of Position
    | EndDraggingItem
    | Zoom of float
    | TogglePreview
    | CreateFunction
    | UpdateFunction of string * Javascript
    | DeleteFunction of string
    | AddMsg of string
    | DeleteMsg of string
    | AddUpdateMessage of message: UserMessage * code: string
    | ModifyUpdateMessage of message: UserMessage * code: string
    | DeleteUpdateMessage of string