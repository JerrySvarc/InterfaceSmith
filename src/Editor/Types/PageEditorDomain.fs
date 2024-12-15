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
    CustomHandlers: Map<string, Javascript>
}

/// <summary></summary>
type RenderContext<'Msg> = {
    Options: ('Msg -> unit) -> RenderingCode -> list<int> -> string -> Map<string, Javascript> -> ReactElement
    Dispatch: 'Msg -> unit
    Json: Json
    Path: int list
    Name: string
    CustomHandlers: Map<string, Javascript>
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
    RightClickMenuIndex: int option
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
    | OpenFieldView
    | AddMsg
    | DeleteMsg
    | AddUpdateFunction
    | RemoveUpdateFunction
    | UpdateMsgEvent of msg: UserMessage * code: string
    | OpenRightClickMenu of position: Position * (PageEditorMsg -> unit)
    | CloseRightClickMenu
    | CreateViewElement of (PageEditorMsg -> unit)