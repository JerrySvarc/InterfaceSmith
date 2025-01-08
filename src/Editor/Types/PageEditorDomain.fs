module Editor.Types.PageEditorDomain

open CoreLogic.Types.RenderingTypes
open System
open Fable.SimpleJson
open Fable.React

/// <summary>Represents a message that can be sent to the Elm-style update function of a Page.</summary>
type UserMessage = string

/// <summary> Represents the Elm-style update function for the created Page, consisting of key-value pairs of messages and the update funcitonality. </summary>
type UpdateFunction = Map<UserMessage, string>


/// <summary>Represents an error relating to uploading a file.</summary>
type FileValidationError =
    | InvalidFileType
    | EmptyFile
    | ReadError of string
    | ParseError of string


/// <summary>The main unit of development for the editor. It represents a single web page following the Elm architecture.
/// It has the necessary components such as model, messages, update and view funtions.</summary>
type Page = {
    Name: string
    Id: Guid
    ParsedJson: Json
    CurrentTree: RenderingCode
    //We save the uploaded json string to then insert it into the resulting application as the model
    JsonString: string
    UserMessages: UserMessage list
    UpdateFunction: UpdateFunction
    CustomFunctions: Map<string, Javascript>
}

/// <summary>The context for rendering the preview of a RenderingCode element alongside its modification menu.</summary>
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

/// <summary>Represents x-axis and y-axis coordinates.</summary>
type Position = { X: float; Y: float }

/// <summary>A draggable element which can be placed onto the canvas.</summary>
type Element = {
    Id: int
    Position: Position
    //Needs to be a function instead of the resulting element, because otherwise the element is not updated when model changes
    Render: PageEditorModel -> (PageEditorMsg -> unit) -> ReactElement
}

/// <summary>The state of the PageEditor application.</summary>
and PageEditorModel = {
    PageData: Page
    FileUploadError: FileValidationError option
    ViewportPosition: Position
    Scale: float
    Elements: Element list
    DraggingElementId: int option
    IsPanning: bool
    LastMousePosition: Position option
    IsPreviewOpen: bool
    IsCodeViewOpen: bool
    ContextMenuVisible: bool
    ContextMenuPosition: Position option
}


and PageEditorMsg =
    | SyncWithMain of PageEditorModel
    | UploadData of Result<string, FileValidationError> * (PageEditorMsg -> unit)
    | SetFileUploadError of FileValidationError option
    | ReplaceCode of RenderingCode * path: int list
    | StartPanning of Position
    | UpdatePanning of Position
    | EndPanning
    | StartDraggingItem of int * Position
    | UpdateDraggingItem of Position
    | EndDraggingItem
    | Zoom of float
    | TogglePreview
    | ToggleCodeView
    | CreateFunction
    | UpdateFunction of string * Javascript
    | RenameFunction of string * string
    | DeleteFunction of string
    | AddMsg of string
    | UpdateMsg of string * string
    | RenameMsg of string * string
    | DeleteMsg of string
    | AddUpdateMessage of message: UserMessage * code: string
    | RenameUpdateMessage of message: UserMessage * code: string
    | ModifyUpdateMessage of message: UserMessage * code: string
    | DeleteUpdateMessage of string