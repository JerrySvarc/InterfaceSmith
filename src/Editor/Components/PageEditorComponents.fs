module Editor.UIComponents.PageEditorComponents

open Editor.Types.EditorDomain
open Editor.Types.PageEditorDomain
open Fable.React
open Feliz
open CoreLogic.Types.RenderingTypes
open Feliz.UseElmish
open Elmish
open Fable.Core.JsInterop
open Fable.SimpleJson
open Editor.Utilities.FileUpload
open Editor.Utilities.Icons
open Editor.Utilities.JsonParsing
open Editor.Components.CustomRendering
open CoreLogic.Operations.DataRecognition
open CoreLogic.Operations.RenderingCode
open Editor.Components.OptionComponents
open CoreLogic.Operations.CodeGeneration
open Editor.Utilities.JavaScriptEditor
open Browser.Types


// PageEditor elmish-style functionality
// This is the main component for the page editor
// It contains the left and right panes and the logic for updating the page data
// It also contains the logic for updating the main page data
// when the user makes changes to the page data

let pageEditorInit (existingModel: PageEditorModel) : PageEditorModel * Cmd<PageEditorMsg> = existingModel, Cmd.none




// This function is used to update the page editor model
// It is called when the user interacts with the page editor
// It updates the page editor model and sends a message to the main page via Cmd.ofMsg

let pageEditorUpdate (msg: PageEditorMsg) (model: PageEditorModel) : PageEditorModel * Cmd<PageEditorMsg> =
    match msg with
    | UploadData jsonString ->
        let loadedDataOption = loadJson jsonString

        match loadedDataOption with
        | Some(data) ->
            match data with
            | JObject obj ->
                let updatedPage = {
                    model.PageData with
                        CurrentTree = recognizeJson data
                        ParsedJson = data
                        JsonString = jsonString
                }

                let updatedEditorPage = {
                    model with
                        PageData = updatedPage
                        FileUploadError = false
                }

                updatedEditorPage, Cmd.ofMsg (SyncWithMain updatedEditorPage)
            | _ -> { model with FileUploadError = true }, Cmd.none
        | None -> { model with FileUploadError = true }, Cmd.none

    | ReplaceCode(code, path) ->
        let newCodes = replace path code model.PageData.CurrentTree

        let updatedPage = {
            model.PageData with
                CurrentTree = newCodes
        }

        let newModel = { model with PageData = updatedPage }
        newModel, Cmd.ofMsg (SyncWithMain newModel)

    | SyncWithMain _ -> model, Cmd.none
    | StartPanning pos when model.DraggingItemId.IsNone ->
        {
            model with
                IsPanning = true
                LastMousePosition = Some pos
        },
        Cmd.none

    | UpdatePanning pos when model.IsPanning ->
        match model.LastMousePosition with
        | Some lastPos ->
            let dx = (pos.X - lastPos.X) / model.Scale
            let dy = (pos.Y - lastPos.Y) / model.Scale

            let newViewport = {
                X = model.ViewportPosition.X + dx
                Y = model.ViewportPosition.Y + dy
            }

            {
                model with
                    ViewportPosition = newViewport
                    LastMousePosition = Some pos
            },
            Cmd.none
        | None -> model, Cmd.none

    | EndPanning ->
        {
            model with
                IsPanning = false
                LastMousePosition = None
        },
        Cmd.none

    | StartDraggingItem(itemId, pos) ->
        {
            model with
                DraggingItemId = Some itemId
                LastMousePosition = Some pos
        },
        Cmd.none

    | UpdateDraggingItem pos ->
        match model.DraggingItemId, model.LastMousePosition with
        | Some itemId, Some lastPos ->
            let dx = (pos.X - lastPos.X) / model.Scale
            let dy = (pos.Y - lastPos.Y) / model.Scale

            let updatedItems =
                model.Items
                |> List.map (fun item ->
                    if item.Id = itemId then
                        {
                            item with
                                Position = {
                                    X = item.Position.X + dx
                                    Y = item.Position.Y + dy
                                }
                        }

                    else
                        item)

            {
                model with
                    Items = updatedItems
                    LastMousePosition = Some pos
            },
            Cmd.none
        | _ -> model, Cmd.none

    | EndDraggingItem ->
        {
            model with
                DraggingItemId = None
                LastMousePosition = None
        },
        Cmd.none

    | Zoom scaleFactor ->
        {
            model with
                Scale = model.Scale * scaleFactor
        },
        Cmd.none

    | AddItem pos ->
        let newItem = {
            Id = model.Items.Length
            Position = { X = pos.X; Y = pos.Y }
            Content = View(Hole(UnNamed))
        }

        {
            model with
                Items = newItem :: model.Items
        },
        Cmd.none

    | TogglePreview ->
        {
            model with
                IsPreviewOpen = not model.IsPreviewOpen
        },
        Cmd.none




// These are the main components that make up the page editor
// They are used to render the page editor and handle user interactions
[<ReactComponent>]
let DataUpload (dispatch) =
    let uploadButtonView onLoad =
        Html.div [
            prop.className "inline-flex items-center"
            prop.children [
                Html.label [
                    prop.className
                        "w-full mt-4 bg-blue-500 hover:bg-blue-600 text-white font-bold py-2 px-4 rounded flex items-center justify-center"
                    prop.children [
                        ReactBindings.React.createElement (
                            uploadIcon,
                            createObj [ "size" ==> 16; "className" ==> "mr-2" ],
                            []
                        )
                        Html.span [ prop.className "font-medium"; prop.text "Upload" ]
                        Html.input [
                            prop.type' "file"
                            prop.className "hidden"
                            prop.onChange (handleFileEvent onLoad)
                        ]
                    ]
                ]
            ]
        ]

    let uploadButton = uploadButtonView (UploadData >> dispatch)

    Html.div [ prop.className "m-2"; prop.children [ uploadButton ] ]


let rec options
    (dispatch: PageEditorMsg -> unit)
    (code: RenderingCode)
    (path: int list)
    (name: string)
    (page: Page)
    : ReactElement =
    match code with
    | HtmlElement _ -> Html.none
    // ElementOption(dispatch, name, code, path, page)
    | HtmlList _ -> Html.none
    //ListOption(dispatch, name, code, path, page)
    | HtmlObject(_) -> Html.none
    //SequenceOption(dispatch, name, code, path, page)
    | Hole _ -> Html.none




[<ReactComponent>]
let JavaScriptEditorView code (dispatch: PageEditorMsg -> unit) =

    let extensions = [| javascript?javascript (); html?html (); css?css () |]

    //let onChange = fun value -> dispatch (UpdateHtmlCode value)

    Html.div [
        prop.className "flex flex-col h-full border-solid border-2 border-black overflow-auto"
        prop.children [
            Html.h3 [ prop.className "font-bold mb-2 px-2"; prop.text "Code preview" ]
            Html.div [
                prop.className "flex-grow overflow-auto"
                prop.children [
                    ReactBindings.React.createElement (
                        CodeMirror,
                        createObj [
                            "value" ==> code
                            "extensions" ==> extensions
                            //"onChange" ==> onChange
                            "theme" ==> "dark"
                            "readOnly" ==> "true"
                        ],
                        []
                    )
                ]
            ]
        ]
    ]






[<ReactComponent>]
let SandboxPreviewView (model: PageEditorModel) dispatch =
    let fullHtml, js =
        generateCode model.PageData.CurrentTree model.PageData.JsonString model.PageData.CustomHandlers

    Html.none

[<ReactComponent>]
let Canvas (model: PageEditorModel) (dispatch: PageEditorMsg -> unit) =

    let viewportTransform (position: Position) = {
        X = (position.X + model.ViewportPosition.X) * model.Scale + 400.0
        Y = (position.Y + model.ViewportPosition.Y) * model.Scale + 300.0
    }


    let renderItem item =
        let pos = viewportTransform item.Position

        Html.div [
            prop.className "absolute bg-blue-500 text-white p-2 rounded shadow-lg"
            prop.style [
                style.left (length.px pos.X)
                style.top (length.px pos.Y)
                style.transform [
                    transform.translateX (length.percent -50)
                    transform.translateY (length.percent -50)
                ]
            ]

            prop.onMouseDown (fun e ->
                e.stopPropagation ()
                dispatch (StartDraggingItem(item.Id, { X = e.clientX; Y = e.clientY })))

            prop.children [ Html.text "Menu item" ]
        ]

    Html.div [
        prop.className "relative overflow-hidden w-full h-full bg-gray-600"

        prop.onMouseDown (fun e ->
            if model.DraggingItemId.IsNone then
                dispatch (StartPanning { X = e.clientX; Y = e.clientY }))

        prop.onMouseMove (fun e ->
            match model.IsPanning, model.DraggingItemId with
            | true, _ -> dispatch (UpdatePanning { X = e.clientX; Y = e.clientY })
            | false, Some _ -> dispatch (UpdateDraggingItem { X = e.clientX; Y = e.clientY })
            | _ -> ())

        prop.onMouseUp (fun _ ->
            dispatch EndPanning
            dispatch EndDraggingItem)

        prop.onWheel (fun e ->
            let scaleFactor = if e.deltaY > 0.0 then 0.9 else 1.1
            dispatch (Zoom scaleFactor))
        prop.children [
            Html.div [
                prop.className "relative"
                prop.children (model.Items |> List.map renderItem)
            ]
        ]
    ]


[<ReactComponent>]
let PageEditorView (pageModel: PageEditorModel) (dispatch: PageEditorMsg -> unit) =
    Html.div [
        prop.className "flex-1 flex overflow-hidden"
        prop.children [ Canvas pageModel dispatch; SandboxPreviewView pageModel dispatch ]
    ]