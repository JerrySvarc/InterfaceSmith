module Editor.Components.PageEditorComponents

open Editor.Types.EditorDomain
open Editor.Types.PageEditorDomain
open Fable.React
open Feliz
open CoreLogic.Types.RenderingTypes
open Elmish
open Fable.Core.JsInterop
open Fable.SimpleJson
open Editor.Utilities.FileUpload
open Editor.Utilities.Icons
open Editor.Utilities.JsonParsing
open Editor.CustomRendering
open CoreLogic.Operations.DataRecognition
open CoreLogic.Operations.RenderingCode
open CoreLogic.Operations.CodeGeneration
open Editor.Utilities.JavaScriptEditor
open Editor.Components.ElementComponents
open Microsoft.FSharp.Reflection
// PageEditor elmish-style functionality

let pageEditorInit () : PageEditorModel * Cmd<PageEditorMsg> =
    let newPage = {
        Name = "New page"
        Id = System.Guid.NewGuid()
        ParsedJson = JNull
        CurrentTree = RenderingCode.Hole(UnNamed)
        JsonString = ""
        CustomFunctions = Map([])
        UserMessages = []
        UpdateFunction = Map([])
    }


    let newPageEditorModel = {
        PageData = newPage
        FileUploadError = None
        ViewportPosition = { X = 0.0; Y = 0.0 }
        Scale = 1.0
        Elements = []
        DraggingElementId = None
        IsPanning = false
        LastMousePosition = None
        IsPreviewOpen = false
        ContextMenuPosition = None
        ContextMenuVisible = false
    }

    newPageEditorModel, Cmd.none



/// <summary>The main Elmish update function for the PageEditor application.</summary>
/// <param name="msg">A PageEditorMsg produced by the interactions with the user interface.</param>
/// <param name="model">A PageEditorModel which is to be changed according to the type of message.</param>
/// <returns>Returns a newly updatted PageEditor state as a PageEditorModel, and a Command of a certain message.</returns>
let pageEditorUpdate (msg: PageEditorMsg) (model: PageEditorModel) : PageEditorModel * Cmd<PageEditorMsg> =
    match msg with
    | UploadData(jsonString, dispatch) ->
        match jsonString with
        | Ok jsonString ->
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

                    let newModelElement = {
                        Id = model.Elements.Length + 1
                        Position = { X = 150.0; Y = 250.0 }
                        Render = fun model dispatch -> ModelElement model dispatch
                    }

                    let viewElement = {
                        Id = model.Elements.Length + 2
                        Position = { X = 1200.0; Y = 550.0 }
                        Render = fun model dispatch -> ViewElement model dispatch
                    }

                    let functionsElement = {
                        Id = model.Elements.Length + 3
                        Position = { X = 300.0; Y = 700.0 }
                        Render = fun model dispatch -> FunctionsElement model.PageData.CustomFunctions dispatch
                    }

                    let updateElement = {
                        Id = model.Elements.Length + 4
                        Position = { X = 1200.0; Y = 1000.0 }
                        Render =
                            fun model dispatch ->
                                MessageAndUpdateElement
                                    model.PageData.UserMessages
                                    model.PageData.UpdateFunction
                                    dispatch
                    }

                    let updatedEditorPage = {
                        model with
                            PageData = updatedPage
                            FileUploadError = None
                            Elements =
                                model.Elements
                                @ [ newModelElement; viewElement; functionsElement; updateElement ]
                    }

                    updatedEditorPage, Cmd.ofMsg (SyncWithMain updatedEditorPage)
                | _ ->
                    {
                        model with
                            FileUploadError =
                                Some(
                                    FileValidationError.ParseError(
                                        "Invalid JSON format. Please upload a valid JSON file containing a single top-level JSON object."
                                    )
                                )
                    },
                    Cmd.none
            | None ->
                {
                    model with
                        FileUploadError =
                            Some(
                                FileValidationError.ParseError(
                                    "Invalid JSON format. Please upload a valid JSON file containing a single top-level JSON object."
                                )
                            )
                },
                Cmd.none
        | Error error ->
            {
                model with
                    FileUploadError = Some(error)
            },
            Cmd.none
    | SetFileUploadError error -> { model with FileUploadError = error }, Cmd.none
    | ReplaceCode(code, path) ->
        let newCodesResult = replace path code model.PageData.CurrentTree

        match newCodesResult with
        | Ok newCodes ->
            let updatedPage = {
                model.PageData with
                    CurrentTree = newCodes
            }

            let newModel = { model with PageData = updatedPage }
            newModel, Cmd.ofMsg (SyncWithMain newModel)
        | Error _ -> model, Cmd.none
    | SyncWithMain _ -> model, Cmd.none
    | StartPanning pos when model.DraggingElementId.IsNone ->
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

            let newModel = {
                model with
                    ViewportPosition = newViewport
                    LastMousePosition = Some pos
            }

            newModel, Cmd.ofMsg (SyncWithMain newModel)
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
                DraggingElementId = Some itemId
                LastMousePosition = Some pos
        },
        Cmd.none

    | UpdateDraggingItem pos ->
        match model.DraggingElementId, model.LastMousePosition with
        | Some itemId, Some lastPos ->
            let dx = (pos.X - lastPos.X) / model.Scale
            let dy = (pos.Y - lastPos.Y) / model.Scale

            let updatedElements =
                model.Elements
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

            let newModel = {
                model with
                    Elements = updatedElements
                    LastMousePosition = Some pos
            }

            newModel, Cmd.ofMsg (SyncWithMain newModel)
        | _ -> model, Cmd.none
    | EndDraggingItem ->
        {
            model with
                DraggingElementId = None
                LastMousePosition = None
        },
        Cmd.none

    | Zoom scaleFactor ->
        let newModel = {
            model with
                Scale = model.Scale * scaleFactor
        }

        newModel, Cmd.ofMsg (SyncWithMain newModel)

    | TogglePreview ->
        let newModel = {
            model with
                IsPreviewOpen = not model.IsPreviewOpen
        }

        newModel, Cmd.ofMsg (SyncWithMain newModel)

    | CreateFunction ->
        let newName = $"newFunction{(model.PageData.CustomFunctions.Count)}"
        let newFunction = JSFunction(newName, "console.log();")
        let newFunctions = model.PageData.CustomFunctions.Add(newName, newFunction)

        {
            model with
                PageData.CustomFunctions = newFunctions
        },
        Cmd.none
    | UpdateFunction(name, code) ->
        let existingFunction =
            match Map.tryFind name model.PageData.CustomFunctions with
            | Some(JSFunction(_, _)) -> true
            | _ -> false

        let newFunction = code

        let newFunctions =
            if existingFunction then
                model.PageData.CustomFunctions |> Map.remove name |> Map.add name newFunction
            else
                model.PageData.CustomFunctions.Add(name, newFunction)

        {
            model with
                PageData = {
                    model.PageData with
                        CustomFunctions = newFunctions
                }
        },
        Cmd.none
    | DeleteFunction(name) ->
        let newFunctions = model.PageData.CustomFunctions.Remove name

        {
            model with
                PageData.CustomFunctions = newFunctions
        },
        Cmd.none

    | AddMsg message ->
        let messageFoundCount =
            model.PageData.UserMessages
            |> List.filter (fun element -> element = message)
            |> List.length

        let defaultUpdateFunction = sprintf "return { ...model };"

        if messageFoundCount = 0 then
            let newMessages = model.PageData.UserMessages @ [ message ]

            {
                model with
                    PageData = {
                        model.PageData with
                            UserMessages = newMessages
                    }
            },
            Cmd.none
        else
            model, Cmd.ofMsg (AddUpdateMessage(message, defaultUpdateFunction))
    | UpdateMsg(message, code) ->
        let newMessages =
            model.PageData.UserMessages
            |> List.map (fun element -> if element = message then code else element)

        {
            model with
                PageData = {
                    model.PageData with
                        UserMessages = newMessages
                }
        },
        Cmd.none
    | DeleteMsg message ->
        let newMessages =
            model.PageData.UserMessages |> List.filter (fun element -> element <> message)

        {
            model with
                PageData = {
                    model.PageData with
                        UserMessages = newMessages
                }
        },
        Cmd.ofMsg (DeleteUpdateMessage(message))

    | AddUpdateMessage(message, code)
    | ModifyUpdateMessage(message, code) ->
        let newUpdate = model.PageData.UpdateFunction.Add(message, code)

        {
            model with
                PageData.UpdateFunction = newUpdate
        },
        Cmd.none
    | DeleteUpdateMessage message ->
        let newUpdate = model.PageData.UpdateFunction.Remove message

        {
            model with
                PageData.UpdateFunction = newUpdate
        },
        Cmd.none
    | RenameFunction(currentName, newName) ->
        let functionJavascrips = model.PageData.CustomFunctions.[currentName]

        match functionJavascrips with
        | JSFunction(_, functionCode) ->
            let newFunction = JSFunction(newName, functionCode)

            let newFunctions =
                model.PageData.CustomFunctions.Remove currentName |> Map.add newName newFunction

            {
                model with
                    PageData.CustomFunctions = newFunctions
            },
            Cmd.none
    | RenameMsg(message, newName) ->
        let newMessages =
            model.PageData.UserMessages
            |> List.map (fun element -> if element = message then newName else element)

        {
            model with
                PageData = {
                    model.PageData with
                        UserMessages = newMessages
                }
        },
        Cmd.ofMsg (RenameUpdateMessage(message, newName))
    | RenameUpdateMessage(message, newName) ->
        let functionCode = model.PageData.UpdateFunction.[message]

        let newUpdate =
            model.PageData.UpdateFunction.Remove message |> Map.add newName functionCode

        {
            model with
                PageData = {
                    model.PageData with
                        UpdateFunction = newUpdate
                }
        },
        Cmd.none
    | StartPanning(_) -> failwith "Panning already started."
    | UpdatePanning(_) -> failwith "Not panning the canvas."





//  These are the main components that make up the page editor.
//  They are used to render the page editor and handle user interactions.
//||---------------------------------------------------------------------------||
let DataUpload model dispatch =


    let uploadButtonView onLoad =
        Html.div [
            prop.className "inline-flex items-center"
            prop.children [
                Html.label [
                    prop.className
                        "bg-blue-500 hover:bg-blue-600 text-white font-bold py-2 px-4 rounded shadow flex items-center justify-center h-10"
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

    let uploadButton =
        uploadButtonView (fun jsonString -> dispatch (UploadData(jsonString, dispatch)))

    Html.div [ prop.className ""; prop.children [ uploadButton ] ]

let toolBarElements model dispatch = [
    DataUpload model dispatch
    Html.button [
        prop.children [
            ReactBindings.React.createElement (downloadIcon, createObj [ "size" ==> 16; "className" ==> "mr-2" ], [])
            Html.span [ prop.text "Save" ]
        ]
        prop.className
            "bg-blue-500 hover:bg-blue-600 text-white font-bold h-10 px-4 rounded shadow flex items-center justify-center"
    ]
    Html.button [
        prop.className
            "bg-green-500 hover:bg-green-600 text-white font-bold h-10 px-4 rounded shadow flex items-center justify-center"
        prop.text "Show source code"
    ]
]

let errorToMessage =
    function
    | InvalidFileType -> "Please upload a valid JSON file"
    | EmptyFile -> "Please select a file"
    | ReadError msg
    | ParseError msg -> msg


let ErrorMessage error dispatch =
    Html.div [
        prop.className "mt-2 bg-red-100 border border-red-400 text-red-700 px-4 py-3 rounded relative"
        prop.children [
            Html.span [ prop.className "block sm:inline"; prop.text (errorToMessage error) ]
            Html.span [
                prop.className "absolute top-0 bottom-0 right-0 px-4 py-3 cursor-pointer"
                prop.onClick (fun _ -> dispatch (SetFileUploadError None))
                prop.children [ Html.span [ prop.className "text-red-500 font-bold"; prop.text "×" ] ]
            ]
        ]
    ]

let ToolBar model dispatch =
    Html.div [
        prop.className
            "flex items-center justify-between bg-gray-500 min-w-fit text-white h-fit px-4 py-2 fixed space-x-4 top-2 left-1/2 transform -translate-x-1/2 z-10 shadow-md border border-gray-700 rounded"
        prop.children [
            Html.nav [
                prop.className "flex space-x-2 items-center h-full"
                prop.children (toolBarElements model dispatch)
            ]
            match model.FileUploadError with
            | Some error -> ErrorMessage error dispatch
            | None -> Html.none
        ]
    ]

/// <summary></summary>
/// <param name="model"></param>
/// <param name="dispatch"></param>
/// <returns></returns>
[<ReactComponent>]
let Canvas (model: PageEditorModel) (dispatch: PageEditorMsg -> unit) =
    let bgPositionX = sprintf "%fpx" (model.ViewportPosition.X * model.Scale)
    let bgPositionY = sprintf "%fpx" (model.ViewportPosition.Y * model.Scale)

    Html.div [
        prop.className "relative overflow-hidden w-full h-full bg-gray-600 "
        //We create the canvas dot pattern using pure CSS
        prop.style [
            style.backgroundImage "radial-gradient(circle, #1F2937 1px, transparent 1px)"
            style.backgroundSize "20px 20px"
            style.backgroundPosition $"{bgPositionX} {bgPositionY}"
        ]

        prop.onMouseDown (fun event ->
            if model.DraggingElementId.IsNone && event.button = 0 then
                dispatch (StartPanning { X = event.clientX; Y = event.clientY }))

        prop.onMouseMove (fun event ->
            match model.IsPanning, model.DraggingElementId with
            | true, _ -> dispatch (UpdatePanning { X = event.clientX; Y = event.clientY })
            | false, Some _ -> dispatch (UpdateDraggingItem { X = event.clientX; Y = event.clientY })
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
                prop.children [
                    renderCanvasElements model dispatch

                ]

            ]
        ]
    ]

/// <summary>The main Elmish 'view' function of the PageEditor application. </summary>
/// <param name="pageModel">The current model of the PageEditor.</param>
/// <param name="dispatch">Elmish 'dispatch' function of type PageEditorMsg -> unit. </param>
/// <returns></returns>
[<ReactComponent>]
let PageEditorView (pageModel: PageEditorModel) (dispatch: PageEditorMsg -> unit) : ReactElement =
    Html.div [
        prop.className "relative h-full w-full flex"
        prop.children [ ToolBar pageModel dispatch; Canvas pageModel dispatch ]
    ]