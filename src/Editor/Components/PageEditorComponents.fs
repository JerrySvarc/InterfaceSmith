module Editor.UIComponents.PageEditorComponents

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
        CustomHandlers = Map([])
        UserMessages = []
        UpdateFunction = Map([])
    }


    let newPageEditorModel = {
        PageData = newPage
        FileUploadError = false
        ViewportPosition = { X = 0.0; Y = 0.0 }
        Scale = 1.0
        Elements = []
        DraggingElementId = None
        IsPanning = false
        LastMousePosition = None
        IsPreviewOpen = false
        RightClickMenuIndex = None
    }

    newPageEditorModel, Cmd.none



[<ReactComponent>]
let InnerValueMenu (currentInnerValue: InnerValue) (code: RenderingCode) path =
    let innerValueOptions = [ "Data"; "Constant"; "Empty" ]

    let constantValue, setConstantValue =
        React.useState (
            match currentInnerValue with
            | Constant str -> str
            | _ -> ""
        )

    let updateInnerValue newValue =
        match code with
        | RenderingCode.HtmlElement(tag, attrs, _, handlers) ->
            //dispatch (ReplaceCode(RenderingCode.HtmlElement(tag, attrs, newValue, handlers), path))
            ()
        | _ -> ()

    Html.span [
        prop.children [
            Html.span [
                prop.children [
                    SelectMenu innerValueOptions (currentInnerValue |> innerValueToString) (fun selectedValue ->
                        let newValue =
                            match selectedValue with
                            | "Data" -> InnerValue.Data
                            | "Constant" -> Constant constantValue
                            | "Empty" -> InnerValue.Empty
                            | _ -> currentInnerValue

                        updateInnerValue newValue)

                ]
            ]

            if innerValueToString currentInnerValue = "Constant" then
                Html.span [
                    prop.children [
                        Html.input [
                            prop.type' "text"
                            prop.value constantValue
                            prop.onChange (fun (value: string) ->
                                setConstantValue value
                                updateInnerValue (Constant value))
                            prop.className "text-xs overflow-auto bg-white border border-black shadow-sm "
                        ]
                    ]
                ]
            else
                Html.none
        ]
    ]


[<ReactComponent>]
let TagMenu (code: RenderingCode) path dispatch =
    let tagOptions = [
        Tags.p.Name
        Tags.h1.Name
        Tags.h2.Name
        Tags.h3.Name
        Tags.h4.Name
        Tags.h5.Name
        Tags.h6.Name
        Tags.strong.Name
        Tags.em.Name
        Tags.a.Name
        Tags.pre.Name
        Tags.code.Name
        Tags.blockquote.Name
        Tags.div.Name
        Tags.span.Name
        Tags.article.Name
        Tags.section.Name
        Tags.header.Name
        Tags.footer.Name
        Tags.nav.Name
        Tags.input.Name
        Tags.li.Name
        Tags.ol.Name
        Tags.ul.Name
        Tags.button.Name
        Tags.label.Name
    ]

    match code with
    | RenderingCode.HtmlElement(tag, attrs, value, handlers) ->
        let changeTag selectedTag =
            dispatch (ReplaceCode(RenderingCode.HtmlElement(stringToTag selectedTag, attrs, value, handlers), path))

        SelectMenu tagOptions tag.Name changeTag
    | _ -> ErrorDisplay "Invalid code type for TagMenu"




[<ReactComponent>]
let AttributeMenu (code: RenderingCode) path (attributes: Attribute list) =
    let selectedKey, setSelectedKey = React.useState ""
    let selectedInnerValue, setSelectedInnerValue = React.useState InnerValue.Empty
    let attributesOpen, setOpenAttributes = React.useState false

    let toggleAttributes () = setOpenAttributes (not attributesOpen)

    let handleSelectChange key =
        match List.tryFind (fun attr -> attr.Key = key) attributes with
        | Some attr ->
            setSelectedKey key
            setSelectedInnerValue attr.Value
        | None -> ()

    let handleAddClick () = ()

    Html.div [
        prop.className "space-y-4 "
        prop.children [
            Html.button [
                prop.className "flex flex-row items-center space-x-2"
                prop.onClick (fun e ->
                    e.stopPropagation () // Prevent toggle conflict
                    toggleAttributes ())

                prop.children [
                    ReactBindings.React.createElement (
                        (if attributesOpen then chevronDown else chevronRight),
                        createObj [ "size" ==> 16; "color" ==> "#000000" ],
                        []
                    )
                    Html.span [ prop.text ("Attributes"); prop.className "text-xs px-1 py-1" ]
                ]
            ]

            if attributesOpen then
                Html.div [
                    prop.children [
                        if attributes.Length = 0 then
                            Html.span [ prop.text "No attributes " ]
                        else
                            SelectMenu (attributes |> List.map (fun attr -> attr.Key)) selectedKey handleSelectChange

                            if selectedKey <> "" then
                                InnerValueMenu selectedInnerValue code path

                                Html.div [
                                    Html.button [
                                        prop.text "Add New Attribute"
                                        prop.className "bg-blue-500 text-white text-xs m-1 px-2 py-1 rounded"
                                        prop.onClick (fun _ -> handleAddClick ())
                                    ]
                                ]
                            else
                                setSelectedKey attributes.Head.Key
                                setSelectedInnerValue attributes.Head.Value
                    ]
                ]
        ]
    ]



let sampleElement =
    RenderingCode.HtmlElement(
        Tags.div, // Use the div tag
        [
            {
                Key = "class"
                Value = Constant "container"
                Namespace = None
            } // class="container"
            {
                Key = "dataasdfoasdfh;uojasdhfjiasdhfjklasdhfjkasdhflk-id"
                Value = Constant "123s45"
                Namespace = None
            } // data-id="12345"
        ],
        Constant "hello",
        []
    )

let sampleList =
    RenderingCode.HtmlList(
        ListType.UnorderedList,
        [
            {
                Key = "class"
                Value = Constant "container"
                Namespace = None
            } // class="container"
            {
                Key = "data-id"
                Value = Constant "123s45"
                Namespace = None
            } // data-id="12345"
        ],
        [ sampleElement ],
        []
    )


let EventHandlerMenu
    (dispatch: PageEditorMsg -> unit, code: RenderingCode, path: int list, customHandlers: Map<string, Javascript>)
    =
    let availableEvents = [ "onClick"; "onMouseOver"; "onBlur"; "onMouseDown"; "onMouseUp" ]



    let (selectedEvent, setSelectedEvent) = React.useState ""
    let (selectedHandler, setSelectedHandler) = React.useState ""
    let (errorMessage, setErrorMessage) = React.useState ""

    Html.div [

    ]




[<ReactComponent>]
let ListOption (name: string) code path =

    let handleChange2 newValue = printfn "Selected:"
    let handleChange3 () = ()

    match code with
    | RenderingCode.HtmlList(listType, attrs, elementCode, handlers) ->
        let listTypeOptions = [ "Unordered"; "Ordered" ]

        Html.div [
            prop.className "bg-gray-300   border border-black w-fit h-fit mt-4"
            prop.children [
                Html.p [ prop.text (name + ":"); prop.className "text-xs font-semibold" ]
                match code with
                | RenderingCode.HtmlList(listType, attrs, itemCodes, handlers) ->
                    Html.div [
                        prop.children [
                            SelectMenu listTypeOptions (listTypeToString listType) handleChange2
                            (AttributeMenu code path attrs)

                        ]
                    ]
                | _ -> Html.none

            ]
        ]
    | _ -> ErrorDisplay "Invalid code type for ListOption"

[<ReactComponent>]
let ElementOption (name: string) code path dispatch =
    let handleChange2 newValue = printfn "Selected:"
    let handleChange3 () = ()

    Html.div [
        prop.onMouseDown (fun e -> e.stopPropagation ())
        prop.className "bg-white  p-4 border border-gray-300 shadow-md"
        prop.style [
            style.resize.both
            style.overflow.auto
            style.whitespace.normal
            style.overflowWrap.normal
            style.wordWrap.normal
            style.width 800
            style.height 600
            style.minWidth (length.px 200)
            style.minHeight (length.px 200)
            style.maxWidth (length.percent 100)
            style.maxHeight (length.percent 100)
        ]
        prop.children [
            Html.div [
                prop.className "bg-gray-300  border border-black w-fit h-fit mt-4"
                prop.children [
                    Html.p [ prop.text (name + ":"); prop.className "text-xs font-semibold" ]
                    match code with
                    | RenderingCode.HtmlElement(_, attrs, innerValue, _) ->
                        Html.div [
                            prop.children [
                                (TagMenu code path dispatch)
                                InnerValueMenu innerValue code path
                                (AttributeMenu code path attrs)
                            ]
                        ]
                    | _ -> Html.none
                ]
            ]
        ]
    ]


/// <summary>This function is used to update the page editor model.
///It is called when the user interacts with the page editor.
///It updates the page editor model and sends a message to the main page via Cmd.ofMsg.</summary>
/// <param name="msg">A PageEditorMsg produced by the interactions with the user interface.</param>
/// <param name="model">A PageEditorModel which is to be changed according to the type of message.</param>
/// <returns></returns>
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

                let newModelElement = {
                    Id = model.Elements.Length + 1
                    Position = { X = 400.0; Y = 350.0 }
                    Content = ModelElement data
                }



                let option = {
                    Id = model.Elements.Length + 2
                    Position = { X = 400.0; Y = 350.0 }
                    Content = ListOption "List" sampleList []
                }

                let updatedEditorPage = {
                    model with
                        PageData = updatedPage
                        FileUploadError = false
                        Elements = model.Elements @ [ newModelElement; option ]
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

            let updatedItems =
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
                    Elements = updatedItems
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
    | OpenFieldView -> failwith "Not Implemented"
    | StartPanning(_)
    | UpdatePanning(_) -> failwith "Not Implemented"
    | AddMsg -> failwith "Not Implemented"
    | DeletMsg -> failwith "Not Implemented"
    | OpenRightClickMenu(position, dispatch) ->
        let elementPosition = {
            X = (model.ViewportPosition.X) / model.Scale
            Y = (model.ViewportPosition.Y) / model.Scale
        }

        let newMenuElement = {
            Id = model.Elements.Length + 1
            Position = position
            Content = (RightClickMenu dispatch)
        }

        let newElements = model.Elements @ [ newMenuElement ]

        {
            model with
                Elements = newElements
                LastMousePosition = Some(position)
                RightClickMenuIndex = Some(newMenuElement.Id)
        },
        Cmd.none
    | AddUpdateFunction -> failwith "Not Implemented"
    | RemoveUpdateFunction -> failwith "Not Implemented"
    | UpdateMsgEvent(msg, code) -> failwith "Not Implemented"
    | CloseRightClickMenu ->
        let newElements =
            match model.RightClickMenuIndex with
            | Some id -> model.Elements |> List.filter (fun item -> id <> item.Id)
            | None -> model.Elements

        {
            model with
                Elements = newElements
                RightClickMenuIndex = None
        },
        Cmd.none




[<ReactComponent>]
let JavaScriptEditorView code (dispatch) =

    let extensions = [| javascript?javascript (); html?html (); css?css () |]

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
                            "theme" ==> "dark"
                            "readOnly" ==> "true"
                        ],
                        []
                    )
                ]
            ]
        ]
    ]

// These are the main components that make up the page editor
// They are used to render the page editor and handle user interactions
let DataUpload dispatch =
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

    let uploadButton = uploadButtonView (UploadData >> dispatch)

    Html.div [ prop.className ""; prop.children [ uploadButton ] ]

let toolBarElements dispatch = [
    DataUpload dispatch
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
    Html.button [
        prop.className
            "bg-yellow-500 hover:bg-yellow-600 text-white font-bold h-10 px-4 rounded shadow flex items-center justify-center"
        prop.text "Preview Page"
        prop.onClick (fun _ -> dispatch TogglePreview)
    ]
]

let ToolBar dispatch =
    Html.div [
        prop.className
            "flex items-center justify-between bg-gray-500 min-w-fit text-white h-fit px-4 py-2 fixed space-x-4 top-2 left-1/2 transform -translate-x-1/2 z-10 shadow-md border border-gray-700 rounded"
        prop.children [
            Html.nav [
                prop.className "flex space-x-2 items-center h-full"
                prop.children (toolBarElements dispatch)
            ]
        ]
    ]


[<ReactComponent>]
let SandboxPreviewView (model: PageEditorModel) dispatch =
    let js =
        """
        const Msg = { IncrementCounter: "IncrementCounter", ToggleVisibility: "ToggleVisibility" };

        const model = { counter: 0, isVisible: true };

        const update = (msg, model) => {
            switch (msg) {
                case Msg.IncrementCounter:
                    return { ...model, counter: model.counter + 1 };
                case Msg.ToggleVisibility:
                    return { ...model, isVisible: !model.isVisible };
                default:
                    return model;
            }
        };

        const view = (model, dispatch) => `
    <div>
        <div>Counter: ${model.counter}</div>
        <button class"bg-gray-500 onclick="window.dispatch(Msg.IncrementCounter)">Increment</button>
        <button onclick="window.dispatch(Msg.ToggleVisibility)">Toggle</button>
        <div style="display: ${model.isVisible ? 'block' : 'none'}">Hello World</div>
    </div>
`;
        function startApp(initialModel, updateFn, viewFn) {
            let currentModel = initialModel;

            const render = () => {
                const root = document.getElementById("app");
                root.innerHTML = viewFn(currentModel, dispatch);
            };

           window.dispatch = (msg) => {
                currentModel = updateFn(msg, currentModel);
                render();
            };

            render();
        }

        startApp(model, update, view);
        """

    let fullHtml =
        $"""
        <!DOCTYPE html>
        <html lang="en">
        <head>
            <meta charset="UTF-8">
            <meta name="viewport" content="width=device-width, initial-scale=1.0">
            <title>Sandbox Preview</title>
        </head>
        <body>
            <div id="app"></div>
            <script>
            {js}
            </script>
        </body>
        </html>
        """

    if model.IsPreviewOpen then
        Html.div [
            prop.className "bg-white"
            prop.children [
                Html.iframe [
                    prop.src "about:blank"
                    prop.custom ("sandbox", "allow-scripts allow-same-origin allow-forms allow-modals")
                    prop.custom ("srcDoc", fullHtml)
                ]
            ]
        ]
    else
        Html.none


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
        //Create the canvas dot pattern using pure CSS
        prop.style [
            style.backgroundImage "radial-gradient(circle, #1F2937 1px, transparent 1px)"
            style.backgroundSize "20px 20px"
            style.backgroundPosition $"{bgPositionX} {bgPositionY}"
        ]

        prop.onMouseDown (fun event ->
            if model.DraggingElementId.IsNone && event.button = 0 then
                dispatch CloseRightClickMenu
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
                prop.children (renderCanvasElements model dispatch)

            ]

        ]
    (*
        prop.onContextMenu (fun (e: Browser.Types.MouseEvent) ->
            e.preventDefault ()
            let target = e.currentTarget :?> Browser.Types.HTMLElement
            let canvasBounds = target.getBoundingClientRect ()

            let mouseX =
                (e.clientX - canvasBounds.left) / model.Scale + model.ViewportPosition.X

            let mouseY = (e.clientY - canvasBounds.top) / model.Scale + model.ViewportPosition.Y


            if model.RightClickMenuIndex = None then
                dispatch (OpenRightClickMenu({ X = mouseX; Y = mouseY }, dispatch)))*)
    ]

/// <summary></summary>
/// <param name="pageModel"></param>
/// <param name="dispatch"></param>
/// <returns></returns>
[<ReactComponent>]
let PageEditorView (pageModel: PageEditorModel) (dispatch: PageEditorMsg -> unit) : ReactElement =
    Html.div [
        prop.className "relative h-full w-full flex"
        prop.children [
            ToolBar dispatch
            Canvas pageModel dispatch
        //SandboxPreviewView pageModel dispatch
        ]
    ]