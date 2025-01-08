module Editor.CustomRendering

open System.Text
open CoreLogic.Types.RenderingTypes
open Fable.SimpleJson
open CoreLogic.Operations.RenderingCode
open Fable.React
open Feliz
open Browser
open Fable.Core.JsInterop
open Editor.Types.PageEditorDomain
open CoreLogic.Operations.DataRecognition
open Editor.Utilities.Icons
open Editor.Types.EditorDomain
open Editor.Components.OptionsComponents

let rec options
    (dispatch: PageEditorMsg -> unit)
    (code: RenderingCode)
    (path: int list)
    (name: string)
    customFunctions
    userMessages
    : ReactElement =
    match code with
    | RenderingCode.HtmlElement _ -> ElementOption name code path customFunctions userMessages dispatch
    | RenderingCode.HtmlList _ -> ListOption name code path customFunctions userMessages dispatch
    | RenderingCode.HtmlObject(_) -> ObjectOption name code path customFunctions userMessages dispatch
    | RenderingCode.Hole _ -> Html.none

/// <summary>Rendering function implementing the increamental creation and live preview functionality.
/// Recursively renders the created UI elements, combining them with the corresponding data when needed.
/// Also renders the modificationMenus for the UI elements. </summary>
/// <param name="context">The rendering context for a particular RenderingCode.</param>
/// <param name="code">The UI element to render.</param>
/// <returns>A ReactElement which consists of the preview for the code and the corresponding modification menus.</returns>
let rec renderingCodeToReactElement (context: RenderContext<PageEditorMsg>) (code: RenderingCode) : ReactElement =

    let renderWithOptions (preview: ReactElement) =
        Html.div [
            prop.className
                "bg-white hover:bg-gray-50 border-l-2 border-transparent hover:border-indigo-400 transition-colors duration-150 p-2"
            prop.children [
                preview
                options context.Dispatch code context.Path context.Name context.CustomFunctions context.UserMessages
            ]
        ]

    let renderAttributes attributes =
        attributes
        |> List.map (fun attr ->
            match attr.Value with
            | Data ->
                attr.Key,
                box (
                    match context.Json with
                    | JBool value -> if value then "true" else "false"
                    | _ -> context.Json |> Json.convertFromJsonAs<string>
                )
            | Constant s -> (attr.Key, box s)
            | InnerValue.Empty -> (attr.Key, box attr.Value))
        |> List.append [ ("className", box "preview") ]
        |> createObj

    let createPreview (tag: string) (attributes: obj) (children: ReactElement list) =
        try
            if tag.ToLower() = "input" then
                ReactBindings.React.createElement (tag, attributes, [])
            else
                ReactBindings.React.createElement (tag, attributes, children)
        with ex ->
            Html.div [ prop.className "error-message"; prop.text $"Unexpected error: {ex.Message}" ]

    let renderHtmlElement (tag: Tag) (attributes: Attributes) (innerValue: InnerValue) =
        let attributes = renderAttributes attributes

        let children =
            match innerValue with
            | Data ->
                try
                    let jsonStr =
                        match context.Json with
                        | JBool value -> if value then "true" else "false"
                        | _ -> context.Json |> Json.convertFromJsonAs<string>

                    [ Html.text jsonStr ]
                with ex -> [ Html.text $"Data parsing error: {ex.Message}" ]
            | InnerValue.Empty -> []
            | Constant value -> [ Html.text value ]

        if context.ShowOptions then
            createPreview (tag.Name) attributes children |> renderWithOptions
        else
            createPreview (tag.Name) attributes children

    let renderHtmlList (listType: ListType) (attributes: Attributes) (codes: RenderingCode list) =
        match context.Json with
        | JArray array ->
            let elements =
                codes
                |> List.mapi (fun index code ->
                    let arrayItem = List.item index array
                    let showOptions = index = 0

                    let newContext = {
                        context with
                            Path = context.Path @ [ index ]
                            Json = arrayItem
                            Name = sprintf "List item"
                            ShowOptions = showOptions && context.ShowOptions
                    }

                    let renderedItem = renderingCodeToReactElement newContext code

                    Html.li [ prop.className "preview"; prop.children [ renderedItem ] ])

            let listTag = listTypeToString listType
            let attributes = renderAttributes attributes

            if context.ShowOptions then
                createPreview listTag attributes elements |> renderWithOptions
            else
                createPreview listTag attributes elements
        | _ -> Html.div [ prop.text "Invalid JSON for HtmlList: not an array" ]

    let renderHtmlObject
        (objType: ObjType)
        (keys: string list)
        (attributes: Attributes)
        (codes: Map<string, RenderingCode>)
        =
        match context.Json with
        | JObject object ->
            let renderedElements =
                keys
                |> List.mapi (fun index key ->
                    let element = codes.TryFind key
                    let jsonValue = object.TryFind key


                    match element, jsonValue with
                    | Some code, Some value ->
                        let newContext = {
                            context with
                                Path = context.Path @ [ index ]
                                Json = value
                                Name = key
                        }

                        renderingCodeToReactElement newContext code
                    | None, Some(_) ->
                        Html.div [ prop.text ("RenderingCode element with the name " + key + " not found.") ]
                    | Some(_), None ->
                        Html.div [ prop.text ("JSON object value with the name " + key + " not found.") ]
                    | None, None ->
                        Html.div [
                            prop.text (
                                "JSON object value and RenderingCode element  with the name "
                                + key
                                + " not found."
                            )
                        ])

            let attributes = renderAttributes attributes
            let tag = objTypeToString objType

            if context.ShowOptions then
                createPreview tag attributes renderedElements |> renderWithOptions
            else
                createPreview tag attributes renderedElements
        | _ -> Html.div [ prop.text "Invalid JSON for Sequence: not an object" ]


    let renderHole (named: FieldHole) =
        let holeName =
            match named with
            | UnNamed -> "Unnamed"
            | Named name -> name

        let fieldType = recognizeJson context.Json

        if context.ShowOptions then
            Html.div [
                prop.className "bg-gray-300 border border-black w-64 h-auto mt-4 p-3 rounded"
                prop.children [
                    Html.div [
                        prop.className "flex justify-between items-center"
                        prop.children [
                            Html.span [
                                prop.className "text-sm font-semibold text-black"
                                prop.text ("Hole: " + holeName)
                            ]
                            Html.button [
                                prop.className
                                    "flex items-center px-3 py-2 ml-4 bg-blue-500 text-white text-sm font-semibold rounded hover:bg-blue-600 focus:outline-none focus:ring-2 focus:ring-blue-500 focus:ring-opacity-50 transition-all duration-200"
                                prop.onClick (fun _ -> context.Dispatch(ReplaceCode(fieldType, context.Path)))
                                prop.children [
                                    ReactBindings.React.createElement (
                                        replaceIcon,
                                        createObj [ "size" ==> 16; "color" ==> "#FFFFFF" ],
                                        []
                                    )
                                    Html.span [ prop.className "ml-1"; prop.text "Replace" ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        else
            Html.none


    match code with
    | RenderingCode.HtmlElement(tag, attrs, innerValue, eventHandlers) -> renderHtmlElement tag attrs innerValue
    | RenderingCode.HtmlList(listType, attrs, codes, eventHandlers) -> renderHtmlList listType attrs codes
    | RenderingCode.HtmlObject(objType, attrs, keys, codes, eventHandlers) -> renderHtmlObject objType keys attrs codes
    | RenderingCode.Hole named -> renderHole named



/// <summary>Render all canvas elementets at the right coordinates, taking into account the position of the viewport which is affected by panning the canvas, and the scale.</summary>
/// <param name="model">The PageEditor's state.</param>
/// <param name="dispatch">PageEditor dispatch function of (PageEditorMsg -> unit).</param>
/// <returns></returns>
let renderCanvasElements (model: PageEditorModel) dispatch =
    let viewportTransform (position: Position) = {
        X = (position.X + model.ViewportPosition.X) * model.Scale
        Y = (position.Y + model.ViewportPosition.Y) * model.Scale
    }

    let renderElement (element: Element) =
        let pos = viewportTransform element.Position

        Html.div [
            prop.key (string element.Id)
            prop.className "absolute flex items-center justify-center w-fit h-fit bg-blue-900 shadow-lg cursor-grab"
            prop.style [
                style.left (length.px pos.X)
                style.top (length.px pos.Y)
                style.transform [
                    transform.translateX (length.percent -50)
                    transform.translateY (length.percent -50)
                    transform.scale model.Scale
                ]
            ]
            prop.onMouseDown (fun e ->
                e.stopPropagation ()
                dispatch (StartDraggingItem(element.Id, { X = e.clientX; Y = e.clientY })))
            prop.children [
                Html.div [
                    prop.className "mt-4 ml-1 mr-1 mb-1"
                    prop.children [ element.Render model dispatch ]
                ]
            ]
            prop.onWheel (fun event -> event.stopPropagation ())
        ]

    Html.div [
        prop.className "relative w-full h-full"
        prop.children (model.Elements |> List.map renderElement)
    ]