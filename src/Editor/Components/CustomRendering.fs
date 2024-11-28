module Editor.Components.CustomRendering

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





let rec renderingCodeToReactElement
    (code: RenderingCode)
    (path: int list)
    (json: Json)
    (name: string)
    (options: (PageEditorMsg -> unit) -> RenderingCode -> list<int> -> string -> Page -> ReactElement)
    (dispatch: PageEditorMsg -> unit)
    (showOptions: bool)
    (page: Page)
    : ReactElement =

    let renderWithOptions (preview: ReactElement) =
        if showOptions then
            Html.div [
                prop.className "flex space-x-4 bg-white rounded-md m-1"
                prop.children [
                    Html.div [ prop.className "flex-grow"; prop.children [ preview ] ]
                    Html.div [
                        prop.className " w-64 p-2 border-l border-gray-200"
                        prop.children [ options dispatch code path name page ]
                    ]
                ]
            ]
        else
            preview

    let createPreview (tag: string) (attributes: obj) (children: ReactElement list) =
        try
            if tag.ToLower() = "input" then
                ReactBindings.React.createElement (tag, attributes, [])
            else
                ReactBindings.React.createElement (tag, attributes, children)
        with ex ->
            Html.div [ prop.className "error-message"; prop.text $"Unexpected error: {ex.Message}" ]

    let renderHtmlElement (tag: Tag) (attrs: Attributes) (innerValue: InnerValue) =
        let attributes =
            attrs
            |> List.map (fun (key, value) ->
                match value with
                | Data -> key, box (json |> Json.convertFromJsonAs<string>)
                | Constant s -> (key, box s)
                | InnerValue.Empty -> (key, box value))
            |> List.append [ ("className", box "preview") ]
            |> createObj

        let children =
            match innerValue with
            | Data ->
                try
                    let jsonStr = json |> Json.convertFromJsonAs<string>
                    [ Html.text jsonStr ]
                with ex -> [ Html.text $"Data parsing error: {ex.Message}" ]
            | InnerValue.Empty -> []
            | Constant value -> [ Html.text value ]

        createPreview (tagToString tag) attributes children |> renderWithOptions

    let renderHtmlList (listType: ListType) (codes: RenderingCode list) =
        match json with
        | JArray array ->
            let elements =
                codes
                |> List.mapi (fun index code ->
                    let arrayItem = List.item index array

                    let renderedItem: ReactElement =
                        if index = 0 then
                            renderingCodeToReactElement
                                code
                                (path @ [ index ])
                                arrayItem
                                name
                                options
                                dispatch
                                true
                                page
                        else
                            renderingCodeToReactElement
                                code
                                (path @ [ index ])
                                arrayItem
                                name
                                options
                                dispatch
                                false
                                page

                    Html.li [ prop.className "preview"; prop.children [ renderedItem ] ])

            let listTag = listTypeToString listType

            createPreview listTag (createObj [ "className" ==> "preview" ]) elements
            |> renderWithOptions
        | _ -> Html.div [ prop.text "Invalid JSON for HtmlList: not an array" ]

    let renderHtmlObject (keys: string list) (codes: Map<string, RenderingCode>) =
        match json with
        | JObject object ->
            let renderedElements =
                keys
                |> List.mapi (fun index key ->
                    let element = codes.TryFind key
                    let jsonValue = object.TryFind key

                    match element, jsonValue with
                    | Some code, Some value ->
                        renderingCodeToReactElement code (path @ [ index ]) value key options dispatch showOptions page
                    | None, Some(_) ->
                        Html.div [ prop.text ("RenderingCode element with the name " + key + " not found.") ]
                    | Some(_), None ->
                        Html.div [ prop.text ("JSON object value with the name " + key + " not found.") ]
                    | None, None ->
                        Html.div [
                            prop.text (
                                "JSON object value and RenderingCode element with the name "
                                + key
                                + " not found."
                            )
                        ])

            Html.div [ prop.className "preview"; prop.children renderedElements ]
            |> renderWithOptions
        | _ -> Html.div [ prop.text "Invalid JSON for Sequence: not an object" ]

    let renderHole (hole: FieldHole) =
        let holeName =
            match hole with
            | UnNamed -> "Unnamed"
            | Named n -> n

        let newElement = recognizeJson json

        Html.div [
            prop.className "flex items-center justify-between p-2 bg-gray-100 rounded"
            prop.children [
                Html.span [ prop.className "text-sm text-gray-600"; prop.text ("Hole: " + holeName) ]
                Html.button [
                    prop.className
                        "px-2 py-1 bg-blue-500 text-white text-sm rounded hover:bg-blue-600 focus:outline-none focus:ring-2 focus:ring-blue-500 focus:ring-opacity-50 flex items-center space-x-1"
                    prop.onClick (fun _ -> dispatch (ReplaceCode(newElement, path)))
                    prop.children [
                        ReactBindings.React.createElement (
                            replaceIcon,
                            createObj [ "size" ==> 16; "color" ==> "#FFFFFF" ],
                            []
                        )
                        Html.span [ prop.text "Replace" ]
                    ]
                ]
            ]
        ]

    match code with
    | HtmlElement(tag, attrs, innerValue, eventHandlers) -> renderHtmlElement tag attrs innerValue
    | HtmlList(listType, codes, eventHandlers) -> renderHtmlList listType codes
    | HtmlObject(objType, keys, codes, eventHandlers) -> renderHtmlObject keys codes
    | Hole named -> renderHole named



let renderElements (model: PageEditorModel) dispatch =
    let viewportTransform (position: Position) = {
        X = (position.X + model.ViewportPosition.X) * model.Scale
        Y = (position.Y + model.ViewportPosition.Y) * model.Scale
    }

    let renderElement element =
        let pos = viewportTransform element.Position
        let baseWidth, baseHeight = 120.0, 50.0
        let scaledWidth = baseWidth * model.Scale
        let scaledHeight = baseHeight * model.Scale

        Html.div [
            prop.className
                "absolute bg-blue-500 text-white rounded shadow-lg flex items-center justify-center draggable"
            prop.style [
                style.left (length.px pos.X)
                style.top (length.px pos.Y)
                style.transform [
                    transform.translateX (length.percent -50)
                    transform.translateY (length.percent -50)
                ]
                style.width (length.px scaledWidth)
                style.height (length.px scaledHeight)
                style.cursor "grabbing"
                style.fontSize (length.px (16.0 * model.Scale))
            ]
            prop.onMouseDown (fun e ->
                e.stopPropagation ()
                dispatch (StartDraggingItem(element.Id, { X = e.clientX; Y = e.clientY })))
            prop.children [ element.Content ]
        ]

    model.Elements |> List.map renderElement