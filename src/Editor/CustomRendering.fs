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

let rec options (dispatch: PageEditorMsg -> unit) (code: RenderingCode) (path: int list) (name: string) : ReactElement =
    match code with
    | RenderingCode.HtmlElement _ -> Html.none //ElementOption dispatch name code path
    | RenderingCode.HtmlList _ -> Html.none
    //ListOption(dispatch, name, code, path)
    | RenderingCode.HtmlObject(_) -> Html.none
    //SequenceOption(dispatch, name, code, path)
    | RenderingCode.Hole _ -> Html.none

/// <summary></summary>
/// <param name="context"></param>
/// <param name="code"></param>
/// <returns></returns>
let rec renderingCodeToReactElement (context: RenderContext<PageEditorMsg>) (code: RenderingCode) : ReactElement =

    let renderWithOptions (preview: ReactElement) =
        Html.div [
            prop.children [ preview; options context.Dispatch code context.Path context.Name ]
        ]

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
            |> List.map (fun attr ->
                match attr.Value with
                | Data -> attr.Key, box (context.Json |> Json.convertFromJsonAs<string>)
                | Constant s -> (attr.Key, box s)
                | InnerValue.Empty -> (attr.Key, box attr.Value))
            |> List.append [ ("className", box "preview") ]
            |> createObj

        let children =
            match innerValue with
            | Data ->
                try
                    let jsonStr = context.Json |> Json.convertFromJsonAs<string>
                    [ Html.text jsonStr ]
                with ex -> [ Html.text $"Data parsing error: {ex.Message}" ]
            | InnerValue.Empty -> []
            | Constant value -> [ Html.text value ]

        createPreview (tag.Name) attributes children |> renderWithOptions

    let renderHtmlList (listType: ListType) (codes: RenderingCode list) =
        match context.Json with
        | JArray array ->
            let elements =
                codes
                |> List.mapi (fun index code ->
                    let arrayItem = List.item index array

                    let newContext = {
                        context with
                            Path = context.Path @ [ index ]
                            Json = arrayItem
                    }

                    let renderedItem = renderingCodeToReactElement newContext code

                    Html.li [ prop.className "preview"; prop.children [ renderedItem ] ])

            let listTag = listTypeToString listType

            createPreview listTag (createObj [ "className" ==> "preview" ]) elements
            |> renderWithOptions
        | _ -> Html.div [ prop.text "Invalid JSON for HtmlList: not an array" ]

    let renderHtmlObject (keys: string list) (codes: Map<string, RenderingCode>) =
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

            Html.div [ prop.className "preview"; prop.children renderedElements ]
            |> renderWithOptions
        | _ -> Html.div [ prop.text "Invalid JSON for Sequence: not an object" ]


    let renderHole (named: FieldHole) =
        let holeName =
            match named with
            | UnNamed -> "Unnamed"
            | Named name -> name

        let fieldType = recognizeJson context.Json

        Html.div [
            prop.className "flex items-center justify-between p-2 bg-gray-100 rounded"
            prop.children [
                Html.span [ prop.className "text-sm text-gray-600"; prop.text ("Hole: " + holeName) ]
                Html.button [
                    prop.className
                        "px-2 py-1 bg-blue-500 text-white text-sm rounded hover:bg-blue-600 focus:outline-none focus:ring-2 focus:ring-blue-500 focus:ring-opacity-50 flex items-center space-x-1"
                    prop.onClick (fun _ -> context.Dispatch(ReplaceCode(fieldType, context.Path)))
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
    | RenderingCode.HtmlElement(tag, attrs, innerValue, eventHandlers) -> renderHtmlElement tag attrs innerValue
    | RenderingCode.HtmlList(listType, attrs, codes, eventHandlers) -> renderHtmlList listType codes
    | RenderingCode.HtmlObject(objType, attrs, keys, codes, eventHandlers) -> renderHtmlObject keys codes
    | RenderingCode.Hole named -> renderHole named



/// <summary></summary>
/// <param name="model"></param>
/// <param name="dispatch"></param>
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
            prop.className "absolute flex items-center justify-center w-fit h-fit bg-blue-900 shadow-lg"
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
        ]

    Html.div [
        prop.className "relative w-full h-full"
        prop.children (model.Elements |> List.map renderElement)
    ]