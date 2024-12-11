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
open Editor.Components.ElementComponents


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
            | Named n -> n

        let fieldType = recognizeJson context.Json
        options context.Dispatch fieldType context.Path holeName

    let renderCustomWrapper (customWrapper: CustomWrapper) =
        let attributes =
            customWrapper.Attributes
            |> List.map (fun attr -> (attr.Key, box attr.Value))
            |> List.append [ ("className", box "preview custom-wrapper") ]
            |> createObj

        let wrappedContent = renderingCodeToReactElement context customWrapper.WrappedCode


        let children =
            wrappedContent
            :: (customWrapper.Children
                |> List.mapi (fun index child ->
                    let newContext = {
                        context with
                            Path = context.Path @ [ -1; index ]
                    }

                    renderingCodeToReactElement newContext child))

        createPreview (customWrapper.Tag.Name) attributes children |> renderWithOptions

    let renderCustomElement (customElement: CustomElement) =
        let attributes =
            customElement.Attributes
            |> List.map (fun attr -> (attr.Key, box attr.Value))
            |> List.append [ ("className", box "preview custom-element") ]
            |> createObj

        let children = [ Html.text customElement.CustomInnerValue ]

        createPreview (customElement.Tag.Name) attributes children |> renderWithOptions

    match code with
    | RenderingCode.HtmlElement(tag, attrs, innerValue, eventHandlers) -> renderHtmlElement tag attrs innerValue
    | RenderingCode.HtmlList(listType, attrs, codes, eventHandlers) -> renderHtmlList listType codes
    | RenderingCode.HtmlObject(objType, attrs, keys, codes, eventHandlers) -> renderHtmlObject keys codes
    | RenderingCode.Hole named -> renderHole named
    | RenderingCode.CustomWrapper customWrapper -> renderCustomWrapper customWrapper
    | RenderingCode.CustomElement customElement -> renderCustomElement customElement



/// <summary></summary>
/// <param name="model"></param>
/// <param name="dispatch"></param>
/// <returns></returns>
let renderCanvasElements (model: PageEditorModel) dispatch =
    let viewportTransform (position: Position) = {
        X = (position.X + model.ViewportPosition.X) * model.Scale
        Y = (position.Y + model.ViewportPosition.Y) * model.Scale
    }

    let renderElement element =
        let pos = viewportTransform element.Position


        Html.div [
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
                Html.div[prop.className "mt-4 ml-1 mr-1 mb-1"
                         prop.children [ element.Content ]]
            ]
        ]

    model.Elements |> List.map renderElement