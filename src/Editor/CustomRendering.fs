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


let rec renderingCodeToReactElement
    (code: RenderingCode)
    (path: int list)
    (json: Json)
    (name: string)
    (options: (PageEditorMsg -> unit) -> RenderingCode -> list<int> -> string -> ReactElement)
    (dispatch: PageEditorMsg -> unit)
    : ReactElement =

    let renderWithOptions (preview: ReactElement) =
        Html.div [ prop.children [ preview; options dispatch code path name ] ]

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

                    let renderedItem =
                        renderingCodeToReactElement code (path @ [ index ]) arrayItem name options dispatch

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
                        renderingCodeToReactElement code (path @ [ index ]) value key options dispatch
                    //TODO: styling
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

        let fieldType = recognizeJson json
        options dispatch fieldType path holeName

    let renderCustomWrapper (customWrapper: CustomWrapper) =
        let attributes =
            customWrapper.Attributes
            |> List.map (fun (key, value) -> (key, box value))
            |> List.append [ ("className", box "preview custom-wrapper") ]
            |> createObj

        let wrappedContent =
            renderingCodeToReactElement customWrapper.WrappedCode (path) json name options dispatch

        let children =
            wrappedContent
            :: (customWrapper.Children
                |> List.mapi (fun index child ->
                    renderingCodeToReactElement child (path @ [ -1; index ]) json name options dispatch))

        createPreview (tagToString customWrapper.Tag) attributes children
        |> renderWithOptions

    let renderCustomElement (customElement: CustomElement) =
        let attributes =
            customElement.Attributes
            |> List.map (fun (key, value) -> (key, box value))
            |> List.append [ ("className", box "preview custom-element") ]
            |> createObj

        let children = [ Html.text customElement.CustomInnerValue ]

        createPreview (tagToString customElement.Tag) attributes children
        |> renderWithOptions

    match code with
    | HtmlElement(tag, attrs, innerValue, eventHandlers) -> renderHtmlElement tag attrs innerValue
    | HtmlList(listType, codes, eventHandlers) -> renderHtmlList listType codes
    | HtmlObject(objType, keys, codes, eventHandlers) -> renderHtmlObject keys codes
    | Hole named -> renderHole named
    | CustomWrapper customWrapper -> renderCustomWrapper customWrapper
    | CustomElement customElement -> renderCustomElement customElement



let renderElements (model: PageEditorModel) dispatch =
    let viewportTransform (position: Position) = {
        X = (position.X + model.ViewportPosition.X) * model.Scale
        Y = (position.Y + model.ViewportPosition.Y) * model.Scale
    }

    printfn "renderElements re-rendering"

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
                Html.div[prop.className "m-4"
                         prop.children [ element.Content ]]
            ]
        ]

    model.Elements |> List.map renderElement