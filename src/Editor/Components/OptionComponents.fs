module Editor.Components.OptionComponents

open Editor.Types
open Fable.React
open CoreLogic.Types.RenderingTypes
open Editor.Types.PageEditorDomain
open CoreLogic.Operations.RenderingCode
open Feliz
open Browser.Types
open Fable.Core.JsInterop
open Microsoft.FSharp.Reflection


let SelectMenu (options: string list) (value: string) (onChange: string -> unit) =
    Html.select [
        prop.className "p-1 bg-white border-2 border-gray-200 rounded"
        prop.onMouseDown (fun e -> e.stopPropagation ())
        prop.value value
        prop.onChange (fun (e: Browser.Types.Event) -> e.target?value |> string |> onChange)
        prop.children (
            Html.option [ prop.value ""; prop.text "Select an option" ]
            :: (options |> List.map (fun opt -> Html.option [ prop.value opt; prop.text opt ]))
        )
    ]

let TableRow (label: string) (content: ReactElement) =
    Html.tr [
        prop.className "hover:bg-gray-50"
        prop.children [
            Html.td [ prop.className "border px-4 py-2"; prop.text label ]
            Html.td [ prop.className "border px-4 py-2"; prop.children [ content ] ]
        ]
    ]

let ErrorDisplay (message: string) =
    Html.div [ prop.className "text-red-500 p-2 bg-red-100 rounded"; prop.text message ]


[<ReactComponent>]
let TagMenu (dispatch, code: RenderingCode, path) =
    let tagOptions =
        FSharpType.GetUnionCases(typeof<Tag>)
        |> Array.map (fun caseInfo -> caseInfo.Name)
        |> Array.toList

    match code with
    | HtmlElement(tag, attrs, value,handlers) ->
        SelectMenu tagOptions (tag.ToString()) (fun selectedTag ->
            let newTag = selectedTag.ToLower() |> stringToTag
            dispatch (ReplaceCode(HtmlElement(newTag, attrs, value,handlers), path)))
    | _ -> ErrorDisplay "Invalid code type for TagMenu"

[<ReactComponent>]
let InnerValueMenu (dispatch, innerValue: InnerValue, code: RenderingCode, path) =
    let innerValueOptions = [ "Data"; "Constant"; "Empty" ]

    let innerValueToString (innerValue: InnerValue) =
        match innerValue with
        | Data -> "Data"
        | Constant _ -> "Constant"
        | Empty -> "Empty"

    match code with
    | HtmlElement(tag, attrs, innerValue, handlers) ->
        SelectMenu innerValueOptions (innerValue |> innerValueToString) (fun selectedValue ->
            let newValue =
                match selectedValue with
                | "Data" -> InnerValue.Data
                | "Constant" -> InnerValue.Constant ""
                | "Empty" -> InnerValue.Empty
                | _ -> innerValue

            dispatch (ReplaceCode(HtmlElement(tag, attrs, innerValue, handlers), path)))
    | _ -> ErrorDisplay "Invalid code type for InnerValueMenu"

[<ReactComponent>]
let AttributeMenu (dispatch: PageEditorMsg -> unit, code: RenderingCode, path: int list) =
    let (newAttrName, setNewAttrName) = React.useState ""
    let (newAttrValue, setNewAttrValue) = React.useState ""

    let updateAttribute (name: string) (newValue: string) =
        match code with
        | HtmlElement(tag, attrs, innerValue, handlers) ->
            let updatedAttrs =
                attrs
                |> List.map (fun (attrName, attrValue) ->
                    if attrName = name then
                        (attrName, Constant newValue)
                    else
                        (attrName, attrValue))
            dispatch (ReplaceCode(HtmlElement(tag, updatedAttrs, innerValue, handlers), path))
        | _ -> ()

    let addNewAttribute () =
        if not (newAttrName.Length < 1) then
            match code with
            | HtmlElement(tag, attrs, innerValue, handlers) ->
                let updatedAttrs = attrs @ [(newAttrName, Constant newAttrValue)]
                dispatch (ReplaceCode(HtmlElement(tag, updatedAttrs, innerValue, handlers), path))
                setNewAttrName ""
                setNewAttrValue ""
            | _ -> ()

    match code with
    | HtmlElement(tag, attrs, innerValue, handlers) ->
        Html.div [
            prop.className "space-y-2 w-64"
            prop.children [
                Html.div [
                    prop.className "font-medium text-sm mb-2"
                    prop.text "Attributes"
                ]
                for (name, value) in attrs do
                    Html.div [
                        prop.key name
                        prop.className "flex items-center space-x-2 mb-1"
                        prop.children [
                            Html.input [
                                prop.className "p-1 bg-white border rounded text-sm w-1/3"
                                prop.value name
                                prop.readOnly true
                            ]
                            Html.input [
                                prop.className "p-1 bg-white border rounded text-sm w-1/3"
                                prop.value (
                                    match value with
                                    | Constant v -> v
                                    | _ -> ""
                                )
                                prop.onChange (fun v -> updateAttribute name v)
                            ]
                            Html.button [
                                prop.className "bg-red-500 text-white text-xs p-1 rounded"
                                prop.onClick (fun _ ->
                                    let updatedAttrs = attrs |> List.filter (fun (n, _) -> n <> name)
                                    dispatch (ReplaceCode(HtmlElement(tag, updatedAttrs, innerValue, handlers), path))
                                )
                                prop.text "Remove"
                            ]
                        ]
                    ]
                Html.div [
                    prop.className "flex items-center space-x-2 mt-2"
                    prop.children [
                        Html.input [
                            prop.className "p-1 bg-white border rounded text-sm w-1/3"
                            prop.placeholder "New attribute name"
                            prop.value newAttrName
                            prop.onChange setNewAttrName
                        ]
                        Html.input [
                            prop.className "p-1 bg-white border rounded text-sm w-1/3"
                            prop.placeholder "New attribute value"
                            prop.value newAttrValue
                            prop.onChange setNewAttrValue
                        ]
                        Html.button [
                            prop.className "bg-green-500 text-white text-xs p-1 rounded"
                            prop.onClick (fun _ -> addNewAttribute())
                            prop.text "Add"
                        ]
                    ]
                ]
            ]
        ]
    | _ -> Html.none  // Return Html.none instead of ErrorDisplay for non-HtmlElement types

[<ReactComponent>]
let ElementOption (dispatch, name: string, code, path) =
    let (collapsed, setCollapsed) = React.useState true

    let toggleCollapse () = setCollapsed (not collapsed)

    match code with
    | HtmlElement(tag, attrs, innerValue, handlers) ->
        Html.div [
            prop.className "bg-white rounded-lg shadow-sm border border-gray-200 overflow-hidden w-64"
            prop.children [
                Html.div [
                    prop.className "flex items-center justify-between p-2 bg-gray-50 cursor-pointer"
                    prop.onClick (fun _ -> toggleCollapse())
                    prop.children [
                        Html.span [
                            prop.className "font-medium text-sm text-gray-700"
                            prop.text name
                        ]
                        Html.span [
                            prop.className "text-gray-500"
                            prop.text (if collapsed then "▼" else "▲")
                        ]
                    ]
                ]
                if not collapsed then
                    Html.div [
                        prop.className "p-2 space-y-2"
                        prop.children [
                            Html.div [
                                prop.className "flex items-center space-x-2"
                                prop.children [
                                    Html.span [ prop.className "text-sm text-gray-600"; prop.text "Tag:" ]
                                    TagMenu(dispatch, code, path)
                                ]
                            ]
                            Html.div [
                                prop.className "flex items-center space-x-2"
                                prop.children [
                                    Html.span [ prop.className "text-sm text-gray-600"; prop.text "Inner Value:" ]
                                    InnerValueMenu(dispatch, innerValue, code, path)
                                ]
                            ]
                            Html.div [
                                prop.className "space-y-1"
                                prop.children [
                                    Html.span [ prop.className "text-sm text-gray-600"; prop.text "Attributes:" ]
                                    AttributeMenu(dispatch, code, path)
                                ]
                            ]
                        ]
                    ]
            ]
        ]
    | _ -> Html.none


[<ReactComponent>]
let ListOption (dispatch, name: string, code, path) =
    match code with
    | HtmlList(listType, elementCode, handlers) ->
        let listTypeOptions =
            FSharpType.GetUnionCases(typeof<ListType>)
            |> Array.map (fun caseInfo -> caseInfo.Name)
            |> Array.toList

        Html.div [
            prop.className "p-4 border border-gray-300 bg-white rounded-lg shadow-sm"
            prop.children [
                Html.h1 [ prop.className "text-xl"; prop.text name ]
                SelectMenu listTypeOptions (listTypeToString listType) (fun selectedListType ->
                    let newListType = selectedListType |> stringToListType
                    dispatch (ReplaceCode(HtmlList(newListType, elementCode, handlers), path)))
            ]
        ]
    | _ -> ErrorDisplay "Invalid code type for ListOption"

[<ReactComponent>]
let SequenceOption (dispatch, name: string, code, path) =
    match code with
    | HtmlObject(objType, keys, elements, handlers) ->
        Html.div [
            prop.className "bg-gray-100 p-4 rounded-lg shadow"
            prop.children [
                Html.h1 [ prop.className "text-xl"; prop.text name ]
                Html.button [
                    prop.className "bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded m-1"
                    prop.onClick (fun _ -> dispatch (ReplaceCode(code, path)))
                    prop.text "Add sequence"
                ]
                Html.button [
                    prop.className "bg-red-500 hover:bg-red-700 text-white font-bold py-2 px-4 rounded m-1"
                    prop.onClick (fun _ -> dispatch (ReplaceCode(Hole(Named name), path)))
                    prop.text "Delete sequence"
                ]
            ]
        ]
    | _ -> ErrorDisplay "Invalid code type for SequenceOption"

