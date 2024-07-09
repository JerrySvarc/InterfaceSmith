module UIComponents.OptionComponents

open Fable.React
open Types.RenderingTypes
open Types.EditorDomain
open Feliz
open Browser.Types
open Fable.Core.JsInterop
open Utilities.EditorUtils
open Microsoft.FSharp.Reflection

(*
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
let AttributeMenu (dispatch, attrName: string, attrVal: InnerValue, code: RenderingCode, path) =
    let (attributeValue, setAttributeValue) = React.useState attrVal

    let updateAttribute (newValue: string) =
        match code with
        | HtmlElement(tag, attrs, innerValue, handlers) ->
            let updatedAttrs =
                attrs
                |> List.map (fun (name, _) ->
                    if name = attrName then
                        (name, Constant newValue)
                    else
                        (name, attributeValue))

            dispatch (ReplaceCode(HtmlElement(tag, attrs, innerValue, handlers), path))
            setAttributeValue (Constant newValue)
        | _ -> ()

    match code with
    | HtmlElement(tag, attrs, innerValue, handlers) ->
        Html.table [
            prop.children [
                TableRow attrName (InnerValueMenu(dispatch, attributeValue, code, path))
                match attributeValue with
                | Constant value ->
                    TableRow
                        (attrName + " value")
                        (Html.input [
                            prop.className "p-1 bg-white"
                            prop.value value
                            prop.onInput (fun e -> updateAttribute (e.target?value |> string))
                        ])
                | _ -> ()
            ]
        ]
    | _ -> ErrorDisplay "Invalid code type for AttributeMenu"

[<ReactComponent>]
let ElementOption (dispatch, name: string, code, path) =
    let (collapsed, setCollapsed) = React.useState true

    let toggleCollapse () = setCollapsed (not collapsed)

    let renderContent () =
        match code with
        | HtmlElement(tag, attrs, innerValue, handlers) ->
            Html.div [
                prop.className "p-4 bg-white rounded items-center justify-start"
                prop.children [
                    Html.h1 [ prop.className "pb-2 text-xl"; prop.text ("Field " + name) ]
                    Html.table [
                        prop.className "table-auto w-full shadow-lg bg-white"
                        prop.children [
                            Html.thead [
                                prop.className "text-left"
                                prop.children [
                                    Html.tr [
                                        prop.className "bg-gray-100"
                                        prop.children [
                                            Html.th [ prop.className "px-4 py-2"; prop.text "Attribute" ]
                                            Html.th [ prop.className "px-4 py-2"; prop.text "Value" ]
                                        ]
                                    ]
                                ]
                            ]
                            Html.tbody [
                                prop.children (
                                    [ TableRow "Tag" (TagMenu(dispatch, code, path)) ]
                                    @ (attrs
                                       |> List.map (fun (name, value) ->
                                           AttributeMenu(dispatch, name, value, code, path)))
                                    @ [ TableRow "Inner value" (InnerValueMenu(dispatch, innerValue, code, path)) ]
                                )
                            ]
                        ]
                    ]
                ]
            ]
        | _ -> ErrorDisplay "Invalid code type for ElementOption"

    Html.div [
        prop.className "flex flex-row p-1 m-1 border-1 items-center border-gray-300 bg-white rounded w-fit h-fit"
        prop.children [
            if collapsed then
                Html.h1 [ prop.className "p-4 bg-white rounded"; prop.text name ]
            else
                renderContent ()
            Html.button [
                prop.className
                    "p-1 m-1 border h-fit border-gray-200 bg-secondary-300 hover:bg-secondary-600 text-black rounded-md"
                prop.text (if collapsed then "Expand" else "Collapse")
                prop.onClick (fun _ -> toggleCollapse ())
            ]
        ]
    ]

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
*)