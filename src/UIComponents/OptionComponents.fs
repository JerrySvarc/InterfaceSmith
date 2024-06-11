module UIComponents.OptionComponents

open Fable.React
open Types
open Feliz
open Browser.Types
open Fable.Core.JsInterop
open System
open Utilities.GeneralUtilities
open Utilities.EditorUtils
open System
open Microsoft.FSharp.Reflection


[<ReactComponent>]
let TagMenu (dispatch, code: RenderingCode, path) =
    let (selectedOption, setSelectedOption) = React.useState ""

    let tagOptions =
        FSharpType.GetUnionCases(typeof<Tag>)
        |> Array.map (fun caseInfo -> caseInfo.Name)

    match code with
    | HtmlElement(tag, attrs, value) ->
        Html.div [
            prop.className "p-1 bg-gray-100 my-1 w-full"
            prop.children [
                Html.select [
                    prop.className "p-1 bg-white"
                    prop.onMouseDown (fun e -> e.stopPropagation ())
                    prop.value (tag.ToString())
                    prop.onChange (fun (e: Browser.Types.Event) ->
                        let selectedTag = e.target?value |> string
                        let newTag = selectedTag.ToLower() |> stringToTag
                        dispatch (ReplaceCode(HtmlElement(newTag, attrs, value), path)))
                    prop.children (
                        [ Html.option [ prop.value ""; prop.text "Select tag" ] ]
                        @ (tagOptions
                           |> Array.toList
                           |> List.map (fun tag -> Html.option [ prop.value tag; prop.text tag ]))
                    )
                ]
            ]
        ]
    | _ -> failwith "Not a valid code type."


[<ReactComponent>]
let InnerValueMenu (dispatch, innerValue, code: RenderingCode, path) =
    let innerValueOptions = [ "Data"; "Constant"; "Empty" ]

    let innerValueToString (innerValue: InnerValue) =
        match innerValue with
        | Data -> "Data"
        | Constant _ -> "Constant"
        | Empty -> "Empty"

    match code with
    | HtmlElement(tag, attrs, value) ->
        Html.select [
            prop.className "p-1 border-2 border-gray-200 bg-white rounded"
            prop.onMouseDown (fun e -> e.stopPropagation ())
            prop.value (innerValue |> innerValueToString)
            prop.onChange (fun (e: Browser.Types.Event) ->
                let selectedValue = e.target?value |> string

                match selectedValue with
                | "Data" -> dispatch (ReplaceCode(HtmlElement(tag, attrs, InnerValue.Data), path))
                | "Constant" -> dispatch (ReplaceCode(HtmlElement(tag, attrs, InnerValue.Constant ""), path))
                | "Empty" -> dispatch (ReplaceCode(HtmlElement(tag, attrs, InnerValue.Empty), path))
                | _ -> ())
            prop.children (
                [ Html.option [ prop.value ""; prop.text "Select inner value" ] ]
                @ (innerValueOptions
                   |> List.map (fun value -> Html.option [ prop.value value; prop.text value ]))
            )
        ]
    | _ -> failwith "Not a valid code type."

[<ReactComponent>]
let AttributeMenu (dispatch, attrName: string, attrVal: InnerValue, code: RenderingCode, path) =
    let (attributeValue, setAttributeValue) = React.useState attrVal

    let setAttributeValue (newCode, value: string) =
        dispatch (ReplaceCode(newCode, path))
        setAttributeValue (Constant value)

    match code with
    | HtmlElement(tag, attrs, value) ->
        match attrVal with
        | Constant value ->
            Html.table [
                prop.children [
                    Html.tr [
                        prop.className "p-1 bg-gray-100 my-1 w-full"
                        prop.children [
                            Html.td [ prop.className "border px-4 py-2"; prop.text attrName ]
                            Html.td [
                                prop.className "border px-4 py-2"
                                prop.children [ InnerValueMenu(dispatch, attributeValue, code, path) ]
                            ]
                        ]
                    ]

                    Html.tr [
                        prop.children [
                            Html.td [ prop.className "border px-4 py-2"; prop.text (attrName + " value") ]
                            Html.td [
                                prop.className "p-1 bg-white"
                                prop.children [
                                    Html.input [
                                        prop.className "p-1 bg-white"
                                        prop.value value
                                        prop.onInput (fun e ->
                                            setAttributeValue (
                                                HtmlElement(tag, attrs, Constant(value)),
                                                e.target?value |> string
                                            ))
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        | _ ->
            Html.tr [
                prop.className "p-1 bg-gray-100 my-1 w-full"
                prop.children [
                    Html.td [ prop.className "border px-4 py-2"; prop.text attrName ]

                    Html.td [
                        prop.className "border px-4 py-2"
                        prop.children [ InnerValueMenu(dispatch, attributeValue, code, path) ]
                    ]
                ]
            ]

    | _ -> failwith "Not a valid code type."


[<ReactComponent>]
let ElementOption (dispatch, name: string, code, path, collapsed: bool) =
    let (collapsed, setCollapsed) = React.useState collapsed

    let attr =
        match code with
        | HtmlElement(_, attrs, _) -> attrs
        | _ -> []

    let innerVal =
        match code with
        | HtmlElement(_, _, innerValue) -> innerValue
        | _ -> Empty

    Html.div [
        prop.className "flex flex-row p-1 m-1 border-1 items-center border-gray-300 bg-white rounded w-fit h-fit"
        prop.children [
            match collapsed with
            | true -> Html.h1 [ prop.className "p-4 bg-white rounded"; prop.text name ]
            | false ->
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
                                        [
                                            Html.tr [
                                                prop.className "hover:bg-gray-50"
                                                prop.children [
                                                    Html.td [ prop.className "border px-4 py-2"; prop.text "Tag" ]
                                                    Html.td [
                                                        prop.className "border px-4 py-2"
                                                        prop.children [ TagMenu(dispatch, code, path) ]
                                                    ]
                                                ]
                                            ]
                                        ]
                                        @ (List.map
                                            (fun (name, value) ->
                                                Fable.Core.JS.console.log (value.ToString())
                                                AttributeMenu(dispatch, name, value, code, path))
                                            attr)
                                        @ [
                                            Html.tr [
                                                prop.className "hover:bg-gray-50"
                                                prop.children [
                                                    Html.td [
                                                        prop.className "border px-4 py-2"
                                                        prop.text "Inner value"
                                                    ]
                                                    Html.td [
                                                        prop.className "border px-4 py-2"
                                                        prop.children [ InnerValueMenu(dispatch, innerVal, code, path) ]
                                                    ]
                                                ]
                                            ]
                                        ]
                                    )
                                ]
                            ]
                        ]
                    ]
                ]
            Html.button [
                prop.className
                    "p-1 m-1 border h-fit border-gray-200 bg-secondary-300 hover:bg-secondary-600 text-black rounded-md"
                prop.text (if collapsed then "Expand" else "Collapse")
                prop.onClick (fun _ -> setCollapsed (not collapsed))
            ]
        ]
    ]

//TODO: implement list option
[<ReactComponent>]
let ListOption (dispatch, name, code, path, collapsed: bool) =
    match code with
    | HtmlList(listType, headers, elementCode) ->
        let listTypeOptions =
            FSharpType.GetUnionCases(typeof<ListType>)
            |> Array.map (fun caseInfo -> caseInfo.Name)

        Html.div [
            prop.className "p-4 border border-gray-300 bg-white rounded-lg shadow-sm"
            prop.children [
                Html.select [
                    prop.className
                        "block w-full p-2 mt-1 border border-gray-300 bg-white rounded-md shadow-sm focus:border-blue-500 focus:ring focus:ring-blue-500 focus:ring-opacity-50" // Enhanced select styling
                    prop.onMouseDown (fun e -> e.stopPropagation ())
                    prop.onClick (fun e -> e.stopPropagation ())
                    prop.value (listTypeToString listType)
                    prop.onChange (fun (e: Browser.Types.Event) ->
                        let selectedListType = e.target?value |> string
                        let newListType = selectedListType |> stringToListType
                        dispatch (ReplaceCode(HtmlList(newListType, headers, elementCode), path)))
                    prop.children (
                        [ Html.option [ prop.value ""; prop.text "Select list type" ] ]
                        @ (listTypeOptions
                           |> Array.toList
                           |> List.map (fun listType -> Html.option [ prop.value listType; prop.text listType ]))
                    )
                ]
            ]
        ]

    | _ -> failwith "Invalid code type."
//TODO: implement sequence option
[<ReactComponent>]
let SequenceOption (dispatch, name, code, path, collapsed: bool) =
    match code with
    | Sequence(elements) ->
        Html.div [
            prop.className "bg-gray-100 p-4 rounded-lg shadow"
            prop.children [
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

    | _ -> Html.div [ prop.className "text-red-500"; prop.text "Invalid code type." ]