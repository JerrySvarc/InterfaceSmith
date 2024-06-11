module UIComponents.OptionComponents

open Fable.React
open Types
open Feliz

//TODO: Implement elementoption
[<ReactComponent>]
let ElementOption (name: string, code, path) =
    Html.div [
        Html.table [
            prop.className "table-auto"
            prop.children [
                Html.tbody [
                    prop.children [
                        Html.tr [ prop.children [ Html.td [ prop.text "Tag" ]; Html.td [ prop.text name ] ] ]
                    ]
                ]
            ]
        ]
    ]
//TODO: implement list option
[<ReactComponent>]
let ListOption (name, code, path) = Html.none
//TODO: implement sequence option
[<ReactComponent>]
let SequenceOption (name, code, path) = Html.none



(*
and elementOptionsComponent =
    React.functionComponent (fun (name :string, code: RenderingCode, path) ->
        let (attributeName, setAttributeName) = React.useState ""
        let (attributeValue, setAttributeValue) = React.useState ""
        let (constantValue, setConstantValue) = React.useState ""

        match code with
        | HtmlElement(tag, attrs, innerValue) ->
            Html.div [
                prop.className "flex flex-row flex-nowrap p-1 border-1 border-gray-300 bg-white rounded"
                prop.children [
                    Html.h1 [
                        prop.className "p-4 bg-white rounded"
                        prop.text name
                    ]
                    let tagOptions =
                        FSharpType.GetUnionCases(typeof<Tag>)
                        |> Array.map (fun caseInfo -> caseInfo.Name)

                    //tag selector
                    Html.div [
                        prop.className "p-1 bg-gray-100  my-1"
                        prop.children [
                            Html.p [ prop.className "font-bold text-gray-700"; prop.text "Change tag" ]
                            Html.select [
                                prop.className "p-1  bg-white "
                                prop.onMouseDown (fun e -> e.stopPropagation ())
                                prop.value (tag.ToString())
                                prop.onChange (fun (e: Browser.Types.Event) ->
                                    let selectedTag = e.target?value |> string
                                    let newTag = selectedTag.ToLower() |> stringToTag
                                    dispatch (ReplaceCode(HtmlElement(newTag, attrs, innerValue), path)))
                                prop.children (
                                    [ Html.option [ prop.value ""; prop.text "Select tag" ] ]
                                    @ (tagOptions
                                    |> Array.toList
                                    |> List.map (fun tag -> Html.option [ prop.value tag; prop.text tag ]))
                                )
                            ]
                        ]
                    ]
                //attribute editor
                    Html.div [
                        prop.className "p-1  bg-gray-100 rounded my-1"
                        prop.children [
                            Html.input [
                                prop.className "p-2 border-2 border-gray-200 bg-white rounded my-2"
                                prop.placeholder "Attribute name"
                                prop.onTextChange (fun e -> setAttributeName (e))
                            ]
                            Html.input [
                                prop.className "p-2 border-2 border-gray-200 bg-white rounded my-2"
                                prop.placeholder "Attribute value"
                                prop.onTextChange (fun e -> setAttributeValue (e))
                            ]
                            Html.button [
                                prop.className "p-2 border-2 border-gray-200 bg-green-500 text-white rounded my-2"
                                prop.text "Add attribute"
                                prop.onClick (fun _ ->
                                    let newAttribute = Types.Attribute(attributeName, Constant(attributeValue))
                                    let updatedAttrs = [newAttribute] @ attrs
                                    dispatch (ReplaceCode(HtmlElement(tag, updatedAttrs, innerValue), path)))
                            ]
                        ]
                    ]

                    let innerValueOptions = [ "Data"; "Constant"; "Empty" ]

                    let innerValueToString (innerValue: InnerValue) =
                        match innerValue with
                        | Data -> "Data"
                        | Constant _ -> "Constant"
                        | Empty -> "Empty"

                    Html.div [
                        prop.className "p-1  bg-gray-100 rounded my-1"
                        prop.children [
                            Html.select [
                                prop.className "p-1 border-2 border-gray-200 bg-white rounded"
                                prop.onMouseDown (fun e -> e.stopPropagation ())
                                prop.value (innerValue |> innerValueToString)
                                prop.onChange (fun (e: Browser.Types.Event) ->
                                    let selectedValue = e.target?value |> string

                                    match selectedValue with
                                    | "Data" ->
                                        dispatch (ReplaceCode(HtmlElement(tag, attrs, InnerValue.Data), path))
                                    | "Constant" ->
                                        dispatch (
                                            ReplaceCode(HtmlElement(tag, attrs, InnerValue.Constant ""), path)
                                        )
                                    | "Empty" ->
                                        dispatch (ReplaceCode(HtmlElement(tag, attrs, InnerValue.Empty), path))
                                    | _ -> ())
                                prop.children (
                                    innerValueOptions
                                    |> List.map (fun value -> Html.option [ prop.value value; prop.text value ])
                                )
                            ]
                            match innerValue with
                            | InnerValue.Constant _ ->
                                Html.input [
                                    prop.className "p-1 border-2 border-gray-200 bg-white rounded"
                                    prop.placeholder "Enter constant value"
                                    prop.value constantValue
                                    prop.onInput (fun e -> setConstantValue (e.target?value |> string))
                                    prop.onBlur (fun _ ->
                                        dispatch (
                                            ReplaceCode(
                                                HtmlElement(tag, attrs, InnerValue.Constant constantValue),
                                                path
                                            )
                                        ))
                                ]
                            | _ -> Html.none
                        ]
                    ]
                ]
            ]
        | _ -> failwith "Not a valid code type.")

and listOptionsComponent =
    React.functionComponent (fun (name, code: RenderingCode, path) ->
        match code with
        | HtmlList(listType, headers, elementCode) ->
            let listTypeOptions =
                FSharpType.GetUnionCases(typeof<ListType>)
                |> Array.map (fun caseInfo -> caseInfo.Name)

            Html.div [
                prop.className "p-4 border-2 border-gray-300 bg-white rounded"
                prop.children [
                    // List type selector
                    Html.select [
                        prop.className "p-2 border-2 border-gray-200 bg-white rounded"
                        prop.onMouseDown (fun e -> e.stopPropagation ())
                        prop.onClick (fun e -> e.stopPropagation ())
                        prop.value ""
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
                ]]

        | _ -> failwith "Invalid code type.")

and sequenceOptionsComponent =
    React.functionComponent (fun (name, code: RenderingCode, path) ->
        match code with
        | Sequence(elements) ->
            Html.div [
                prop.className ""
                prop.children [
                    Html.button [
                        prop.onClick(fun _ -> dispatch ( ReplaceCode (code,path)))
                        prop.text "Add sequence"
                    ]
                ]
            ]

        | _ -> failwith "Invalid code type.")
*)