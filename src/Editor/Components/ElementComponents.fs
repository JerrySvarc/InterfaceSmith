module Editor.Components.ElementComponents

open Editor.Types
open Fable.React
open CoreLogic.Types.RenderingTypes
open Editor.Types.PageEditorDomain
open CoreLogic.Operations.RenderingCode
open CoreLogic.Types.RenderingTypes
open Feliz
open Browser.Types
open Fable.Core.JsInterop
open Microsoft.FSharp.Reflection
open Editor.Utilities.Icons
open Editor.Types.EditorDomain
open Fable.SimpleJson
open CoreLogic.Types
open System.Reflection

// Contains option menu components for each type of rendering code
// Each component takes a dispatch function, the current code, and the path to the code in the tree
// The dispatch function is used to send messages to the parent component to update the specific code

let SelectMenu (options: string list) (value: string) (onChange: string -> unit) =
    Html.select [
        prop.className
            "text-xs w-36 h-fit bg-white border border-black shadow-sm focus:border-indigo-500 focus:ring focus:ring-indigo-200 focus:ring-opacity-50"
        prop.onMouseDown (fun e -> e.stopPropagation ())
        prop.value value
        prop.onChange (fun (e: Browser.Types.Event) -> e.target?value |> string |> onChange)
        prop.children (
            (options
             |> List.map (fun opt -> Html.option [ prop.className "text-xs"; prop.value opt; prop.text opt ]))
        )
    ]

let ErrorDisplay (message: string) =
    Html.div [
        prop.className "bg-red-100 border-l-4 border-red-500 text-red-700 p-4 rounded flex items-center space-x-2"
        prop.children [ Html.span [ prop.className "font-medium"; prop.text message ] ]
    ]

(*
[<ReactComponent>]
let TagMenu dispatch (code: RenderingCode) path =
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
let InnerValueMenu (dispatch, currentInnerValue: InnerValue, code: RenderingCode, path) =
    let innerValueOptions = [ "Data"; "Constant"; "Empty" ]

    let constantValue, setConstantValue =
        React.useState (
            match currentInnerValue with
            | Constant str -> str
            | _ -> ""
        )

    let innerValueToString (iv: InnerValue) =
        match iv with
        | Data -> "Data"
        | Constant _ -> "Constant"
        | Empty -> "Empty"

    let updateInnerValue newValue =
        match code with
        | RenderingCode.HtmlElement(tag, attrs, _, handlers) ->
            dispatch (ReplaceCode(RenderingCode.HtmlElement(tag, attrs, newValue, handlers), path))
        | _ -> ()

    match code with
    | RenderingCode.HtmlElement(tag, attrs, _, handlers) ->
        Html.div [
            prop.className "mb-4"
            prop.children [
                Html.label [
                    prop.className "block text-sm font-medium text-gray-700 mb-1"
                    prop.htmlFor "inner-value-select"
                    prop.text "Inner Value"
                ]
                Html.div [
                    prop.className "flex space-x-2"
                    prop.children [
                        Html.div [
                            prop.className "flex-grow"
                            prop.children [
                                SelectMenu
                                    innerValueOptions
                                    (currentInnerValue |> innerValueToString)
                                    (fun selectedValue ->
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
                            Html.input [
                                prop.type' "text"
                                prop.value constantValue
                                prop.onChange (fun (value: string) ->
                                    setConstantValue value
                                    updateInnerValue (Constant value))
                                prop.className
                                    "flex-grow p-2 border border-gray-300 rounded-md shadow-sm focus:border-indigo-500 focus:ring focus:ring-indigo-200 focus:ring-opacity-50"
                            ]
                    ]
                ]
            ]
        ]
    | _ -> ErrorDisplay "Invalid code type for InnerValueMenu"



/// <summary></summary>
/// <param name="dispatch"></param>
/// <param name="name"></param>
/// <param name="code"></param>
/// <param name="path"></param>
/// <typeparam name="'a"></typeparam>
/// <returns></returns>
let ElementOption dispatch name code path =

    let options2 = [ "Option 1"; "Option 2"; "Option 3" ]
    let selectedValue2 = "Option 1" // Or dynamically set from state
    let handleChange2 newValue = printfn "Selected: %s" newValue

    Html.div [
        prop.onMouseDown (fun e -> e.stopPropagation ())
        prop.className "bg-white flex flex-col p-4"
        prop.children [
            Html.h1 [ prop.text "Todo"; prop.className "preview" ]
            Html.div [
                prop.children [
                    (TagMenu dispatch code path)
                    SelectMenu options2 selectedValue2 handleChange2
                ]
            ]
            Html.ul [
                prop.className "preview"
                prop.children [
                    Html.div [
                        prop.className "bg-gray-300"
                        prop.children [ Html.input [ prop.type' "checkbox" ]; Html.span "get groceries" ]
                    ]
                    Html.li [ prop.text "todo2" ]
                ]
            ]
        ]
    ]
*)
let rec options (dispatch: PageEditorMsg -> unit) (code: RenderingCode) (path: int list) (name: string) : ReactElement =
    match code with
    | RenderingCode.HtmlElement _ -> Html.none //ElementOption dispatch name code path
    | RenderingCode.HtmlList _ -> Html.none
    //ListOption(dispatch, name, code, path)
    | RenderingCode.HtmlObject(_) -> Html.none
    //SequenceOption(dispatch, name, code, path)
    | RenderingCode.Hole _ -> Html.none
    | RenderingCode.CustomWrapper(_) -> failwith "Not Implemented"
    | RenderingCode.CustomElement(_) -> failwith "Not Implemented"

let Collapsible =
    React.memo
        (fun
            (props:
                {|
                    title: string
                    children: ReactElement list
                    key: string
                |}) ->
            let (isExpanded, setIsExpanded) = React.useState false

            let handleClick _ =
                setIsExpanded (not isExpanded)
                printfn "Toggled state to: %A" (not isExpanded)

            Html.div [
                Html.div [
                    prop.className "cursor-pointer text-blue-500 font-bold flex items-center"
                    prop.onClick handleClick
                    prop.children [
                        if isExpanded then
                            ReactBindings.React.createElement (
                                chevronDown,
                                createObj [ "size" ==> 16; "color" ==> "#FFFFFF" ],
                                []
                            )
                        else
                            ReactBindings.React.createElement (
                                chevronRight,
                                createObj [ "size" ==> 16; "color" ==> "#FFFFFF" ],
                                []
                            )
                        Html.span [ prop.className "ml-1"; prop.text props.title ]
                    ]
                ]
                if isExpanded then
                    Html.div [ prop.className "ml-4 mt-1 space-y-1"; prop.children props.children ]
            ])

[<ReactComponent>]
let ModelElement (json: Json) =
    let rec displayField (json: Json) : ReactElement =
        match json with
        | JObject obj ->
            let entries =
                obj
                |> Map.toList
                |> List.map (fun (key, value) ->
                    Html.div [
                        Html.span [ prop.className "font-bold"; prop.text $"{key}: " ]
                        displayField value
                    ])

            Collapsible {|
                title = "Object"
                children = entries
                key = $"obj-{System.Guid.NewGuid()}"
            |}

        | JArray arr ->
            let items =
                arr
                |> List.mapi (fun index value ->
                    Html.div [
                        Html.span [ prop.className "font-bold"; prop.text $"[{index}]: " ]
                        displayField value
                    ])

            Collapsible {|
                title = $"[ List of {List.length arr} items ]"
                children = items
                key = $"arr-{arr.Length}"
            |}

        | JString str -> Html.span [ prop.className "text-red-400"; prop.text $"\"{str}\"" ]
        | JNumber num -> Html.span [ prop.className "text-purple-400"; prop.text $"{num}" ]
        | JBool b -> Html.span [ prop.className "text-yellow-400"; prop.text (if b then "true" else "false") ]
        | JNull -> Html.span [ prop.className "text-gray-500 italic"; prop.text "null" ]

    Html.div [
        prop.className "bg-gray-900 text-white rounded w-56"
        prop.children [
            Html.h3 [ prop.className "font-bold mb-4 text-white"; prop.text "JSON Model" ]
            displayField json
        ]
        prop.onMouseDown (fun e -> e.stopPropagation ())
    ]




[<ReactComponent>]
let RightClickMenu dispatch =
    Html.div [
        prop.className "border border-black bg-gray-600 text-white "
        prop.children [
            Html.div [ prop.text "Create Message" ]
            Html.div [ prop.text "Create Attribute" ]
            Html.div [ prop.text "Create Attribute" ]
        ]
    ]

[<ReactComponent>]
let ViewElement model dispatch =
    Html.div [
        prop.className "border border-black bg-gray-600 text-white "
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
    // prop.children [ render ]
    ]

let MsgOverview model dispatch = Html.div []
let MsgElement msgs dispatch = Html.div []
let UpdateOverview model dispatch = Html.div []
let UpdateElement updateFun dispatch = Html.div []