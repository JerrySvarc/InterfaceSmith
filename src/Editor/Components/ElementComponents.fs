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
open Editor.Components.OptionsComponents
open Editor.CustomRendering
open Editor.Utilities.JavaScriptEditor

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
let ModelElement (json: Json) dispatch =
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
            Html.button [
                prop.onClick (fun _ -> dispatch (CreateViewElement dispatch))
                prop.text "Open View"
            ]
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
let ViewElement currentTree parsedJson dispatch =
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
        let renderContext = {
            Options = options
            Dispatch = dispatch
            Path = []
            Json = parsedJson
            Name = "View"
        }

        prop.children [ renderingCodeToReactElement renderContext currentTree ]
    ]

[<ReactComponent>]
let JavaScriptEditorView code (dispatch) =

    let extensions = [| javascript?javascript (); html?html (); css?css () |]

    Html.div [
        prop.className "flex flex-col h-full border-solid border-2 border-black overflow-auto"
        prop.children [
            Html.h3 [ prop.className "font-bold mb-2 px-2"; prop.text "Code preview" ]
            Html.div [
                prop.className "flex-grow overflow-auto"
                prop.children [
                    ReactBindings.React.createElement (
                        CodeMirror,
                        createObj [
                            "value" ==> code
                            "extensions" ==> extensions
                            "theme" ==> "dark"
                            "readOnly" ==> "true"
                        ],
                        []
                    )
                ]
            ]
        ]
    ]

let MsgOverview model dispatch = Html.div []
let MsgElement msgs dispatch = Html.div []
let UpdateOverview model dispatch = Html.div []
let UpdateElement updateFun dispatch = Html.div []