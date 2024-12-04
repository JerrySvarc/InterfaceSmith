module Editor.Components.ElementComponents

open Editor.Types
open Fable.React
open CoreLogic.Types.RenderingTypes
open Editor.Types.PageEditorDomain
open CoreLogic.Operations.RenderingCode
open Feliz
open Browser.Types
open Fable.Core.JsInterop
open Microsoft.FSharp.Reflection
open Editor.Utilities.Icons
open Editor.Types.EditorDomain
open Fable.SimpleJson

// Contains option menu components for each type of rendering code
// Each component takes a dispatch function, the current code, and the path to the code in the tree
// The dispatch function is used to send messages to the parent component to update the specific code

let SelectMenu (options: string list) (value: string) (onChange: string -> unit) =
    Html.select [
        prop.className
            "text-xs w-1/3 h-fit p-2 bg-white border border-gray-300 shadow-sm focus:border-indigo-500 focus:ring focus:ring-indigo-200 focus:ring-opacity-50"
        prop.onMouseDown (fun e -> e.stopPropagation ())
        prop.value value
        prop.onChange (fun (e: Browser.Types.Event) -> e.target?value |> string |> onChange)
        prop.children (
            Html.option [ prop.value "string"; prop.text "Select an option" ]
            :: (options
                |> List.map (fun opt -> Html.option [ prop.className "text-xs"; prop.value opt; prop.text opt ]))
        )
    ]


let rec options (dispatch: PageEditorMsg -> unit) (code: RenderingCode) (path: int list) (name: string) : ReactElement =
    match code with
    | HtmlElement _ -> Html.none
    // ElementOption(dispatch, name, code, path)
    | HtmlList _ -> Html.none
    //ListOption(dispatch, name, code, path)
    | HtmlObject(_) -> Html.none
    //SequenceOption(dispatch, name, code, path)
    | Hole _ -> Html.none
    | CustomWrapper(_) -> failwith "Not Implemented"
    | CustomElement(_) -> failwith "Not Implemented"

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



let ViewElement content =
    Html.div [ prop.className "text-white rounded"; prop.children [ content ] ]


let MsgOverview model dispatch = Html.div []
let MsgElement msgs dispatch = Html.div []
let UpdateOverview model dispatch = Html.div []
let UpdateElement updateFun dispatch = Html.div []