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
open Editor.Components.CustomRendering

// Contains option menu components for each type of rendering code
// Each component takes a dispatch function, the current code, and the path to the code in the tree
// The dispatch function is used to send messages to the parent component to update the specific code
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


[<ReactComponent>]
let ModelElement (json: Json) =
    let rec displayField (json: Json) =
        match json with
        | JObject obj ->
            Html.div [
                prop.className "ml-4 space-y-2"
                prop.children (
                    obj
                    |> Map.toList
                    |> List.map (fun (key, value) ->
                        Html.div [
                            prop.className "space-y-1"
                            prop.children [
                                Html.div [ prop.className "font-bold text-blue-400"; prop.text $"{key}:" ]
                                displayField value
                            ]
                        ])
                )
            ]
        | JArray arr ->
            Html.div [
                prop.className "ml-4 space-y-2"
                prop.children (
                    arr
                    |> List.mapi (fun i item ->
                        Html.div [
                            prop.className "flex flex-row space-x-2 items-start"
                            prop.children [
                                Html.div [ prop.className "font-bold text-green-400"; prop.text $"[{i}]:" ]
                                displayField item
                            ]
                        ])
                )
            ]
        | JString str -> Html.div [ prop.className "text-red-400"; prop.text $"\"{str}\"" ]
        | JNumber num -> Html.div [ prop.className "text-purple-400"; prop.text $"{num}" ]
        | JBool b -> Html.div [ prop.className "text-yellow-400"; prop.text (if b then "true" else "false") ]
        | JNull -> Html.div [ prop.className "text-gray-500 italic"; prop.text "null" ]


    Html.div [
        prop.className "font-mono bg-gray-900 text-white rounded"
        prop.children [
            Html.h3 [ prop.className "font-bold mb-4  text-white"; prop.text "JSON Model" ]
            displayField json
        ]
        prop.onMouseDown (fun e -> e.stopPropagation ())

    ]


let ViewElement code path json name dispatch =
    Html.div [
        prop.className "font-mono bg-gray-900 text-white rounded"
        prop.children [ renderingCodeToReactElement code path json name options dispatch ]
    ]


let MsgOverview model dispatch = Html.div []
let MsgElement msgs dispatch = Html.div []
let UpdateOverview model dispatch = Html.div []
let UpdateElement updateFun dispatch = Html.div []