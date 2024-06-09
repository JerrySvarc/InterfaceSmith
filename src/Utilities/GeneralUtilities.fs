module Utilities.GeneralUtilities

open Feliz
open Types


open Fable.SimpleJson
let uiBlock (content: ReactElement list) =
    Html.div [
        prop.className "flex bg-white border border-gray-300 p-6 rounded-lg shadow-md justify-center items-center"
        prop.children content
    ]

let uiBox (content: ReactElement list) =
    Html.div [
        prop.className "flex bg-white border border-gray-300 p-6 rounded-lg shadow-md"
        prop.children content
    ]

let rec prettyPrintJson (json: Json) (indent: string) : ReactElement =
    match json with
    | JArray arr ->
        Html.div [
            prop.children (arr |> List.map (fun item -> prettyPrintJson item (indent + "  ")))
        ]
    | JObject(obj) ->
        Html.div [
            prop.children (
                obj
                |> Map.toList
                |> List.map (fun (key, value) ->
                    Html.div [
                        prop.children [ Html.text (indent + key + ": "); prettyPrintJson value (indent + "  ") ]
                    ])
            )
        ]
    | JNull -> Html.text "null"
    | JBool b -> Html.text (string b)
    | JNumber n -> Html.text (string n)
    | JString s -> Html.text ("\"" + s + "\"")



let prettyPrint (json: Json) = prettyPrintJson json " "