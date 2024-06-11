module Utilities.GeneralUtilities

open Feliz
open Types
open Fable.React
open Fable.Core.JsInterop
open Fable.SimpleJson



let prettyPrint (json: Json) =
    Html.pre [
        prop.className "bg-gray-200 border-gray-400 p-4 m-1"
        prop.children [ Html.text (Json.stringify (json)) ]
    ]