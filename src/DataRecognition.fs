module DataRecognition

open Types
open System
open Fable.SimpleJson
open Microsoft.FSharp.Collections

let recognizeJson (json: Json) =
    match json with
    | JArray array ->
        match array.IsEmpty with
        | true -> HtmlList(List, false, Hole [], [AllChildrenSelector])
        | false ->
            HtmlList(List, false, Hole [], [])
    | JObject obj ->
        let jsonArray = obj |> Map.toList
        let codes = List.map (fun (key, value) ->Hole [FieldSelector key]) jsonArray
        Sequence(codes)
    | JNull -> Hole []
    | _ -> HtmlElement("div", [], Data(AllChildrenSelector))
