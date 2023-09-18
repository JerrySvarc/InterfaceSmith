module DataRecognition

open Types
open System
open Fable.SimpleJson
open Microsoft.FSharp.Collections

//give this function JSON and it will return all rendering codes which can be used to display this JSON
let recognizeJson (json: Json) : RenderingCode =
    let rec parse (json : Json) =
        match json with
        | JArray array ->
            let itemCode =  Array.Parallel.map(fun value -> parse value) (array |> List.toArray)
            HtmlList(false, itemCode |> Array.toList)
        | JObject obj ->
            let jsonArray = obj |> Map.toArray
            let codes = Array.Parallel.map(fun (key, value) -> parse value) jsonArray
            Sequence(codes |> Array.toList)
        | JNull -> Hole
        | _ -> HtmlElement("", [], Data(json))
    parse json