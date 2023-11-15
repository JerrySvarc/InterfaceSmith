module DataRecognition

open Types
open System
open Fable.SimpleJson
open Microsoft.FSharp.Collections

//give this function JSON and it will return all rendering codes which can be used to display this JSON
let recognizeJson (json: Json) : RenderingCode =
    let parse (json: Json) =
        match json with
        | JArray array ->
            match array.IsEmpty with
            | true -> HtmlList(List, false, json, Hole json)
            | false ->
                HtmlList(List, false, json, Hole(json))
        | JObject obj ->
            let jsonArray = obj |> Map.toArray
            let codes = Array.map (fun (key, value) ->Hole value) jsonArray
            Sequence(codes |> Array.toList)
        | JNull -> Hole JNull
        | _ -> HtmlElement("", [], Data(Hole json) )
    parse json
