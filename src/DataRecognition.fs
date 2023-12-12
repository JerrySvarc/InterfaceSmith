module DataRecognition

open Types
open System
open Fable.SimpleJson
open Microsoft.FSharp.Collections

let recognizeJson (json: Json) : (RenderingCode * Json) list =
    match json with
    | JArray array ->
        match array.IsEmpty with
        | true -> [(HtmlList(List, false, Hole UnNamed ), JArray [])]
        | false -> [(HtmlList(List, false, Hole UnNamed ), JArray array)]
    | JObject obj ->
        let jsonArray = obj |> Map.toList
        let codes = List.map (fun (key, value) -> (Hole (Named key), value)) jsonArray
        codes
    | JNull -> [(Hole UnNamed, JNull)]
    | _ -> [(HtmlElement("div", [], Data), json)]