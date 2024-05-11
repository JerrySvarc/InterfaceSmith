module DataRecognition

open Types
open System
open Fable.SimpleJson
open Microsoft.FSharp.Collections

let recognizeJson (json: Json) =
    match json with
    | JArray array ->
        match array.IsEmpty with
        | true -> HtmlList(UnorderedList, None, Hole UnNamed)
        | false -> HtmlList(UnorderedList, None, Hole UnNamed)
    | JObject obj ->
        let jsonArray = obj |> Map.toList
        let codes = List.map (fun (key, value) -> Hole(Named key)) jsonArray
        Sequence(codes |> Array.ofList)
    | JNull -> Hole UnNamed
    | _ -> HtmlElement(Div, Attributes([]), Data)