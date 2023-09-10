module DataRecognition

open Types
open System
open Fable.SimpleJson

//give this function JSON and it will return all rendering codes which can be used to display this JSON
let fieldToCode (json: Json) : RenderingCode =
    match json with
    | JArray array ->
        HtmlList(false, Data(json), Hole)
    | JObject object ->
        Sequence([Hole])
    | JNull -> Hole
    | _ -> HtmlElement("", [], Reference(Data(json)))

