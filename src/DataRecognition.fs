module DataRecognition
open Types
open System
open Fable.SimpleJson



//give this function JSON and it will return all rendering codes which can be used to display this JSON
let rec componentOptions (json : Json) : RenderingCode  =
    match json with
    | JArray array ->
        let arrayElements =
            List.map (fun item -> componentOptions item) array
        HtmlList(false, Data(json), arrayElements)
    | JObject object ->
        let sequenceElements =
            Map.map (fun key item -> componentOptions item) object
            |> Map.toSeq
            |> Seq.map snd
            |> List.ofSeq
        Sequence(sequenceElements)
    | JNull -> Hole
    | _ ->
        HtmlElement("", [], Reference(Data(json)))