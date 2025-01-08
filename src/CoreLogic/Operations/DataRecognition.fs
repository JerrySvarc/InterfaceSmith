module CoreLogic.Operations.DataRecognition

open CoreLogic.Types.RenderingTypes
open Fable.SimpleJson
open Microsoft.FSharp.Collections

/// <summary>Recognition of the JSON data - Creates a new RenderingCode UI element and creates holes for its children if the item is a collection.</summary>
/// <param name="json">The JSON field to be recognized.</param>
/// <returns>A new RenderingCode element with its children as new Holes.</returns>
let recognizeJson (json: Json) =
    match json with
    | JArray array ->
        match array.IsEmpty with
        | true -> RenderingCode.HtmlList(UnorderedList, [], [], [])
        | false ->
            let codes = List.map (fun item -> RenderingCode.Hole(Named "List item")) array
            RenderingCode.HtmlList(UnorderedList, [], codes, [])
    | JObject obj ->
        let keys = obj.Keys |> List.ofSeq
        let codes = obj |> Map.map (fun key value -> RenderingCode.Hole(Named key))
        RenderingCode.HtmlObject(Div, [], keys, codes, [])
    | JNull -> RenderingCode.Hole UnNamed
    | _ -> RenderingCode.HtmlElement(Tags.div, [], Data, [])