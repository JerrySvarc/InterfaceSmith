module CoreLogic.Operations.DataRecognition

open CoreLogic.Types.RenderingTypes
open Fable.SimpleJson
open Microsoft.FSharp.Collections

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