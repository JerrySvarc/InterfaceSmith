module CoreLogic.Operations.DataRecognition

open CoreLogic.Types.RenderingTypes
open Fable.SimpleJson
open Microsoft.FSharp.Collections

let recognizeJson (json: Json) =
    match json with
    | JArray array ->
        match array.IsEmpty with
        | true -> HtmlList(UnorderedList, [], [])
        | false ->
            let codes = List.map (fun item -> Hole(Named "List item")) array
            HtmlList(UnorderedList, codes, [])
    | JObject obj ->
        let keys = obj.Keys |> List.ofSeq
        let codes = obj |> Map.map (fun key value -> Hole(Named key))
        HtmlObject(ObjType.Empty, keys, codes, [])
    | JNull -> Hole UnNamed
    | _ -> HtmlElement(Div, [], Data, [])