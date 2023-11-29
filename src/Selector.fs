module Selector

open Types
open Fable.SimpleJson

let rec select (json : Json) (selectors : Selector list) : Json list =
    match selectors with
    | [] -> json::[]
    | head::tail ->
        match head, json with
        | FieldSelector name, JObject obj ->
            let success,field = obj.TryGetValue(name)
            match success with
            | true -> select field tail
            | false -> failwith "Specified field not found"
        | ArraySelector index, JArray array ->
            let item = List.tryItem index array
            match item with
            | Some i -> select i tail
            | None -> failwith "Specified field not found"
        | AllChildrenSelector , _->
            match json with
            | JObject obj ->
                let names,vals  = List.unzip (Map.toList obj)
                vals |> List.collect (fun v -> select v tail)
            | JArray arr ->
                arr |> List.collect (fun v -> select v tail)
            | _ -> failwith "Not Implemented"
        | _ , _ -> failwith "Not Implemented"

let json = JObject(Map.ofList [("a", JObject(Map.ofList [("a", JString("b")); ("c", JString("d"))])); ("c", JString("d"))])