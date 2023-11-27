module Selector

open Types
open Fable.SimpleJson

let rec select (json : Json) (selectors : Selector list) : Json list =
    let selector = selectors.Head
    match json with
    | JObject obj ->
        match selector with
        | FieldSelector name ->
            let success,field = obj.TryGetValue(name)
            match success with
            | true -> [field]
            | false -> failwith "Specified field not found"
        | AllChildrenSelector ->
            let names,vals  = List.unzip (Map.toList obj)
            vals
        | _ -> failwith "Object not indexed."
    | JNull -> failwith "Not Implemented"
    | _ -> failwith "Not Implemented"


let json = JObject(Map.ofList [("a", JString("b")); ("c", JString("d"))])