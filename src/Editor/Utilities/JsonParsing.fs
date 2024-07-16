module Editor.Utilities.JsonParsing

open System
open Fable.SimpleJson

// simple parsing based on the documentation https://github.com/Zaid-Ajaj/Fable.SimpleJson
let loadJson json =
    json
    |> SimpleJson.tryParse
    |> function
        | Some(JObject dict) -> Some((JObject) dict)
        | _ -> None