module DataProcessing.DataLoading

open Types
open System
open Fable.SimpleJson

//fable alternative to System.Text.Json.JsonDocument
// the system.text namespace is not compatible with fable unfortunately
// simple parsing based on the documentation https://github.com/Zaid-Ajaj/Fable.SimpleJson
let loadJson json =
    json
    |> SimpleJson.tryParse
    |> function
        | Some(JObject dict) ->
            let value key = Map.tryFind key dict

            [ value "data" ]
            |> List.choose id
            |> function
                | [ JObject data ] -> Some((JObject) data)
                | _ -> None
        | _ -> None