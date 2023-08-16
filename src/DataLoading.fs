module DataLoading
open Types
open System
open Fable.SimpleJson

let exampleData = @"
{
    ""data"": {
        ""tasks"": [
            {
                ""id"": 1,
                ""task"": ""Complete project proposal"",
                ""completed"": false
            },
            {
                ""id"": 2,
                ""task"": ""Prepare presentation slides"",
                ""completed"": true
            },
            {
                ""id"": 3,
                ""task"": ""Send meeting agenda to team"",
                ""completed"": false
            }
        ]
    }
}"

//fable alternative to System.Text.Json.JsonDocument
// the system.text namespace is not compatible with fable unfortunately
// simple parsing based on the documentation https://github.com/Zaid-Ajaj/Fable.SimpleJson
let loadJson json=
    json
    |> SimpleJson.tryParse
    |> function
        | Some (JObject dict) ->
            let value key = Map.tryFind key dict
            [value "data"]
            |> List.choose id
            |> function
                | [JObject data]  ->
                    Some data
                | _ -> None
        | _ -> None
