module CodeGeneration
open Types
open Fable.SimpleJson


let rec combineDataAndCode (inputData : Json) (inputCode : RenderingCode) : RenderingCode =
    match inputData,inputCode with
    | JObject obj, Sequence(objects) ->
        let newCodes = List.map2 (fun (key, value) object -> ( combineDataAndCode value object)) (obj |> Map.toList) objects
        Sequence (newCodes )
    | JArray array, HtmlList(listType,numbered,data, code) ->
        HtmlList(listType,numbered, inputData, code)
    | JNull, _ -> Hole
    | _ , HtmlElement(tag, attrs, data) -> HtmlElement(tag, attrs, Data inputData)
    | _, _ -> Hole


let rec generateCode (code: RenderingCode) =
    match code with
    | Hole -> ""
    | HtmlElement(tag, attrs, data) ->
        let attrs = attrs |> List.map (fun (key, value) -> " " + key + "=\"" + value + "\"") |> String.concat ""
        let outputTag =
            match tag with
            | "" -> "p"
            | _ -> tag
        match data with
        | Constant text ->
            "<" + outputTag + attrs+ "> " + text + "</" + outputTag + ">"
        | Empty ->
            ""
        | Data json ->
            "<" + outputTag + attrs+ "> " + SimpleJson.toString json + "</" + outputTag + ">"
    | HtmlList(listType,numbered,data, code) ->
        match data with
        | JArray array ->
            let topTag =
                match listType with
                | List ->
                    if numbered then
                        "ol"
                    else
                        "ul"
                | Table -> "table"
            let itemTag =
                match listType with
                | List -> "li"
                | Table -> "td"
            let optionalTableTag =
                match listType with
                | List -> ""
                | Table -> "tr"
            let html : string =
                array
                |> List.map (fun item ->
                match item,code with
                | JObject obj, Sequence(objects) ->
                    let newCodes = List.map2 (fun (key, value) object -> ( combineDataAndCode value object)) (obj |> Map.toList) objects
                    let html = List.map (fun item -> "<" + itemTag + ">" + generateCode item + "</" + itemTag + ">") newCodes |> String.concat "\n"
                    if optionalTableTag = "" then
                        html
                    else
                        "<" + optionalTableTag + ">" + html + "</" + optionalTableTag + ">"
                | JArray array, HtmlList(listType,numbered,data, code) ->
                    let codes = List.map (fun item -> combineDataAndCode item code) array
                    let html = List.map (fun item -> "<" + itemTag + ">" + generateCode item + "</" + itemTag + ">") codes |> String.concat "\n"
                    "<" + optionalTableTag + ">" + html + "</" + optionalTableTag + ">"
                | JNull, _ -> ""
                | _ , _ ->
                    generateCode (combineDataAndCode item code)
                )
                |> String.concat "\n"
            "<" + topTag + ">\n" + html+ "</" + topTag + ">"
        | _ -> ""
    | Sequence (items) ->
        generateCode (HtmlElement("div",List.Empty , Constant(List.map (fun item -> generateCode item) (items) |> String.concat "\n")))
