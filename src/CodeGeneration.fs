module CodeGeneration
open Types
open Fable.SimpleJson

let rec generateCode (code: RenderingCode) =
    match code with
    | Hole -> ""
    | HtmlElement(tag, attrs, data) ->
        let attrs = attrs |> List.map (fun (key, value) -> " " + key + "=\"" + value + "\"") |> String.concat ""
        match data with
        | Constant text ->
            "<" + tag + attrs+ "> " + text + "</" + tag + ">"
        | Empty ->
            "<" + tag + attrs+ "/>"
        | Data json ->
            "<" + tag + attrs+ "> " + SimpleJson.toString json + "</" + tag + ">"
    | HtmlList (numbered, codes) ->
        let tag = if numbered then "ol" else "ul"
        let code =
            codes
            |> List.map (fun item ->
            match item with
            | Sequence (subItems) ->
                let subCodes = List.map (fun subItem -> "<li>" + generateCode subItem + "</li>") subItems |> String.concat "\n"
                "<div>\n" + subCodes + "</div>"
            | _ ->  "<li>" + generateCode item + "</li>"
            )
            |> String.concat "\n"
        "<" + tag + ">\n" + code + "</" + tag + ">"
    | Sequence (items) ->
         generateCode (HtmlElement("div",List.Empty , Constant(List.map (fun item -> generateCode item) (items) |> String.concat "\n")))