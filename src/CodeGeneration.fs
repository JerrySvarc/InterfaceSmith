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
            "<" + tag + attrs+ "> " + json.ToString() + "</" + tag + ">"
    | HtmlList (numbered, codes) ->
        let tag = if numbered then "ol" else "ul"
        let codes =
            List.map (fun item -> "<li>" + generateCode item + "</li>") (codes) |> String.concat "\n"
        "<" + tag + ">\n" + codes + "</" + tag + ">"
    | Sequence (items) ->
        List.map (fun item -> generateCode item) (items) |> String.concat "\n"