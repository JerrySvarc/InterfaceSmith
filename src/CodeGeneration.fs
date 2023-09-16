module CodeGeneration
open Types
open Fable.SimpleJson
open System

//creates a string of HTML code from a RenderingCode
 let rec toHtml (code: RenderingCode) =
    match code with
    | HtmlElement (tag, attrs, innerText) ->
        let attrString =
            attrs
            |> List.map (fun (key, value) -> key + "=\"" + value.ToString() + "\"")
            |> String.concat " "
        "<" + tag + " " + attrString + ">" + innerText.ToString() + "</" + tag + ">\n"
    | HtmlList (numbered, innerData, itemCode) ->
        let innerDataString = innerData.ToString()
        let itemCodeString = toHtml itemCode
        "<ul>" + innerDataString + itemCodeString + "</ul>\n"
    | Sequence (items) ->
        items
        |> List.map (fun item -> toHtml item)
        |> String.concat "\n"
    | Hole -> ""