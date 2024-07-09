module CodeGeneration

open System.Text
open Types.RenderingTypes
open Fable.SimpleJson
open Utilities.EditorUtils

let generateAttributeString (attrs: Attributes) =
    attrs
    |> List.map (fun (key, value) ->
        match value with
        | Data -> $"%s{key}=\"{{{{data}}}}\""
        | Constant str -> $"%s{key}=\"%s{str}\""
        | Empty -> key)
    |> String.concat " "

// Helper function to generate event handler string
let generateEventHandlerString (eventHandlers: (string * Javascript) list) =
    eventHandlers
    |> List.map (fun (event, JSFunction(name, code)) ->
        $"%s{event}=\"%s{name}(this)\"")
    |> String.concat " "

// Recursive function to generate HTML
let rec generateHtml (code: RenderingCode) : string =
    let sb = StringBuilder()

    match code with
    | HtmlElement(tag, attrs, innerValue, eventHandlers) ->
        let tagStr = tagToString tag
        let attrStr = generateAttributeString attrs
        let eventStr = generateEventHandlerString eventHandlers
        sb.Append($"<{tagStr} {attrStr} {eventStr}>") |> ignore

        match innerValue with
        | Data -> sb.Append("{{data}}") |> ignore
        | Constant str -> sb.Append(str) |> ignore
        | Empty -> ()

        sb.Append($"</{tagStr}>") |> ignore

    | HtmlList(listType, items, eventHandlers) ->
        let listTag = listTypeToString listType
        let eventStr = generateEventHandlerString eventHandlers
        sb.AppendLine($"<{listTag} {eventStr}>") |> ignore
        for item in items do
            sb.AppendLine($"  <li>{generateHtml item}</li>") |> ignore
        sb.Append($"</{listTag}>") |> ignore

    | HtmlObject(objType, keyOrdering, codes, eventHandlers) ->
        sb.AppendLine("<div>") |> ignore
        for key in keyOrdering do
            match Map.tryFind key codes with
            | Some code ->
                sb.AppendLine($"  <div data-key=\"{key}\">") |> ignore
                sb.AppendLine($"    {generateHtml code}") |> ignore
                sb.AppendLine("  </div>") |> ignore
            | None -> ()
        sb.Append("</div>") |> ignore

    | CustomWrapper wrapper ->
        let tagStr = tagToString wrapper.Tag
        let attrStr = generateAttributeString wrapper.Attributes
        let eventStr = generateEventHandlerString wrapper.EventHandlers
        sb.AppendLine($"<{tagStr} {attrStr} {eventStr}>") |> ignore
        sb.AppendLine(generateHtml wrapper.WrappedCode) |> ignore
        for child in wrapper.Children do
            //sb.AppendLine(generateHtml (CustomElement child)) |> ignore
        sb.Append($"</{tagStr}>") |> ignore

    | CustomElement element ->
        let tagStr = tagToString element.Tag
        let attrStr = generateAttributeString element.Attributes
        let eventStr = generateEventHandlerString element.EventHandlers
        sb.Append($"<{tagStr} {attrStr} {eventStr}>") |> ignore
        sb.Append(element.CustomInnerValue) |> ignore
        sb.Append($"</{tagStr}>") |> ignore

    | Hole fieldHole ->
        match fieldHole with
        | Named name -> sb.Append($"{{{{ {name} }}}}") |> ignore
        | UnNamed -> sb.Append("{{ }}") |> ignore

    sb.ToString()



// Function to generate JavaScript
let generateJavaScript (code: RenderingCode) : string =
    let sb = StringBuilder()

    let rec collectEventHandlers (code: RenderingCode) : (string * Javascript) list =
        match code with
        | HtmlElement(_, _, _, handlers)
        | HtmlList(_, _, handlers)
        | HtmlObject(_, _, _, handlers) -> handlers
        | CustomWrapper wrapper ->
            wrapper.EventHandlers //@
            //(wrapper.WrappedCode |> collectEventHandlers) @
           // (wrapper.Children |> List.collect (fun c -> c.EventHandlers))
        | CustomElement element -> element.EventHandlers
        | Hole _ -> []

    let handlers = collectEventHandlers code

    for (_, JSFunction(name, code)) in handlers do
        sb.AppendLine($"function {name}(element) {{") |> ignore
        sb.AppendLine(code) |> ignore
        sb.AppendLine("}") |> ignore

    sb.ToString()

// Function to generate all code
let generateCode (code: RenderingCode) =
    let html = generateHtml code
    let js = generateJavaScript code
    (html, js)
