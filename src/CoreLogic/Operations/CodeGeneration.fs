module CoreLogic.Operations.CodeGeneration

open System.Text
open CoreLogic.Types.RenderingTypes
open CoreLogic.Operations.RenderingCode
open Fable.SimpleJson

let generateAttributeString (attrs: Attributes) =
    attrs
    |> List.map (fun (key, value) ->
        match value with
        | Data -> $"%s{key}=\"{{{{data}}}}\""
        | Constant str -> $"%s{key}=\"%s{str}\""
        | Empty -> key)
    |> String.concat " "

let generateEventHandlerString (eventHandlers: (string * Javascript) list) =
    eventHandlers
    |> List.map (fun (event, JSFunction(name, _)) -> $"%s{event}=\"{name}(this)\"")
    |> String.concat " "

let rec generateHtml (code: RenderingCode) (json: Json) : string =
    let sb = StringBuilder()

    match code, json with
    | HtmlElement(tag, attrs, innerValue, eventHandlers), _ ->
        let tagStr = tagToString tag
        let attrStr = generateAttributeString attrs
        let eventStr = generateEventHandlerString eventHandlers
        sb.Append($"<{tagStr} {attrStr} {eventStr}>") |> ignore

        match innerValue with
        | Data ->
            match json with
            | JString str -> sb.Append(str) |> ignore
            | _ -> sb.Append(Json.serialize json) |> ignore
        | Constant str -> sb.Append(str) |> ignore
        | Empty -> ()

        sb.Append($"</{tagStr}>") |> ignore

    | HtmlList(listType, items, eventHandlers), JArray jsonArray ->
        let listTag = listTypeToString listType
        let eventStr = generateEventHandlerString eventHandlers
        sb.AppendLine($"<{listTag} {eventStr}>") |> ignore

        for item, jsonItem in List.zip items jsonArray do
            sb.AppendLine($"  <li>{generateHtml item jsonItem}</li>") |> ignore

        sb.Append($"</{listTag}>") |> ignore

    | HtmlObject(objType, keyOrdering, codes, eventHandlers), JObject jsonObj ->
        sb.AppendLine("<div>") |> ignore

        for key in keyOrdering do
            match Map.tryFind key codes, Map.tryFind key jsonObj with
            | Some code, Some jsonValue ->
                sb.AppendLine($"  <div data-key=\"{key}\">") |> ignore
                sb.AppendLine($"    {generateHtml code jsonValue}") |> ignore
                sb.AppendLine("  </div>") |> ignore
            | _ -> ()

        sb.Append("</div>") |> ignore

    | CustomWrapper wrapper, _ ->
        let tagStr = tagToString wrapper.Tag
        let attrStr = generateAttributeString wrapper.Attributes
        let eventStr = generateEventHandlerString wrapper.EventHandlers
        sb.AppendLine($"<{tagStr} {attrStr} {eventStr}>") |> ignore
        sb.AppendLine(generateHtml wrapper.WrappedCode json) |> ignore

        for child in wrapper.Children do
            sb.AppendLine(generateHtml child json) |> ignore

        sb.Append($"</{tagStr}>") |> ignore

    | CustomElement element, _ ->
        let tagStr = tagToString element.Tag
        let attrStr = generateAttributeString element.Attributes
        let eventStr = generateEventHandlerString element.EventHandlers
        sb.Append($"<{tagStr} {attrStr} {eventStr}>") |> ignore
        sb.Append(element.CustomInnerValue) |> ignore
        sb.Append($"</{tagStr}>") |> ignore

    | Hole fieldHole, _ ->
        match fieldHole with
        | Named name -> sb.Append($"{{{{ {name} }}}}") |> ignore
        | UnNamed -> sb.Append("{{ }}") |> ignore

    | _, _ ->
        // Fallback for mismatched structures
        sb.Append(Json.serialize json) |> ignore

    sb.ToString()

let generateJavaScript (code: RenderingCode) : string =
    let sb = StringBuilder()

    let rec collectEventHandlers (code: RenderingCode) : (string * Javascript) list =
        match code with
        | HtmlElement(_, _, _, handlers)
        | HtmlList(_, _, handlers)
        | HtmlObject(_, _, _, handlers) -> handlers
        | CustomWrapper wrapper ->
            wrapper.EventHandlers @
            (collectEventHandlers wrapper.WrappedCode) @
            (wrapper.Children |> List.collect collectEventHandlers)
        | CustomElement element -> element.EventHandlers
        | Hole _ -> []

    let handlers = collectEventHandlers code

    for (_, JSFunction(name, code)) in handlers do
        sb.AppendLine($"function {name}(element) {{") |> ignore
        sb.AppendLine(code) |> ignore
        sb.AppendLine("}") |> ignore

    sb.ToString()

let generateCode (code: RenderingCode) (json: Json) =
    let html = generateHtml code json
    let js = generateJavaScript code
    (html, js)