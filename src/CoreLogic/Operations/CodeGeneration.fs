module CoreLogic.Operations.CodeGeneration

open CoreLogic.Types.RenderingTypes
open Fable.SimpleJson
open CoreLogic.Operations.RenderingCode

let indentCode (code: string) (indentLevel: int) =
    let indent = String.replicate indentLevel "  "

    code.Split('\n')
    |> Array.map (fun line ->
        if System.String.IsNullOrWhiteSpace(line) then
            line
        else
            indent + line)
    |> String.concat "\n"

let rec generateHtmlStructure
    (code: RenderingCode)
    (path: string)
    (indentLevel: int)
    : string * (string * string * string) list =
    let indent = String.replicate indentLevel "  "

    match code with
    | HtmlElement(tag, attrs, innerValue, handlers) ->
        let attrString =
            attrs
            |> List.map (fun (k, v) ->
                match v with
                | Data -> sprintf "%s=\"${%s}\"" k path
                | Constant str -> sprintf "%s=\"%s\"" k str
                | Empty -> k)
            |> String.concat " "

        let (idAttr, eventHandlers) =
            if not (List.isEmpty handlers) then
                let handlerName =
                    handlers |> List.head |> snd |> (fun (JSFunction(name, _)) -> name)

                let elementId = sprintf "%sElement" handlerName

                (sprintf "id=\"%s\"" elementId,
                 handlers
                 |> List.map (fun (event, JSFunction(name, _)) -> (elementId, event, name)))
            else
                ("", [])

        let content =
            match innerValue with
            | Data -> sprintf "${%s}" path
            | Constant str -> str
            | Empty -> ""

        let htmlString =
            sprintf "%s<%s %s %s>%s</%s>" indent (tagToString tag) attrString idAttr content (tagToString tag)

        (htmlString, eventHandlers)

    | HtmlList(listType, items, handlers) ->
        let listTag = listTypeToString listType

        let (idAttr, eventHandlers) =
            if not (List.isEmpty handlers) then
                let handlerName =
                    handlers |> List.head |> snd |> (fun (JSFunction(name, _)) -> name)

                let elementId = sprintf "%sElement" handlerName

                (sprintf "id=\"%s\"" elementId,
                 handlers
                 |> List.map (fun (event, JSFunction(name, _)) -> (elementId, event, name)))
            else
                ("", [])

        let (itemHtml, itemHandlers) =
            generateHtmlStructure (List.head items) "item" (indentLevel + 1)

        let htmlString =
            sprintf
                "%s<%s %s>${%s.map((item, index) => `\n%s\n%s`).join('')}</%s>"
                indent
                listTag
                idAttr
                path
                (indentCode itemHtml 1)
                indent
                listTag

        (htmlString, eventHandlers @ itemHandlers)

    | HtmlObject(_, keys, codes, handlers) ->
        let (idAttr, eventHandlers) =
            if not (List.isEmpty handlers) then
                let handlerName =
                    handlers |> List.head |> snd |> (fun (JSFunction(name, _)) -> name)

                let elementId = sprintf "%sElement" handlerName

                (sprintf "id=\"%s\"" elementId,
                 handlers
                 |> List.map (fun (event, JSFunction(name, _)) -> (elementId, event, name)))
            else
                ("", [])

        let (objectContent, childHandlers) =
            keys
            |> List.map (fun key ->
                match Map.tryFind key codes with
                | Some code ->
                    let (childHtml, childEventHandlers) =
                        generateHtmlStructure code (sprintf "%s.%s" path key) (indentLevel + 1)

                    (sprintf "%s<div data-key=\"%s\">\n%s\n%s</div>" indent key childHtml indent, childEventHandlers)
                | None -> ("", []))
            |> List.unzip

        let htmlString =
            sprintf "%s<div %s>\n%s\n%s</div>" indent idAttr (String.concat "\n" objectContent) indent

        (htmlString, eventHandlers @ List.concat childHandlers)

    | Hole _ -> (sprintf "%s<!-- Hole -->" indent, [])

let generateJavaScript (code: RenderingCode) (customHandlers: Map<string, Javascript>) : string =
    let customFunctions =
        customHandlers
        |> Map.toList
        |> List.map (fun (name, JSFunction(_, body)) ->
            [
                sprintf "function %s(event, data) {" name
                indentCode body 1
                "  renderApp(data);"
                "}"
                ""
            ]
            |> String.concat "\n")
        |> String.concat "\n"

    let (htmlString, eventHandlers) = generateHtmlStructure code "data" 0

    let renderFunction =
        [
            "function renderApp(data) {"
            "  const app = document.getElementById('app');"
            "  app.innerHTML = `"
            indentCode htmlString 2
            "  `;"
            "  setupEventListeners(data);"
            "}"
            ""
        ]
        |> String.concat "\n"

    let setupEventListeners =
        [ "function setupEventListeners(data) {" ]
        @ (eventHandlers
           |> List.map (fun (elementId, event, handlerName) ->
               sprintf "  const %s = document.getElementById('%s');" elementId elementId
               + sprintf
                   "\n  if (%s) %s.addEventListener('%s', (event) => %s(event, data));"
                   elementId
                   elementId
                   (event.ToLower().Substring(2))
                   handlerName))
        @ [ "}"; "" ]
        |> String.concat "\n"

    let initCode =
        [
            "let appData;"
            ""
            "document.addEventListener('DOMContentLoaded', function() {"
            "  appData = JSON.parse(document.getElementById('appData').textContent);"
            "  renderApp(appData);"
            "});"
        ]
        |> String.concat "\n"

    [ customFunctions; renderFunction; setupEventListeners; initCode ]
    |> String.concat "\n\n"


let generateFullHtml (code: RenderingCode) (jsonString: string) (customHandlers: Map<string, Javascript>) : string =
    let js = generateJavaScript code customHandlers

    [
        "<!DOCTYPE html>"
        "<html lang=\"en\">"
        "<head>"
        "    <meta charset=\"UTF-8\">"
        "    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">"
        "    <title>Generated Data-Driven App</title>"
        "</head>"
        "<body class=\"bg-gray-100 p-4\">"
        "    <div id=\"app\" class=\"max-w-md mx-auto bg-white p-6 rounded-lg shadow-md\"></div>"
        "    <script id=\"appData\" type=\"application/json\">"
        indentCode jsonString 2
        "    </script>"
        "    <script>"
        indentCode js 2
        "    </script>"
        "</body>"
        "</html>"
    ]
    |> String.concat "\n"

// Generates the full HTML and JavaScript code for the given RenderingCode and JSON data
let generateCode (code: RenderingCode) (json: string) (customHandlers: Map<string, Javascript>) : string * string =
    let fullHtml = generateFullHtml code json customHandlers
    let js = generateJavaScript code customHandlers
    (fullHtml, js)