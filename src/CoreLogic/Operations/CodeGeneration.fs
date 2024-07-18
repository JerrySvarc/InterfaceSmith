module CoreLogic.Operations.CodeGeneration

open CoreLogic.Types.RenderingTypes
open Fable.SimpleJson
open CoreLogic.Operations.RenderingCode

let indentCode (code: string) (indentLevel: int) =
    let indent = String.replicate indentLevel "  "
    code.Split('\n')
    |> Array.map (fun line -> if System.String.IsNullOrWhiteSpace(line) then line else indent + line)
    |> String.concat "\n"

let rec generateHtmlStructure (code: RenderingCode) (path: string) (indentLevel: int): string =
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

        let handlerString =
            handlers
            |> List.map (fun (event, JSFunction(name, _)) -> sprintf "%s=\"%s(event, data)\"" event name)
            |> String.concat " "

        let content =
            match innerValue with
            | Data -> sprintf "${%s}" path
            | Constant str -> str
            | Empty -> ""

        sprintf "%s<%s %s %s>%s</%s>" indent (tagToString tag) attrString handlerString content (tagToString tag)

    | HtmlList(listType, items, _) ->
        let listTag = listTypeToString listType
        sprintf "%s<%s>${%s.map((item, index) => `\n%s`).join('')\n%s}</%s>"
            indent
            listTag
            path
            (indentCode (generateHtmlStructure (List.head items) "item" (indentLevel + 1)) 1)
            indent
            listTag

    | HtmlObject(_, keys, codes, _) ->
        keys
        |> List.map (fun key ->
            match Map.tryFind key codes with
            | Some code ->
                sprintf "%s<div data-key=\"%s\">\n%s\n%s</div>"
                    indent
                    key
                    (generateHtmlStructure code (sprintf "%s.%s" path key) (indentLevel + 1))
                    indent
            | None -> "")
        |> String.concat "\n"

    | Hole _ -> sprintf "%s<!-- Hole -->" indent

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
            ] |> String.concat "\n")
        |> String.concat "\n"

    let renderFunction =
        [
            "function renderApp(data) {"
            "  const app = document.getElementById('app');"
            "  app.innerHTML = `"
            indentCode (generateHtmlStructure code "data" 0) 2
            "  `;"
            "}"
            ""
        ] |> String.concat "\n"

    let initCode =
        [
            "document.addEventListener('DOMContentLoaded', function() {"
            "  const data = JSON.parse(document.getElementById('appData').textContent);"
            "  renderApp(data);"
            "});"
        ] |> String.concat "\n"

    [ customFunctions; renderFunction; initCode ]
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
        "    <script src=\"https://cdn.tailwindcss.com\"></script>"
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
    ] |> String.concat "\n"

let generateCode (code: RenderingCode) (json: string) (customHandlers: Map<string, Javascript>) : string * string =
    let fullHtml = generateFullHtml code json customHandlers
    let js = generateJavaScript code customHandlers
    (fullHtml, js)