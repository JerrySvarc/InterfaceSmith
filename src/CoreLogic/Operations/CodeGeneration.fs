module CoreLogic.Operations.CodeGeneration

open CoreLogic.Types.RenderingTypes
open Fable.SimpleJson
open CoreLogic.Operations.RenderingCode
let rec generateHtmlStructure (code: RenderingCode) (path: string) : string =
    match code with
    | HtmlElement (tag, attrs, innerValue, handlers) ->
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
        sprintf "<%s %s %s>%s</%s>" (tagToString tag) attrString handlerString content (tagToString tag)
    | HtmlList (listType, items, _) ->
        let listTag = listTypeToString listType
        sprintf "<%s>${%s.map((item, index) => `%s`).join('')}</%s>"
            listTag
            path
            (generateHtmlStructure (List.head items) "item")
            listTag
    | HtmlObject (_, keys, codes, _) ->
        keys
        |> List.map (fun key ->
            match Map.tryFind key codes with
            | Some code -> sprintf "<div data-key=\"%s\">%s</div>" key (generateHtmlStructure code (sprintf "%s.%s" path key))
            | None -> "")
        |> String.concat "\n"
    | CustomWrapper wrapper ->
        sprintf "<div class=\"custom-wrapper\">%s</div>"
            (generateHtmlStructure wrapper.WrappedCode path)
    | CustomElement element ->
        sprintf "<div class=\"custom-element\">%s</div>" element.CustomInnerValue
    | Hole _ ->
        "<!-- Hole -->"

let generateRenderFunction (code: RenderingCode) : string =
        let printFormatToString =
            sprintf """
    function renderApp(data) {
      const app = document.getElementById('app');
      app.innerHTML = `
    %s
      `;
    }
    """ (generateHtmlStructure code "data")

let generateJavaScript (code: RenderingCode) (customHandlers: Map<string, Javascript>) : string =
    let customFunctions =
        customHandlers
        |> Map.toList
        |> List.map (fun (name, JSFunction(_, body)) ->
            sprintf """
        function %s(event, data) {
        %s
          renderApp(data);
        }""" name body)
                |> String.concat "\n\n"

    let renderFunction = generateRenderFunction code

    let initCode = """
document.addEventListener('DOMContentLoaded', function() {
  const data = JSON.parse(document.getElementById('appData').textContent);
  renderApp(data);
});
"""

    String.concat "\n\n" [customFunctions; renderFunction; initCode]

let generateFullHtml (code: RenderingCode) (json: Json) (customHandlers: Map<string, Javascript>) : string =
    let jsonString = Json.serialize json
    let js = generateJavaScript code customHandlers

    sprintf """
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Generated Data-Driven App</title>
    <script src="https://cdn.tailwindcss.com"></script>
</head>
<body class="bg-gray-100 p-4">
    <div id="app" class="max-w-md mx-auto bg-white p-6 rounded-lg shadow-md"></div>
    <script id="appData" type="application/json">
%s
    </script>
    <script>
%s
    </script>
</body>
</html>
""" jsonString

let generateCode (code: RenderingCode) (json: Json) (customHandlers: Map<string, Javascript>) : string * string =
    let fullHtml = generateFullHtml code json customHandlers
    let js = generateJavaScript code customHandlers
    (fullHtml, js)