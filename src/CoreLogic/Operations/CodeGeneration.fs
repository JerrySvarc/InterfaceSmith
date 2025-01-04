module CoreLogic.Operations.CodeGeneration

open CoreLogic.Types.RenderingTypes
open CoreLogic.Operations.RenderingCode
open Editor.Types.PageEditorDomain

/// <summary>Generates a JavaScript application using the Elm architecture based on the provided UI element structure, JSON data, and
/// custom functions and custom messages. The resulting application consists of a Model, update function, user defined Msgs, and the view functions.</summary>
/// <param name="code">The RenderingCode representing the UI elements.</param>
/// <param name="jsonString">Uploaded JSON data in its textual representation.</param>
/// <param name="customFunctions">Custom defined functions.</param>
/// <param name="updateFunction">THe update function consisting of messages and updates to the model based on the messages.</param>
/// <returns>Generated application in JavaScript following the Elm architecture. Attach to html with a HTML element with the id 'app'. </returns>
let generateJavaScript
    (code: RenderingCode)
    (jsonString: string)
    (customFunctions: Map<string, Javascript>)
    (updateFunction: UpdateFunction)
    : string =

    let escapeJsString (s: string) =
        s.Replace("\"", "\\\"").Replace("\n", "\\n")

    let messageTypes =
        updateFunction
        |> Map.toList
        |> List.map (fun (name, _) -> sprintf "%s: \"%s\"" name name)
        |> String.concat ",\n        "

    let customFunctionsJs =
        customFunctions
        |> Map.toList
        |> List.map (fun (name, JSFunction(_, body)) ->
            sprintf
                """
            function %s(event, model) {
                %s
            }"""
                name
                body)
        |> String.concat "\n"

    let updateCases =
        updateFunction
        |> Map.toList
        |> List.map (fun (msg, code) ->
            sprintf
                """
                case Msg.%s:
                    %s
                    """
                msg
                code)
        |> String.concat "\n"

    let generateAttributes (attrs: Attributes) (path: string) =
        attrs
        |> List.map (fun attr ->
            match attr.Value with
            | Data ->
                if attr.Key = "checked" then
                    sprintf "${%s ? 'checked' : ''}" path
                else
                    sprintf "%s=\"${%s}\"" attr.Key path
            | Constant s -> sprintf "%s=\"%s\"" attr.Key s
            | Empty -> "")
        |> String.concat " "

    let generateEventHandlers (handlers: (string * EventHandler) list) =
        handlers
        |> List.map (fun (event, handler) ->
            match handler with
            | MsgHandler msg -> sprintf "%s=\"window.dispatch(Msg.%s, event)\"" event msg
            | JsHandler name -> sprintf "%s=\"%s(event, model)\"" event name)
        |> String.concat " "

    let wrapInTag tag attrs events content =
        sprintf """<%s %s %s>%s</%s>""" tag attrs events content tag

    let rec generateView (path: string) (code: RenderingCode) =
        match code with
        | RenderingCode.HtmlElement(tag, attrs, value, handlers) ->
            let content =
                match value with
                | Empty -> ""
                | Data -> sprintf "${%s}" path
                | Constant s -> escapeJsString s

            if tag.IsSelfClosing then
                sprintf """<%s %s %s />""" tag.Name (generateAttributes attrs path) (generateEventHandlers handlers)
            else
                wrapInTag tag.Name (generateAttributes attrs path) (generateEventHandlers handlers) content

        | RenderingCode.HtmlList(listType, attrs, items, handlers) ->
            let itemTemplate = List.head items

            let listContent =
                sprintf
                    """${%s.map((item, index) => `
                        <li data-index="${index}">%s</li>`
                    ).join('')}"""
                    path
                    (generateView "item" itemTemplate)

            let tag = if listType = UnorderedList then "ul" else "ol"
            wrapInTag tag (generateAttributes attrs path) (generateEventHandlers handlers) listContent

        | RenderingCode.HtmlObject(objType, attrs, keys, codes, handlers) ->
            let children =
                keys
                |> List.map (fun key ->
                    match Map.tryFind key codes with
                    | Some code -> generateView (sprintf "%s.%s" path key) code
                    | None -> "")
                |> String.concat "\n"

            wrapInTag
                (objTypeToString objType)
                (generateAttributes attrs path)
                (generateEventHandlers handlers)
                children

        | RenderingCode.Hole(Named name) -> sprintf """<!-- HOLE: %s -->""" name
        | RenderingCode.Hole(UnNamed) -> """<!-- UNNAMED HOLE -->"""

    sprintf
        """
const Msg = { %s };

const Model = %s;

%s

const update = (msg, event, model) => {
    switch (msg) {
        %s
        default:
            return model;
    }
};

const view = (model, dispatch) => `%s`;

function startApplication(initialModel, updateFunction, viewFunction) {
    let currentModel = initialModel;
    const render = () => {
        const root = document.getElementById("app");
        root.innerHTML = viewFunction(currentModel, dispatch);
    };
    window.dispatch = (msg, event) => {
        currentModel = updateFunction(msg, event, currentModel);
        render();
    };
    render();
}

startApplication(Model, update, view);
        """
        messageTypes
        jsonString
        customFunctionsJs
        updateCases
        (generateView "model" code)