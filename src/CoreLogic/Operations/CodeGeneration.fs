module CoreLogic.Operations.CodeGeneration

open CoreLogic.Types.RenderingTypes
open Fable.SimpleJson
open CoreLogic.Operations.RenderingCode
open Editor.Types.PageEditorDomain


let generateJavaScript
    (code: RenderingCode)
    (jsonString: string)
    (customFunctions: Map<string, Javascript>)
    (updateFunction: UpdateFunction)
    : string =

    let messageTypes =
        updateFunction
        |> Map.toList
        |> List.map (fun (name, msgCode) -> sprintf "%s: \"%s\"" name name)
        |> String.concat "\n        "


    let customFunctionsJs =
        customFunctions
        |> Map.toList
        |> List.map (fun (name, JSFunction(_, body)) -> sprintf "function %s(event, model) {\n    %s\n}" name body)
        |> String.concat "\n"


    let updateCases =
        updateFunction
        |> Map.toList
        |> List.map (fun (msg, code) ->
            sprintf
                """case Msg.%s:
                    %s //update the model like this { ...model, counter: model.counter + 1 }"""
                msg
                code)
        |> String.concat "\n"


    let generateAttributes (attrs: Attributes) (path: string) =
        attrs
        |> List.map (fun attr ->
            match attr.Value with
            | Data -> sprintf "%s=\"${model.%s.%s}\"" attr.Key path attr.Key
            | Constant s -> sprintf "%s=\"%s\"" attr.Key s
            | Empty -> "")
        |> String.concat " "

    let generateEventHandlers (handlers: (string * EventHandler) list) =
        handlers
        |> List.map (fun (event, handler) ->
            match handler with
            | MsgHandler msg -> sprintf "%s=\"window.dispatch(Msg.%s)\"" event msg
            | JsHandler(name) ->
                sprintf
                    "%s=\"%s(event, model)\""
                    event
                    (customFunctions[name]
                     |> function
                         | JSFunction(_, _) -> name))
        |> String.concat " "

    let wrapInTag tag attrs events content =
        sprintf "<%s %s %s>%s</%s>" tag attrs events content tag

    let rec generateView (path: string) (code: RenderingCode) =
        match code with
        | RenderingCode.HtmlElement(tag, attrs, value, handlers) ->
            match value with
            | Empty -> ""
            | Data ->
                wrapInTag
                    tag.Name
                    (generateAttributes attrs path)
                    (generateEventHandlers handlers)
                    (if tag.IsSelfClosing then
                         ""
                     else
                         (sprintf "${model.%s}" path))
            | Constant s -> wrapInTag tag.Name (generateAttributes attrs path) (generateEventHandlers handlers) s

        | RenderingCode.HtmlList(listType, attrs, items, handlers) ->
            let itemsHtml =
                items
                |> List.mapi (fun i item -> generateView (sprintf "%s[%d]" path i) item)
                |> String.concat "\n"

            let tag = if listType = UnorderedList then "ul" else "ol"
            wrapInTag tag (generateAttributes attrs path) (generateEventHandlers handlers) itemsHtml

        | RenderingCode.HtmlObject(objType, attrs, keys, codes, handlers) ->
            let children =
                keys
                |> List.map (fun key ->
                    match Map.tryFind key codes with
                    | Some code -> generateView (if path = "" then key else sprintf "%s.%s" path key) code
                    | None -> "")
                |> String.concat "\n"

            wrapInTag
                (objTypeToString objType)
                (generateAttributes attrs path)
                (generateEventHandlers handlers)
                children
        | RenderingCode.Hole(Named name) -> $"<!--  HOLE for field {name} ------>"
        | RenderingCode.Hole(UnNamed) -> "<!--Unnamed HOLE------>"

    sprintf
        """
        const Msg = { %s };

        const model = %s;

        %s

        const update = (msg, model) => {
            switch (msg) {
                %s
                default:
                    return model;
            }
        };

        const view = (model, dispatch) => `%s`;

        function init(initialModel, updateFunction, viewFunction) {
            let currentModel = initialModel;

            const render = () => {
                const root = document.getElementById("app");
                root.innerHTML = viewFunction(currentModel, dispatch);
            };

            window.dispatch = (msg) => {
                currentModel = updateFunction(msg, currentModel);
                render();
            };

            render();
        }

        init(model, update, view);
        """
        messageTypes
        jsonString
        customFunctionsJs
        updateCases
        (generateView "" code)