module CoreLogic.Operations.CodeGeneration

open CoreLogic.Types.RenderingTypes
open CoreLogic.Operations.RenderingCode
open Editor.Types.PageEditorDomain

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

        const model = %s;

        %s

        const update = (msg, event, model) => {
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
                root.innerHTML = view(currentModel, dispatch);
            };
            window.dispatch = (msg, event) => {
                currentModel = update(msg, event, currentModel);
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
        (generateView "model" code)
(*"""


    const Msg = { NewMessage1: "NewMessage1",
    NewMessage2: "NewMessage2",
    NewMessage3: "NewMessage3",
    NewMessage4: "NewMessage4" };

    const model = {
"InputField": "",
"AddTodo": "Add todo",
"Todos": [
    {
        "text": "Complete project proposal",
        "completed": false
    }
],
"Others": {
    "CompletedCount": 0,
    "AllDoneButton": "All done"
}
}
;


        function functionExample(event, model) {
            console.log()
        }

    const update = (msg, event, model) => {
switch (msg) {
    case Msg.NewMessage1:
        return {
            ...model,
            InputField: event.target.value
        };

    case Msg.NewMessage2:
        if (!model.InputField.trim()) return model;
        return {
            ...model,
            InputField: "",
            Todos: [...model.Todos, {
                text: model.InputField.trim(),
                completed: false
            }]
        };

    case Msg.NewMessage3:
        const todoIndex = parseInt(event.target.closest('li').dataset.index);
        const updatedTodos = model.Todos.map((todo, index) =>
            index === todoIndex
                ? {...todo, completed: !todo.completed}
                : todo
        );
        return {
            ...model,
            Todos: updatedTodos,
            Others: {
                ...model.Others,
                CompletedCount: updatedTodos.filter(todo => todo.completed).length
            }
        };

    case Msg.NewMessage4:
        const allCompleted = model.Todos.map(todo => ({...todo, completed: true}));
        return {
            ...model,
            Todos: allCompleted,
            Others: {
                ...model.Others,
                CompletedCount: allCompleted.length
            }
        };

    default:
        return model;
}
};

const view = (model, dispatch) => `
<div>
    <input
        value="${model.InputField}"
        type="text"
        onblur="dispatch('${Msg.NewMessage1}', event)"
    />
    <button onclick="dispatch('${Msg.NewMessage2}', event)">${model.AddTodo}</button>
    <ul>${model.Todos.map((item, index) => `
        <li data-index="${index}">
            <div>
                <input
                    type="checkbox"
                    ${item.completed ? 'checked' : ''}
                    onchange="dispatch('${Msg.NewMessage3}', event)"
                />
                <span>${item.text}</span>
            </div>
        </li>`
    ).join('')}</ul>
    <div>
        <button onclick="dispatch('${Msg.NewMessage4}', event)">
            ${model.Others.AllDoneButton}
        </button>
        <div>${model.Others.CompletedCount}</div>
    </div>
</div>`;


    function init(initialModel, updateFunction, viewFunction) {
        let currentModel = initialModel;
        const render = () => {
            const root = document.getElementById("app");
            root.innerHTML = view(currentModel, dispatch);
        };
        window.dispatch = (msg,event) => {
            currentModel = update(msg, event, currentModel);
            render();
        };
        render();
    }

    init(model, update, view);

    """*)