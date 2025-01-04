module Editor.Components.ElementComponents

open Editor.Types
open Fable.React
open CoreLogic.Types.RenderingTypes
open Editor.Types.PageEditorDomain
open CoreLogic.Operations.RenderingCode
open CoreLogic.Types.RenderingTypes
open Feliz
open Browser.Types
open Fable.Core.JsInterop
open Microsoft.FSharp.Reflection
open Editor.Utilities.Icons
open Editor.Types.EditorDomain
open Fable.SimpleJson
open CoreLogic.Types
open System.Reflection
open Editor.Components.OptionsComponents
open Editor.CustomRendering
open Editor.Utilities.JavaScriptEditor
open CoreLogic.Operations.CodeGeneration

let Collapsible =
    React.functionComponent
        (fun
            (props:
                {|
                    title: string
                    children: ReactElement list
                |}) ->
            let (isExpanded, setIsExpanded) = React.useState false

            let handleToggle _ = setIsExpanded (not isExpanded)

            Html.div [
                Html.div [
                    prop.className "cursor-pointer text-blue-500 font-bold flex items-center"
                    prop.onClick handleToggle
                    prop.children [
                        if isExpanded then
                            ReactBindings.React.createElement (
                                chevronDown,
                                createObj [ "size" ==> 16; "color" ==> "#FFFFFF" ],
                                []
                            )
                        else
                            ReactBindings.React.createElement (
                                chevronRight,
                                createObj [ "size" ==> 16; "color" ==> "#FFFFFF" ],
                                []
                            )
                        Html.span [ prop.className "ml-1"; prop.text props.title ]
                    ]
                ]
                if isExpanded then
                    Html.div [ prop.className "ml-4 mt-1 space-y-1"; prop.children props.children ]
            ])

[<ReactComponent>]
let ModelElement model dispatch =
    let rec displayField (json: Json) : ReactElement =
        match json with
        | JObject obj ->
            let entries =
                obj
                |> Map.toList
                |> List.map (fun (key, value) ->
                    Html.div [
                        Html.span [ prop.className "font-bold"; prop.text $"{key}: " ]
                        displayField value
                    ])

            Collapsible {|
                title = "Object"
                children = entries
            |}

        | JArray arr ->
            let items =
                arr
                |> List.mapi (fun index value ->
                    Html.div [
                        Html.span [ prop.className "font-bold"; prop.text $"[{index}]: " ]
                        displayField value
                    ])

            Collapsible {|
                title = $"[ List of {List.length arr} items ]"
                children = items
            |}

        | JString str -> Html.span [ prop.className "text-red-400"; prop.text $"\"{str}\"" ]
        | JNumber num -> Html.span [ prop.className "text-purple-400"; prop.text $"{num}" ]
        | JBool b -> Html.span [ prop.className "text-yellow-400"; prop.text (if b then "true" else "false") ]
        | JNull -> Html.span [ prop.className "text-gray-500 italic"; prop.text "null" ]

    Html.div [
        prop.className "bg-gray-900 text-white rounded w-56"
        prop.children [
            Html.h3 [ prop.className "font-bold mb-4 text-white"; prop.text "Uploaded JSON data" ]
            displayField model.PageData.ParsedJson
        ]
        prop.onMouseDown (fun e -> e.stopPropagation ())
    ]

[<ReactComponent>]
let SandboxPreviewView (model: PageEditorModel) dispatch =
    let js =
        generateJavaScript
            model.PageData.CurrentTree
            model.PageData.JsonString
            model.PageData.CustomFunctions
            model.PageData.UpdateFunction

    let fullHtml =
        $"""
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Sandbox Preview</title>
</head>
<body>
    <div id="app"></div>
    <script>
    {js}
    </script>
</body>
</html>
        """

    if model.IsPreviewOpen then
        Html.div [
            prop.className "w-full h-full border"
            prop.children [
                Html.iframe [
                    prop.className "w-full h-full"
                    prop.src "about:blank"
                    prop.custom ("sandbox", "allow-scripts allow-same-origin allow-forms allow-modals")
                    prop.custom ("srcDoc", fullHtml)
                ]
            ]
        ]
    else
        Html.none

[<ReactComponent>]
let ViewElement model dispatch =
    let showOptions, setShowOptions = React.useState true
    let toggleOptions () = setShowOptions (not showOptions)

    Html.div [
        prop.onMouseDown (fun e -> e.stopPropagation ())
        prop.className "bg-white  p-4 border border-gray-300 shadow-md"
        prop.style [
            style.resize.both
            style.overflow.auto
            style.whitespace.normal
            style.overflowWrap.normal
            style.wordWrap.normal
            style.width 800
            style.height 600
            style.minWidth (length.px 200)
            style.minHeight (length.px 200)
            style.maxWidth (length.percent 100)
            style.maxHeight (length.percent 100)
        ]
        let renderContext = {
            Options = options
            Dispatch = dispatch
            Path = []
            Json = model.PageData.ParsedJson
            Name = "Uploaded Data"
            CustomFunctions = model.PageData.CustomFunctions
            ShowOptions = showOptions
            UserMessages = model.PageData.UserMessages
        }

        prop.children [
            Html.div [
                prop.className
                    "bg-gray-100 border border-gray-300 rounded-lg p-2 shadow-lg flex items-center justify-between mb-2"
                prop.children [
                    Html.button [
                        prop.onClick (fun _ -> toggleOptions ())
                        prop.text "Toggle Options"
                        prop.className
                            "bg-blue-600 text-white text-sm px-4 py-2 rounded shadow-md hover:bg-blue-500 transition-all"
                    ]

                    Html.button [
                        prop.onClick (fun _ -> dispatch TogglePreview)
                        if model.IsPreviewOpen then
                            prop.text "Stop application"

                            prop.className
                                "bg-red-600 text-white text-sm px-4 py-2 rounded shadow-md hover:bg-red-500 transition-all"

                        else
                            prop.text "Run application"

                            prop.className
                                "bg-green-600 text-white text-sm px-4 py-2 rounded shadow-md hover:bg-green-500 transition-all"
                    ]
                ]
            ]
            if not model.IsPreviewOpen then
                renderingCodeToReactElement renderContext model.PageData.CurrentTree
            else
                SandboxPreviewView model dispatch
        ]
    ]

[<ReactComponent>]
let JavaScriptEditorView code onChange =
    let extensions = [| javascript?javascript () |]

    Html.div [
        prop.style [ style.height (length.percent 100); style.width (length.percent 100) ]
        prop.className "border-solid border-2 border-black"
        prop.onMouseDown (fun e -> e.stopPropagation ())
        prop.children [
            ReactBindings.React.createElement (
                CodeMirror,
                createObj [
                    "value" ==> code
                    "extensions" ==> extensions
                    "theme" ==> "light"
                    "height" ==> "100%"
                    "style" ==> {| height = "100%"; width = "100%" |}
                    "onChange" ==> onChange
                ],
                []
            )
        ]
    ]



[<ReactComponent>]
let private EditableListElement<'a>
    (title: string)
    (items: Map<string, 'a>)
    (renderEditor: string * 'a -> ReactElement)
    (dispatch: 'msg -> unit)
    (onNew: unit -> unit)
    (onRename: string * string -> unit)
    (onDelete: string -> unit)
    =

    let (selectedItem, setSelectedItem) = React.useState<string option> None
    let (isRenaming, setIsRenaming) = React.useState false
    let (newName, setNewName) = React.useState ""

    let itemNames =
        if not items.IsEmpty then
            items |> Map.keys |> Seq.toList
        else
            []

    Html.div [
        prop.className "bg-white p-4 border border-gray-300 shadow-md"
        prop.style [
            style.resize.both
            style.overflow.hidden
            style.minWidth (length.px 200)
            style.minHeight (length.px 200)
            style.maxWidth (length.percent 100)
            style.maxHeight (length.percent 100)
            style.width (length.px 400)
            style.height (length.px 600)
            style.display.flex
            style.flexDirection.column
        ]
        prop.onMouseDown (fun e -> e.stopPropagation ())
        prop.children [
            Html.h1 title
            Html.div [
                prop.style [ style.flexShrink 0; style.marginBottom (length.px 8) ]
                prop.className "flex items-center gap-2"
                prop.children [
                    if isRenaming && selectedItem.IsSome then
                        Html.input [
                            prop.type' "text"
                            prop.className "text-xs px-2 py-1 border border-black"
                            prop.value newName
                            prop.onChange setNewName
                            prop.onKeyDown (fun e ->
                                if e.key = "Enter" then
                                    onRename (selectedItem.Value, newName.Trim())
                                    setIsRenaming false
                                elif e.key = "Escape" then
                                    setIsRenaming false)
                        ]
                    else
                        SelectMenu
                            itemNames
                            (selectedItem |> Option.defaultValue (sprintf "Select %s" title))
                            (fun value ->
                                if value = sprintf "Select %s" title then
                                    setSelectedItem None
                                else
                                    setSelectedItem (Some value))

                    Html.button [
                        prop.className "text-xs px-2 py-1 bg-white border border-black hover:bg-gray-50"
                        prop.text (sprintf "New %s" title)
                        prop.onClick (fun _ -> onNew ())
                    ]

                    match selectedItem with
                    | Some item when not isRenaming ->
                        React.fragment [
                            Html.button [
                                prop.className "text-xs px-2 py-1 bg-white border border-black hover:bg-gray-50"
                                prop.text "Rename"
                                prop.onClick (fun _ ->
                                    setNewName item
                                    setIsRenaming true)
                            ]
                            Html.button [
                                prop.className "text-xs px-2 py-1 bg-red-500 text-white hover:bg-red-600"
                                prop.text (sprintf "Delete %s" title)
                                prop.onClick (fun _ ->
                                    onDelete item
                                    setSelectedItem None)
                            ]
                        ]
                    | _ -> Html.none
                ]
            ]

            match selectedItem with
            | Some name when Map.containsKey name items -> renderEditor (name, items[name])
            | _ ->
                Html.div [
                    prop.className "flex items-center justify-center h-full text-gray-500"
                    prop.text (sprintf "Select %s to edit" title)
                ]
        ]
    ]

[<ReactComponent>]
let FunctionsElement (functions: Map<string, Javascript>) dispatch =
    EditableListElement
        "Custom Functions"
        functions
        (fun (name, jsFunc) ->
            Html.div [
                prop.style [ style.flexGrow 1; style.height (length.percent 100) ]
                prop.children [
                    match jsFunc with
                    | JSFunction(_, code) ->
                        JavaScriptEditorView code (fun newCode ->
                            dispatch (UpdateFunction(name, JSFunction(name, newCode))))
                ]
            ])
        dispatch
        (fun () -> dispatch CreateFunction)
        (fun (oldName, newName) -> dispatch (RenameFunction(oldName, newName)))
        (fun name -> dispatch (DeleteFunction name))

[<ReactComponent>]
let MessageAndUpdateElement (messages: string list) (updateFunction: Map<string, string>) dispatch =
    EditableListElement
        "Messages"
        (messages
         |> List.map (fun m -> m, Map.tryFind m updateFunction |> Option.defaultValue "return { ...model };")
         |> Map.ofList)
        (fun (msg, code) ->
            Html.div [
                prop.style [
                    style.flexGrow 1
                    style.height (length.percent 100)
                    style.minHeight (length.px 200)
                    style.overflow.hidden
                ]
                prop.children [
                    JavaScriptEditorView code (fun newCode -> dispatch (ModifyUpdateMessage(msg, newCode)))
                ]
            ])
        dispatch
        (fun () ->
            let newMsg = sprintf "NewMessage%d" (messages.Length + 1)
            dispatch (AddMsg newMsg))
        (fun (oldName, newName) -> dispatch (RenameMsg(oldName, newName)))
        (fun msg -> dispatch (DeleteMsg msg))