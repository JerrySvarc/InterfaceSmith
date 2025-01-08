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

/// <summary> A collapsible element that can be used to display JSON data </summary>
[<ReactComponent>]
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

/// <summary> A canvas element that displays the uploaded JSON data, featuring collapsible fields. </summary>
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

/// <summary> The preview of the created application, displayed in an iframe. </summary>
[<ReactComponent>]
let SandboxPreviewView (model: PageEditorModel) dispatch =
    let fullHtml =
        generateFullApp
            model.PageData.CurrentTree
            model.PageData.JsonString
            model.PageData.CustomFunctions
            model.PageData.UpdateFunction

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

/// <summary>The main resizible view element. Used to provide the incremental creation functionality + live preview + serves as the sandbox preview element. </summary>
/// <param name="model">The PageEditor's state.</param>
/// <param name="dispatch">PageEditor dispatch function of (PageEditorMsg -> unit).</param>
/// <returns>The main view canvase element for creating, editing and previewing created Page elements.</returns>
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

/// <summary>Provides the CodeMirror editor for writing the custom JavaScript code. Some light extensions added.  </summary>
/// <param name="code">The editor's default code value.</param>
/// <param name="onChange">A Callback function to change the code value.</param>
/// <returns>The CodeMirror editor window.</returns>
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



/// <summary>A general element for creating and editing code of specified elements.
/// </summary>
/// <param name="title">The name of what we want to edit.</param>
/// <param name="items">The options which we choose to edit. </param>
/// <param name="renderEditor">Render the editing window, for us most likely the JavaScriptEditorView. </param>
/// <param name="dispatch">PageEditor dispatch function of (PageEditorMsg -> unit).</param>
/// <param name="onNew">A Callback funtion to create a new list element.</param>
/// <param name="onRename">A Callback funtion to rename the selected list element.</param>
/// <param name="onDelete">A Callback funtion to delete the selected list element.</param>
/// <returns></returns>
[<ReactComponent>]
let private CodeBasedListElement<'a>
    (title: string)
    (items: Map<string, 'a>)
    (renderEditor: string * 'a -> ReactElement)
    (dispatch: PageEditorMsg -> unit)
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

/// <summary>The canvas element used to create, modify and delete custom functions. Uses the CodeBasedListElement.</summary>
/// <param name="functions">The custom functions.</param>
/// <param name="dispatch">PageEditor dispatch function of (PageEditorMsg -> unit).</param>
/// <returns></returns>
[<ReactComponent>]
let FunctionsElement (functions: Map<string, Javascript>) (dispatch: PageEditorMsg -> unit) =
    CodeBasedListElement
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

/// <summary>The canvas element used to create, modify and delete custom Elm-style messages. Uses the CodeBasedListElement.</summary>
/// <param name="messages">The created messages.</param>
/// <param name="updateFunction">The update function consisting of the messages and corresponding JavaScript code.</param>
/// <param name="dispatch">PageEditor dispatch function of (PageEditorMsg -> unit).</param>
/// <returns></returns>
[<ReactComponent>]
let MessageAndUpdateElement (messages: string list) (updateFunction: Map<string, string>) dispatch =
    CodeBasedListElement
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