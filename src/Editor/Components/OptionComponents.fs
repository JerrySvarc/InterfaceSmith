module Editor.Components.OptionComponents

open Editor.Types
open Fable.React
open CoreLogic.Types.RenderingTypes
open Editor.Types.PageEditorDomain
open CoreLogic.Operations.RenderingCode
open Feliz
open Browser.Types
open Fable.Core.JsInterop
open Microsoft.FSharp.Reflection
open Editor.Utilities.Icons
open Editor.Types.EditorDomain

// Contains option menu components for each type of rendering code
// Each component takes a dispatch function, the current code, and the path to the code in the tree
// The dispatch function is used to send messages to the parent component to update the specific code

[<ReactComponent>]
let EventHandlerMenu
    (dispatch: PageEditorMsg -> unit, code: RenderingCode, path: int list, customHandlers: Map<string, Javascript>)
    =
    let availableEvents = [ "onClick"; "onMouseOver"; "onMouseOut"; "onKeyPress"; "onFocus"; "onBlur" ]

    let (selectedEvent, setSelectedEvent) = React.useState ""
    let (selectedHandler, setSelectedHandler) = React.useState ""
    let (errorMessage, setErrorMessage) = React.useState ""

    let addHandler eventName handlerName =
        try
            let newHandler =
                match Map.tryFind handlerName customHandlers with
                | Some handler -> handler
                | None ->
                    setErrorMessage (sprintf "Handler '%s' not found in custom handlers" handlerName)
                    JSFunction(handlerName, "")

            let newCode =
                match code with
                | HtmlElement(tag, attrs, innerValue, handlers) ->
                    HtmlElement(tag, attrs, innerValue, (eventName, newHandler) :: handlers)
                | HtmlList(listType, items, handlers) -> HtmlList(listType, items, (eventName, newHandler) :: handlers)
                | HtmlObject(objType, keys, elements, handlers) ->
                    HtmlObject(objType, keys, elements, (eventName, newHandler) :: handlers)
                | Hole _ ->
                    setErrorMessage "Cannot add handler to a Hole"
                    code

            dispatch (ReplaceCode(newCode, path))
            setSelectedEvent ""
            setSelectedHandler ""
            setErrorMessage ""
        with ex ->
            setErrorMessage (sprintf "Error adding handler: %s" ex.Message)

    let removeHandler eventName =
        let newCode =
            match code with
            | HtmlElement(tag, attrs, innerValue, handlers) ->
                HtmlElement(tag, attrs, innerValue, List.filter (fun (name, _) -> name <> eventName) handlers)
            | HtmlList(listType, items, handlers) ->
                HtmlList(listType, items, List.filter (fun (name, _) -> name <> eventName) handlers)
            | HtmlObject(objType, keys, elements, handlers) ->
                HtmlObject(objType, keys, elements, List.filter (fun (name, _) -> name <> eventName) handlers)
            | Hole _ -> code

        dispatch (ReplaceCode(newCode, path))

    let getHandlers =
        match code with
        | HtmlElement(_, _, _, handlers) -> handlers
        | HtmlList(_, _, handlers) -> handlers
        | HtmlObject(_, _, _, handlers) -> handlers
        | Hole _ -> []

    Html.div [
        prop.className "mb-4"
        prop.children [
            Html.h3 [ prop.className "text-xs font-medium mb-2"; prop.text "Event Handlers" ]
            if not (System.String.IsNullOrEmpty errorMessage) then
                Html.div [
                    prop.className "bg-red-100 border-l-4 border-red-500 text-red-700 p-2 mb-2"
                    prop.text errorMessage
                ]
            Html.div [
                prop.className "flex "
                prop.children [
                    Html.select [
                        prop.className "flex-grow p-2 border border-gray-300 rounded-md"
                        prop.value selectedEvent
                        prop.onChange (fun (e: Browser.Types.Event) ->
                            setSelectedEvent (e.target?value |> string)
                            setSelectedHandler "")
                        prop.children (
                            Html.option [ prop.value ""; prop.text "Select an event" ]
                            :: (availableEvents
                                |> List.filter (fun e -> not (getHandlers |> List.exists (fun (name, _) -> name = e)))
                                |> List.map (fun e -> Html.option [ prop.value e; prop.text e ]))
                        )
                    ]
                    Html.select [
                        prop.className "flex-grow p-2 border border-gray-300 rounded-md"
                        prop.value selectedHandler
                        prop.onChange (fun (e: Browser.Types.Event) -> setSelectedHandler (e.target?value |> string))
                        prop.children (
                            Html.option [ prop.value ""; prop.text "Select a handler" ]
                            :: (customHandlers
                                |> Map.toList
                                |> List.map (fun (name, _) -> Html.option [ prop.value name; prop.text name ]))
                        )
                    ]
                    Html.button [
                        prop.className "bg-blue-500 text-white rounded p-2"
                        prop.onClick (fun _ ->
                            if selectedEvent <> "" && selectedHandler <> "" then
                                addHandler selectedEvent selectedHandler
                            else
                                setErrorMessage "Please select both an event and a handler")
                        prop.text "Add Handler"
                    ]
                ]
            ]
            Html.div [
                prop.className "space-y-2"
                prop.children (
                    getHandlers
                    |> List.map (fun (eventName, handler) ->
                        Html.div [
                            prop.key eventName
                            prop.className "flex justify-between items-center bg-gray-100 p-2 rounded"
                            prop.children [
                                Html.span [ prop.text (sprintf "%s: %s" eventName (handler.ToString())) ]
                                Html.button [
                                    prop.className "text-red-500"
                                    prop.onClick (fun _ -> removeHandler eventName)
                                    prop.text "Remove"
                                ]
                            ]
                        ])
                )
            ]
        ]
    ]

let SelectMenu (options: string list) (value: string) (onChange: string -> unit) =
    Html.select [
        prop.className
            "w-full p-2 bg-white border border-gray-300 rounded-md shadow-sm focus:border-indigo-500 focus:ring focus:ring-indigo-200 focus:ring-opacity-50"
        prop.onMouseDown (fun e -> e.stopPropagation ())
        prop.value value
        prop.onChange (fun (e: Browser.Types.Event) -> e.target?value |> string |> onChange)
        prop.children (
            Html.option [ prop.value ""; prop.text "Select an option" ]
            :: (options |> List.map (fun opt -> Html.option [ prop.value opt; prop.text opt ]))
        )
    ]

let ErrorDisplay (message: string) =
    Html.div [
        prop.className "bg-red-100 border-l-4 border-red-500 text-red-700 p-4 rounded flex items-center space-x-2"
        prop.children [ Html.span [ prop.className "font-medium"; prop.text message ] ]
    ]

[<ReactComponent>]
let TagMenu (dispatch, code: RenderingCode, path) =
    let tagOptions =
        FSharpType.GetUnionCases(typeof<Tag>)
        |> Array.map (fun caseInfo -> caseInfo.Name)
        |> Array.toList

    match code with
    | HtmlElement(tag, attrs, value, handlers) ->
        Html.div [
            prop.className "mb-4"
            prop.children [
                Html.label [
                    prop.className "block text-sm font-medium text-gray-700 mb-1"
                    prop.htmlFor "tag-select"
                    prop.text "Element Tag"
                ]
                Html.select [
                    prop.id "tag-select"
                    prop.className
                        "w-full p-2 bg-white border border-gray-300 rounded-md shadow-sm focus:border-indigo-500 focus:ring focus:ring-indigo-200 focus:ring-opacity-50"
                    prop.onMouseDown (fun e -> e.stopPropagation ())
                    prop.value (tag.ToString())
                    prop.onChange (fun (e: Browser.Types.Event) ->
                        let selectedTag = e.target?value |> string
                        let newTag = selectedTag.ToLower() |> stringToTag
                        dispatch (ReplaceCode(HtmlElement(newTag, attrs, value, handlers), path)))
                    prop.children (
                        [ Html.option [ prop.value ""; prop.text "Select tag" ] ]
                        @ (tagOptions
                           |> List.map (fun tag -> Html.option [ prop.value tag; prop.text tag ]))
                    )
                ]
            ]
        ]
    | _ -> ErrorDisplay "Invalid code type for TagMenu"

[<ReactComponent>]
let InnerValueMenu (dispatch, currentInnerValue: InnerValue, code: RenderingCode, path) =
    let innerValueOptions = [ "Data"; "Constant"; "Empty" ]

    let constantValue, setConstantValue =
        React.useState (
            match currentInnerValue with
            | Constant str -> str
            | _ -> ""
        )

    let innerValueToString (iv: InnerValue) =
        match iv with
        | Data -> "Data"
        | Constant _ -> "Constant"
        | Empty -> "Empty"

    let updateInnerValue newValue =
        match code with
        | HtmlElement(tag, attrs, _, handlers) ->
            dispatch (ReplaceCode(HtmlElement(tag, attrs, newValue, handlers), path))
        | _ -> ()

    match code with
    | HtmlElement(tag, attrs, _, handlers) ->
        Html.div [
            prop.className "mb-4"
            prop.children [
                Html.label [
                    prop.className "block text-sm font-medium text-gray-700 mb-1"
                    prop.htmlFor "inner-value-select"
                    prop.text "Inner Value"
                ]
                Html.div [
                    prop.className "flex space-x-2"
                    prop.children [
                        Html.div [
                            prop.className "flex-grow"
                            prop.children [
                                SelectMenu
                                    innerValueOptions
                                    (currentInnerValue |> innerValueToString)
                                    (fun selectedValue ->
                                        let newValue =
                                            match selectedValue with
                                            | "Data" -> InnerValue.Data
                                            | "Constant" -> Constant constantValue
                                            | "Empty" -> InnerValue.Empty
                                            | _ -> currentInnerValue

                                        updateInnerValue newValue)
                            ]
                        ]
                        if innerValueToString currentInnerValue = "Constant" then
                            Html.input [
                                prop.type' "text"
                                prop.value constantValue
                                prop.onChange (fun (value: string) ->
                                    setConstantValue value
                                    updateInnerValue (Constant value))
                                prop.className
                                    "flex-grow p-2 border border-gray-300 rounded-md shadow-sm focus:border-indigo-500 focus:ring focus:ring-indigo-200 focus:ring-opacity-50"
                            ]
                    ]
                ]
            ]
        ]
    | _ -> ErrorDisplay "Invalid code type for InnerValueMenu"

[<ReactComponent>]
let AttributeItem (name: string, value: InnerValue, onUpdate: InnerValue -> unit, onDelete: unit -> unit) =
    let (localValue, setLocalValue) = React.useState value

    let updateValue newValue =
        setLocalValue newValue
        onUpdate newValue

    Html.div [
        prop.className "flex items-center space-x-2 mb-2"
        prop.children [
            Html.input [
                prop.className "bg-gray-100 border rounded p-1 flex-grow overflow-auto w-16"
                prop.value name
                prop.readOnly true
            ]
            Html.select [
                prop.className "border rounded p-1"
                prop.value (
                    match localValue with
                    | Constant _ -> "constant"
                    | Data -> "data"
                    | Empty -> "empty"
                )
                prop.onChange (fun v ->
                    match v with
                    | "constant" -> updateValue (Constant "")
                    | "data" -> updateValue Data
                    | "empty" -> updateValue Empty
                    | _ -> ())
                prop.children [
                    Html.option [ prop.value "constant"; prop.text "Constant" ]
                    Html.option [ prop.value "data"; prop.text "Data" ]
                    Html.option [ prop.value "empty"; prop.text "Empty" ]
                ]
            ]
            match localValue with
            | Constant v ->
                Html.input [
                    prop.className "border rounded p-1 flex-grow"
                    prop.value v
                    prop.onChange (fun v -> updateValue (Constant v))
                ]
            | _ -> Html.none
            Html.button [
                prop.className "bg-red-500 text-white rounded p-1"
                prop.onClick (fun _ -> onDelete ())
                prop.children [ Html.text "Delete" ]
            ]
        ]
    ]

[<ReactComponent>]
let NewAttributeForm (onAdd: string * InnerValue -> unit) =
    let (name, setName) = React.useState ""
    let (value, setValue) = React.useState (Constant "")

    let addAttribute () =
        if not (System.String.IsNullOrWhiteSpace name) then
            onAdd (name, value)
            setName ""
            setValue (Constant "")

    Html.div [
        prop.className "flex items-center space-x-2 mt-4"
        prop.children [
            Html.input [
                prop.className "border rounded p-1 flex-grow"
                prop.placeholder "New attribute name"
                prop.value name
                prop.onChange setName
            ]
            Html.select [
                prop.className "border rounded p-1"
                prop.value (
                    match value with
                    | Constant _ -> "constant"
                    | Data -> "data"
                    | Empty -> "empty"
                )
                prop.onChange (fun v ->
                    match v with
                    | "constant" -> setValue (Constant "")
                    | "data" -> setValue Data
                    | "empty" -> setValue Empty
                    | _ -> ())
                prop.children [
                    Html.option [ prop.value "constant"; prop.text "Constant" ]
                    Html.option [ prop.value "data"; prop.text "Data" ]
                    Html.option [ prop.value "empty"; prop.text "Empty" ]
                ]
            ]
            match value with
            | Constant v ->
                Html.input [
                    prop.className "border rounded p-1 flex-grow"
                    prop.placeholder "Value"
                    prop.value v
                    prop.onChange (fun v -> setValue (Constant v))
                ]
            | _ -> Html.none
            Html.button [
                prop.className "bg-green-500 text-white rounded p-1"
                prop.onClick (fun _ -> addAttribute ())
                prop.text "Add"
            ]
        ]
    ]

[<ReactComponent>]
let AttributeMenu (dispatch: PageEditorMsg -> unit, code: RenderingCode, path: int list) =
    match code with
    | HtmlElement(tag, attrs, innerValue, handlers) ->
        let updateAttribute name newValue =
            let updatedAttrs =
                attrs
                |> List.map (fun (attrName, attrValue) ->
                    if attrName = name then
                        (attrName, newValue)
                    else
                        (attrName, attrValue))

            dispatch (ReplaceCode(HtmlElement(tag, updatedAttrs, innerValue, handlers), path))

        let deleteAttribute name =
            let updatedAttrs = attrs |> List.filter (fun (attrName, _) -> attrName <> name)
            dispatch (ReplaceCode(HtmlElement(tag, updatedAttrs, innerValue, handlers), path))

        let addAttribute (name, value) =
            let updatedAttrs = attrs @ [ (name, value) ]
            dispatch (ReplaceCode(HtmlElement(tag, updatedAttrs, innerValue, handlers), path))

        Html.div [
            prop.className "mb-4"
            prop.children [
                Html.h3 [ prop.className "text-lg font-medium mb-2"; prop.text "Attributes" ]
                for (name, value) in attrs do
                    AttributeItem(name, value, (updateAttribute name), (fun () -> deleteAttribute name))
                NewAttributeForm addAttribute
            ]
        ]
    | _ -> Html.none

[<ReactComponent>]
let ElementOption (dispatch, name: string, code, path, page: Page) =
    let (collapsed, setCollapsed) = React.useState false

    let toggleCollapse () = setCollapsed (not collapsed)

    match code with
    | HtmlElement(tag, attrs, innerValue, handlers) ->
        Html.div [
            prop.className "bg-white rounded-lg shadow-sm border border-gray-200 overflow-auto"
            prop.children [
                Html.div [
                    prop.className "flex items-center justify-between p-4 bg-gray-50 cursor-pointer"
                    prop.onClick (fun _ -> toggleCollapse ())
                    prop.children [
                        Html.h2 [ prop.className "text-lg font-semibold text-gray-700"; prop.text name ]
                        let icon = if collapsed then chevronRight else chevronDown
                        ReactBindings.React.createElement (icon, createObj [ "size" ==> 20; "color" ==> "#4B5563" ], [])
                    ]
                ]
                if not collapsed then
                    Html.div [
                        prop.className "p-4 space-y-4"
                        prop.children [
                            TagMenu(dispatch, code, path)
                            InnerValueMenu(dispatch, innerValue, code, path)
                            AttributeMenu(dispatch, code, path)
                            EventHandlerMenu(dispatch, code, path, page.CustomHandlers)
                        ]
                    ]
            ]
        ]
    | _ -> ErrorDisplay "Invalid code type for ElementOption"

[<ReactComponent>]
let ListOption (dispatch, name: string, code, path, page: Page) =
    let (collapsed, setCollapsed) = React.useState true

    match code with
    | HtmlList(listType, elementCode, handlers) ->
        let listTypeOptions =
            FSharpType.GetUnionCases(typeof<ListType>)
            |> Array.map (fun caseInfo -> caseInfo.Name)
            |> Array.toList

        Html.div [
            prop.className "bg-white rounded-lg shadow-sm border border-gray-200 overflow-hidden"
            prop.children [
                Html.div [
                    prop.className "flex items-center justify-between p-2 bg-gray-50 cursor-pointer"
                    prop.onClick (fun _ -> setCollapsed (not collapsed))
                    prop.children [
                        Html.span [ prop.className "text-sm font-medium text-gray-700"; prop.text name ]
                        let icon = if collapsed then chevronRight else chevronDown
                        ReactBindings.React.createElement (icon, createObj [ "size" ==> 20; "color" ==> "#4B5563" ], [])
                    ]
                ]
                if not collapsed then
                    Html.div [
                        prop.className "p-2 space-y-2"
                        prop.children [
                            Html.div [
                                prop.className "flex items-center space-x-2"
                                prop.children [
                                    Html.label [
                                        prop.children [
                                            SelectMenu
                                                listTypeOptions
                                                (listTypeToString listType)
                                                (fun selectedListType ->
                                                    let newListType = selectedListType |> stringToListType

                                                    dispatch (
                                                        ReplaceCode(HtmlList(newListType, elementCode, handlers), path)
                                                    ))
                                        ]
                                    ]
                                ]
                            ]
                            EventHandlerMenu(dispatch, code, path, page.CustomHandlers)
                        ]
                    ]
            ]
        ]
    | _ -> ErrorDisplay "Invalid code type for ListOption"



//Allows for reordering keys inside a sequence
let moveKey (keys: string list) (index: int) (direction: int) =
    if index + direction >= 0 && index + direction < List.length keys then
        let item = List.item index keys
        let newKeys = List.removeAt index keys |> List.insertAt (index + direction) item
        newKeys
    else
        keys

let renderTypeSelector (objType, dispatch, keys, elements, handlers, path) =
    Html.div [
        prop.className "flex items-center space-x-2"
        prop.children [
            Html.label [
                prop.className "text-xs font-medium text-gray-700"
                prop.htmlFor "obj-type-select"
                prop.text "Type:"
            ]
            Html.select [
                prop.id "obj-type-select"
                prop.className "flex-grow p-1 text-xs border border-gray-300 rounded-md"
                prop.value (objTypeToString objType)
                prop.onChange (fun (e: Browser.Types.Event) ->
                    let newObjType = e.target?value |> string |> stringToObjType
                    dispatch (ReplaceCode(HtmlObject(newObjType, keys, elements, handlers), path)))
                prop.children (
                    FSharpType.GetUnionCases(typeof<Tag>)
                    |> Array.map (fun case -> Html.option [ prop.value case.Name; prop.text case.Name ])
                )
            ]
        ]
    ]

let renderKey (key: string, i, keys, objType, elements, handlers, dispatch, path) =
    Html.div [
        prop.key key
        prop.className "flex items-center justify-between mb-1"
        prop.children [
            Html.span [ prop.className "text-xs text-gray-600"; prop.text key ]
            Html.div [
                prop.className "flex space-x-1"
                prop.children [
                    Html.button [
                        let isDisabled = i = 0
                        prop.disabled isDisabled
                        prop.className "p-1 text-gray-500 hover:text-gray-700 disabled:text-gray-300"

                        prop.onClick (fun _ ->
                            let newKeys = moveKey keys i -1
                            dispatch (ReplaceCode(HtmlObject(objType, newKeys, elements, handlers), path)))

                        prop.children [
                            ReactBindings.React.createElement (arrowUpIcon, createObj [ "size" ==> 12 ], [])
                        ]
                    ]
                    Html.button [
                        let isDisabled = i = List.length keys - 1
                        prop.disabled isDisabled
                        prop.className "p-1 text-gray-500 hover:text-gray-700 disabled:text-gray-300"

                        prop.onClick (fun _ ->
                            let newKeys = moveKey keys i 1
                            dispatch (ReplaceCode(HtmlObject(objType, newKeys, elements, handlers), path)))

                        prop.children [
                            ReactBindings.React.createElement (arrowDownIcon, createObj [ "size" ==> 12 ], [])
                        ]
                    ]
                ]
            ]
        ]
    ]

let renderKeys (keys, objType, elements, handlers, dispatch, path) =
    Html.div [
        prop.className "mt-2"
        prop.children (
            [
                Html.h4 [ prop.className "text-xs font-medium text-gray-700 mb-1"; prop.text "Keys:" ]
            ]
            @ (keys
               |> List.mapi (fun i key -> renderKey (key, i, keys, objType, elements, handlers, dispatch, path))
               |> List.ofSeq)
        )
    ]

let renderDeleteButton (dispatch, name, path) =
    Html.button [
        prop.className
            "w-full mt-2 px-2 py-1 bg-red-500 hover:bg-red-600 text-white text-xs font-medium rounded-md focus:outline-none focus:ring-2 focus:ring-red-500 focus:ring-opacity-50 flex items-center justify-center space-x-1"
        prop.onClick (fun _ -> dispatch (ReplaceCode(Hole(Named name), path)))
        prop.children [
            ReactBindings.React.createElement (trashIcon, createObj [ "size" ==> 12; "color" ==> "#FFFFFF" ], [])
            Html.span [ prop.text "Delete" ]
        ]
    ]

[<ReactComponent>]
let SequenceOption (dispatch, name: string, code, path, page: Page) =
    let (collapsed, setCollapsed) = React.useState true

    match code with
    | HtmlObject(objType, keys, elements, handlers) ->
        Html.div [
            prop.className "bg-white rounded-lg shadow-sm border border-gray-200 overflow-hidden"
            prop.children [
                Html.div [
                    prop.className "flex items-center justify-between p-2 bg-gray-50 cursor-pointer"
                    prop.onClick (fun _ -> setCollapsed (not collapsed))
                    prop.children [
                        Html.span [ prop.className "text-sm font-medium text-gray-700"; prop.text name ]
                        let icon = if collapsed then chevronRight else chevronDown
                        ReactBindings.React.createElement (icon, createObj [ "size" ==> 16; "color" ==> "#4B5563" ], [])
                    ]
                ]
                if not collapsed then
                    Html.div [
                        prop.className "p-2 space-y-2"
                        prop.children [
                            renderTypeSelector (objType, dispatch, keys, elements, handlers, path)
                            renderKeys (keys, objType, elements, handlers, dispatch, path)
                            EventHandlerMenu(dispatch, code, path, page.CustomHandlers)

                            renderDeleteButton (dispatch, name, path)
                        ]
                    ]
            ]
        ]
    | _ -> ErrorDisplay "Invalid code type for SequenceOption"