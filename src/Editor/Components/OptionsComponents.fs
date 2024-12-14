module Editor.Components.OptionsComponents

open Editor.Types.PageEditorDomain
open Fable.React
open Feliz
open CoreLogic.Types.RenderingTypes
open Fable.Core.JsInterop
open Editor.Utilities.Icons
open CoreLogic.Operations.RenderingCode
open Microsoft.FSharp.Reflection


// Contains option menu components for each type of rendering code
// Each component takes a dispatch function, the current code, and the path to the code in the tree
// The dispatch function is used to send messages to the parent component to update the specific code

let SelectMenu (options: string list) (value: string) (onChange: string -> unit) =
    Html.select [
        prop.className
            "text-xs w-36 h-fit bg-white border border-black shadow-sm focus:border-indigo-500 focus:ring focus:ring-indigo-200 focus:ring-opacity-50"
        prop.onMouseDown (fun e -> e.stopPropagation ())
        prop.value value
        prop.onChange (fun (e: Browser.Types.Event) -> e.target?value |> string |> onChange)
        prop.children (
            (options
             |> List.map (fun opt -> Html.option [ prop.className "text-xs"; prop.value opt; prop.text opt ]))
        )
    ]

let ErrorDisplay (message: string) =
    Html.div [
        prop.className "bg-red-100 border-l-4 border-red-500 text-red-700 p-4 rounded flex items-center space-x-2"
        prop.children [ Html.span [ prop.className "font-medium"; prop.text message ] ]
    ]

[<ReactComponent>]
let InnerValueMenu (currentInnerValue: InnerValue) (code: RenderingCode) path =
    let innerValueOptions = [ "Data"; "Constant"; "Empty" ]

    let constantValue, setConstantValue =
        React.useState (
            match currentInnerValue with
            | Constant str -> str
            | _ -> ""
        )

    let updateInnerValue newValue =
        match code with
        | RenderingCode.HtmlElement(tag, attrs, _, handlers) ->
            //dispatch (ReplaceCode(RenderingCode.HtmlElement(tag, attrs, newValue, handlers), path))
            ()
        | _ -> ()

    Html.span [
        prop.children [
            Html.span [
                prop.children [
                    SelectMenu innerValueOptions (currentInnerValue |> innerValueToString) (fun selectedValue ->
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
                Html.span [
                    prop.children [
                        Html.input [
                            prop.type' "text"
                            prop.value constantValue
                            prop.onChange (fun (value: string) ->
                                setConstantValue value
                                updateInnerValue (Constant value))
                            prop.className "text-xs overflow-auto bg-white border border-black shadow-sm "
                        ]
                    ]
                ]
            else
                Html.none
        ]
    ]


[<ReactComponent>]
let TagMenu (code: RenderingCode) path =
    let tagOptions = [
        Tags.p.Name
        Tags.h1.Name
        Tags.h2.Name
        Tags.h3.Name
        Tags.h4.Name
        Tags.h5.Name
        Tags.h6.Name
        Tags.strong.Name
        Tags.em.Name
        Tags.a.Name
        Tags.pre.Name
        Tags.code.Name
        Tags.blockquote.Name
        Tags.div.Name
        Tags.span.Name
        Tags.article.Name
        Tags.section.Name
        Tags.header.Name
        Tags.footer.Name
        Tags.nav.Name
        Tags.input.Name
        Tags.li.Name
        Tags.ol.Name
        Tags.ul.Name
        Tags.button.Name
        Tags.label.Name
    ]

    match code with
    | RenderingCode.HtmlElement(tag, attrs, value, handlers) ->
        //let changeTag selectedTag =
        //dispatch (ReplaceCode(RenderingCode.HtmlElement(stringToTag selectedTag, attrs, value, handlers), path))
        let handleChange2 newValue = printfn "Selected:"
        SelectMenu tagOptions tag.Name handleChange2
    | _ -> ErrorDisplay "Invalid code type for TagMenu"



[<ReactComponent>]
let AttributeRow
    (attr: Attribute)
    (isEditing: bool)
    (onEditKey: string -> unit)
    (onKeyChange: string -> string -> unit)
    (onValueChange: InnerValue -> unit)
    (onDelete: unit -> unit)
    (code: RenderingCode)
    path
    =
    Html.tr [
        prop.className "border-b hover:bg-gray-50"
        prop.children [
            Html.td [
                prop.className "p-2 text-sm"
                prop.children [
                    if isEditing then
                        Html.input [
                            prop.className "border rounded px-2 py-1 text-sm w-full"
                            prop.defaultValue attr.Key
                            prop.autoFocus true
                            prop.onBlur (fun e ->
                                let newKey = unbox<string> e.target?value
                                onEditKey ""
                                onKeyChange attr.Key newKey)
                            prop.onKeyDown (fun e ->
                                if e.key = "Enter" then
                                    let newKey = unbox<string> e.target?value
                                    onEditKey ""
                                    onKeyChange attr.Key newKey)
                        ]
                    else
                        Html.span [
                            prop.className "cursor-pointer"
                            prop.text attr.Key
                            prop.onClick (fun _ -> onEditKey attr.Key)
                        ]
                ]
            ]

            Html.td [
                prop.className "p-2 text-sm"
                prop.children [ InnerValueMenu attr.Value code path ]
            ]

            Html.div [
                prop.className "ml-auto relative hover:bg-red-600 rounded"
                prop.children [
                    Html.button [
                        prop.className "flex items-center justify-center w-8 h-8 rounded-full"
                        prop.onClick (fun _ -> onDelete ())
                        prop.children [
                            ReactBindings.React.createElement (
                                trashIcon,
                                createObj [ "size" ==> 16; "color" ==> "#000000" ],
                                []
                            )
                        ]
                    ]
                ]
            ]

        ]
    ]

[<ReactComponent>]
let AttributeMenu (code: RenderingCode) path (attributes: Attribute list) =
    let editingKey, setEditingKey = React.useState ""
    let attributesOpen, setAttributesOpen = React.useState false

    let toggleAttributes () = setAttributesOpen (not attributesOpen)

    let handleKeyChange oldKey newKey =
        if not (System.String.IsNullOrWhiteSpace newKey) then
            ()

    let handleValueChange key newValue = ()

    let handleDelete key = ()

    let handleAddClick () = ()

    Html.div [
        prop.className "space-y-4"
        prop.children [
            Html.button [
                prop.className "flex flex-row items-center space-x-2"
                prop.onClick (fun e ->
                    e.stopPropagation () // Prevent propagation
                    toggleAttributes ())
                prop.children [
                    ReactBindings.React.createElement (
                        (if attributesOpen then chevronDown else chevronRight),
                        createObj [ "size" ==> 16; "color" ==> "#000000" ],
                        []
                    )
                    Html.span [ prop.text "Attributes"; prop.className "text-xs px-1 py-1" ]
                ]
            ]

            if attributesOpen then
                Html.div [
                    prop.className "space-y-2"
                    prop.children [
                        Html.table [
                            prop.className "w-full border-collapse"
                            prop.children [
                                Html.thead [
                                    Html.tr [
                                        prop.className "border-b"
                                        prop.children [
                                            Html.th [
                                                prop.className "text-left text-sm font-medium text-gray-600"
                                                prop.text "Property"
                                            ]
                                            Html.th [
                                                prop.className "text-left text-sm font-medium text-gray-600"
                                                prop.text "Value"
                                            ]
                                            Html.th [ prop.className "text-sm" ]
                                        ]
                                    ]
                                ]
                                Html.tbody [
                                    attributes
                                    |> List.map (fun attr ->
                                        AttributeRow
                                            attr
                                            (editingKey = attr.Key)
                                            setEditingKey
                                            (fun oldKey newKey -> handleKeyChange oldKey newKey)
                                            (fun newValue -> handleValueChange attr.Key newValue)
                                            (fun () -> handleDelete attr.Key)
                                            code
                                            path)
                                    |> prop.children
                                ]
                            ]
                        ]

                        // Add New Attribute Button
                        Html.div [
                            prop.className "mt-4"
                            prop.children [
                                Html.button [
                                    prop.className
                                        "bg-blue-500 text-white text-sm px-4 py-2 rounded shadow hover:bg-blue-600"
                                    prop.text "Add New Attribute"
                                    prop.onClick (fun _ -> handleAddClick ())
                                ]
                            ]
                        ]
                    ]
                ]
        ]
    ]



[<ReactComponent>]
let EventHandlerMenu code path customHandlers eventHandlers =
    let availableEvents = [ "onClick"; "onMouseOver"; "onMouseOut"; "onKeyPress"; "onFocus"; "onBlur" ]

    let selectedEvent, setSelectedEvent = React.useState ""
    let selectedHandler, setSelectedHandler = React.useState ""
    let menuOpen, setMenuOpen = React.useState false

    // Toggle visibility
    let toggleMenu () = setMenuOpen (not menuOpen)

    let addHandler () =
        if selectedEvent <> "" && selectedHandler <> "" then
            let newHandler =
                match Map.tryFind selectedHandler customHandlers with
                | Some js -> JsHandler js
                | None -> JsHandler(JSFunction(selectedHandler, ""))

            let updatedHandlers = (selectedEvent, newHandler) :: eventHandlers

            let updatedCode =
                match code with
                | RenderingCode.HtmlElement(tag, attrs, innerValue, _) ->
                    RenderingCode.HtmlElement(tag, attrs, innerValue, updatedHandlers)
                | RenderingCode.HtmlList(listType, attrs, items, _) ->
                    RenderingCode.HtmlList(listType, attrs, items, updatedHandlers)
                | RenderingCode.HtmlObject(objType, attrs, keys, codes, _) ->
                    RenderingCode.HtmlObject(objType, attrs, keys, codes, updatedHandlers)
                | _ -> code

            //dispatch (ReplaceCode(updatedCode, path))
            setSelectedEvent ""
            setSelectedHandler ""

    let removeHandler eventName =
        let updatedHandlers =
            eventHandlers |> List.filter (fun (name, _) -> name <> eventName)

        let updatedCode =
            match code with
            | RenderingCode.HtmlElement(tag, attrs, innerValue, _) ->
                RenderingCode.HtmlElement(tag, attrs, innerValue, updatedHandlers)
            | RenderingCode.HtmlList(listType, attrs, items, _) ->
                RenderingCode.HtmlList(listType, attrs, items, updatedHandlers)
            | RenderingCode.HtmlObject(objType, attrs, keys, codes, _) ->
                RenderingCode.HtmlObject(objType, attrs, keys, codes, updatedHandlers)
            | _ -> code

        ()
    //dispatch (ReplaceCode(updatedCode, path))

    Html.div [
        prop.className "space-y-4"
        prop.children [
            Html.button [
                prop.className "flex flex-row items-center space-x-2"
                prop.onClick (fun e ->
                    e.stopPropagation ()
                    toggleMenu ())
                prop.children [
                    ReactBindings.React.createElement (
                        (if menuOpen then chevronDown else chevronRight),
                        createObj [ "size" ==> 16; "color" ==> "#000000" ],
                        []
                    )
                    Html.span [ prop.text "Handlers"; prop.className "text-xs px-1 py-1" ]
                ]
            ]

            if menuOpen then
                Html.div [
                    prop.className "flex space-x-2"
                    prop.children [
                        Html.select [
                            prop.className "p-2 border border-gray-300 rounded text-xs"
                            prop.value selectedEvent
                            prop.onChange (fun (e: Browser.Types.Event) -> setSelectedEvent (e.target?value |> string))
                            prop.children (
                                Html.option [ prop.value ""; prop.text "Select an event" ]
                                :: (availableEvents
                                    |> List.filter (fun e ->
                                        not (List.exists (fun (name, _) -> name = e) eventHandlers))
                                    |> List.map (fun e -> Html.option [ prop.value e; prop.text e ]))
                            )
                        ]
                        Html.select [
                            prop.className "p-2 border border-gray-300 rounded text-xs"
                            prop.value selectedHandler
                            prop.onChange (fun (e: Browser.Types.Event) ->
                                setSelectedHandler (e.target?value |> string))
                            prop.children (
                                Html.option [ prop.value ""; prop.text "Select a handler" ]
                                :: (customHandlers
                                    |> Map.toList
                                    |> List.map (fun (name, _) -> Html.option [ prop.value name; prop.text name ]))
                            )
                        ]
                        Html.button [
                            prop.className "bg-blue-500 text-white px-4 rounded text-xs"
                            prop.text "Add Handler"
                            prop.onClick (fun _ -> addHandler ())
                        ]
                    ]
                ]

                Html.table [
                    prop.className "w-full border-collapse"
                    prop.children [
                        Html.thead [
                            Html.tr [
                                prop.children [
                                    Html.th [
                                        prop.className "text-left text-xs font-medium text-gray-600"
                                        prop.text "Event"
                                    ]
                                    Html.th [
                                        prop.className "text-left text-xs font-medium text-gray-600"
                                        prop.text "Handler"
                                    ]
                                    Html.th []
                                ]
                            ]
                        ]
                        Html.tbody [
                            eventHandlers
                            |> List.map (fun (eventName, handler) ->
                                Html.tr [
                                    prop.className "border-b hover:bg-gray-50"
                                    prop.children [
                                        Html.td [ prop.text eventName; prop.className "p-2 text-sm" ]
                                        Html.td [
                                            prop.text (
                                                match handler with
                                                | JsHandler(JSFunction(name, _)) -> sprintf "JS: %s" name
                                                | MsgHandler msg -> sprintf "Msg: %s" msg
                                            )
                                            prop.className "p-2 text-sm"
                                        ]
                                        Html.td [
                                            prop.className "p-2 text-sm text-right"
                                            prop.children [
                                                Html.button [
                                                    prop.className "text-red-500 text-xs"
                                                    prop.text "Remove"
                                                    prop.onClick (fun _ -> removeHandler eventName)
                                                ]
                                            ]
                                        ]
                                    ]
                                ])
                            |> prop.children
                        ]
                    ]
                ]
        ]
    ]

let moveKey (keys: string list) (index: int) (direction: int) =
    if index + direction >= 0 && index + direction < List.length keys then
        let item = List.item index keys
        let newKeys = List.removeAt index keys |> List.insertAt (index + direction) item
        newKeys
    else
        keys

let KeyRow (key: string, index: int, keyOrdering, objType, codes, handlers, dispatch, path) =

    Html.div [
        prop.key key
        prop.className "flex items-center justify-between mb-1"
        prop.children [
            Html.span [ prop.className "text-xs text-gray-600"; prop.text key ]
            Html.div [
                prop.className "flex space-x-1"
                prop.children [
                    Html.button [
                        prop.disabled ((index = 0))
                        prop.className "p-1 text-gray-500 hover:text-gray-700 disabled:text-gray-300"
                        prop.onClick (fun _ ->
                            let newKeyOrdering = moveKey keyOrdering index -1

                            let updatedCode =
                                RenderingCode.HtmlObject(
                                    objType,
                                    attrs = [],
                                    keyOrdering = newKeyOrdering,
                                    codes = codes,
                                    eventHandlers = handlers
                                )

                            dispatch (ReplaceCode(updatedCode, path)))
                        prop.children [
                            ReactBindings.React.createElement (arrowUpIcon, createObj [ "size" ==> 12 ], [])
                        ]
                    ]
                    Html.button [
                        prop.disabled ((index = List.length keyOrdering - 1))
                        prop.className "p-1 text-gray-500 hover:text-gray-700 disabled:text-gray-300"
                        prop.onClick (fun _ ->
                            let newKeyOrdering = moveKey keyOrdering index 1

                            let updatedCode =
                                RenderingCode.HtmlObject(
                                    objType,
                                    attrs = [],
                                    keyOrdering = newKeyOrdering,
                                    codes = codes,
                                    eventHandlers = handlers
                                )

                            dispatch (ReplaceCode(updatedCode, path)))
                        prop.children [
                            ReactBindings.React.createElement (arrowDownIcon, createObj [ "size" ==> 12 ], [])
                        ]
                    ]
                ]
            ]
        ]
    ]

[<ReactComponent>]
let KeysList (keyOrdering, objType, codes, handlers, dispatch, path) =
    let collapsed, setCollapsed = React.useState true

    let toggleMenu () = setCollapsed (not collapsed)

    Html.div [
        prop.children [
            Html.button [
                prop.className "flex flex-row items-center space-x-2"
                prop.onClick (fun e ->
                    e.stopPropagation ()
                    toggleMenu ())
                prop.children [
                    ReactBindings.React.createElement (
                        (if not collapsed then chevronDown else chevronRight),
                        createObj [ "size" ==> 16; "color" ==> "#000000" ],
                        []
                    )
                    Html.span [ prop.text "Key ordering"; prop.className "text-xs px-1 py-1" ]
                ]
            ]
            if not collapsed then
                Html.div [
                    prop.className "space-y-1"
                    prop.children (
                        keyOrdering
                        |> List.mapi (fun i key ->
                            Html.div [
                                prop.children [ KeyRow(key, i, keyOrdering, objType, codes, handlers, dispatch, path) ]
                            ])
                    )
                ]
        ]
    ]



[<ReactComponent>]
let SequenceOption
    (dispatch: PageEditorMsg -> unit)
    (name: string)
    (code: RenderingCode)
    (path: int list)
    customHandlers
    =


    let handleChange2 newValue = printfn "Selected:"

    match code with
    | RenderingCode.HtmlObject(objType, attrs, keyOrdering, codes, handlers) ->
        Html.div [
            prop.className "bg-white p-4 border border-gray-300 shadow-md"
            prop.children [
                Html.div [
                    prop.className "p-2 space-y-2"
                    prop.children [
                        (SelectMenu [] (objTypeToString objType) handleChange2)
                        AttributeMenu code path attrs
                        KeysList(keyOrdering, objType, codes, handlers, dispatch, path)
                        EventHandlerMenu code path customHandlers handlers
                        Html.div [
                            prop.className "ml-auto relative hover:bg-red-600 rounded"
                            prop.children [
                                Html.button [
                                    prop.className "flex items-center justify-center w-8 h-8 rounded-full"
                                    prop.onClick (fun _ -> dispatch (ReplaceCode(RenderingCode.Hole(Named name), path)))
                                    prop.children [
                                        ReactBindings.React.createElement (
                                            trashIcon,
                                            createObj [ "size" ==> 16; "color" ==> "#000000" ],
                                            []
                                        )
                                    ]
                                ]
                            ]
                        ]

                    ]
                ]
            ]
        ]
    | _ -> Html.div [ prop.text "Invalid code type for SequenceOption" ]




[<ReactComponent>]
let ListOption (name: string) code path model =

    let handleChange2 newValue = printfn "Selected:"
    let handleChange3 () = ()

    match code with
    | RenderingCode.HtmlList(listType, attrs, elementCode, handlers) ->
        let listTypeOptions = [ "Unordered"; "Ordered" ]

        Html.div [
            prop.className "bg-white  p-4 border border-gray-300 shadow-md"
            prop.children [
                Html.p [ prop.text (name + ":"); prop.className "text-xs font-semibold" ]
                match code with
                | RenderingCode.HtmlList(listType, attrs, itemCodes, handlers) ->
                    Html.div [
                        prop.children [
                            SelectMenu listTypeOptions (listTypeToString listType) handleChange2
                            (AttributeMenu code path attrs)
                            (EventHandlerMenu code path model.CustomHandlers handlers)

                        ]
                    ]
                | _ -> Html.none

            ]
        ]
    | _ -> ErrorDisplay "Invalid code type for ListOption"

[<ReactComponent>]
let ElementOption (name: string) code path model =
    let handleChange2 newValue = printfn "Selected:"
    let handleChange3 () = ()

    Html.div [
        prop.onMouseDown (fun e -> e.stopPropagation ())
        prop.className "bg-white  p-4 border border-gray-300 shadow-md"
        prop.children [
            Html.div [
                prop.className "bg-gray-300  border border-black w-fit h-fit mt-4"
                prop.children [
                    Html.p [ prop.text (name + ":"); prop.className "text-xs font-semibold" ]
                    match code with
                    | RenderingCode.HtmlElement(_, attrs, innerValue, handlers) ->
                        Html.div [
                            prop.children [
                                (TagMenu code path)
                                InnerValueMenu innerValue code path
                                (AttributeMenu code path attrs)
                                (EventHandlerMenu code path model.CustomHandlers handlers)
                            ]
                        ]
                    | _ -> Html.none
                ]
            ]
        ]
    ]