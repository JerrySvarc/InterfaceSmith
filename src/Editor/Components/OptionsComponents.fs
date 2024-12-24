module Editor.Components.OptionsComponents

open Editor.Types.PageEditorDomain
open Fable.React
open Feliz
open CoreLogic.Types.RenderingTypes
open Fable.Core.JsInterop
open Editor.Utilities.Icons
open CoreLogic.Operations.RenderingCode
open Microsoft.FSharp.Reflection


//                  General helper components
//||------------------------------------------------------------||
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

//    General option components common for all RenderingCodes
//||-----------------------------------------------------------||
[<ReactComponent>]
let InnerValueMenu (currentInnerValue: InnerValue) updateInnerValue =
    let innerValueOptions = [ "Data"; "Constant"; "Empty" ]

    let constantValue, setConstantValue =
        React.useState (
            match currentInnerValue with
            | Constant str -> str
            | _ -> ""
        )

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
let AttributeRow
    (attr: Attribute)
    (isEditing: bool)
    (onEditKey: string -> unit)
    (onKeyChange: string -> string -> unit)
    (onValueChange: InnerValue -> unit)
    (onDelete: unit -> unit)
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
                prop.children [ InnerValueMenu attr.Value onValueChange ]
            ]

            Html.td [
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

let AttributeMenu (code: RenderingCode) path (attributes: Attribute list) dispatch =
    let editingKey, setEditingKey = React.useState ""
    let menuOpen, setMenuOpen = React.useState false
    let newKey, setNewKey = React.useState ""
    let newValue, setNewValue = React.useState Empty

    let toggleMenu () = setMenuOpen (not menuOpen)

    let updateAttributes newAttrs =
        let updatedCode =
            match code with
            | RenderingCode.HtmlElement(tag, _, innerValue, handlers) ->
                RenderingCode.HtmlElement(tag, newAttrs, innerValue, handlers)
            | RenderingCode.HtmlList(listType, _, items, handlers) ->
                RenderingCode.HtmlList(listType, newAttrs, items, handlers)
            | RenderingCode.HtmlObject(objType, _, keys, codes, handlers) ->
                RenderingCode.HtmlObject(objType, newAttrs, keys, codes, handlers)
            | _ -> code

        dispatch (ReplaceCode(updatedCode, path))

    let handleKeyChange oldKey newKey =
        if not (System.String.IsNullOrWhiteSpace newKey) then
            let updatedAttributes =
                attributes
                |> List.map (fun attr ->
                    if attr.Key = oldKey then
                        { attr with Key = newKey }
                    else
                        attr)

            updateAttributes updatedAttributes

    let handleValueChange key newValue =
        let updatedAttributes =
            attributes
            |> List.map (fun attr ->
                if attr.Key = key then
                    { attr with Value = newValue }
                else
                    attr)

        updateAttributes updatedAttributes

    let handleDelete key =
        let newAttributes = attributes |> List.filter (fun attr -> attr.Key <> key)
        updateAttributes newAttributes

    let handleAddNewAttribute () =
        if not (System.String.IsNullOrWhiteSpace newKey) then
            let updatedAttributes =
                attributes
                |> List.map (fun attr ->
                    if attr.Key = newKey then
                        { attr with Value = newValue }
                    else
                        attr)
                |> fun attrs ->
                    if List.exists (fun attr -> attr.Key = newKey) attrs then
                        attrs
                    else
                        attrs
                        @ [
                            {
                                Key = newKey
                                Value = newValue
                                Namespace = None
                            }
                        ]

            updateAttributes updatedAttributes
            setNewKey ""
            setNewValue Empty

    Html.div [
        prop.className "space-y-4"
        prop.children [
            Html.button [
                prop.className "flex flex-row items-center space-x-2"
                prop.onClick (fun e ->
                    e.stopPropagation () // Prevent propagation
                    toggleMenu ())
                prop.children [
                    ReactBindings.React.createElement (
                        (if menuOpen then chevronDown else chevronRight),
                        createObj [ "size" ==> 16; "color" ==> "#000000" ],
                        []
                    )
                    Html.span [ prop.text "Attributes"; prop.className "text-xs px-1 py-1" ]
                ]
            ]

            if menuOpen then
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
                                            (fun () -> handleDelete attr.Key))
                                    |> prop.children
                                ]
                            ]
                        ]

                        Html.div [
                            prop.className "mt-4 space-y-2"
                            prop.children [
                                Html.div [
                                    prop.className "flex space-x-2"
                                    prop.children [
                                        Html.input [
                                            prop.className "text-sm px-2 py-1 border border-gray-300 rounded"
                                            prop.placeholder "Attribute Name"
                                            prop.value newKey
                                            prop.onChange (fun (event: Browser.Types.Event) ->
                                                setNewKey (event.target?value |> Option.defaultValue ""))
                                        ]
                                    ]
                                ]
                                Html.button [
                                    prop.className
                                        "bg-blue-500 text-white text-sm px-4 py-2 rounded shadow hover:bg-blue-600"
                                    prop.text "Add New Attribute"
                                    prop.onClick (fun _ -> handleAddNewAttribute ())
                                ]
                            ]
                        ]
                    ]
                ]
        ]
    ]



[<ReactComponent>]
let EventHandlerMenu code path customHandlers eventHandlers dispatch =
    let availableEvents = [ "onClick"; "onMouseOver"; "onMouseOut"; "onKeyPress"; "onFocus"; "onBlur" ]

    let selectedEvent, setSelectedEvent = React.useState ""
    let selectedHandler, setSelectedHandler = React.useState ""
    let menuOpen, setMenuOpen = React.useState false

    let toggleMenu () = setMenuOpen (not menuOpen)

    let updateCode updatedHandlers =
        let updatedCode =
            match code with
            | RenderingCode.HtmlElement(tag, attrs, innerValue, _) ->
                RenderingCode.HtmlElement(tag, attrs, innerValue, updatedHandlers)
            | RenderingCode.HtmlList(listType, attrs, items, _) ->
                RenderingCode.HtmlList(listType, attrs, items, updatedHandlers)
            | RenderingCode.HtmlObject(objType, attrs, keys, codes, _) ->
                RenderingCode.HtmlObject(objType, attrs, keys, codes, updatedHandlers)
            | _ -> code

        dispatch (ReplaceCode(updatedCode, path))
        setSelectedEvent ""
        setSelectedHandler ""


    let addHandler () =
        if selectedEvent <> "" && selectedHandler <> "" then
            let newHandler =
                match Map.tryFind selectedHandler customHandlers with
                | Some js -> JsHandler js
                | None -> JsHandler(JSFunction(selectedHandler, ""))

            let updatedHandlers = (selectedEvent, newHandler) :: eventHandlers
            updateCode updatedHandlers


    let removeHandler eventName =
        let updatedHandlers =
            eventHandlers |> List.filter (fun (name, _) -> name <> eventName)

        updateCode updatedHandlers


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


//       Option components for modification of a HtmlObject
// ||---------------------------------------------------------------||

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
    (name: string)
    (code: RenderingCode)
    (path: int list)
    customHandlers
    (dispatch: PageEditorMsg -> unit)
    =

    let changeObjType newValueString =
        let newValue = stringToObjType newValueString
        printf "%s" newValueString

        match code with
        | RenderingCode.HtmlObject(_, attrs, keys, codes, handlers) ->
            dispatch (ReplaceCode(RenderingCode.HtmlObject(newValue, attrs, keys, codes, handlers), path))
        | _ -> ()

    let objTypeOptions =
        FSharpType.GetUnionCases(typeof<ObjType>)
        |> Array.map (fun item -> item.Name.ToLower())
        |> Array.toList

    match code with
    | RenderingCode.HtmlObject(objType, attrs, keyOrdering, codes, handlers) ->
        Html.div [
            prop.className "bg-gray-300 border border-black w-fit h-fit mt-4"
            prop.children [
                Html.p [ prop.text (name + ":"); prop.className "text-xs font-semibold" ]
                Html.div [
                    prop.className "p-2 space-y-1"
                    prop.children [
                        (SelectMenu objTypeOptions (objTypeToString objType) changeObjType)
                        AttributeMenu code path attrs dispatch
                        KeysList(keyOrdering, objType, codes, handlers, dispatch, path)
                        EventHandlerMenu code path customHandlers handlers dispatch
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



//      HtmlList modification components
// ||----------------------------------------||

[<ReactComponent>]
let ListOption (name: string) code path customHandlers dispatch =

    let changeListType newValueString =
        let newValue = (stringToListType newValueString)

        match code with
        | RenderingCode.HtmlList(_, attrs, itemCodes, handlers) ->
            dispatch (ReplaceCode(RenderingCode.HtmlList(newValue, attrs, itemCodes, handlers), path))
        | _ -> ()


    let listTypeOptions = [ "Unordered"; "Ordered" ]

    let listTypeToListOption (listType: ListType) : string =
        match listType with
        | ListType.UnorderedList -> "Unordered"
        | ListType.OrderedList -> "Ordered"

    match code with
    | RenderingCode.HtmlList(listType, attrs, itemCodes, handlers) ->
        Html.div [
            prop.onMouseDown (fun e -> e.stopPropagation ())
            prop.className "bg-gray-300 border border-black w-fit h-fit mt-4 group"
            prop.children [
                Html.p [ prop.text (name + ":"); prop.className "text-xs font-semibold" ]
                Html.div [
                    prop.children [
                        SelectMenu listTypeOptions (listTypeToListOption listType) changeListType
                        Html.div [
                            prop.className "hidden group-hover:block"
                            prop.children [
                                AttributeMenu code path attrs dispatch
                                EventHandlerMenu code path customHandlers handlers dispatch
                            ]
                        ]
                    ]
                ]
            ]
        ]
    | _ -> ErrorDisplay "Invalid code type for ListOption"



//      HtmlElement modification components
// ||----------------------------------------||

[<ReactComponent>]
let TagMenu (code: RenderingCode) path dispatch =
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
        let changeTag selectedTag =
            dispatch (ReplaceCode(RenderingCode.HtmlElement(stringToTag selectedTag, attrs, value, handlers), path))

        SelectMenu tagOptions tag.Name changeTag
    | _ -> ErrorDisplay "Invalid code type for TagMenu"

[<ReactComponent>]
let ElementOption (name: string) code path customHandlers dispatch =

    let updateInnerValue newVal =
        match code with
        | RenderingCode.HtmlElement(tag, attrs, _, handlers) ->
            dispatch (ReplaceCode(RenderingCode.HtmlElement(tag, attrs, newVal, handlers), path))
        | _ -> ()

    Html.div [
        prop.onMouseDown (fun e -> e.stopPropagation ())
        prop.className "bg-gray-300 text-black  border border-black w-fit h-fit mt-4 group"
        prop.children [
            Html.p [ prop.text (name + ":"); prop.className "text-xs font-semibold" ]
            match code with
            | RenderingCode.HtmlElement(_, attrs, innerValue, handlers) ->
                Html.div [
                    prop.children [
                        (TagMenu code path dispatch)
                        InnerValueMenu innerValue updateInnerValue
                        (AttributeMenu code path attrs dispatch)
                        (EventHandlerMenu code path customHandlers handlers dispatch)
                    ]
                ]
            | _ -> Html.none
        ]
    ]