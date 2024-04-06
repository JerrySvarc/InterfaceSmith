module Editor

open Elmish
open Feliz
open Types
open Fable.SimpleJson
open DataLoading
open DataRecognition
open FileUpload
open System
open EditorUtils


type Model = {
    CurrentPage: Page
    FileUploadError: bool
    EditingName: bool
    EditingCode: bool
    NameInput: string
}

type Msg =
    | UploadData of string
    | ChangeName of string
    | SetInput of string
    | ChangeNameEditMode of bool
    | SavePage of Page
    | ReplaceCode of RenderingCode * int list

let init () = {
    CurrentPage = {
        Name = "New component"
        Code = Hole(UnNamed)
        Id = Guid.NewGuid()
        Data = JNull
    }
    FileUploadError = false
    EditingName = false
    NameInput = ""
    EditingCode = false
}

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | UploadData data ->
        let loadedDataOption = loadJson data

        match loadedDataOption with
        | Some(data) ->
            match data with
            | JObject obj ->
                let newComponent = {
                    model.CurrentPage with
                        Code = recognizeJson data
                        Data = data
                }

                {
                    model with
                        CurrentPage = newComponent
                        FileUploadError = false
                },
                Cmd.none
            | _ -> { model with FileUploadError = true }, Cmd.none
        | None -> { model with FileUploadError = true }, Cmd.none
    | ChangeName newName ->
        {
            model with
                CurrentPage = {
                    model.CurrentPage with
                        Name = newName
                }
                NameInput = ""
                EditingName = false
        },
        Cmd.none
    | SetInput input -> { model with NameInput = input }, Cmd.none
    | ChangeNameEditMode value ->
        {
            model with
                EditingName = value
                NameInput = ""
        },
        Cmd.none
    | SavePage comp -> model, Cmd.none
    | ReplaceCode(code, path) ->
        let newcodes = replace path code model.CurrentPage.Code

        let newComponent: Page = {
            model.CurrentPage with
                Code = newcodes
        }

        {
            model with
                CurrentPage = newComponent
        },
        Cmd.none


let view (model: Model) (dispatch: Msg -> unit) =

    let upploadButtonView onLoad =
        Html.div [
            prop.className "mt-10 w-full h-full flex items-center justify-center"
            prop.children [
                Html.div [
                    prop.className "flex justify-center"
                    prop.children [
                        Html.label [
                            prop.children [
                                Html.input [
                                    prop.type' "file"
                                    prop.name "component-data"
                                    prop.onChange (handleFileEvent onLoad)
                                    prop.className "hidden"
                                ]
                                Html.span [
                                    prop.className
                                        "px-8 py-4 bg-blue-500 text-white rounded cursor-pointer hover:bg-blue-700 transition duration-200 ease-in-out text-xl"
                                    prop.children [ Html.text "Select a file" ]
                                ]
                            ]
                        ]
                    ]
                ]
                if model.FileUploadError then
                    Html.div [
                        prop.className "mt-4 p-4 bg-red-500 text-white rounded"
                        prop.children [ Html.text "The selected file could not be used for creation." ]
                    ]
                else
                    Html.text ""
            ]
        ]

    let uploadButton = upploadButtonView (UploadData >> dispatch)

    let nameEditView =
        let nameInput =
            Html.input [
                prop.className "border-2 border-blue-500 rounded-md pl-3"
                prop.onTextChange (fun text -> dispatch (SetInput text))
            ]

        let nameDisplay =
            let nameText = model.CurrentPage.Name
            Html.h1 [ prop.className "text-2xl font-semibold m-0 p-0 pl-3"; prop.text nameText ]

        let nameContainer =
            Html.div [
                prop.className "p-0"
                prop.children [
                    if model.EditingName then nameInput else nameDisplay
                ]
            ]

        let editButton =
            let buttonText, buttonColor =
                if model.EditingName then
                    if model.NameInput.Length > 0 then
                        "Save", "bg-blue-500 hover:bg-blue-700"
                    else
                        "Name must be at least one character", "bg-yellow-500 hover:bg-yellow-700"
                else
                    "Edit name", "bg-gray-500 hover:bg-gray-700"

            let onClick =
                if model.EditingName && model.NameInput.Length > 0 then
                    (fun _ -> dispatch (ChangeName model.NameInput))
                else if not model.EditingName then
                    (fun _ -> dispatch (ChangeNameEditMode(model.EditingName |> not)))
                else
                    (fun _ -> ())

            Html.button [
                prop.className (
                    buttonColor
                    + " text-white px-4 py-2 rounded-md transition duration-200 ease-in-out"
                )
                prop.children [ Html.text buttonText ]
                prop.onClick onClick
            ]

        let cancelButton =
            Html.button [
                prop.className
                    "bg-red-500 hover:bg-red-700 text-white px-4 py-2 rounded-md transition duration-200 ease-in-out"
                prop.children [ Html.text "Cancel" ]
                prop.onClick (fun _ -> dispatch (ChangeNameEditMode false))
            ]

        let saveButton =
            let buttonText, buttonColor =
                if model.CurrentPage.Data = JNull then
                    "Upload data first to save a component", "bg-yellow-500 hover:bg-yellow-700"
                else
                    "Save component", "bg-green-500 hover:bg-green-700"

            Html.button [
                prop.className (
                    buttonColor
                    + " text-white px-4 py-2 rounded-md transition duration-200 ease-in-out"
                )
                prop.children [ Html.text buttonText ]
                prop.onClick (fun _ -> dispatch (SavePage model.CurrentPage))
            ]

        Html.div [
            prop.className "p-7 border-2 border-gray-300 rounded-md"
            prop.children [
                nameContainer
                Html.div [
                    prop.className "flex space-x-4 mt-4"
                    prop.children [ editButton; if model.EditingName then cancelButton else saveButton ]
                ]
            ]
        ]

    let dropdownItem (text: string) (setSelectedItem: string -> unit) =
        Html.a [
            prop.className "block px-4 py-2 text-sm text-gray-700 hover:bg-gray-100"
            prop.href "#"
            prop.text text
            prop.onClick (fun _ -> setSelectedItem text)
        ]

    let dropdownMenu items (defaultText: string) =
        let (dropdownVisible, setDropdownVisible) = React.useState false
        let (selectedItem, setSelectedItem) = React.useState defaultText

        Html.div [
            prop.className "group relative inline-block text-left"
            prop.children [
                Html.button [
                    prop.className
                        "inline-flex justify-center w-full rounded-md border border-gray-300 shadow-sm px-4 py-2 bg-white text-sm font-medium text-gray-700 hover:bg-gray-50 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500"
                    prop.text selectedItem
                    prop.onClick (fun _ -> setDropdownVisible (not dropdownVisible))
                    prop.children [ Html.span [ prop.text "Dropdown button" ] ]
                ]
                Html.div [
                    prop.className (
                        sprintf
                            "origin-top-right absolute right-0 mt-2 w-56 rounded-md shadow-lg bg-white ring-1 ring-black ring-opacity-5 %s"
                            (if dropdownVisible then "block" else "hidden")
                    )
                    prop.custom ("role", "menu")
                    prop.onMouseLeave (fun _ -> setDropdownVisible false)
                    prop.children [
                        Html.div [
                            prop.className "py-1"
                            prop.children (items |> List.map (fun item -> dropdownItem item setSelectedItem))
                        ]
                    ]
                ]
            ]
        ]

    let elementOptionsComponent =
        React.functionComponent
            (fun
                (props:
                    {|
                        element: RenderingCode
                        path: int list
                        name: string
                    |}) ->
                let (editedElement, setEditedElement) = React.useState props.element
                let (tag, setTag) = React.useState ""
                let (attributes, setAttributes) = React.useState ""
                let (text, setText) = React.useState ""

                let updateElement (tag: string) (attributes: string) (text: string) element =
                    let attributesList =
                        if String.IsNullOrWhiteSpace(attributes) then
                            []
                        else
                            attributes.Split(',')
                            |> Array.map (fun (kv: string) -> kv.Split('='))
                            |> Array.map (fun kv -> (kv.[0].Trim(), kv.[1].Trim()))
                            |> Array.toList

                    match element with
                    | HtmlElement(oldTag, oldAttributes, oldText) ->
                        let newTag = if String.IsNullOrWhiteSpace(tag) then oldTag else tag

                        let newAttributes =
                            if attributesList.Length = 0 then
                                oldAttributes
                            else
                                attributesList

                        let newText =
                            if String.IsNullOrWhiteSpace(text) then oldText
                            elif newTag = "input" then Value.Empty
                            else Constant text

                        HtmlElement(newTag, newAttributes, newText)
                    | _ -> failwith "Not an element"

                Html.div [
                    prop.className "p-4 bg-white rounded shadow"
                    prop.draggable true
                    prop.children [
                        Html.h4 [ prop.className "text-lg font-medium"; prop.text props.name ]
                        Html.div [
                            prop.className "mt-4"
                            prop.children [
                                Html.input [
                                    prop.className "border p-2 rounded"
                                    prop.placeholder "Tag"
                                    prop.onTextChange (fun newTag ->
                                        setTag newTag
                                        let updatedElement = updateElement newTag attributes text editedElement
                                        setEditedElement updatedElement)
                                ]
                                Html.input [
                                    prop.className "border p-2 rounded mt-2"
                                    prop.placeholder "Attributes"
                                    prop.onTextChange (fun newAttributes ->
                                        setAttributes newAttributes
                                        let updatedElement = updateElement tag newAttributes text editedElement
                                        setEditedElement updatedElement)
                                ]
                                Html.input [
                                    prop.className "border p-2 rounded mt-2"
                                    prop.placeholder "Text"
                                    prop.onTextChange (fun newText ->
                                        setText newText
                                        let updatedElement = updateElement tag attributes newText editedElement
                                        setEditedElement updatedElement)
                                ]
                                Html.button [
                                    prop.className "px-4 py-2 bg-blue-500 text-white rounded hover:bg-blue-600 mt-4"
                                    prop.text ("Add element " + props.name)
                                    prop.onClick (fun _ ->
                                        let updatedElement = updateElement tag attributes text editedElement
                                        setEditedElement updatedElement
                                        dispatch (ReplaceCode(updatedElement, props.path)))
                                ]
                            ]
                        ]
                    ]
                ])

    let listOptionsComponent =
        React.functionComponent
            (fun
                (props:
                    {|
                        list: RenderingCode
                        path: int list
                    |}) ->
                match props.list with
                | HtmlList(listType, numbered, code) ->
                    Html.div [
                        prop.className "p-4 bg-white rounded shadow"
                        prop.children [
                            Html.h4 [ prop.className "text-lg font-medium"; prop.text "List" ]
                            dropdownMenu [ "Unordered list"; "Ordered list" ] (listType.ToString())
                            dropdownMenu [ "List"; "Table" ] "List or table"
                        ]
                    ]
                | _ -> failwith "Not a list")

    let sequenceOptionsComponent =
        React.functionComponent
            (fun
                (props:
                    {|
                        sequence: RenderingCode
                        path: int list
                        name: string
                    |}) ->
                let (isCollapsed, setCollapsed) = React.useState (true)

                Html.div [
                    Html.button [
                        prop.onClick (fun _ -> setCollapsed (not isCollapsed))
                        prop.text (if isCollapsed then "Show options" else "Hide options")
                        prop.className "px-4 py-2 bg-blue-500 text-white rounded hover:bg-blue-600"
                    ]
                    if not isCollapsed then
                        match props.sequence with
                        | Sequence(_) ->
                            Html.div [
                                prop.className "p-4 bg-white rounded shadow"
                                prop.children [
                                    Html.h4 [ prop.className "text-lg font-medium"; prop.text props.name ]
                                    Html.div [
                                        prop.className "mt-4"
                                        prop.children [
                                            Html.button [
                                                prop.className
                                                    "px-4 py-2 bg-blue-500 text-white rounded hover:bg-blue-600"
                                                prop.text "Edit"
                                                prop.onClick (fun _ ->
                                                    dispatch (ReplaceCode(props.sequence, props.path)))
                                            ]
                                        ]
                                    ]
                                ]
                            ]
                        | _ -> failwith "Not a sequence"
                    else
                        Html.text ""
                ])

    let options (code: RenderingCode) (path: int list) (name: string) =
        match code with
        | HtmlElement _ ->
            elementOptionsComponent {|
                element = code
                path = path
                name = name
            |}
        | HtmlList _ -> listOptionsComponent {| list = code; path = path |}
        | Sequence(_) ->
            sequenceOptionsComponent {|
                sequence = code
                path = path
                name = name
            |}
        | Hole _ -> failwith "Hole cannot contain another hole"


    let preview =
        Html.div [
            prop.className " w-1/2 flex justify-center"
            prop.children [
                renderingCodeToReactElement model.CurrentPage.Code [] model.CurrentPage.Data options
            ]
        ]

    let componentsView =
        Html.div [
            prop.className "flex justify-center w-1/2"
            prop.children [
                Html.div [
                    prop.className "flex"
                    prop.children [
                        renderingCodeToReactElement model.CurrentPage.Code [] model.CurrentPage.Data options
                    ]
                ]
            ]
        ]

    let editorView =
        Html.section [
            prop.className "h-screen w-screen"
            prop.children [
                if model.CurrentPage.Data <> JNull then
                    nameEditView
                Html.div [
                    prop.children [
                        if model.CurrentPage.Data = JNull then
                            uploadButton
                        else
                            componentsView
                            preview
                    ]
                ]
            ]
        ]



    Html.div [ prop.className "mt-16 flex"; prop.children [ editorView ] ]