module Editor

open Elmish
open Feliz
open Feliz.Bulma
open Types
open Fable.SimpleJson
open DataLoading
open DataRecognition
open FileUpload
open System

open Fable.React

type Model = {
    CurrentComponent: Component
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
    | SaveComponent of Component
    | ReplaceCode of RenderingCode * int list

let rec replace path replacementElement (currentCode: RenderingCode) =
    match path with
    | [] -> replacementElement
    | head :: tail ->
        match currentCode with
        | HtmlList(lt, n, item) ->
            let newItems =
                match item with
                | Hole _ -> item
                | _ -> replace tail replacementElement item

            HtmlList(lt, n, newItems)
        | Sequence(items) ->
            let newItems =
                items
                |> List.mapi (fun i item ->
                    if i = head then
                        replace tail replacementElement item
                    else
                        item)

            Sequence(newItems)
        | _ -> currentCode

let init () = {
    CurrentComponent = {
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
                    model.CurrentComponent with
                        Code = recognizeJson data
                        Data = data
                }

                {
                    model with
                        CurrentComponent = newComponent
                        FileUploadError = false
                },
                Cmd.none
            | _ -> { model with FileUploadError = true }, Cmd.none
        | None -> { model with FileUploadError = true }, Cmd.none
    | ChangeName newName ->
        {
            model with
                CurrentComponent = {
                    model.CurrentComponent with
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
    | SaveComponent comp -> model, Cmd.none
    | ReplaceCode(code, path) ->
        let newcodes = replace path code model.CurrentComponent.Code

        let newComponent: Component = {
            model.CurrentComponent with
                Code = newcodes
        }

        {
            model with
                CurrentComponent = newComponent
        },
        Cmd.none


let view (model: Model) (dispatch: Msg -> unit) =

    let upploadButtonView onLoad =
        Html.div [
            prop.className "mt-10 block "
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
                                    prop.className "px-4 py-2 bg-blue-500 text-white rounded cursor-pointer"
                                    prop.children [ Html.text "Choose a file…" ]
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
        Html.div [
            prop.className "p-4 border rounded"
            prop.children [
                if model.EditingName then
                    Html.div [
                        prop.className "block"
                        prop.children [
                            Html.input [
                                prop.className "border p-2 rounded"
                                prop.onTextChange (fun text -> dispatch (SetInput text))
                            ]
                        ]
                    ]
                else
                    Html.div [
                        prop.className "block"
                        prop.children [
                            let nameText = "Component name: " + model.CurrentComponent.Name
                            Html.h1 [ prop.className "text-xl font-bold" ]
                            Html.text nameText
                        ]
                    ]
                Html.div [
                    prop.className "flex space-x-4"
                    prop.children [
                        Html.button [
                            prop.className (
                                if model.EditingName then
                                    if model.NameInput.Length > 0 then
                                        "bg-blue-500 text-white px-4 py-2 rounded"
                                    else
                                        "bg-yellow-500 text-white px-4 py-2 rounded"
                                else
                                    "bg-gray-500 text-white px-4 py-2 rounded"
                            )
                            prop.children [
                                if model.EditingName then
                                    if model.NameInput.Length > 0 then
                                        Html.text "Save"
                                    else
                                        Html.text "Name must be at least one character"
                                else
                                    Html.text "Edit name"
                            ]
                            prop.onClick (
                                if model.EditingName && model.NameInput.Length > 0 then
                                    (fun _ -> dispatch (ChangeName model.NameInput))
                                else if not model.EditingName then
                                    (fun _ -> dispatch (ChangeNameEditMode(model.EditingName |> not)))
                                else
                                    (fun _ -> ())
                            )
                        ]
                        if model.EditingName then
                            Html.button [
                                prop.className "bg-red-500 text-white px-4 py-2 rounded"
                                prop.children [ Html.text "Cancel" ]
                                prop.onClick (fun _ -> dispatch (ChangeNameEditMode false))
                            ]
                        else
                            Html.button [
                                prop.className (
                                    if model.CurrentComponent.Data = JNull then
                                        "bg-yellow-500 text-white px-4 py-2 rounded"
                                    else
                                        "bg-green-500 text-white px-4 py-2 rounded"
                                )
                                prop.children [
                                    if model.CurrentComponent.Data = JNull then
                                        Html.text "Upload data first to save a component"
                                    else
                                        Html.text "Save component"
                                ]
                                prop.onClick (fun _ -> dispatch (SaveComponent model.CurrentComponent))
                            ]
                    ]
                ]
            ]
        ]

    let dropdownItem (text: string) =
        Html.a [
            prop.className "block px-4 py-2 text-sm text-gray-700 hover:bg-gray-100"
            prop.href "#"
            prop.text text
        ]


    let dropdownMenu items (defaultText: string) =
        Html.div [
            prop.className "relative inline-block text-left"
            prop.children [
                Html.button [
                    prop.className
                        "inline-flex justify-center w-full rounded-md border border-gray-300 shadow-sm px-4 py-2 bg-white text-sm font-medium text-gray-700 hover:bg-gray-50 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500"
                    prop.text defaultText
                    prop.children [ Html.span [ prop.text "Dropdown button" ] ]
                ]
                Html.div [
                    prop.className
                        "origin-top-right absolute right-0 mt-2 w-56 rounded-md shadow-lg bg-white ring-1 ring-black ring-opacity-5"
                    prop.custom ("role", "menu")
                    prop.children [
                        Html.div [ prop.className "py-1"; prop.children (items |> List.map dropdownItem) ]
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

                let updateElement text element =
                    match element with
                    | HtmlElement(tag, attrs, innerText) ->
                        let newElement =
                            match innerText with
                            | Data -> HtmlElement(tag, attrs, Data)
                            | Value.Empty -> HtmlElement(tag, attrs, Value.Empty)
                            | Constant s -> HtmlElement(tag, attrs, Constant text)

                        newElement
                    | _ -> failwith "Not an element"

                let newElement =
                    match editedElement with
                    | HtmlElement(tag, attrs, innerText) ->
                        let newElement =
                            match innerText with
                            | Data -> HtmlElement(tag, attrs, Data)
                            | Value.Empty -> HtmlElement(tag, attrs, Value.Empty)
                            | Constant s -> HtmlElement(tag, attrs, Constant s)

                        newElement
                    | _ -> failwith "Not an element"

                Html.div [
                    prop.className "p-4 bg-white rounded shadow"
                    prop.children [
                        Html.h4 [ prop.className "text-lg font-medium"; prop.text props.name ]
                        Html.div [
                            prop.className "mt-4"
                            prop.children [
                                Html.input [
                                    prop.className "border p-2 rounded"
                                    prop.onTextChange (fun text -> setEditedElement (updateElement text newElement))
                                ]
                                Html.button [
                                    prop.className "px-4 py-2 bg-blue-500 text-white rounded hover:bg-blue-600 mt-4"
                                    prop.text ("Add element " + props.name)
                                    prop.onClick (fun _ -> dispatch (ReplaceCode(newElement, props.path)))
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
                                        prop.className "px-4 py-2 bg-blue-500 text-white rounded hover:bg-blue-600"
                                        prop.text "Edit"
                                        prop.onClick (fun _ -> dispatch (ReplaceCode(props.sequence, props.path)))
                                    ]
                                ]
                            ]
                        ]
                    ]
                | _ -> failwith "Not a sequence")

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

    let rec renderingCodeToReactElement (code: RenderingCode) (path: int list) (json: Json) : ReactElement =
        match code with
        | HtmlElement(tag, attrs, innerText) ->
            let props = attrs |> List.toSeq |> dict

            match innerText with
            | Data ->
                let selectedFields = json
                let jsonStr = selectedFields |> Json.convertFromJsonAs<String>
                ReactBindings.React.createElement ("div", props, [ str jsonStr ])
            | Value.Empty -> ReactBindings.React.createElement (tag, props, [])
            | Constant s -> ReactBindings.React.createElement ("div", props, [ str s ])
        | HtmlList(listType, numbered, code) ->
            ReactBindings.React.createElement ("div", [], [ options code path "List" ])
        | Sequence codes ->
            let jsonList =
                match json with
                | JObject object -> object |> Map.toList
                | _ -> failwith "Not a sequence"

            let renderedElements =
                List.mapi
                    (fun index code ->
                        let (_, jsonSubObject) = List.item index jsonList
                        renderingCodeToReactElement code (path @ [ index ]) jsonSubObject)
                    codes

            ReactBindings.React.createElement ("div", [], renderedElements)
        | Hole named ->
            let name =
                match named with
                | UnNamed -> "Unnamed"
                | Named name -> name

            let fieldType = json |> recognizeJson
            let optionPane = options fieldType path name
            ReactBindings.React.createElement ("div", [], [ optionPane ])

    let editorView =
        Html.section [
            prop.className "h-screen w-screen"
            prop.children [
                if model.CurrentComponent.Data <> JNull then
                    nameEditView
                Html.div[prop.children[if model.CurrentComponent.Data = JNull then
                                           uploadButton
                                       else
                                           Html.div [
                                               prop.className "flex justify-center"
                                               prop.children [
                                                   Html.div [
                                                       prop.className "flex"
                                                       prop.children [
                                                           renderingCodeToReactElement
                                                               model.CurrentComponent.Code
                                                               []
                                                               model.CurrentComponent.Data
                                                       ]
                                                   ]
                                               ]
                                           ]]]
            ]
        ]

    Html.div [ prop.className "mt-16 flex"; prop.children [ editorView ] ]