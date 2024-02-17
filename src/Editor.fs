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

type Model =
    { CurrentComponent : Component
      FileUploadError : bool
      EditingName : bool
      EditingCode : bool
      NameInput : string
    }

type Msg =
    | UploadData of string
    | ChangeName of string
    | SetInput of string
    | ChangeNameEditMode of bool
    | SaveComponent of Component
    | ReplaceCode of RenderingCode * int list

let rec replace path replacementElement (currentCode : RenderingCode) =
    match path with
    | [] -> replacementElement
    | head::tail ->
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
                |> List.mapi (fun i item -> if i = head then replace tail replacementElement item else item)
            Sequence(newItems)
        | _ -> currentCode
let init() =
    {CurrentComponent = {Name = "New component"; Code = Hole (UnNamed); Id = Guid.NewGuid(); Data = JNull};
        FileUploadError = false; EditingName = false; NameInput = ""; EditingCode = false;}

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | UploadData data ->
        let loadedDataOption = loadJson data
        match loadedDataOption with
        | Some(data)  ->
            match data with
            | JObject obj ->
                let newComponent = {model.CurrentComponent with Code = recognizeJson data; Data = data;}
                {model with CurrentComponent = newComponent; FileUploadError = false;  }, Cmd.none
            | _ ->
                {model with FileUploadError = true}, Cmd.none
        | None ->
            {model with FileUploadError = true}, Cmd.none
    | ChangeName newName->
        {model with CurrentComponent = {model.CurrentComponent with Name = newName}; NameInput = ""; EditingName = false}, Cmd.none
    | SetInput input->
        {model with NameInput = input}, Cmd.none
    | ChangeNameEditMode value ->
        {model with EditingName = value; NameInput = ""}, Cmd.none
    | SaveComponent comp ->
        model, Cmd.none
    | ReplaceCode (code, path) ->
        Fable.Core.JS.console.log model.CurrentComponent.Code
        let newcodes = replace path code model.CurrentComponent.Code
        Fable.Core.JS.console.log newcodes
        let newComponent: Component = {model.CurrentComponent with Code = newcodes}
        {model with CurrentComponent = newComponent}, Cmd.none


let view (model: Model) (dispatch: Msg -> unit) =

    let upploadButtonView onLoad =
        Bulma.block[
            Bulma.file[
                file.isNormal
                prop.classes ["is-centered"]
                prop.children [
                    Bulma.fileLabel.label [
                        Bulma.fileInput [
                            prop.type' "file"
                            prop.name "component-data"
                            prop.onChange ( handleFileEvent onLoad)
                        ]
                        Bulma.fileCta [
                            Bulma.fileLabel.span [
                                prop.text "Choose a file…"
                            ]
                        ]
                    ]
                ]
            ]
            if  model.FileUploadError then
                Bulma.hero[prop.text "The selected file could not be used for creation."]
            else
                Html.text ""
        ]

    let uploadButton = upploadButtonView (UploadData >> dispatch)

    let nameEditView =
        Bulma.box[
                if model.EditingName then
                    Bulma.block[
                        Bulma.input.text[
                        prop.onTextChange (fun text -> dispatch (SetInput text))
                        ]
                    ]
                else
                    Bulma.block[
                        let nameText = "Component name: " + model.CurrentComponent.Name
                        Html.h1 (nameText)
                    ]
                Bulma.block[
                    Bulma.buttons[
                        Bulma.button.button[
                            color.isPrimary
                            if model.EditingName then
                                if model.NameInput.Length > 0 then
                                    prop.text "Save"
                                    prop.onClick (fun _ -> dispatch (ChangeName model.NameInput))
                                else
                                    color.isWarning
                                    prop.text "Name must be at least one character"
                            else
                                prop.text "Edit name"
                                prop.onClick (fun _ -> dispatch (ChangeNameEditMode (model.EditingName |> not )))
                        ]
                        if model.EditingName then
                            Bulma.button.button[
                                prop.text "Cancel"
                                color.isDanger
                                prop.onClick (fun _ -> dispatch (ChangeNameEditMode false))
                            ]
                        else
                             Bulma.button.button[
                                if model.CurrentComponent.Data = JNull then
                                    color.isWarning
                                    prop.text "Upload data first to save a component"
                                else
                                    prop.text "Save component"
                                    color.isSuccess
                                    prop.onClick (fun _ -> dispatch (SaveComponent model.CurrentComponent))
                            ]
                    ]
                ]
            ]

    let dropdownItem (text: string) =
        Html.a [
            prop.className "dropdown-item"
            prop.href "#"
            prop.text text
        ]


    let dropdownMenu items (defaultText :string)=
        Bulma.block[
            prop.className "dropdown is-hoverable"
            prop.children [
                Html.div [
                    prop.className "dropdown-trigger"
                    prop.children [
                        Html.button [
                            prop.className "button"
                            prop.text defaultText
                            prop.children [
                                Html.span [
                                    prop.text "Dropdown button"
                                ]
                                Html.span [
                                    prop.className "icon is-small"
                                    prop.children [
                                        Html.i [
                                            prop.className "fas fa-angle-down"
                                            prop.custom ("aria-hidden", "true")
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
                Html.div [
                    prop.className "dropdown-menu"
                    prop.id "dropdown-menu"
                    prop.custom ("role", "menu")
                    prop.children [
                        Html.div [
                            prop.className "dropdown-content"
                            prop.children (items |> List.map dropdownItem)
                        ]
                    ]
                ]
            ]
        ]


    let elementOptionsComponent =
        React.functionComponent(fun (props: {| element: RenderingCode; path: int list; name: string |}) ->
            let (editedElement, setEditedElement) = React.useState props.element
            let updateElement text element =
                match element with
                | HtmlElement (tag, attrs, innerText) ->
                    let newElement =
                        match innerText with
                        | Data -> HtmlElement (tag, attrs, Data)
                        | Value.Empty -> HtmlElement (tag, attrs, Value.Empty)
                        | Constant s -> HtmlElement (tag, attrs, Constant text)
                    newElement
                | _ -> failwith "Not an element"
            let newElement =
                match editedElement with
                | HtmlElement (tag, attrs, innerText) ->
                    let newElement =
                        match innerText with
                        | Data -> HtmlElement (tag, attrs, Data)
                        | Value.Empty -> HtmlElement (tag, attrs, Value.Empty)
                        | Constant s -> HtmlElement (tag, attrs, Constant s)
                    newElement
                | _ -> failwith "Not an element"

            Bulma.box[
                Bulma.title[
                    prop.text props.name
                ]
                Bulma.block[
                    Bulma.input.text[
                        prop.onTextChange (fun text -> setEditedElement (updateElement text newElement))
                    ]
                    Bulma.buttons[
                        Bulma.button.button[
                            prop.text ("Add element" + (props.path.ToString()))
                            prop.onClick (fun _ -> dispatch (ReplaceCode (newElement, props.path)))
                        ]
                    ]
                ]
            ]
        )

    let listOptionsComponent =
        React.functionComponent(fun (props: {| list: RenderingCode; path: int list |}) ->
            match props.list with
            | HtmlList (listType, numbered, code) ->
                Bulma.box[
                    Bulma.title[
                        prop.text "List"
                    ]
                    dropdownMenu [
                        "Unordered list"
                        "Ordered list"
                    ]   (listType.ToString())
                    dropdownMenu [
                        "List"
                        "Table"
                    ] "List or table"
                ]
            | _ -> failwith "Not a list"
        )

    let sequenceOptionsComponent =
        React.functionComponent(fun (props: {| sequence: RenderingCode; path: int list; name: string |}) ->
            match props.sequence with
            | Sequence(_) ->
                Bulma.box[
                    Bulma.title[
                        prop.text props.name
                    ]
                    Bulma.block[
                        Bulma.buttons[
                            Bulma.button.button[
                                prop.text "Edit"
                                prop.onClick (fun _ -> dispatch (ReplaceCode (props.sequence, props.path)))
                            ]
                        ]
                    ]
                ]
            | _ -> failwith "Not a sequence"
        )

    let options (code : RenderingCode) (path : int list) (name : string) =
        match code with
        | HtmlElement _ ->
            elementOptionsComponent {| element = code; path = path; name = name |}
        | HtmlList _->
            listOptionsComponent {| list = code; path = path |}
        | Sequence(_) ->
            sequenceOptionsComponent {| sequence = code; path = path; name = name |}
        | Hole _ -> failwith "Hole cannot contain another hole"

    let rec renderingCodeToReactElement (code: RenderingCode) (path : int list) (json : Json) : ReactElement =
        //printfn "Json %A with code %A" json code
            match code with
            | HtmlElement (tag, attrs, innerText) ->
                let props = attrs |> List.toSeq |> dict
                match innerText with
                    | Data ->
                        let selectedFields = json
                        let jsonStr = selectedFields |> Json.convertFromJsonAs<String>
                        Bulma.block[ReactBindings.React.createElement("h1", props, [str jsonStr])]
                    | Value.Empty -> Bulma.block[ReactBindings.React.createElement(tag, props, [])]
                    | Constant s -> Bulma.block[ReactBindings.React.createElement(tag, props, [str s])]
            | HtmlList (listType, numbered, code) ->
                Bulma.block[options code path "List"]
            | Sequence codes ->
                let jsonList =
                    match json with
                    | JObject object -> object |> Map.toList
                    | _ -> failwith "Not a sequence"
                let renderedElements =
                    List.mapi (fun index code ->
                        let (_, jsonSubObject) = List.item index jsonList
                        renderingCodeToReactElement code (path @ [index]) jsonSubObject) codes
                Bulma.block[ReactBindings.React.createElement("h4", [], renderedElements)]
            | Hole named ->
                let name =
                    match named with
                    | UnNamed -> "Unnamed"
                    | Named name -> name
                let fieldType = json |> recognizeJson
                let optionPane = options fieldType path name
                Bulma.block[optionPane]

    let editorView  =
        Bulma.box[
            if model.CurrentComponent.Data <> JNull then
                nameEditView
            Bulma.box[
                prop.children[
                if model.CurrentComponent.Data = JNull then
                    uploadButton
                else
                    Bulma.columns[
                        prop.classes ["is-centered"]
                        prop.children[
                            Bulma.column[
                                prop.children[
                                    renderingCodeToReactElement model.CurrentComponent.Code [] model.CurrentComponent.Data
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
    editorView