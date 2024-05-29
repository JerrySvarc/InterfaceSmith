module Main

open Elmish
open Feliz
open Types
open Fable.SimpleJson
open DataLoading
open DataRecognition
open FileUpload
open System
open EditorUtils
open AppUtilities
open Microsoft.FSharp.Reflection
open Browser
open Fable.Core.JsInterop

type TabType =
    | Main
    | Editor
    | Download

type Model = {
    CurrentPage: Page
    FileUploadError: bool
    EditingName: bool
    EditingCode: bool
    NameInput: string
    CurrentTab: TabType
    IsPreview: bool
    CurrModifiedElement: RenderingCode * int list
}

type Msg =
    | UploadData of string
    | ChangeName of string
    | SetInput of string
    | ChangeNameEditMode of bool
    | SavePage of Page
    | ReplaceCode of RenderingCode * int list
    | ChangeTab of TabType
    | TogglePreview
    | SetCurrentModifiedElement of RenderingCode * int list

let init () : Model * Cmd<Msg> =
    {
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
        CurrentTab = Main
        IsPreview = true
        CurrModifiedElement = (Hole(UnNamed), [])
    },
    Cmd.none

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
    | ChangeTab tab -> { model with CurrentTab = tab }, Cmd.none
    | TogglePreview ->
        {
            model with
                IsPreview = not model.IsPreview
        },
        Cmd.none
    | SetCurrentModifiedElement(code, path) ->
        {
            model with
                CurrModifiedElement = (code, path)
        },
        Cmd.none

let view (model: Model) (dispatch: Msg -> unit) =

    let uploadButtonView onLoad =
        Html.div [
            prop.onClick (fun _ ->
                Browser.Dom.document.getElementById("file-input")?click()
            )
            prop.children [
                Html.button [
                    prop.text "Upload a JSON file"
                    prop.className "px-4 py-2 bg-secondary-200 text-white font-semibold rounded hover:bg-secondary-500 transition duration-200 ease-in-out inline-block"
                ]
                Html.input [
                    prop.id "file-input"
                    prop.type' "file"
                    prop.name "component-data"
                    prop.onChange (handleFileEvent onLoad)
                    prop.style [style.display.none]
                ]
                Html.p [
                    prop.className "text-sm text-gray-500 mt-2 bg-yellow-100 p-3 rounded border border-yellow-200"
                    prop.text "Make sure the JSON file is valid and contains a field called 'data' containing the data you want to use."
                ]
            ]
        ]

    let uploadButton = uploadButtonView (UploadData >> dispatch)

    let menu (options: (string * TabType) list) =
            Html.div [
                prop.className "flex flex-col bg-gray-800 text-white p-4"
                prop.children (
                    options
                    |> List.map (fun (name, page) ->
                        Html.div [
                            prop.className "text-left py-2 pl-4 cursor-pointer hover:bg-blue-500 hover:text-white border-b border-gray-700 text-lg transition-colors duration-200"
                            prop.children [ Html.text name ]
                            prop.onClick (fun _ -> dispatch (ChangeTab page))
                        ])
                )]

    let menuOptions = [ ("Main", Main); ("Editor", Editor); ("Download", Download) ]

    let mainPage =
        Html.div [
            prop.className "flex flex-col items-center justify-center h-screen bg-gradient-to-r from-green-400 to-blue-500 text-white"
            prop.children [
                Html.h1 [
                    prop.className "text-6xl font-bold text-center mb-4"
                    prop.text "Welcome to Data-Driven UI editor"
                ]
                Html.p [
                    prop.className "text-2xl text-center mb-2"
                    prop.text "The editor allows you to create web applications based on concrete data."
                ]
                Html.p [
                    prop.className "text-2xl text-center mb-2"
                    prop.text "Upload a JSON file to get started. Then use the menus to create and modify UI elements."
                ]
                Html.button [
                    prop.className "mt-8 px-4 py-2 text-black border-black bg-amber-400 rounded shadow font-bold text-xl"
                    prop.text "Get Started"
                    prop.onClick(fun _ -> dispatch (ChangeTab Editor))
                ]
            ]
        ]

    let rec options (code: RenderingCode) (path: int list) (name: string) : ReactElement =
        match code with
        | HtmlElement _ -> elementOptionsComponent (name, code, path)
        | HtmlList _ -> listOptionsComponent (name, code, path)
        | Sequence(_) -> sequenceOptionsComponent (name ,code, path)
        | Hole _ -> uiBlock ([ Html.text "No options available." ])

    and collapsableOption =
        React.functionComponent (fun (option : ReactElement) ->
            let (collapsed, setCollapsed) = React.useState true

            Html.div [
                prop.children [
                    Html.button [
                        prop.text "Toggle options"
                        prop.onClick (fun _ -> setCollapsed (not collapsed))
                    ]
                    match collapsed with
                    | false -> option
                    |true -> Html.none
                ]
            ]
        )

    //TODO: Refactor elementoptions
    and elementOptionsComponent =
        React.functionComponent (fun (name :string, code: RenderingCode, path) ->
            let (attributeName, setAttributeName) = React.useState ""
            let (attributeValue, setAttributeValue) = React.useState ""
            let (constantValue, setConstantValue) = React.useState ""

            match code with
            | HtmlElement(tag, attrs, innerValue) ->
                Html.div [
                    prop.className "flex flex-row flex-nowrap p-1 border-1 border-gray-300 bg-white rounded"
                    prop.children [
                        Html.h1 [
                            prop.className "p-4 bg-white rounded"
                            prop.text name
                        ]
                        let tagOptions =
                            FSharpType.GetUnionCases(typeof<Tag>)
                            |> Array.map (fun caseInfo -> caseInfo.Name)

                        //tag selector
                        Html.div [
                            prop.className "p-1 bg-gray-100  my-1"
                            prop.children [
                                Html.p [ prop.className "font-bold text-gray-700"; prop.text "Change tag" ]
                                Html.select [
                                    prop.className "p-1  bg-white "
                                    prop.onMouseDown (fun e -> e.stopPropagation ())
                                    prop.value (tag.ToString())
                                    prop.onChange (fun (e: Browser.Types.Event) ->
                                        let selectedTag = e.target?value |> string
                                        let newTag = selectedTag.ToLower() |> stringToTag
                                        dispatch (ReplaceCode(HtmlElement(newTag, attrs, innerValue), path)))
                                    prop.children (
                                        [ Html.option [ prop.value ""; prop.text "Select tag" ] ]
                                        @ (tagOptions
                                        |> Array.toList
                                        |> List.map (fun tag -> Html.option [ prop.value tag; prop.text tag ]))
                                    )
                                ]
                            ]
                        ]
                    //attribute editor
                        Html.div [
                            prop.className "p-1  bg-gray-100 rounded my-1"
                            prop.children [
                                Html.input [
                                    prop.className "p-2 border-2 border-gray-200 bg-white rounded my-2"
                                    prop.placeholder "Attribute name"
                                    prop.onTextChange (fun e -> setAttributeName (e))
                                ]
                                Html.input [
                                    prop.className "p-2 border-2 border-gray-200 bg-white rounded my-2"
                                    prop.placeholder "Attribute value"
                                    prop.onTextChange (fun e -> setAttributeValue (e))
                                ]
                                Html.button [
                                    prop.className "p-2 border-2 border-gray-200 bg-green-500 text-white rounded my-2"
                                    prop.text "Add attribute"
                                    prop.onClick (fun _ ->
                                        let newAttribute = Types.Attribute(attributeName, Constant(attributeValue))
                                        let updatedAttrs = [newAttribute] @ attrs
                                        dispatch (ReplaceCode(HtmlElement(tag, updatedAttrs, innerValue), path)))
                                ]
                            ]
                        ]

                        let innerValueOptions = [ "Data"; "Constant"; "Empty" ]

                        let innerValueToString (innerValue: InnerValue) =
                            match innerValue with
                            | Data -> "Data"
                            | Constant _ -> "Constant"
                            | Empty -> "Empty"

                        Html.div [
                            prop.className "p-1  bg-gray-100 rounded my-1"
                            prop.children [
                                Html.select [
                                    prop.className "p-1 border-2 border-gray-200 bg-white rounded"
                                    prop.onMouseDown (fun e -> e.stopPropagation ())
                                    prop.value (innerValue |> innerValueToString) // Set the initial value to the current innerValue
                                    prop.onChange (fun (e: Browser.Types.Event) ->
                                        let selectedValue = e.target?value |> string

                                        match selectedValue with
                                        | "Data" ->
                                            dispatch (ReplaceCode(HtmlElement(tag, attrs, InnerValue.Data), path))
                                        | "Constant" ->
                                            dispatch (
                                                ReplaceCode(HtmlElement(tag, attrs, InnerValue.Constant ""), path)
                                            )
                                        | "Empty" ->
                                            dispatch (ReplaceCode(HtmlElement(tag, attrs, InnerValue.Empty), path))
                                        | _ -> ())
                                    prop.children (
                                        innerValueOptions
                                        |> List.map (fun value -> Html.option [ prop.value value; prop.text value ])
                                    )
                                ]
                                match innerValue with
                                | InnerValue.Constant _ ->
                                    Html.input [
                                        prop.className "p-1 border-2 border-gray-200 bg-white rounded"
                                        prop.placeholder "Enter constant value"
                                        prop.value constantValue
                                        prop.onInput (fun e -> setConstantValue (e.target?value |> string))
                                        prop.onBlur (fun _ ->
                                            dispatch (
                                                ReplaceCode(
                                                    HtmlElement(tag, attrs, InnerValue.Constant constantValue),
                                                    path
                                                )
                                            ))
                                    ]
                                | _ -> Html.none
                            ]
                        ]
                    ]
                ]
            | _ -> failwith "Not a valid code type.")

    and listOptionsComponent =
        React.functionComponent (fun (name, code: RenderingCode, path) ->
            match code with
            | HtmlList(listType, headers, elementCode) ->
                let listTypeOptions =
                    FSharpType.GetUnionCases(typeof<ListType>)
                    |> Array.map (fun caseInfo -> caseInfo.Name)

                Html.div [
                    prop.className "p-4 border-2 border-gray-300 bg-white rounded"
                    prop.children [
                        // List type selector
                        Html.select [
                            prop.className "p-2 border-2 border-gray-200 bg-white rounded"
                            prop.onMouseDown (fun e -> e.stopPropagation ())
                            prop.onClick (fun e -> e.stopPropagation ())
                            prop.value (listType.ToString())
                            prop.onChange (fun (e: Browser.Types.Event) ->
                                let selectedListType = e.target?value |> string
                                let newListType = selectedListType |> stringToListType
                                dispatch (ReplaceCode(HtmlList(newListType, headers, elementCode), path)))
                            prop.children (
                                listTypeOptions
                                |> Array.toList
                                |> List.map (fun listType -> Html.option [ prop.value listType; prop.text listType ])
                            )
                        ]
                    ]]

            | _ -> failwith "Invalid code type.")
    //TODO: implement this
    and sequenceOptionsComponent =
        React.functionComponent (fun (name, code: RenderingCode, path) ->
            match code with
            | Sequence(elements) ->
                Html.div [
                    prop.className ""
                    prop.children [
                        Html.button [
                            prop.onClick(fun _ -> dispatch ( ReplaceCode (code,path)))
                            prop.text "Add sequence"
                        ]
                    ]
                ]

            | _ -> failwith "Invalid code type.")



    let editor =
        Html.div [
            prop.className "p-4 border-2 border-gray-300 bg-white rounded"
            prop.children [
                Html.div [
                    prop.className "p-4 bg-white rounded"
                    prop.children [
                        if model.CurrentPage.Data = JNull then
                            if model.CurrentPage.Data = JNull then
                                uploadButton
                            else
                                uiBlock ([ Html.text "Data uploaded successfully!" ])

                            if model.FileUploadError then
                                Html.div [
                                    prop.className "w-full mt-4 p-4 bg-secondary-600 text-white rounded"
                                    prop.children [ Html.text "The selected file cannot be used." ]
                                ]
                        else
                            renderingCodeToReactElement
                                model.CurrentPage.Code
                                []
                                model.CurrentPage.Data
                                options
                                model.IsPreview

                           //suggestions for selected element
                            Html.text (model.CurrModifiedElement.ToString())
                    ]
                ]
            ]
        ]
    //TODO: Implement download page
    let download =
        Html.div [
            prop.className " "
            prop.children [
                match model.CurrentPage.Code with
                | Hole _ -> uiBlock ([ Html.p [ prop.text "Create page elements first." ] ])
                | _ ->
                    Html.p [ prop.className ""; prop.text "Download the source code of the page." ]

                    Html.button [ prop.className " "; prop.text "Download" ]
            ]
        ]
    let togglePreviewButton =
        Html.button [
            prop.className "px-2 py-2 bg-green-200 text-green-800 font-semibold rounded hover:bg-green-300 transition duration-200 ease-in-out inline-block"
            prop.text "Toggle preview"
            prop.onClick (fun _ -> dispatch TogglePreview)
        ]


    Html.div [
        prop.className "flex"
        prop.children [
            Html.div [
                prop.className "w-64  overflow-auto"
                prop.children [
                    menu menuOptions
                ]
            ]
            Html.div [
                prop.className "flex-grow bg-white p-4 overflow-auto h-full min-h-screen"
                prop.children [
                    match model.CurrentTab with
                    | Main -> mainPage
                    | Editor -> [editor; togglePreviewButton] |> Html.div
                    | Download -> uiBlock [ download ]
                ]
            ]
        ]
    ]