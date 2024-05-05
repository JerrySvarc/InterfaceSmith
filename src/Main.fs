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
        IsPreview = false
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

    let upploadButtonView onLoad =
        Html.div [
            prop.className "items-center justify-center"
            prop.children [
                Html.div [
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
                                        "px-8 py-4 bg-blue-500 text-white rounded cursor-pointer hover:bg-blue-600 transition duration-200 ease-in-out text-xl"
                                    prop.children [ Html.text "Select a file" ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]

    let uploadButton = upploadButtonView (UploadData >> dispatch)

    let menu (options: (string * TabType) list) =
        Html.div [
            prop.className "flex border-b fixed top-0 left-0 w-full z-50 bg-white"
            prop.children (
                options
                |> List.map (fun (name, page) ->
                    Html.div [
                        prop.className "flex-1 text-center py-4 cursor-pointer hover:bg-gray-100"
                        prop.children [ Html.text name ]
                        prop.onClick (fun _ -> dispatch (ChangeTab page))
                    ])
            )
        ]

    let menuOptions = [ ("Main", Main); ("Editor", Editor); ("Download", Download) ]

    let mainPage =
        Html.div [
            prop.className
                "flex flex-col items-center justify-center min-h-screen bg-primary-100 p-6 w-full max-w-full font-display"
            prop.children [
                Html.h1 [
                    prop.className "text-5xl font-bold text-primary-900 mb-8 w-full text-center"
                    prop.text "Welcome to the data-driven UI editor!"
                ]
                Html.div [
                    prop.className
                        "bg-white shadow-md rounded px-8 pt-6 pb-8 mb-4 flex flex-col my-2 w-full md:w-3/4 lg:w-1/2"
                    prop.children [
                        Html.p [
                            prop.className "text-2xl text-primary-700 mb-4 w-full text-center"
                            prop.text "Please upload a JSON file to get started."
                        ]
                        Html.p [
                            prop.className "text-2xl text-primary-700 mb-4 w-full text-center"
                            prop.text "You can also create a new component from scratch."
                        ]
                        Html.p [
                            prop.className "text-2xl text-primary-700 mb-4 w-full text-center"
                            prop.text "Select the 'Editor' tab to start editing the component."
                        ]
                        Html.p [
                            prop.className "text-2xl text-primary-700 mb-4 w-full text-center"
                            prop.text "Select the 'Download' tab to download the created page."
                        ]

                        if model.CurrentPage.Data = JNull then
                            uiBlock [ uploadButton ]
                        else
                            uiBlock ([ Html.text "Data uploaded successfully!" ])

                        if model.FileUploadError then
                            uiBlock (
                                [
                                    Html.div [
                                        prop.className "w-full mt-4 p-4 bg-red-600 text-white rounded"
                                        prop.children [ Html.text "The selected file cannot be used." ]
                                    ]
                                ]
                            )
                    ]
                ]
            ]
        ]

    let tags = [
        "p"
        "h1"
        "h2"
        "h3"
        "h4"
        "h5"
        "h6"
        "strong"
        "em"
        "a"
        "li"
        "ul"
        "ol"
        "pre"
        "code"
        "blockquote"
        "div"
        "span"
        "article"
        "section"
        "header"
        "footer"
        "nav"
        "main"
        "input"
    ]



    let elementOptionsComponent code path =
        Html.div [
            prop.className "flex items-center bg-white shadow-md rounded px-4 py-2 mb-4 font-display"
            prop.onClick (fun _ -> dispatch (SetCurrentModifiedElement(code, path)))
            prop.children [
                Html.p [ prop.className "text-primary-700 mr-2"; prop.text "Tag: " ]
                Html.input [ prop.className "border border-primary-500 rounded px-2 py-1 mr-2 flex-grow" ]
                Html.button [
                    prop.className "px-4 py-2 bg-secondary-500 text-white rounded hover:bg-secondary-600"
                    prop.text "Save"
                    prop.onClick (fun _ -> dispatch (ReplaceCode(code, path)))
                ]
            ]
        ]




    let listOptionsComponent code path =
        Html.div [
            prop.className "flex items-center bg-white shadow-md rounded px-4 py-2 mb-4 font-display"
            prop.onClick (fun _ -> dispatch (SetCurrentModifiedElement(code, path)))
            prop.children [
                Html.p [ prop.className "text-primary-700 mr-2"; prop.text "List: " ]
                Html.input [ prop.className "border border-primary-500 rounded px-2 py-1 mr-2 flex-grow" ]
                Html.button [
                    prop.className "px-4 py-2 bg-secondary-500 text-white rounded hover:bg-secondary-600"
                    prop.text "Save"
                    prop.onClick (fun _ -> dispatch (ReplaceCode(code, path)))
                ]
            ]
        ]

    let sequenceOptionsComponent code path =
        Html.div [
            prop.className "flex items-center bg-white shadow-md rounded px-4 py-2 mb-4 font-display"
            prop.onClick (fun _ -> dispatch (SetCurrentModifiedElement(code, path)))
            prop.children [
                Html.p [ prop.className "text-primary-700 mr-2"; prop.text "Sequence: " ]
                Html.input [ prop.className "border border-primary-500 rounded px-2 py-1 mr-2 flex-grow" ]
                Html.button [
                    prop.className "px-4 py-2 bg-secondary-500 text-white rounded hover:bg-secondary-600"
                    prop.text "Save"
                    prop.onClick (fun _ -> dispatch (ReplaceCode(code, path)))
                ]
            ]
        ]

    let options (code: RenderingCode) (path: int list) (name: string) : ReactElement =
        match code with
        | HtmlElement _ -> elementOptionsComponent code path
        | HtmlList _ -> listOptionsComponent code path
        | Sequence(_) -> sequenceOptionsComponent code path
        | Hole _ -> uiBlock ([ Html.text "No options available." ])

    let editor =
        Html.div [
            prop.className "flex justify-center items-center h-full w-full pt-16"
            prop.children [
                Html.div [
                    prop.className "flex flex-col justify-around w-full max-w-md"
                    prop.children [
                        if model.CurrentPage.Data = JNull then
                            uiBlock ([ Html.text "Upload data to start!" ])
                        else
                            renderingCodeToReactElement
                                model.CurrentPage.Code
                                []
                                model.CurrentPage.Data
                                options
                                model.IsPreview

                            Html.text (model.CurrModifiedElement.ToString())

                            Html.button [
                                prop.className "self-center mt-4"
                                prop.text "Toggle preview"
                                prop.onClick (fun _ -> dispatch TogglePreview)
                            ]

                            uiBox ([ prettyPrint model.CurrentPage.Data ])
                    ]
                ]
            ]
        ]

    let download =
        Html.div [
            prop.className "flex  w-full "
            prop.children [
                match model.CurrentPage.Code with
                | Hole _ -> uiBlock ([ Html.p [ prop.text "Create page elements first." ] ])
                | _ ->
                    Html.p [
                        prop.className "flex text-2xl mb-4"
                        prop.text "Download the source code of the page."
                    ]

                    Html.button [
                        prop.className
                            " flex px-8 py-4 bg-blue-500 text-white rounded cursor-pointer hover:bg-blue-600 transition duration-200 ease-in-out text-xl"
                        prop.text "Download"
                    ]
            ]
        ]



    Html.div [
        prop.className "flex justify-center items-center h-full w-full pt-16"
        prop.children [
            Html.div [
                prop.className "flex justify-around w-full"
                prop.children [
                    menu menuOptions
                    match model.CurrentTab with
                    | Main -> mainPage
                    | Editor -> editor
                    | Download -> uiBlock [ download ]
                ]
            ]
        ]
    ]