module UIComponents.EditorComponents

open Feliz
open Elmish
open Fable.React
open Types.EditorDomain
open Types.RenderingTypes
open Fable.SimpleJson
open Fable.Core.JsInterop
open Utilities.EditorUtils
open UIComponents.OptionComponents
open Utilities.FileUpload

open UIComponents.DownloadPageComponents
open Browser.Types
open Fable.Core.JS
open Fable.Core

let rec options (dispatch: Msg -> unit) (code: RenderingCode) (path: int list) (name: string) : ReactElement =
    match code with
    | HtmlElement _ -> ElementOption(dispatch, name, code, path)
    | HtmlList _ -> ListOption(dispatch, name, code, path)
    | HtmlObject(_) -> SequenceOption(dispatch, name, code, path)
    | Hole _ -> Html.none

[<ReactComponent>]
let DataUpload (dispatch) =
    let uploadButtonView onLoad =
        Html.div [
            prop.className "flex m-1 p-2 "
            prop.children [
                Html.label [
                    prop.className "block"
                    prop.children [
                        Html.input [
                            prop.type' "file"
                            prop.className
                                "block w-full text-sm text-black file:m-2 file:p-3  file:rounded-md file:border file:text-sm file:font-semibold file:bg-secondary-300 file:text-black hover:file:bg-secondary-600"
                            prop.onChange (handleFileEvent onLoad)
                        ]
                    ]
                ]
            ]
        ]

    let uploadButton = uploadButtonView (UploadData >> dispatch)


    Html.div [ prop.children [ uploadButton ] ]




[<ReactComponent>]
let PreviewButton (dispatch) =
    Html.button [
        prop.className "bg-secondary-300 border-gray-400 m-1 p-2 rounded-md text-xl  hover:bg-secondary-600"
        prop.text "Preview"
        prop.onClick (fun _ -> dispatch TogglePreview)
    ]

[<ReactComponent>]
let ToggleOptionsButton (dispatch) =
    Html.button [
        prop.className "bg-secondary-300 border-gray-400 m-1 p-2 rounded-md text-xl  hover:bg-secondary-600"
        prop.text "Toggle options"
        prop.onClick (fun _ -> dispatch ToggleOptions)
    ]


[<ReactComponent>]
let PageHeader (page: Page, dispatch) =
    let (pageName, changeName) = React.useState (page.Name)
    let (editing, setEditing) = React.useState false

    let saveAndUpdateName (name) =
        dispatch (ChangeName name)
        changeName name
        setEditing false

    Html.div [
        prop.className "flex flex-row bg-white mt-3 justify-evenly items-center"
        prop.children [
            Html.div [
                prop.className "flex items-center"
                prop.children [
                    Html.div [
                        prop.className "flex items-center"
                        prop.children [
                            Html.p [ prop.className "text-xl m-1 p-2"; prop.text "Page Name: " ]
                            Html.input [
                                prop.type' "text"
                                match editing with
                                | true -> prop.className "rounded-md border-slate-500 m-1 p-2 border"
                                | false -> prop.className "rounded-md border-slate-500 m-1 p-2 border hidden"
                                prop.value pageName
                                prop.onTextChange (fun e -> changeName e)
                            ]
                            Html.p [
                                prop.className (
                                    match editing with
                                    | true -> sprintf "text-xl m-1 p-2 hidden"
                                    | _ -> sprintf "text-xl m-1 p-2 "
                                )
                                prop.text pageName
                            ]
                            Html.button [
                                prop.className
                                    "bg-secondary-300 border-gray-400 m-1 p-2 rounded-md text-xl  hover:bg-secondary-600"
                                match editing with
                                | true ->
                                    prop.text "Save"
                                    prop.onClick (fun _ -> saveAndUpdateName pageName)
                                | false ->
                                    prop.text "Edit"
                                    prop.onClick (fun _ -> setEditing true)
                            ]
                        ]
                    ]

                ]
            ]
            DataUpload(dispatch)
            PreviewButton(dispatch)
        ]
    ]


[<ReactComponent>]
let EditingWindow (model: Model, dispatch: Msg -> unit) : ReactElement =
    Html.div [
        prop.className "flex flex-col p-4 m-2 min-h-screen overflow-auto"

    ]

[<ReactComponent>]
let Editor (model: Model, dispatch) =
    Html.div [
        prop.className "mt-1"
        prop.children [ ]
    ]