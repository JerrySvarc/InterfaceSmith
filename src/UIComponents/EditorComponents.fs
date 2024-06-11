module UIComponents.EditorComponents

open Feliz
open Elmish
open Fable.React
open Types
open Fable.SimpleJson
open Fable.Core.JsInterop
open Utilities.EditorUtils
open UIComponents.OptionComponents
open Utilities.FileUpload
open Utilities.GeneralUtilities
open UIComponents.DownloadPageComponents
open Browser.Types
open Fable.Core.JS

let rec options (code: RenderingCode) (path: int list) (name: string) : ReactElement =
    match code with
    | HtmlElement _ -> ElementOption(name, code, path)
    | HtmlList _ -> ListOption(name, code, path)
    | Sequence(_) -> SequenceOption(name, code, path)
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
let PageHeader (page: Page, dispatch) =
    let (pageName, changeName) = React.useState (page.Name)
    let (editing, setEditing) = React.useState false
    let inputRef = React.useRef (None)

    let focusTextInput () =
        match inputRef.current with
        | None -> ()
        | Some element ->
            console.log "focus"
            let inputElement = unbox<HTMLInputElement> element
            inputElement.focus ()

    let saveAndUpdateName (name) =
        dispatch (ChangeName name)
        setEditing (not editing)

    let editName () =
        setEditing (not editing)
        focusTextInput ()

    Html.div [
        prop.className "flex flex-row bg-white mt-3 justify-evenly items-center"
        prop.children [
            Html.div [
                prop.className "flex items-center"
                prop.children [
                    match editing with
                    | true ->
                        Html.div [
                            prop.className "flex items-center"
                            prop.children [
                                Html.input [
                                    prop.ref inputRef

                                    prop.className "rounded-md border-slate-500 m-1 p-2 border"
                                    prop.value pageName
                                    prop.onTextChange (fun e -> changeName e)
                                ]
                                Html.button [
                                    prop.className
                                        "bg-secondary-300 border-gray-400 m-1 p-2 rounded-md text-xl  hover:bg-secondary-600"
                                    prop.text "Save"
                                    prop.onMouseDown (fun _ -> saveAndUpdateName pageName)
                                ]
                            ]
                        ]
                    | false ->
                        Html.div [
                            prop.className "flex items-center"
                            prop.children [
                                Html.p [ prop.className "text-xl m-1 p-2"; prop.text pageName ]
                                Html.button [
                                    prop.className
                                        "bg-secondary-300 border-gray-400 m-1 p-2 rounded-md text-xl hover:bg-secondary-600 "
                                    prop.text "Edit"
                                    prop.onMouseDown (fun _ -> editName ())
                                ]
                            ]
                        ]
                ]
            ]
            DataUpload(dispatch)
            DownloadButton()
        ]
    ]


[<ReactComponent>]
let JsonPreview (json: Json) =
    Html.div [
        prop.className "bg-gray-200 border-gray-400 p-4 m-1"
        prop.children [ prettyPrint (json) ]
    ]



[<ReactComponent>]
let EditingWindow (model: Model, dispatch) =
    Html.div [
        prop.className "border-gray-400 p-4 m-2"
        prop.children [
            match model.CurrentPage.Data with
            | JNull -> Html.p [ prop.text "Upload data to start" ]
            | _ -> renderingCodeToReactElement model.CurrentPage.Code [] model.CurrentPage.Data options true
        //JsonPreview(model.CurrentPage.Data)
        ]
    ]

[<ReactComponent>]
let Editor (model: Model, dispatch) =
    Html.div [
        prop.className "mt-1"
        prop.children [ PageHeader(model.CurrentPage, dispatch); EditingWindow(model, dispatch) ]
    ]