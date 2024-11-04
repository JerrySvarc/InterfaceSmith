module Editor.UIComponents.PageEditorComponents

open Editor.Types.EditorDomain
open Editor.Types.PageEditorDomain
open Fable.React
open Feliz
open CoreLogic.Types.RenderingTypes
open Feliz.UseElmish
open Elmish
open Fable.Core.JsInterop
open Fable.SimpleJson
open Editor.Utilities.FileUpload
open Editor.Utilities.Icons
open Editor.Utilities.JsonParsing
open Editor.Components.CustomRendering
open CoreLogic.Operations.DataRecognition
open CoreLogic.Operations.RenderingCode
open Editor.Components.OptionComponents
open CoreLogic.Operations.CodeGeneration
open Editor.Utilities.JavaScriptEditor
open Browser.Types

// PageEditor elmish-style functionality
// This is the main component for the page editor
// It contains the left and right panes and the logic for updating the page data
// It also contains the logic for updating the main page data
// when the user makes changes to the page data

let pageEditorInit (page: Page) : PageEditorModel * Cmd<PageEditorMsg> =
    let newModel = {
        PageData = page
        FileUploadError = false
        ActiveRightTab = JavaScriptEditor
    }

    newModel, Cmd.none
// This function is used to update the page editor model
// It is called when the user interacts with the page editor
// It updates the page editor model and sends a message to the main page via Cmd.ofMsg

let pageEditorUpdate (msg: PageEditorMsg) (model: PageEditorModel) : PageEditorModel * Cmd<PageEditorMsg> =
    match msg with
    | UploadData jsonString ->
        let loadedDataOption = loadJson jsonString

        match loadedDataOption with
        | Some(data) ->
            match data with
            | JObject obj ->
                let updatedPage = {
                    model.PageData with
                        CurrentTree = recognizeJson data
                        ParsedJson = data
                        JsonString = jsonString
                }

                {
                    model with
                        PageData = updatedPage
                        FileUploadError = false
                },
                Cmd.ofMsg (SyncWithMain updatedPage)
            | _ -> { model with FileUploadError = true }, Cmd.none
        | None -> { model with FileUploadError = true }, Cmd.none

    | ReplaceCode(code, path) ->
        let newCodes = replace path code model.PageData.CurrentTree

        let updatedPage = {
            model.PageData with
                CurrentTree = newCodes
        }

        let newModel = { model with PageData = updatedPage }
        newModel, Cmd.ofMsg (SyncWithMain updatedPage)

    | SyncWithMain _ -> model, Cmd.none



// These are the main components that make up the page editor
// They are used to render the page editor and handle user interactions
[<ReactComponent>]
let DataUpload (dispatch) =
    let uploadButtonView onLoad =
        Html.div [
            prop.className "inline-flex items-center"
            prop.children [
                Html.label [
                    prop.className
                        "w-full mt-4 bg-blue-500 hover:bg-blue-600 text-white font-bold py-2 px-4 rounded flex items-center justify-center"
                    prop.children [
                        ReactBindings.React.createElement (
                            uploadIcon,
                            createObj [ "size" ==> 16; "className" ==> "mr-2" ],
                            []
                        )
                        Html.span [ prop.className "font-medium"; prop.text "Upload" ]
                        Html.input [
                            prop.type' "file"
                            prop.className "hidden"
                            prop.onChange (handleFileEvent onLoad)
                        ]
                    ]
                ]
            ]
        ]

    let uploadButton = uploadButtonView (UploadData >> dispatch)

    Html.div [ prop.className "m-2"; prop.children [ uploadButton ] ]


let rec options
    (dispatch: PageEditorMsg -> unit)
    (code: RenderingCode)
    (path: int list)
    (name: string)
    (page: Page)
    : ReactElement =
    match code with
    | HtmlElement _ -> ElementOption(dispatch, name, code, path, page)
    | HtmlList _ -> ListOption(dispatch, name, code, path, page)
    | HtmlObject(_) -> SequenceOption(dispatch, name, code, path, page)
    | Hole _ -> Html.none




[<ReactComponent>]
let JavaScriptEditorView (model: PageEditorModel) (dispatch: PageEditorMsg -> unit) =
    let fullHtml, js =
        generateCode model.PageData.CurrentTree model.PageData.JsonString model.PageData.CustomHandlers

    let extensions = [| javascript?javascript (); html?html (); css?css () |]

    //let onChange = fun value -> dispatch (UpdateHtmlCode value)

    Html.div [
        prop.className "flex flex-col h-full border-solid border-2 border-black overflow-auto"
        prop.children [
            Html.h3 [ prop.className "font-bold mb-2 px-2"; prop.text "Code preview" ]
            Html.div [
                prop.className "flex-grow overflow-auto"
                prop.children [
                    ReactBindings.React.createElement (
                        CodeMirror,
                        createObj [
                            "value" ==> fullHtml
                            "extensions" ==> extensions
                            //"onChange" ==> onChange
                            "theme" ==> "dark"
                            "readOnly" ==> "true"
                        ],
                        []
                    )
                ]
            ]
        ]
    ]


[<ReactComponent>]
let SandboxPreviewView (model: PageEditorModel) =
    let fullHtml, js =
        generateCode model.PageData.CurrentTree model.PageData.JsonString model.PageData.CustomHandlers

    Html.iframe [
        prop.src "about:blank"
        prop.custom ("sandbox", "allow-scripts allow-forms allow-modals")
        prop.custom ("srcDoc", fullHtml)
        prop.style [
            style.width (length.percent 100)
            style.height (length.px 500)
            style.border (1, borderStyle.solid, color.gray)
        ]
    ]


[<ReactComponent>]
let PageEditor (page: Page) (dispatch: Msg -> unit) =

    let model, pageEditorDispatch =
        React.useElmish (
            (fun () -> pageEditorInit page),
            (fun msg model ->
                let newModel, cmd = pageEditorUpdate msg model

                match msg with
                | SyncWithMain page -> newModel, Cmd.ofEffect (fun _ -> dispatch (UpdatePage page))
                | _ -> newModel, cmd),
            [| box page |]
        )



    Html.div [
        prop.className "flex-1 flex overflow-hidden bg-white"
        prop.children [


        ]
    ]