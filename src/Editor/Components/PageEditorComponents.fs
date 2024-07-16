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
open Feliz.prop
open Editor.Utilities.JavaScriptEditor

// PageEditor elmish-style functionality
let pageEditorInit (page: Page) : PageEditorModel * Cmd<PageEditorMsg> =
    let newModel = {
        PageData = page
        FileUploadError = false
        ActiveRightTab = GeneratedCode
    }
    newModel, Cmd.none

let pageEditorUpdate (msg: PageEditorMsg) (model: PageEditorModel) : PageEditorModel * Cmd<PageEditorMsg> =
    match msg with
    | UploadData jsonString ->
        let loadedDataOption = loadJson jsonString
        match loadedDataOption with
        | Some(data) ->
            match data with
            | JObject obj ->
                let updatedPage = {model.PageData with CurrentTree = recognizeJson data; ParsedJson = data}
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
        let updatedPage = { model.PageData with CurrentTree = newCodes }
        let newModel = { model with PageData = updatedPage }
        newModel, Cmd.ofMsg (SyncWithMain updatedPage)

    | SyncWithMain _ ->
        model, Cmd.none
    | SetActiveRightTab tab ->
        { model with ActiveRightTab = tab }, Cmd.none


[<ReactComponent>]
let DataUpload (dispatch) =
    let uploadButtonView onLoad =
        Html.div [
            prop.className "inline-flex items-center"
            prop.children [
                Html.label [
                    prop.className "w-full mt-4 bg-blue-500 hover:bg-blue-600 text-white font-bold py-2 px-4 rounded flex items-center justify-center"
                    prop.children [
                        ReactBindings.React.createElement(uploadIcon, createObj [ "size" ==> 16; "className" ==> "mr-2" ],[])
                        Html.span [
                            prop.className "font-medium"
                            prop.text "Upload"
                        ]
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

    Html.div [
        prop.className "m-2"
        prop.children [ uploadButton ]
    ]

let rec options (dispatch: PageEditorMsg -> unit) (code: RenderingCode) (path: int list) (name: string) : ReactElement =
    match code with
    | HtmlElement _ -> ElementOption(dispatch, name, code, path)
    | HtmlList _ -> ListOption(dispatch, name, code, path)
    | HtmlObject(_) -> SequenceOption(dispatch, name, code, path)
    | Hole _ -> Html.none
    | CustomWrapper customWrapper -> failwith "todo"
    | CustomElement customElement -> failwith "todo"



[<ReactComponent>]
let GeneratedCodeView (model: PageEditorModel) =
    let html, js = generateCode model.PageData.CurrentTree model.PageData.ParsedJson
    Html.div [
        prop.className "space-y-4"
        prop.children [
            Html.div [
                prop.className "bg-gray-100 p-4 rounded"
                prop.children [
                    Html.h3 [prop.className "font-bold"; prop.text "Generated HTML"]
                    Html.pre [prop.text html]
                ]
            ]
            Html.div [
                prop.className "bg-gray-100 p-4 rounded"
                prop.children [
                    Html.h3 [prop.className "font-bold"; prop.text "Generated JavaScript"]
                    Html.pre [prop.text js]
                ]
            ]
        ]
    ]

[<ReactComponent>]
let JavaScriptEditorView (model: PageEditorModel) (dispatch: PageEditorMsg -> unit) =
    let options = createObj [
        "mode" ==> "javascript"
        "theme" ==> "dark"
        "lineNumbers" ==> true
        "autoCloseBrackets" ==> true
        "matchBrackets" ==> true
        "showCursorWhenSelecting" ==> true
        "tabSize" ==> 2
    ]

    let onChange = fun (_editor: obj) (_data: obj) (value: string) ->
        dispatch (UpdateJavaScriptCode value)

    Html.div [
        prop.className "h-full border-solid border-2 border-black overflow-hidden"
        prop.children [
            Html.h3 [prop.className "font-bold mb-2"; prop.text "JavaScript Editor"]
            Html.div [prop.className "font-bold mb-2"; prop.text"Select function to edit"]
            ReactBindings.React.createElement(
                CodeMirror,
                createObj [
                    "value" ==> (generateJavaScript model.PageData.CurrentTree)
                    "options" ==> options
                    "onBeforeChange" ==> onChange
                    "height" ==> "400px"
                ],[]
            )
        ]
    ]


[<ReactComponent>]
let SandboxPreviewView (model: PageEditorModel) =
    // Implement sandbox preview here
    Html.div [
        prop.text "Sandbox Preview (To be implemented)"
    ]

[<ReactComponent>]
let RightPaneTabButton (label: string) (isActive: bool) (onClick: unit -> unit) =
    Html.button [
        prop.className [
            "px-4 py-2 font-medium rounded-t-lg"
            if isActive then "bg-white text-blue-600" else "bg-gray-200 text-gray-600 hover:bg-gray-300"
        ]
        prop.text label
        prop.onClick (fun _ -> onClick())
    ]


[<ReactComponent>]
let PageEditor (page: Page) (dispatch: Msg -> unit) =

    let model, pageEditorDispatch = React.useElmish(
        (fun () -> pageEditorInit page),
        (fun msg model ->
            let newModel, cmd = pageEditorUpdate msg model
            match msg with
            | SyncWithMain page ->
                newModel, Cmd.ofEffect (fun _ -> dispatch (UpdatePage page))

            | _ -> newModel, cmd
        ),
        [| box page |]
    )

    let leftWindow =
         Html.div [
                prop.className "w-1/2 p-4 overflow-auto"
                prop.children [
                    Html.div [
                        prop.className "h-1/2 flex flex-col"
                        prop.children [
                            Html.h3 [ prop.className "font-bold mb-2"; prop.text "Preview" ]
                            Html.div [
                                prop.className "flex center-right mb-2"
                                prop.children [
                                    DataUpload pageEditorDispatch
                                ]
                            ]
                            renderingCodeToReactElement
                                model.PageData.CurrentTree
                                []
                                model.PageData.ParsedJson
                                "Data"
                                options
                                pageEditorDispatch
                        ]
                    ]
                ]
            ]

    let rightWindow =
        Html.div [
            prop.className "w-1/2 flex flex-col"
            prop.children [
                Html.div [
                    prop.className "flex border-b"
                    prop.children [
                        RightPaneTabButton "Generated Code" (model.ActiveRightTab = GeneratedCode) (fun () -> pageEditorDispatch (SetActiveRightTab GeneratedCode))
                        RightPaneTabButton "JavaScript Editor" (model.ActiveRightTab = JavaScriptEditor) (fun () -> pageEditorDispatch (SetActiveRightTab JavaScriptEditor))
                        RightPaneTabButton "Sandbox Preview" (model.ActiveRightTab = SandboxPreview) (fun () -> pageEditorDispatch (SetActiveRightTab SandboxPreview))
                    ]
                ]
                Html.div [
                    prop.className "flex-1 p-4 bg-white overflow-auto"
                    prop.children [
                        match model.ActiveRightTab with
                        | GeneratedCode -> GeneratedCodeView model
                        | JavaScriptEditor -> JavaScriptEditorView model pageEditorDispatch
                        | SandboxPreview -> SandboxPreviewView model
                    ]
                ]
            ]
        ]


    Html.div [
        prop.className "flex-1 flex overflow-hidden"
        prop.children [
            leftWindow
            rightWindow

        ]
    ]



