module Editor.UIComponents.PageEditorComponents

open Feliz
open Fable.React
open Editor.Types.EditorModel
open Feliz.UseElmish
open Editor.Utilities.Icons
open Fable.Core.JsInterop
open Elmish
open Editor.Types.PageModel
open Editor.Utilities.FileUpload
open Editor.Operations.CustomRendering

// PageEditor elmish-style functionality
let pageEditorInit () : PageEditorModel * Cmd<PageEditorMsg> =
    {
        IsJavaScriptMode = false
        JsCode = "// Write your custom JavaScript here"
    },
    Cmd.none

let pageEditorUpdate (msg: PageEditorMsg) (model: PageEditorModel) : PageEditorModel * Cmd<PageEditorMsg> =
    match msg with
    | ToggleMode ->
        {
            model with
                IsJavaScriptMode = not model.IsJavaScriptMode
        },
        Cmd.none
    | UpdateJsCode code -> { model with JsCode = code }, Cmd.none

(*
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

*)

[<ReactComponent>]
let PageEditor (page: Page) mainDispatch =
    let model, dispatch =
        React.useElmish (pageEditorInit, pageEditorUpdate, [| box page |])

    Html.div [
        prop.className "flex-1 flex overflow-hidden"
        prop.children [
            // Left panel
            Html.div [
                prop.className "w-1/2 flex flex-col border-r"
                prop.children [
                    // JSON data window
                    Html.div [
                        prop.className "h-1/2 p-4 overflow-auto border-b"
                        prop.children [
                            Html.h3 [ prop.className "font-bold mb-2"; prop.text "JSON Data" ]
                            Html.p "JSON data will be displayed here"
                        ]
                    ]
                    // Element modification / JavaScript window
                    Html.div [
                        prop.className "h-1/2 flex flex-col"
                        prop.children [
                            Html.div [
                                prop.className "flex justify-between items-center p-2 bg-gray-200"
                                prop.children [
                                    Html.h3 [
                                        prop.className "font-bold"
                                        prop.text (
                                            if model.IsJavaScriptMode then
                                                "Custom JavaScript"
                                            else
                                                "Element Modification"
                                        )
                                    ]
                                    Html.button [
                                        prop.className "p-1 rounded hover:bg-gray-300"
                                        prop.onClick (fun _ -> dispatch ToggleMode)
                                        prop.children [
                                            if model.IsJavaScriptMode then
                                                ReactBindings.React.createElement (
                                                    settingsIcon,
                                                    createObj [ "size" ==> 20; "color" ==> "#4A5568" ],
                                                    []
                                                )
                                            else
                                                ReactBindings.React.createElement (
                                                    codeIcon,
                                                    createObj [ "size" ==> 20; "color" ==> "#4A5568" ],
                                                    []
                                                )
                                        ]
                                    ]
                                ]
                            ]
                            Html.div [
                                prop.className "flex-1 overflow-auto p-4"
                                prop.children [
                                    if model.IsJavaScriptMode then
                                        Html.textarea [
                                            prop.className "w-full h-full resize-none border rounded p-2"
                                            prop.value model.JsCode
                                            prop.onChange (fun e -> dispatch (UpdateJsCode e))
                                            prop.placeholder "Write your custom JavaScript here"
                                        ]
                                    else
                                        Html.div [ prop.children [ Html.p "Element modification options go here" ] ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]

            // Right panel - Preview sandbox
            Html.div [
                prop.className "w-1/2 p-4 overflow-auto"
                prop.children [
                    Html.h3 [ prop.className "font-bold mb-2"; prop.text "Preview" ]
                    Html.p "Preview of the application will be shown here"
                ]
            ]
        ]
    ]

(*
let rec options (dispatch: Msg -> unit) (code: RenderingCode) (path: int list) (name: string) : ReactElement =
    match code with
    | HtmlElement _ -> ElementOption(dispatch, name, code, path)
    | HtmlList _ -> ListOption(dispatch, name, code, path)
    | HtmlObject(_) -> SequenceOption(dispatch, name, code, path)
    | Hole _ -> Html.none

*)