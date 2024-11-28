module Editor.Components.OptionComponents

open Editor.Types
open Fable.React
open CoreLogic.Types.RenderingTypes
open Editor.Types.PageEditorDomain
open CoreLogic.Operations.RenderingCode
open Feliz
open Browser.Types
open Fable.Core.JsInterop
open Microsoft.FSharp.Reflection
open Editor.Utilities.Icons
open Editor.Types.EditorDomain

// Contains option menu components for each type of rendering code
// Each component takes a dispatch function, the current code, and the path to the code in the tree
// The dispatch function is used to send messages to the parent component to update the specific code
let rec options
    (dispatch: PageEditorMsg -> unit)
    (code: RenderingCode)
    (path: int list)
    (name: string)
    (page: Page)
    : ReactElement =
    match code with
    | HtmlElement _ -> Html.none
    // ElementOption(dispatch, name, code, path, page)
    | HtmlList _ -> Html.none
    //ListOption(dispatch, name, code, path, page)
    | HtmlObject(_) -> Html.none
    //SequenceOption(dispatch, name, code, path, page)
    | Hole _ -> Html.none




let ModelElement json dispatch =
    let rec displayField json =
        Html.div [
            prop.className ""
            prop.children [

            ]
        ]

    Html.div [ prop.className ""; prop.children [ displayField json ] ]

let MainView model dispatch = Html.div []

let ViewElement code path dispatch = Html.div []

let MsgOverview model dispatch = Html.div []
let MsgElement msgs dispatch = Html.div []
let UpdateOverview model dispatch = Html.div []
let UpdateElement updateFun dispatch = Html.div []