module Index

open Elmish
open Fable.Remoting.Client
open Types
open Fable.SimpleJson
open DataLoading
open Overview
open Editor

type Page =
    | Overview
    | Editor
    | Preview

type Model =
    {   CurrentPage : Page
        OverviewModel : Overview.Model
        EditorModel : Editor.Model }


type Msg =
    | ChangePage of Page
    | OverviewMsg of Overview.Msg
    | EditorMsg of Editor.Msg



let init () : Model * Cmd<Msg> =
    {CurrentPage = Overview; OverviewModel = Overview.init(); EditorModel = Editor.init() }, Cmd.none


let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | ChangePage page -> { model with CurrentPage = page }, Cmd.none
    | OverviewMsg msg  ->
        let updatedOverview, overviewCmd = Overview.update msg model.OverviewModel
        {model with OverviewModel = updatedOverview}, Cmd.none
    | EditorMsg msg ->
        let updatedEditor, editorCmd = Editor.update msg model.EditorModel
        {model with EditorModel = updatedEditor}, Cmd.none


open FileUpload
open Feliz
open Feliz.Bulma

let view (model: Model) (dispatch: Msg -> unit) =

    //Definition of different UI elements used in the application

    let navBar =
        Bulma.navbarMenu[
            Bulma.navbarBrand.a [
                Bulma.navbarItem.a [
                    prop.text "Value driven UI editor"
                ]
                Html.button [
                    prop.text "Home"
                ]
                Bulma.navbarItem.a [
                    prop.text "Editor"
                ]
        ]
    ]

    let mainView =
        Html.div [
            navBar
            match model.CurrentPage with
            | Overview ->
                Html.div [
                   Overview.view model.OverviewModel  (OverviewMsg >> dispatch)
                ]
            | Editor -> failwith "k"
                   // Editor.view model.EditorModel (EditorMsg >> dispatch)
            | Preview ->
                Bulma.columns [

                ]
        ]

    mainView
