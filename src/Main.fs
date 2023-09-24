module Index

open Elmish
open Fable.Remoting.Client
open Types
open Fable.SimpleJson
open DataLoading
open Overview
open Editor
open System

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
        match msg with
        | EditComponent guid ->
            let found, chosenComponent = model.OverviewModel.CreatedComponents.TryGetValue(guid)
            if found then
                {model with EditorModel = { CurrentComponent = chosenComponent; FileUploadError = false; EditingName = false; NameInput = ""; RenderingCodes = []}; CurrentPage = Editor}, Cmd.none
            else
                model,Cmd.none
        | _  ->
            let updatedOverview, overviewCmd = Overview.update msg model.OverviewModel
            {model with OverviewModel = updatedOverview}, Cmd.none
    | EditorMsg msg ->
        match msg with
        | SaveComponent newComponent ->
            let updatedMap = model.OverviewModel.CreatedComponents.Add(newComponent.Id, newComponent)
            {model with OverviewModel = {model.OverviewModel with CreatedComponents = updatedMap}; EditorModel = Editor.init(); CurrentPage = Overview}, Cmd.none
        | _ ->
            let updatedEditor, editorCmd = Editor.update msg model.EditorModel
            {model with EditorModel = updatedEditor}, Cmd.none


open FileUpload
open Feliz
open Feliz.Bulma

let view (model: Model) (dispatch: Msg -> unit) =

    //Definition of different UI elements used in the application

    let navBar =
        Bulma.navbar[
            prop.children [
                Bulma.navbarBrand.div [
                    color.isPrimary
                    prop.children[
                        Bulma.navbarItem.a [
                            prop.text "Value driven UI"
                        ]
                    ]
                ]
                Bulma.navbarMenu[
                    Bulma.navbarStart.div[
                        Bulma.navbarItem.a [
                            color.isLink
                            prop.text "Home"
                            prop.onClick (fun _ -> dispatch (ChangePage Overview))
                        ]
                        Bulma.navbarItem.a [
                            color.isLink
                            prop.text "Editor"
                            prop.onClick (fun _ -> dispatch (ChangePage Editor))
                        ]
                    ]
                ]
            ]
        ]

    let mainView =
        Bulma.block [
            Bulma.box[
                navBar
            ]
            match model.CurrentPage with
            | Overview ->
                   Overview.view model.OverviewModel  (OverviewMsg >> dispatch)
            | Editor ->
                   Editor.view model.EditorModel (EditorMsg >> dispatch)
            | Preview ->
                Bulma.columns []
        ]

    mainView
