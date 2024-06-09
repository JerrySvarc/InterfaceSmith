module Main

open Elmish
open Feliz
open Types
open Fable.SimpleJson
open System
open DataProcessing.DataLoading
open DataProcessing.DataRecognition
open Utilities.EditorUtils
open Utilities.FileUpload
open Utilities.GeneralUtilities
open UIComponents.OptionComponents


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

    //TODO: Create upload button
    let uploadButtonView onLoad =
        Html.div [ prop.onChange (handleFileEvent onLoad); prop.style [ style.display.none ] ]

    let uploadButton = uploadButtonView (UploadData >> dispatch)


    let rec options (code: RenderingCode) (path: int list) (name: string) : ReactElement =
        match code with
        | HtmlElement _ -> elementOptionsComponent name code path
        | HtmlList _ -> listOptionsComponent name code path
        | Sequence(_) -> sequenceOptionsComponent name code path
        | Hole _ -> uiBlock ([ Html.text "No options available." ])

    Html.div [
        prop.className "flex"
        prop.children [

        ]
    ]