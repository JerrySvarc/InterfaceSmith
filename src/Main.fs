module Main

open Elmish
open Feliz
open Types
open Fable.SimpleJson
open System
open DataProcessing.DataLoading
open DataProcessing.DataRecognition
open Utilities.EditorUtils
open Utilities.GeneralUtilities
open UIComponents.MainPageComponents
open UIComponents.EditorComponents


let init () : Model * Cmd<Msg> =
    {
        CurrentPage = {
            Name = "New page"
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
        OptionsCollapsed = true
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
    | ToggleOptions ->
        {
            model with
                OptionsCollapsed = not model.OptionsCollapsed
        },
        Cmd.none
    | SetCurrentModifiedElement(code, path) ->
        {
            model with
                CurrModifiedElement = (code, path)
        },
        Cmd.none



let view (model: Model) (dispatch: Msg -> unit) =
    let titlePage =
        Html.div [
            prop.className
                "flex flex-col items-center justify-center h-screen bg-gradient-to-r from-green-400 to-blue-500 text-white"
            prop.children [
                Html.h1 [
                    prop.className "text-6xl font-bold text-center mb-4"
                    prop.text "Data-Driven UI editor"
                ]
                Html.button [
                    prop.className
                        "mt-8 px-4 py-2 text-black border-black bg-amber-400 rounded shadow font-bold text-xl"
                    prop.text "Get Started"
                    prop.onClick (fun _ -> dispatch (ChangeTab Types.Editor))
                ]
            ]
        ]

    match model.CurrentTab with
    | Main -> Application(titlePage, model, dispatch)
    | Editor -> Application(Editor(model, dispatch), model, dispatch)