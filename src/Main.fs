module Main

open Elmish
open Feliz
open Types.EditorDomain
open Types.RenderingTypes
open Fable.SimpleJson
open System
open DataProcessing.DataLoading
open DataProcessing.DataRecognition
open Utilities.EditorUtils
open UIComponents.MainPageComponents
open UIComponents.EditorComponents
open RenderingCode


let newPage =  {
    Name = "New page"
    Id = Guid.NewGuid()
    Data = JNull
    Code =  Hole(UnNamed)
    FileUploadError = false
}
let init () : Model * Cmd<Msg> =
    {
        CurrentPage = newPage.Id
        Pages = Map[newPage.Id, newPage]
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
               model,Cmd.none
            | _ ->  model,Cmd.none
        | None ->  model,Cmd.none
    | ChangeName newName ->
        model,Cmd.none
    | SavePage comp -> model, Cmd.none
    | ReplaceCode(code, path) ->
        model,Cmd.none
    | ChangeTab tab ->  model,Cmd.none
    | TogglePreview ->
         model,Cmd.none
    | ToggleOptions ->
        model,Cmd.none
    | SetCurrentModifiedElement(code, path) ->
        model,Cmd.none



let view (model: Model) (dispatch: Msg -> unit) =

   Html.div []