module Types.EditorDomain

open Fable.SimpleJson
open System
open RenderingTypes


type Page = {
    Name: string
    Id: Guid
    Data: Json
    Code: RenderingCode
    FileUploadError: bool
}

//Application state

type TabType =
    | Main
    | Editor


type Model = {
    CurrentPage: Guid
    Pages : Map<Guid, Page>
}

type Msg =
    | UploadData of string
    | ChangeName of string
    | SavePage of Page
    | ReplaceCode of RenderingCode * int list
    | ChangeTab of TabType
    | TogglePreview
    | ToggleOptions
    | SetCurrentModifiedElement of RenderingCode * int list