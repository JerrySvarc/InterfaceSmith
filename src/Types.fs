module Types

open Fable.SimpleJson
open System

type Tag =
    | P
    | H1
    | H2
    | H3
    | H4
    | H5
    | H6
    | Strong
    | Em
    | A
    | Pre
    | Code
    | Blockquote
    | Div
    | Span
    | Article
    | Section
    | Header
    | Footer
    | Nav
    | Input

type Attribute = string * InnerValue

and Attributes = Attribute list

and InnerValue =
    | Data
    | Constant of string
    | Empty

type ListType =
    | UnorderedList
    | OrderedList
    | Table

type FieldHole =
    | Named of string
    | UnNamed

type RenderingCode =
    | HtmlElement of tag: Tag * attrs: Attributes * innerValue: InnerValue
    | HtmlList of listType: ListType * headers: string list option * itemCode: RenderingCode list
    | Sequence of RenderingCode array
    | Hole of FieldHole

type Page = {
    Name: string
    Id: Guid
    Data: Json
    Code: RenderingCode
}

//Application state

type TabType =
    | Main
    | Editor


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
    | SavePage of Page
    | ReplaceCode of RenderingCode * int list
    | ChangeTab of TabType
    | TogglePreview
    | SetCurrentModifiedElement of RenderingCode * int list