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
    | Li
    | Ul
    | Ol
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
    | Main
    | Input

type Attribute = Attribute of string * string

type Attributes = Attributes of Attribute list

type InnerValue =
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
    | HtmlList of listType: ListType * headers: string list option * code: RenderingCode
    | Sequence of RenderingCode array
    | Hole of FieldHole

type Page = {
    Name: string
    Id: Guid
    Data: Json
    Code: RenderingCode
}