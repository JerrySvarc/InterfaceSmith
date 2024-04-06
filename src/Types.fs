module Types

open Fable.SimpleJson
open System

type Value =
    | Data
    | Empty
    | Constant of string

and ListType =
    | List
    | Table

and FieldHole =
    | Named of string
    | UnNamed

and RenderingCode =
    | HtmlElement of tag: string * attrs: (string * string) list * innerText: Value
    | HtmlList of listType: ListType * numbered: bool * code: RenderingCode
    | Sequence of RenderingCode list
    | Hole of FieldHole

type Page = {
    Name: string
    Id: Guid
    Data: Json
    Code: RenderingCode
}