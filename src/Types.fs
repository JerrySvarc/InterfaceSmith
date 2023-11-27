module Types

open Fable.SimpleJson
open System

type Selector =
    | FieldSelector of string
    | ArraySelector of int
    | AllChildrenSelector

type Value =
    | Data of Selector
    | Empty
    | Constant of string
and ListType =
    | List
    | Table
and RenderingCode =
    | HtmlElement of tag: string * attrs: (string * string) list * innerText: Value
    | HtmlList of listType: ListType * numbered: bool * code: RenderingCode * selectors : Selector list
    | Sequence of (RenderingCode list)
    | Hole of selectors : Selector list

type Component =
    { Name: string
      Id: Guid
      Data : Json
      Code: RenderingCode }