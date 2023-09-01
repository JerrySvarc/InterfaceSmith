module Types

open Fable.SimpleJson
open System

type Reference =
    | Field of string
    | Data of Json

type Value =
    | Reference of Reference
    | Empty
    | Constant of string

type RenderingCode =
    | HtmlElement of tag: string * attrs: (string * Value) list * innerText: Value
    | HtmlList of numbered: bool * innerData: Reference * itemCode: RenderingCode list
    | Sequence of (RenderingCode list)
    | Hole

type Component =
    {   Name : string
        Id : Guid
        JsonData : Json
        Code : RenderingCode}