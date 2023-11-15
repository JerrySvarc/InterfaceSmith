module Types

open Fable.SimpleJson
open System

type Selector =
    | FieldSelector of string
    | AllChildrenSelector

type Value =
    | Data of Selector list
    | Empty
    | Constant of string
and ListType =
    | List
    | Table
and RenderingCode =
    | HtmlElement of tag: string * attrs: (string * string) list * innerText: Value * selectors : Selector list
    | HtmlList of listType: ListType * numbered: bool * selectors : Selector list * code: RenderingCode
    | Sequence of (RenderingCode list) * selectors:  Selector list
    | Hole of selectors : Selector list
    override this.ToString() =
        match this with
        | HtmlElement (tag, attrs, innerText) ->
            "HtmlElement: "
            + tag
            + (List.map (fun (key, value) -> key + " " + value.ToString()) attrs
               |> String.concat (", "))
            + innerText.ToString()
        | HtmlList (listType ,numbered, data,code) ->
            "HtmlList: "
        | Sequence (items) ->
            "Sequence: "
            + (List.map (fun item -> item.ToString()) items
               |> String.concat (", "))
        | Hole _-> " !Hole! "

type Component =
    { Name: string
      Id: Guid
      Code: RenderingCode
      Data : Json}