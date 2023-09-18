module Types

open Fable.SimpleJson
open System

type Value =
    | Data of Json
    | Empty
    | Constant of string

type RenderingCode =
    | HtmlElement of tag: string * attrs: (string * string) list * innerText: Value
    | HtmlList of numbered: bool *  codes : RenderingCode list
    | Sequence of (RenderingCode list)
    | Hole
    override this.ToString() =
        match this with
        | HtmlElement (tag, attrs, innerText) ->
            "HtmlElement: "
            + tag
            + (List.map (fun (key, value) -> key + " " + value.ToString()) attrs
               |> String.concat (", "))
            + innerText.ToString()
        | HtmlList (numbered, itemCode) ->
            "HtmlList: "
        | Sequence (items) ->
            "Sequence: "
            + (List.map (fun item -> item.ToString()) items
               |> String.concat (", "))
        | Hole -> " !Hole! "

type Component =
    { Name: string
      Id: Guid
      JsonData: Json
      Code: RenderingCode }