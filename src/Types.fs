module Types

open Fable.SimpleJson
open System

type Value =
    | Data of Json
    | Empty
    | Constant of string

type RenderingCode =
    | HtmlElement of tag: string * attrs: (string * Value) list * innerText: Value
    | HtmlList of numbered: bool * innerData: Value * itemCode: RenderingCode
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
        | HtmlList (numbered, innerData, itemCode) ->
            "HtmlList: " + innerData.ToString() + " " + itemCode.ToString()
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