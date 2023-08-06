module Types

type Reference =
    | Field of string
    | Data

type Value =
    | Reference of Reference
    | Empty
    | Constant of string


type RenderingCode =
    | HtmlElement of tag: string * attrs: (string * Value) list * innerText: Value
    | HtmlList of numbered: bool * innerData: Reference * itemCode: RenderingCode
    | HtmlTextInput of innerData: Value
    | Sequence of RenderingCode list
    | Hole

type Component =
    {   Name : string
        Code : RenderingCode}