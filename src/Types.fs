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
    | Sequence of RenderingCode list
    | Hole