module CoreLogic.Types.RenderingTypes

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
    | Li
    | Ol
    | Ul
    | Button
    | Label


type Attribute = string * InnerValue

and Attributes = Attribute list

and InnerValue =
    | Data
    | Constant of string
    | Empty

type ListType =
    | UnorderedList
    | OrderedList

type ObjType =
    | Div
    | Span
    | Article
    | Section
    | Form

type FieldHole =
    | Named of string
    | UnNamed

type RenderingCode =
    | HtmlElement of tag: Tag * attrs: Attributes * innerValue: InnerValue * eventHandlers: (string * Javascript) list
    | HtmlList of listType: ListType * itemCode: RenderingCode list * eventHandlers: (string * Javascript) list
    | HtmlObject of
        objectType: ObjType *
        keyOrdering: string list *
        codes: Map<string, RenderingCode> *
        eventHandlers: (string * Javascript) list
    | CustomWrapper of CustomWrapper
    | CustomElement of CustomElement
    | Hole of FieldHole

// Represents a JavaScript code
// JSFunction: Represents a JavaScript function used for event handling of custom RenderingCode events, accepts only the parameter "this"
and Javascript = JSFunction of name: string * code: string

and CustomWrapper = {
    Tag: Tag
    Attributes: Attributes
    WrappedCode: RenderingCode
    Children: RenderingCode list
    EventHandlers: (string * Javascript) list
}

and CustomElement = {
    Tag: Tag
    Attributes: Attributes
    CustomInnerValue: string
    EventHandlers: (string * Javascript) list
}