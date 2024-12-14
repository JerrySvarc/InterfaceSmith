module CoreLogic.Types.RenderingTypes

/// <summary></summary>
type Tag = {
    Name: string
    Namespace: string option
    IsSelfClosing: bool
}

/// <summary></summary>
type Attribute = {
    Key: string
    Value: InnerValue
    Namespace: string option
}

and Attributes = Attribute list

/// <summary></summary>
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

/// <summary></summary>
[<RequireQualifiedAccess>]
type RenderingCode =
    | HtmlElement of tag: Tag * attrs: Attributes * innerValue: InnerValue * eventHandlers: (string * EventHandler) list
    | HtmlList of
        listType: ListType *
        attrs: Attributes *
        itemCode: RenderingCode list *
        eventHandlers: (string * EventHandler) list
    | HtmlObject of
        objectType: ObjType *
        attrs: Attributes *
        keyOrdering: string list *
        codes: Map<string, RenderingCode> *
        eventHandlers: (string * EventHandler) list
    | Hole of FieldHole

and EventHandler =
    | JsHandler of Javascript
    | MsgHandler of string

// Represents a JavaScript code
// JSFunction: Represents a JavaScript function used for event handling of custom RenderingCode events, accepts only the parameter "this"
and Javascript = JSFunction of name: string * code: string


/// <summary></summary>
[<RequireQualifiedAccess>]
module Tags =

    let p = {
        Name = "p"
        Namespace = None
        IsSelfClosing = false
    }

    let h1 = {
        Name = "h1"
        Namespace = None
        IsSelfClosing = false
    }

    let h2 = {
        Name = "h2"
        Namespace = None
        IsSelfClosing = false
    }

    let h3 = {
        Name = "h3"
        Namespace = None
        IsSelfClosing = false
    }

    let h4 = {
        Name = "h4"
        Namespace = None
        IsSelfClosing = false
    }

    let h5 = {
        Name = "h5"
        Namespace = None
        IsSelfClosing = false
    }

    let h6 = {
        Name = "h6"
        Namespace = None
        IsSelfClosing = false
    }

    let strong = {
        Name = "strong"
        Namespace = None
        IsSelfClosing = false
    }

    let em = {
        Name = "em"
        Namespace = None
        IsSelfClosing = false
    }

    let a = {
        Name = "a"
        Namespace = None
        IsSelfClosing = false
    }

    let pre = {
        Name = "pre"
        Namespace = None
        IsSelfClosing = false
    }

    let code = {
        Name = "code"
        Namespace = None
        IsSelfClosing = false
    }

    let blockquote = {
        Name = "blockquote"
        Namespace = None
        IsSelfClosing = false
    }

    let div = {
        Name = "div"
        Namespace = None
        IsSelfClosing = false
    }

    let span = {
        Name = "span"
        Namespace = None
        IsSelfClosing = false
    }

    let article = {
        Name = "article"
        Namespace = None
        IsSelfClosing = false
    }

    let section = {
        Name = "section"
        Namespace = None
        IsSelfClosing = false
    }

    let header = {
        Name = "header"
        Namespace = None
        IsSelfClosing = false
    }

    let footer = {
        Name = "footer"
        Namespace = None
        IsSelfClosing = false
    }

    let nav = {
        Name = "nav"
        Namespace = None
        IsSelfClosing = false
    }

    let input = {
        Name = "input"
        Namespace = None
        IsSelfClosing = true
    }

    let li = {
        Name = "li"
        Namespace = None
        IsSelfClosing = false
    }

    let ol = {
        Name = "ol"
        Namespace = None
        IsSelfClosing = false
    }

    let ul = {
        Name = "ul"
        Namespace = None
        IsSelfClosing = false
    }

    let button = {
        Name = "button"
        Namespace = None
        IsSelfClosing = false
    }

    let label = {
        Name = "label"
        Namespace = None
        IsSelfClosing = false
    }