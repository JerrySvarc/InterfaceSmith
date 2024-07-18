module CoreLogic.Operations.RenderingCode

open CoreLogic.Types.RenderingTypes

let tagToString tag =
    match tag with
    | P -> "p"
    | H1 -> "h1"
    | H2 -> "h2"
    | H3 -> "h3"
    | H4 -> "h4"
    | H5 -> "h5"
    | H6 -> "h6"
    | Strong -> "strong"
    | Em -> "em"
    | A -> "a"
    | Pre -> "pre"
    | Code -> "code"
    | Blockquote -> "blockquote"
    | Tag.Div -> "div"
    | Tag.Span -> "span"
    | Tag.Article -> "article"
    | Tag.Section -> "section"
    | Header -> "header"
    | Footer -> "footer"
    | Nav -> "nav"
    | Input -> "input"
    | Ol -> "ol"
    | Li -> "li"
    | Ul -> "ul"
    | Button -> "button"
    | Label -> "label"

let stringToTag str =
    match str with
    | "p" -> P
    | "h1" -> H1
    | "h2" -> H2
    | "h3" -> H3
    | "h4" -> H4
    | "h5" -> H5
    | "h6" -> H6
    | "strong" -> Strong
    | "em" -> Em
    | "a" -> A
    | "pre" -> Pre
    | "code" -> Code
    | "blockquote" -> Blockquote
    | "div" -> Tag.Div
    | "span" -> Tag.Span
    | "article" -> Tag.Article
    | "section" -> Tag.Section
    | "header" -> Header
    | "footer" -> Footer
    | "nav" -> Nav
    | "input" -> Input
    | "ol" -> Ol
    | "li" -> Li
    | "ul" -> Ul
    | "button" -> Button
    | "label" -> Label
    | _ -> failwith "Invalid tag"


let stringToListType (str: string) =
    let loweredStr = str.ToLower()

    match loweredStr with
    | "unorderedlist" -> UnorderedList
    | "orderedlist" -> OrderedList
    | _ -> failwith "Invalid list type"

let listTypeToString listType =
    match listType with
    | UnorderedList -> "ul"
    | OrderedList -> "ol"

let objTypeToString objType =
    match objType with
    | Div -> "div"
    | Span -> "span"
    | Article -> "article"
    | Section -> "section"
    | Form -> "form"

let stringToObjType str =
    match str with
    | "div" -> Div
    | "span" -> Span
    | "article" -> Article
    | "section" -> Section
    | "form" -> Form
    | _ -> failwith "Invalid object type"



// Replaces a RenderingCode element at the given path with the replacement element
let rec replace (path: int list) (replacementElement: RenderingCode) (currentCode: RenderingCode) =
    match path with
    | [] -> replacementElement
    | head :: tail ->
        match currentCode with
        | HtmlList(lt, items, handlers) ->
            let newItems =
                match items with
                | [] -> items
                | _ ->
                    if head < items.Length then
                        items |> List.map (replace tail replacementElement)
                    else
                        items

            HtmlList(lt, newItems, handlers)
        | HtmlObject(objType, keys, items, handlers) ->
            match List.tryItem head keys with
            | Some key ->
                let value = items.TryFind key

                match value with
                | Some code ->
                    let newCode = replace tail replacementElement code
                    let newItems = items.Add(key, newCode)
                    HtmlObject(objType, keys, newItems, handlers)
                | None -> HtmlObject(objType, keys, items, handlers)
            | None -> HtmlObject(objType, keys, items, handlers)
        | _ -> currentCode