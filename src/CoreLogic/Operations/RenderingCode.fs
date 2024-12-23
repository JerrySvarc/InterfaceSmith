module CoreLogic.Operations.RenderingCode

open CoreLogic.Types.RenderingTypes

let tagToString tag = tag.Name

let stringToTag str =
    match str with
    | "p" -> Tags.p
    | "h1" -> Tags.h1
    | "h2" -> Tags.h2
    | "h3" -> Tags.h3
    | "h4" -> Tags.h4
    | "h5" -> Tags.h5
    | "h6" -> Tags.h6
    | "strong" -> Tags.strong
    | "em" -> Tags.em
    | "a" -> Tags.a
    | "pre" -> Tags.pre
    | "code" -> Tags.code
    | "blockquote" -> Tags.blockquote
    | "div" -> Tags.div
    | "span" -> Tags.span
    | "article" -> Tags.article
    | "section" -> Tags.section
    | "header" -> Tags.header
    | "footer" -> Tags.footer
    | "nav" -> Tags.nav
    | "input" -> Tags.input
    | "li" -> Tags.li
    | "ol" -> Tags.ol
    | "ul" -> Tags.ul
    | "button" -> Tags.button
    | "label" -> Tags.label
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

let stringToObjType (str: string) =
    let loweredString = str.ToLower()

    match loweredString with
    | "div" -> Div
    | "span" -> Span
    | "article" -> Article
    | "section" -> Section
    | "form" -> Form
    | _ -> failwith "Invalid object type"


let innerValueToString (innerValue: InnerValue) =
    match innerValue with
    | Data -> "Data"
    | Constant _ -> "Constant"
    | Empty -> "Empty"

let stringToInnerValue stringVal : InnerValue =
    match stringVal with
    | "Data" -> Data
    | "Constant" -> Constant ""
    | "Empty"
    | _ -> Empty



/// <summary></summary>
/// <param name="path"></param>
/// <param name="currentCode"></param>
/// <returns></returns>
let rec deleteElement (path: int list) (currentCode: RenderingCode) : RenderingCode =
    match path with
    | [] -> RenderingCode.Hole UnNamed // Replace with a hole when deleting
    | [ lastIndex ] ->
        match currentCode with
        | RenderingCode.HtmlList(lt, attrs, items, handlers) ->
            let newItems = items |> List.removeAt lastIndex
            RenderingCode.HtmlList(lt, attrs, newItems, handlers)
        | RenderingCode.HtmlObject(objType, attrs, keys, items, handlers) ->
            let keyToRemove = List.tryItem lastIndex keys

            match keyToRemove with
            | Some key ->
                let newKeys = keys |> List.filter ((<>) key)
                let newItems = items |> Map.remove key
                RenderingCode.HtmlObject(objType, attrs, newKeys, newItems, handlers)
            | None -> currentCode
        | _ -> currentCode
    | head :: tail ->
        match currentCode with
        | RenderingCode.HtmlList(lt, attrs, items, handlers) ->
            let newItems =
                items
                |> List.mapi (fun i item -> if i = head then deleteElement tail item else item)

            RenderingCode.HtmlList(lt, attrs, newItems, handlers)
        | RenderingCode.HtmlObject(objType, attrs, keys, items, handlers) ->
            let newItems =
                items
                |> Map.map (fun k v ->
                    if List.tryItem head keys = Some k then
                        deleteElement tail v
                    else
                        v)

            RenderingCode.HtmlObject(objType, attrs, keys, newItems, handlers)
        | _ -> currentCode

/// <summary></summary>
/// <param name="path"></param>
/// <param name="currentCode"></param>
/// <returns></returns>
let rec getElementAtPath (path: int list) (currentCode: RenderingCode) : RenderingCode =
    match path with
    | [] -> currentCode
    | head :: tail ->
        match currentCode with
        | RenderingCode.HtmlList(_, _, items, _) ->
            if head < items.Length then
                getElementAtPath tail (List.item head items)
            else
                currentCode
        | RenderingCode.HtmlObject(_, _, keys, items, _) ->
            match List.tryItem head keys with
            | Some key ->
                match Map.tryFind key items with
                | Some value -> getElementAtPath tail value
                | None -> currentCode
            | None -> currentCode
        | _ -> currentCode

/// <summary></summary>
/// <param name="path"></param>
/// <param name="replacementElement"></param>
/// <param name="currentCode"></param>
/// <returns></returns>
let rec replace (path: int list) (replacementElement: RenderingCode) (currentCode: RenderingCode) : RenderingCode =
    match path with
    | [] -> replacementElement
    | head :: tail ->
        match currentCode with
        | RenderingCode.HtmlList(lt, attrs, items, handlers) ->
            let newItems =
                match items with
                | [] -> items
                | _ ->
                    if head < items.Length then
                        items |> List.map (replace tail replacementElement)
                    else
                        items

            RenderingCode.HtmlList(lt, attrs, newItems, handlers)
        | RenderingCode.HtmlObject(objType, attrs, keys, items, handlers) ->
            match List.tryItem head keys with
            | Some key ->
                let value = items.TryFind key

                match value with
                | Some code ->
                    let newCode = replace tail replacementElement code
                    let newItems = items.Add(key, newCode)
                    RenderingCode.HtmlObject(objType, attrs, keys, newItems, handlers)
                | None -> RenderingCode.HtmlObject(objType, attrs, keys, items, handlers)
            | None -> RenderingCode.HtmlObject(objType, attrs, keys, items, handlers)
        | _ -> currentCode

let reorderObjectKeys (path: int list) (newOrder: string list) (currentCode: RenderingCode) : RenderingCode =
    let rec reorder code =
        match code with
        | RenderingCode.HtmlObject(objType, attrs, _, items, handlers) ->
            let newItems =
                newOrder
                |> List.choose (fun key -> Map.tryFind key items |> Option.map (fun v -> (key, v)))
                |> Map.ofList

            RenderingCode.HtmlObject(objType, attrs, newOrder, newItems, handlers)
        | _ -> code

    replace path (reorder (getElementAtPath path currentCode)) currentCode