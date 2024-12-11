module CoreLogic.Operations.RenderingCode

open CoreLogic.Types.RenderingTypes

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

let stringToObjType str =
    match str with
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
let rec wrapCode (path: int list) (tag: string) (currentCode: RenderingCode) : RenderingCode =
    match path with
    | [] ->
        RenderingCode.CustomWrapper {
            Tag = Tags.div
            Attributes = []
            WrappedCode = currentCode
            Children = []
            EventHandlers = []
        }
    | head :: tail ->
        match currentCode with
        | RenderingCode.HtmlList(listType, attrs, items, handlers) ->
            let newItems =
                items
                |> List.mapi (fun i item -> if i = head then wrapCode tail tag item else item)

            RenderingCode.HtmlList(listType, attrs, newItems, handlers)
        | RenderingCode.HtmlObject(objType, attrs, keys, items, handlers) ->
            let newItems =
                items
                |> Map.map (fun k v ->
                    if List.tryItem head keys = Some k then
                        wrapCode tail tag v
                    else
                        v)

            RenderingCode.HtmlObject(objType, attrs, keys, newItems, handlers)
        | RenderingCode.CustomWrapper cw ->
            let newWrappedCode = wrapCode tail tag cw.WrappedCode
            RenderingCode.CustomWrapper { cw with WrappedCode = newWrappedCode }
        | _ -> currentCode

/// <summary></summary>
/// <param name="path"></param>
/// <param name="customElement"></param>
/// <param name="currentCode"></param>
/// <returntagToString s></returns>
let rec addCustomElement (path: int list) (customElement: CustomElement) (currentCode: RenderingCode) : RenderingCode =
    match path with
    | [] -> RenderingCode.CustomElement customElement
    | head :: tail ->
        match currentCode with
        | RenderingCode.HtmlList(listType, attrs, items, handlers) ->
            let newItems =
                items
                |> List.mapi (fun i item ->
                    if i = head then
                        addCustomElement tail customElement item
                    else
                        item)

            RenderingCode.HtmlList(listType, attrs, newItems, handlers)
        | RenderingCode.HtmlObject(objType, attrs, keys, items, handlers) ->
            let newItems =
                items
                |> Map.map (fun k v ->
                    if List.tryItem head keys = Some k then
                        addCustomElement tail customElement v
                    else
                        v)

            RenderingCode.HtmlObject(objType, attrs, keys, newItems, handlers)
        | RenderingCode.CustomWrapper wrapper ->
            let newWrappedCode = addCustomElement tail customElement wrapper.WrappedCode

            RenderingCode.CustomWrapper {
                wrapper with
                    WrappedCode = newWrappedCode
            }
        | _ -> currentCode

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
        | RenderingCode.CustomWrapper cw ->
            let newChildren = cw.Children |> List.removeAt lastIndex
            RenderingCode.CustomWrapper { cw with Children = newChildren }
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
        | RenderingCode.CustomWrapper cw ->
            let newWrappedCode = deleteElement tail cw.WrappedCode
            RenderingCode.CustomWrapper { cw with WrappedCode = newWrappedCode }
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
        | RenderingCode.CustomWrapper cw ->
            if head = -1 then
                getElementAtPath tail cw.WrappedCode
            else if head < List.length cw.Children then
                getElementAtPath tail (List.item head cw.Children)
            else
                currentCode
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
        | RenderingCode.CustomWrapper(customWrapper) ->
            match head with
            | -1 ->
                let newItems =
                    match customWrapper.Children with
                    | [] -> customWrapper.Children
                    | _ ->
                        customWrapper.Children
                        |> List.mapi (fun i item ->
                            if tail.Head = i then
                                replace tail.Tail replacementElement item
                            else
                                item)

                RenderingCode.CustomWrapper(
                    {
                        customWrapper with
                            Children = newItems
                    }
                )
            | _ ->
                let newCode = replace tail replacementElement customWrapper.WrappedCode

                RenderingCode.CustomWrapper(
                    {
                        customWrapper with
                            WrappedCode = newCode
                    }
                )
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