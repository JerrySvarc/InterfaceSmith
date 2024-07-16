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
    | Div -> "div"
    | Span -> "span"
    | Article -> "article"
    | Section -> "section"
    | Header -> "header"
    | Footer -> "footer"
    | Nav -> "nav"
    | Input -> "input"
    | Ol -> "ol"
    | Li -> "li"
    | Ul -> "ul"
    | Button -> "button"

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
    | "div" -> Div
    | "span" -> Tag.Span
    | "article" -> Article
    | "section" -> Section
    | "header" -> Header
    | "footer" -> Footer
    | "nav" -> Nav
    | "input" -> Input
    | "ol" -> Ol
    | "li" -> Li
    | "ul" -> Ul
    | "button" -> Button
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


let rec wrapCode (path: int list) (tag: string) (currentCode: RenderingCode) : RenderingCode =
    match path with
    | [] ->
        CustomWrapper {
            Tag = stringToTag tag
            Attributes = []
            WrappedCode = currentCode
            Children = []
            EventHandlers = []
        }
    | head :: tail ->
        match currentCode with
        | HtmlList(lt, items, handlers) ->
            let newItems =
                items
                |> List.mapi (fun i item ->
                    if i = head then wrapCode tail tag item
                    else item)
            HtmlList(lt, newItems, handlers)
        | HtmlObject(objType, keys, items, handlers) ->
            let newItems =
                items
                |> Map.map (fun k v ->
                    if List.tryItem head keys = Some k then wrapCode tail tag v
                    else v)
            HtmlObject(objType, keys, newItems, handlers)
        | CustomWrapper cw ->
            let newWrappedCode = wrapCode tail tag cw.WrappedCode
            CustomWrapper { cw with WrappedCode = newWrappedCode }
        | _ -> currentCode

let rec addCustomElement (path: int list) (customElement: CustomElement) (currentCode: RenderingCode) : RenderingCode =
    match path with
    | [] -> CustomElement customElement
    | head :: tail ->
        match currentCode with
        | HtmlList(lt, items, handlers) ->
            let newItems =
                items
                |> List.mapi (fun i item ->
                    if i = head then addCustomElement tail customElement item
                    else item)
            HtmlList(lt, newItems, handlers)
        | HtmlObject(objType, keys, items, handlers) ->
            let newItems =
                items
                |> Map.map (fun k v ->
                    if List.tryItem head keys = Some k then addCustomElement tail customElement v
                    else v)
            HtmlObject(objType, keys, newItems, handlers)
        | CustomWrapper cw ->
            let newWrappedCode = addCustomElement tail customElement cw.WrappedCode
            CustomWrapper { cw with WrappedCode = newWrappedCode }
        | _ -> currentCode

let rec deleteElement (path: int list) (currentCode: RenderingCode) : RenderingCode =
    match path with
    | [] -> Hole UnNamed // Replace with a hole when deleting
    | [lastIndex] ->
        match currentCode with
        | HtmlList(lt, items, handlers) ->
            let newItems = items |> List.removeAt lastIndex
            HtmlList(lt, newItems, handlers)
        | HtmlObject(objType, keys, items, handlers) ->
            let keyToRemove = List.tryItem lastIndex keys
            match keyToRemove with
            | Some key ->
                let newKeys = keys |> List.filter ((<>) key)
                let newItems = items |> Map.remove key
                HtmlObject(objType, newKeys, newItems, handlers)
            | None -> currentCode
        | CustomWrapper cw ->
            let newChildren = cw.Children |> List.removeAt lastIndex
            CustomWrapper { cw with Children = newChildren }
        | _ -> currentCode
    | head :: tail ->
        match currentCode with
        | HtmlList(lt, items, handlers) ->
            let newItems =
                items
                |> List.mapi (fun i item ->
                    if i = head then deleteElement tail item
                    else item)
            HtmlList(lt, newItems, handlers)
        | HtmlObject(objType, keys, items, handlers) ->
            let newItems =
                items
                |> Map.map (fun k v ->
                    if List.tryItem head keys = Some k then deleteElement tail v
                    else v)
            HtmlObject(objType, keys, newItems, handlers)
        | CustomWrapper cw ->
            let newWrappedCode = deleteElement tail cw.WrappedCode
            CustomWrapper { cw with WrappedCode = newWrappedCode }
        | _ -> currentCode

let rec getElementAtPath (path: int list) (currentCode: RenderingCode) : RenderingCode =
    match path with
    | [] -> currentCode
    | head :: tail ->
        match currentCode with
        | HtmlList(_, items, _) ->
            if head < items.Length then
                getElementAtPath tail (List.item head items)
            else
                currentCode
        | HtmlObject(_, keys, items, _) ->
            match List.tryItem head keys with
            | Some key ->
                match Map.tryFind key items with
                | Some value -> getElementAtPath tail value
                | None -> currentCode
            | None -> currentCode
        | CustomWrapper cw ->
            if head = -1 then
                getElementAtPath tail cw.WrappedCode
            else if head < List.length cw.Children then
                getElementAtPath tail (List.item head cw.Children)
            else
                currentCode
        | _ -> currentCode

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
        | CustomWrapper(customWrapper) ->
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

                CustomWrapper(
                    {
                        customWrapper with
                            Children = newItems
                    }
                )
            | _ ->
                let newCode = replace tail replacementElement customWrapper.WrappedCode

                CustomWrapper(
                    {
                        customWrapper with
                            WrappedCode = newCode
                    }
                )
        | _ -> currentCode

let reorderObjectKeys (path: int list) (newOrder: string list) (currentCode: RenderingCode) : RenderingCode =
    let rec reorder code =
        match code with
        | HtmlObject(objType, _, items, handlers) ->
            let newItems =
                newOrder
                |> List.choose (fun key -> Map.tryFind key items |> Option.map (fun v -> (key, v)))
                |> Map.ofList
            HtmlObject(objType, newOrder, newItems, handlers)
        | _ -> code

    replace path (reorder (getElementAtPath path currentCode)) currentCode
