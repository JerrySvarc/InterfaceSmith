module CoreLogic.Operations.RenderingCode

open CoreLogic.Types.RenderingTypes

//                          Utilities
// ||---------------------------------------------------------------||
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
    | "unordered" -> ListType.UnorderedList
    | "ordered" -> ListType.OrderedList
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

//              Modification of the RenderingCode AST
// ||---------------------------------------------------------------||

/// <summary>
/// Replaces a RenderingCode element at the specified path with an unnamed Hole in the AST.
/// </summary>
/// <param name="path">List of integer indices for traversing nested structures.
/// Example using this structure:
/// <pre>
/// HtmlObject(Div)                    // Path: []
/// ├── "key1" -> HtmlElement(div)    // Path: [0]
/// └── "key2" -> HtmlList            // Path: [1]
/// |    ├── HtmlElement(Li, "Item 1") // Path: [1,0]
/// |    └── HtmlElement(Li, "Item 2") // Path: [1,1]
///
/// Path traversal:
/// - [0]: Selects first key's element (key1 -> div)
/// - [1]: Selects second key's element (key2 -> list)
/// - [1,0]: First li element inside the list
/// - [1,1]: Second li element inside the list
/// </pre>
/// </param>
/// <param name="currentCode">The root RenderingCode element to start traversal from.</param>
/// <returns>
/// Result containing either:
/// - Ok: Modified RenderingCode AST with target element replaced by Hole
/// - Error: String message describing why operation failed
/// </returns>
/// <remarks>
/// Common error cases:
/// - Empty path: Returns Hole
/// - Invalid index: Returns Error
/// - Wrong element type: Returns Error
/// - Path beyond tree depth: Returns Error
/// </remarks>
let rec deleteElement (path: int list) (currentCode: RenderingCode) : Result<RenderingCode, string> =
    match path with
    | [] -> Ok(RenderingCode.Hole UnNamed)
    | [ lastIndex ] ->
        match currentCode with
        | RenderingCode.HtmlList(lt, attrs, items, handlers) when lastIndex >= 0 && lastIndex < items.Length ->
            Ok(RenderingCode.HtmlList(lt, attrs, List.removeAt lastIndex items, handlers))
        | RenderingCode.HtmlObject(objType, attrs, keys, items, handlers) ->
            match List.tryItem lastIndex keys with
            | Some key ->
                Ok(
                    RenderingCode.HtmlObject(
                        objType,
                        attrs,
                        List.filter ((<>) key) keys,
                        Map.remove key items,
                        handlers
                    )
                )
            | None -> Error $"Invalid key index: {lastIndex}"
        | RenderingCode.HtmlList _ -> Error $"Index out of bounds: {lastIndex}"
        | _ -> Error "Cannot delete from this element type"
    | head :: tail ->
        match currentCode with
        | RenderingCode.HtmlList(lt, attrs, items, handlers) when head >= 0 && head < items.Length ->
            let updateItem i item =
                if i = head then
                    match deleteElement tail item with
                    | Ok modified -> modified
                    | Error e -> item
                else
                    item

            Ok(RenderingCode.HtmlList(lt, attrs, List.mapi updateItem items, handlers))
        | RenderingCode.HtmlObject(objType, attrs, keys, items, handlers) ->
            match List.tryItem head keys with
            | Some key ->
                let newItems =
                    items
                    |> Map.map (fun k v ->
                        if k = key then
                            match deleteElement tail v with
                            | Ok modified -> modified
                            | Error _ -> v
                        else
                            v)

                Ok(RenderingCode.HtmlObject(objType, attrs, keys, newItems, handlers))
            | None -> Error $"Invalid key index: {head}"
        | RenderingCode.HtmlList _ -> Error $"Index out of bounds: {head}"
        | _ -> Error "Cannot navigate through this element type"

/// <summary>
/// Traverses the RenderingCode AST to find an element at the specified path.
/// </summary>
/// <param name="path">List of integer indices for traversing nested structures.
/// Example using this structure:
/// <pre>
/// HtmlObject(Div)                    // Path: []
/// ├── "key1" -> HtmlElement(div)    // Path: [0]
/// └── "key2" -> HtmlList            // Path: [1]
/// |    ├── HtmlElement(Li, "Item 1") // Path: [1,0]
/// |    └── HtmlElement(Li, "Item 2") // Path: [1,1]
///
/// Path traversal:
/// - [0]: Selects first key's element (key1 -> div)
/// - [1]: Selects second key's element (key2 -> list)
/// - [1,0]: First li element inside the list
/// - [1,1]: Second li element inside the list
/// </pre>
/// </param>
/// <param name="currentCode">The RenderingCode element to start traversal from</param>
/// <returns>
/// The RenderingCode element at specified path if found;
/// Returns original currentCode if path is invalid
/// </returns>
/// <remarks>
/// Common cases:
/// - Empty path: Returns currentCode
/// - Invalid index: Returns currentCode
/// - Valid path: Returns target element
/// - Mismatched types: Returns currentCode
/// </remarks>
let rec getElementAtPath (path: int list) (currentCode: RenderingCode) : Result<RenderingCode, string> =
    match path with
    | [] -> Ok currentCode
    | head :: tail ->
        match currentCode with
        | RenderingCode.HtmlList(_, _, items, _) ->
            if head < items.Length then
                getElementAtPath tail (List.item head items)
            else
                Error $"Index {head} out of bounds in HtmlList"
        | RenderingCode.HtmlObject(_, _, keys, items, _) ->
            match List.tryItem head keys with
            | Some key ->
                match Map.tryFind key items with
                | Some value -> getElementAtPath tail value
                | None -> Error $"Value not found for key {key} in HtmlObject"
            | None -> Error $"Key index {head} out of bounds in HtmlObject"
        | _ -> Error "Invalid path: element is neither List nor Object"



/// <summary>
/// Replaces a RenderingCode element at the specified path with a new element.
/// </summary>
/// <param name="path">List of integer indices for traversing nested structures.
/// Example using this structure:
/// <pre>
/// HtmlObject(Div)                    // Path: []
/// ├── "key1" -> HtmlElement(div)    // Path: [0]
/// └── "key2" -> HtmlList            // Path: [1]
/// |    ├── HtmlElement(Li, "Item 1") // Path: [1,0]
/// |    └── HtmlElement(Li, "Item 2") // Path: [1,1]
///
/// Path traversal:
/// - [0]: Selects first key's element (key1 -> div)
/// - [1]: Selects second key's element (key2 -> list)
/// - [1,0]: First li element inside the list
/// - [1,1]: Second li element inside the list
/// </pre>
/// </param>
/// <param name="replacementElement">New RenderingCode element to insert at target location.</param>
/// <param name="currentCode">The root RenderingCode element to start traversal from.</param>
/// <returns>
/// Result containing either:
/// - Ok: Modified RenderingCode AST with target element replaced
/// - Error: String message describing why operation failed
/// </returns>
/// <remarks>
/// Common error cases:
/// - Invalid index: Returns Error
/// - Wrong element type: Returns Error
/// - Path beyond tree depth: Returns Error
///
/// Preserves original elements on error in nested operations
/// </remarks>
let rec replace
    (path: int list)
    (replacementElement: RenderingCode)
    (currentCode: RenderingCode)
    : Result<RenderingCode, string> =
    match path with
    | [] -> Ok replacementElement
    | head :: tail ->
        match currentCode with
        | RenderingCode.HtmlList(lt, attrs, items, handlers) ->
            if head < 0 || head >= items.Length then
                Error $"Index out of bounds: {head}"
            else
                let newItems =
                    items
                    |> List.mapi (fun i item ->
                        if i = head then
                            match replace tail replacementElement item with
                            | Ok modified -> modified
                            | Error _ -> item
                        else
                            item)

                Ok(RenderingCode.HtmlList(lt, attrs, newItems, handlers))

        | RenderingCode.HtmlObject(objType, attrs, keys, items, handlers) ->
            match List.tryItem head keys with
            | Some key ->
                match Map.tryFind key items with
                | Some value ->
                    match replace tail replacementElement value with
                    | Ok newValue ->
                        Ok(RenderingCode.HtmlObject(objType, attrs, keys, Map.add key newValue items, handlers))
                    | Error e -> Error e
                | None -> Error $"Key not found: {key}"
            | None -> Error $"Invalid key index: {head}"

        | _ -> Error "Invalid element type for replacement"

/// <summary>
/// Reorders keys in an HtmlObject at the specified path while preserving the object structure.
/// </summary>
/// <param name="path">List of integer indices for traversing nested structures.
/// Example using this structure:
/// <pre>
/// HtmlObject(Div)                    // Path: []
/// ├── "key1" -> HtmlElement(div)    // Path: [0]
/// └── "key2" -> HtmlList            // Path: [1]
/// |    ├── HtmlElement(Li, "Item 1") // Path: [1,0]
/// |    └── HtmlElement(Li, "Item 2") // Path: [1,1]
///
/// Path traversal:
/// - [0]: Selects first key's element (key1 -> div)
/// - [1]: Selects second key's element (key2 -> list)
/// - [1,0]: First li element inside the list
/// - [1,1]: Second li element inside the list
/// </pre>
/// </param>
/// <param name="newOrder">New ordering of keys to apply to target HtmlObject.
/// Must contain exactly the same keys as existing object.</param>
/// <param name="currentCode">Root RenderingCode element to start traversal from</param>
/// <returns>
/// Result containing either:
/// - Ok: Modified RenderingCode AST with reordered HtmlObject
/// - Error: String message describing validation or traversal failure
/// </returns>
/// <remarks>
/// Validation ensures:
/// - New order has same number of keys as original
/// - All keys in new order exist in original
/// - No duplicate keys in new order
///
/// Common error cases:
/// - Invalid path: Returns Error
/// - Target not HtmlObject: Returns Error
/// - Invalid key order: Returns Error with specific message
/// - Traversal failure: Returns Error
/// </remarks>
let reorderObjectKeys
    (path: int list)
    (newOrder: string list)
    (currentCode: RenderingCode)
    : Result<RenderingCode, string> =

    let validateNewOrder (existingKeys: string list) (newKeys: string list) =
        if List.length existingKeys <> List.length newKeys then
            Error "New order must contain same number of keys"
        elif List.exists (fun k -> not (List.contains k existingKeys)) newKeys then
            Error "New order contains invalid keys"
        elif List.distinct newKeys <> newKeys then
            Error "New order contains duplicate keys"
        else
            Ok newKeys

    let reorderObject (code: RenderingCode) =
        match code with
        | RenderingCode.HtmlObject(objType, attrs, existingKeys, items, handlers) ->
            validateNewOrder existingKeys newOrder
            |> Result.map (fun validOrder ->
                let newItems =
                    validOrder
                    |> List.choose (fun key -> Map.tryFind key items |> Option.map (fun v -> (key, v)))
                    |> Map.ofList

                RenderingCode.HtmlObject(objType, attrs, validOrder, newItems, handlers))
        | _ -> Error "Target element is not an HtmlObject"

    getElementAtPath path currentCode
    |> Result.bind reorderObject
    |> Result.bind (fun reordered -> replace path reordered currentCode)