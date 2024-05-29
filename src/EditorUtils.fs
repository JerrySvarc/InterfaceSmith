module EditorUtils

open Fable.React
open Types
open Fable.SimpleJson
open System
open DataRecognition
open Feliz
open AppUtilities
open Browser
open Fable.Core.JsInterop

let rec addCode path newCode = 0

let rec replace path replacementElement (currentCode: RenderingCode) =
    match path with
    | [] -> replacementElement
    | head :: tail ->
        match currentCode with
        | HtmlList(lt, n, items) ->
            let newItems =
                match items with
                | [] -> items
                | _ -> items |> List.mapi (fun i item -> replace tail replacementElement item)

            console.log (newItems.ToString())
            HtmlList(lt, n, newItems)
        | Sequence(items) ->
            let newItems =
                items
                |> Array.mapi (fun i item ->
                    if i = head then
                        replace tail replacementElement item
                    else
                        item)

            Sequence(newItems)
        | _ -> currentCode


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
    | Main -> "main"
    | Input -> "input"

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
    | "main" -> Main
    | "input" -> Input
    | _ -> failwith "Invalid tag"


let stringToListType (str: string) =
    let loweredStr = str.ToLower()

    match loweredStr with
    | "unorderedlist" -> UnorderedList
    | "orderedlist" -> OrderedList
    | "table" -> Table
    | _ -> failwith "Invalid list type"

let listTypeToString listType =
    match listType with
    | UnorderedList -> "ul"
    | OrderedList -> "ol"
    | Table -> "table"

let rec renderingCodeToReactElement
    (code: RenderingCode)
    (path: int list)
    (json: Json)
    (options: RenderingCode -> int list -> string -> ReactElement)
    (showOptions: bool)
    =
    match code with
    | HtmlElement(tag, attrs, innerValue) ->
        let attributes =
            attrs
            |> List.map (fun (key, value) -> (key, box value))
            |> List.toSeq
            |> Seq.append [ ("className", box "preview") ]
            |> createObj

        let preview =
            match innerValue with
            | Data ->
                let selectedFields = json
                let jsonStr = selectedFields |> Json.convertFromJsonAs<String>

                ReactBindings.React.createElement (tagToString tag, attributes, [ str jsonStr ])
            | Empty -> ReactBindings.React.createElement (tagToString tag, [], [])
            | Constant value -> ReactBindings.React.createElement (tagToString tag, attributes, [ str value ])

        match showOptions with
        | true ->
            Html.div [
                prop.children [
                    preview
                    match showOptions with
                    | true -> options code path "Element"
                    | false -> Html.none
                ]
            ]
        | false -> preview


    | HtmlList(listType, headers, codes) ->
        match json with
        | JArray array ->
            let listOptions = options code path "List"
            let tag = listTypeToString listType

            let elements =
                List.mapi
                    (fun index code ->
                        let arrayItem = List.item index array

                        let renderedItem =
                            if index = 0 then
                                renderingCodeToReactElement code (path @ [ index ]) arrayItem options showOptions
                            else
                                renderingCodeToReactElement code (path @ [ index ]) arrayItem options false

                        let itemTag =
                            match listType with
                            | Table -> "td"
                            | _ -> "li"

                        ReactBindings.React.createElement (
                            itemTag,
                            [ ("className", box "preview") ] |> createObj,
                            [ renderedItem ]
                        ))
                    codes

            ReactBindings.React.createElement (tag, [ ("className", box "preview") ] |> createObj, elements)

        | _ -> failwith "not a list"
    | Sequence codes ->
        let jsonList =
            match json with
            | JObject object -> object |> Map.toList
            | _ -> failwith "Not a sequence"

        let renderedElements =
            Array.mapi
                (fun index code ->
                    let (_, jsonSubObject) = List.item index jsonList
                    renderingCodeToReactElement code (path @ [ index ]) jsonSubObject options showOptions)
                codes

        ReactBindings.React.createElement ("div", [], renderedElements)
    | Hole named ->
        let name =
            match named with
            | UnNamed -> "Unnamed"
            | Named name -> name

        let fieldType = json |> recognizeJson
        let optionPane = options fieldType path name

        match showOptions with
        | true -> optionPane
        | false -> Html.none