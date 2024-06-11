module Utilities.EditorUtils

open Fable.React
open Types
open Fable.SimpleJson
open System
open DataProcessing.DataRecognition
open Feliz
open Utilities.GeneralUtilities
open Browser
open Fable.Core.JsInterop



//TODO: implement ability to switch codes inside a sequence


//TODO: implement ability to add codes into a sequence
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
    | "input" -> Input
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


let rec renderingCodeToReactElement
    (code: RenderingCode)
    (path: int list)
    (json: Json)
    (name: string)
    (optionsCollapsed: bool)
    (options: (Msg -> unit) -> RenderingCode -> list<int> -> string -> bool -> ReactElement)
    (showOptions: bool)
    (dispatch: Msg -> unit)

    : ReactElement =
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
        | true -> Html.div [ prop.children [ preview; options dispatch code path name optionsCollapsed ] ]
        | false -> preview


    | HtmlList(listType, headers, codes) ->
        match json with
        | JArray array ->
            let listOptions = options dispatch code path "List"
            let tag = listTypeToString listType

            let elements =
                List.mapi
                    (fun index code ->
                        let arrayItem = List.item index array

                        let renderedItem =
                            if index = 0 then
                                renderingCodeToReactElement
                                    code
                                    (path @ [ index ])
                                    arrayItem
                                    name
                                    optionsCollapsed
                                    options
                                    showOptions
                                    dispatch

                            else
                                renderingCodeToReactElement
                                    code
                                    (path @ [ index ])
                                    arrayItem
                                    name
                                    optionsCollapsed
                                    options
                                    false
                                    dispatch


                        let itemTag = "li"

                        ReactBindings.React.createElement (
                            itemTag,
                            [ ("className", box "preview") ] |> createObj,
                            [ renderedItem ]
                        ))
                    codes

            let preview =
                ReactBindings.React.createElement (tag, [ ("className", box "preview") ] |> createObj, elements)

            match showOptions with
            | true ->
                Html.div [
                    prop.className " border-3 bg-white rounded-md m-1"
                    prop.children [ preview; options dispatch code path name optionsCollapsed ]
                ]
            | false -> preview

        | _ -> failwith "not a list"
    | Sequence codes ->
        let jsonList =
            match json with
            | JObject object -> object |> Map.toList
            | _ -> failwith "Not a sequence"

        let renderedElements =
            Array.mapi
                (fun index code ->
                    let (field, jsonSubObject) = List.item index jsonList

                    renderingCodeToReactElement
                        code
                        (path @ [ index ])
                        jsonSubObject
                        field
                        optionsCollapsed
                        options
                        showOptions
                        dispatch)
                codes

        let preview =
            ReactBindings.React.createElement ("div", ("className", box "border"), renderedElements)

        match showOptions with
        | true ->
            Html.div [
                prop.className "border border-secondary-900 bg-white rounded-md m-1"
                prop.children [ options dispatch code path name optionsCollapsed; preview ]
            ]
        | false -> preview

    | Hole named ->
        let name =
            match named with
            | UnNamed -> "Unnamed"
            | Named name -> name

        let fieldType = json |> recognizeJson
        let optionPane: ReactElement = options dispatch fieldType path name optionsCollapsed

        match showOptions with
        | true -> optionPane
        | false -> Html.none