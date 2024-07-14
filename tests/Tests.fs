module Tests

open Fable.Mocha
open System.Collections.Generic
open Types.RenderingTypes
open RenderingCode

let RenderingCodeReplacementTests =
    testList "Replace Function Tests" [
        testCase "Replace a root element"
        <| fun _ ->
            let original = HtmlElement(P, [], Empty, [])
            let replacement = HtmlElement(Div, [], Empty, [])
            let result = replace [] replacement original
            Expect.equal result replacement "Should replace the root element"

        testCase "Replace all items in HtmlList"
        <| fun _ ->
            let original =
                HtmlList(
                    UnorderedList,
                    [
                        HtmlElement(Li, [], Empty, [])
                        HtmlElement(Li, [], Empty, [])
                        HtmlElement(Li, [], Empty, [])
                    ],
                    []
                )

            let replacement = HtmlElement(Strong, [], Empty, [])
            let result = replace [ 1 ] replacement original

            match result with
            | HtmlList(listType, items, handlers) ->
                Expect.equal listType UnorderedList "List type should remain unchanged"
                Expect.equal (List.length items) 3 "List should still have 3 items"

                items
                |> List.iter (fun item -> Expect.equal item replacement "Each item should be replaced")

                Expect.isEmpty handlers "Handlers should remain empty"
            | _ -> failtest "Result should be an HtmlList"

        testCase "Replace item in nested HtmlObject"
        <| fun _ ->
            let original =
                HtmlObject(
                    "div",
                    [ "header"; "content"; "footer" ],
                    Map.ofList [
                        "header", HtmlElement(H1, [], Empty, [])
                        "content",
                        HtmlObject(
                            "section",
                            [ "title"; "body" ],
                            Map.ofList [
                                "title", HtmlElement(H2, [], Empty, [])
                                "body", HtmlElement(P, [], Empty, [])
                            ],
                            []
                        )
                        "footer", HtmlElement(Footer, [], Empty, [])
                    ],
                    []
                )

            let replacement = HtmlElement(H3, [], Empty, [])
            let result = replace [ 1; 0 ] replacement original

            match result with
            | HtmlObject(_, _, items, _) ->
                match items.["content"] with
                | HtmlObject(_, _, innerItems, _) ->
                    Expect.equal innerItems.["title"] replacement "Should replace the nested title element"
                | _ -> failtest "Inner content should be an HtmlObject"
            | _ -> failtest "Result should be an HtmlObject"

        testCase "Replace wrapped code in CustomWrapper"
        <| fun _ ->
            let original =
                CustomWrapper {
                    Tag = Div
                    Attributes = []
                    WrappedCode = HtmlElement(P, [], Empty, [])
                    Children = []
                    EventHandlers = []
                }

            let replacement = HtmlElement(Span, [], Empty, [])
            let result = replace [ 0 ] replacement original

            match result with
            | CustomWrapper wrapper -> Expect.equal wrapper.WrappedCode replacement "Should replace the wrapped code"
            | _ -> failtest "Result should be a CustomWrapper"

        testCase "Replace child in CustomWrapper"
        <| fun _ ->
            let original =
                CustomWrapper {
                    Tag = Div
                    Attributes = []
                    WrappedCode = HtmlElement(P, [], Empty, [])
                    Children = [ HtmlElement(Span, [], Empty, []); HtmlElement(Em, [], Empty, []) ]
                    EventHandlers = []
                }

            let replacement = HtmlElement(Strong, [], Empty, [])
            let result = replace [ -1; 1 ] replacement original

            match result with
            | CustomWrapper wrapper ->
                Expect.equal (List.item 1 wrapper.Children) replacement "Should replace the second child"
            | _ -> failtest "Result should be a CustomWrapper"


        testCase "No replacement for invalid path"
        <| fun _ ->
            let original = HtmlElement(P, [], Empty, [])
            let replacement = HtmlElement(Div, [], Empty, [])
            let result = replace [ 99 ] replacement original
            Expect.equal result original "Should not replace anything for invalid path"

        testCase "Replace in empty HtmlList"
        <| fun _ ->
            let original = HtmlList(UnorderedList, [], [])
            let replacement = HtmlElement(Li, [], Empty, [])
            let result = replace [ 0 ] replacement original
            Expect.equal result original "Should not modify an empty list"

        testCase "Replace in HtmlList with out-of-bounds index"
        <| fun _ ->
            let original = HtmlList(UnorderedList, [ HtmlElement(Li, [], Empty, []) ], [])
            let replacement = HtmlElement(Strong, [], Empty, [])
            let result = replace [ 1 ] replacement original
            Expect.equal result original "Should not modify the list for out-of-bounds index"

        testCase "Replace in HtmlObject with non-existent key"
        <| fun _ ->
            let original =
                HtmlObject("div", [ "notHeader" ], Map.ofList [ ("header", HtmlElement(H1, [], Empty, [])) ], [])

            let replacement = HtmlElement(H2, [], Empty, [])
            let result = replace [ 0 ] replacement original
            Expect.equal result original "Should not modify the object for non-existent key"

        testCase "Replace CustomElement"
        <| fun _ ->
            let original =
                CustomElement {
                    Tag = Span
                    Attributes = []
                    CustomInnerValue = "Hello"
                    EventHandlers = []
                }

            let replacement = HtmlElement(P, [], Empty, [])
            let result = replace [] replacement original
            Expect.equal result replacement "Should replace CustomElement at root level"

        testCase "Replace Hole"
        <| fun _ ->
            let original = Hole(Named "placeholder")
            let replacement = HtmlElement(Div, [], Empty, [])
            let result = replace [] replacement original
            Expect.equal result replacement "Should replace Hole at root level"

        testCase "Deep nested replacement"
        <| fun _ ->
            let original =
                HtmlObject(
                    "div",
                    [ "content" ],
                    Map.ofList [
                        "content",
                        HtmlList(
                            OrderedList,
                            [
                                HtmlElement(Li, [], Empty, [])
                                CustomWrapper {
                                    Tag = Span
                                    Attributes = []
                                    WrappedCode = HtmlElement(Em, [], Empty, [])
                                    Children = [ HtmlElement(Strong, [], Empty, []) ]
                                    EventHandlers = []
                                }
                            ],
                            []
                        )
                    ],
                    []
                )

            let replacement = HtmlElement(Code, [], Empty, [])
            let result = replace [ 0; 1; -1; 0 ] replacement original

            match result with
            | HtmlObject(_, _, items, _) ->
                match items.["content"] with
                | HtmlList(_, listItems, _) ->
                    match List.item 1 listItems with
                    | CustomWrapper wrapper ->
                        Expect.equal (List.head wrapper.Children) replacement "Should replace deeply nested element"
                    | _ -> failtest "Expected CustomWrapper"
                | _ -> failtest "Expected HtmlList"
            | _ -> failtest "Expected HtmlObject"

        testCase "Replace with complex event handlers"
        <| fun _ ->
            let original =
                HtmlElement(Tag.Input, [], Empty, [ ("click", JSFunction("handleClick", "console.log('clicked')")) ])

            let replacement =
                HtmlElement(A, [], Empty, [ ("hover", JSFunction("handleHover", "console.log('hovered')")) ])

            let result = replace [] replacement original
            Expect.equal result replacement "Should replace element including event handlers"

        testCase "Replace in CustomWrapper with multiple children"
        <| fun _ ->
            let original =
                CustomWrapper {
                    Tag = Div
                    Attributes = []
                    WrappedCode = HtmlElement(P, [], Empty, [])
                    Children = [
                        HtmlElement(Span, [], Empty, [])
                        HtmlElement(Em, [], Empty, [])
                        HtmlElement(Strong, [], Empty, [])
                    ]
                    EventHandlers = []
                }

            let replacement = HtmlElement(Div, [], Empty, [])
            let result = replace [ -1; 1 ] replacement original

            match result with
            | CustomWrapper wrapper ->
                Expect.equal
                    (List.item 1 wrapper.Children)
                    replacement
                    "Should replace the second child in CustomWrapper"
            | _ -> failtest "Expected CustomWrapper"

        testCase "Attempt to replace in Hole"
        <| fun _ ->
            let original = Hole(UnNamed)
            let replacement = HtmlElement(P, [], Empty, [])
            let result = replace [ 0 ] replacement original
            Expect.equal result original "Should not modify Hole for non-empty path"
    ]

let all = testList "All" [ RenderingCodeReplacementTests ]

[<EntryPoint>]
let main _ = Mocha.runTests all