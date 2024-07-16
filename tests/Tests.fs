module Tests

open Fable.Mocha
open System.Collections.Generic
open CoreLogic.Types.RenderingTypes
open CoreLogic.Operations.CodeGeneration
open CoreLogic.Operations.RenderingCode
open Fable.SimpleJson


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

let RenderingCodeOperationTests =
    testList "RenderingCode Operation Tests" [
        testList "wrapCode Tests" [
            testCase "Wrap root element"
            <| fun _ ->
                let original = HtmlElement(P, [], Empty, [])
                let result = wrapCode [] "div" original
                match result with
                | CustomWrapper cw ->
                    Expect.equal cw.Tag Div "Wrapper tag should be Div"
                    Expect.equal cw.WrappedCode original "Wrapped code should be the original element"
                | _ -> failtest "Result should be a CustomWrapper"

            testCase "Wrap nested element"
            <| fun _ ->
                let original = HtmlList(UnorderedList, [HtmlElement(Li, [], Empty, [])], [])
                let result = wrapCode [0] "span" original
                match result with
                | HtmlList(_, items, _) ->
                    match List.head items with
                    | CustomWrapper cw ->
                        Expect.equal cw.Tag Span "Wrapper tag should be Span"
                        Expect.equal cw.WrappedCode (HtmlElement(Li, [], Empty, [])) "Wrapped code should be the Li element"
                    | _ -> failtest "Nested element should be a CustomWrapper"
                | _ -> failtest "Result should still be an HtmlList"
        ]

        testList "addCustomElement Tests" [
            testCase "Add custom element to root"
            <| fun _ ->
                let customElement = { Tag = Div; Attributes = []; CustomInnerValue = "Custom"; EventHandlers = [] }
                let result = addCustomElement [] customElement (Hole UnNamed)
                Expect.equal result (CustomElement customElement) "Should add custom element at root"

            testCase "Add custom element to nested location"
            <| fun _ ->
                let customElement = { Tag = Span; Attributes = []; CustomInnerValue = "Nested"; EventHandlers = [] }
                let original = HtmlObject("div", ["content"], Map.ofList [("content", HtmlElement(P, [], Empty, []))], [])
                let result = addCustomElement [0] customElement original
                match result with
                | HtmlObject(_, _, items, _) ->
                    match items.["content"] with
                    | CustomElement ce -> Expect.equal ce customElement "Should add custom element at nested location"
                    | _ -> failtest "Nested element should be a CustomElement"
                | _ -> failtest "Result should still be an HtmlObject"
        ]

        testList "deleteElement Tests" [
            testCase "Delete root element"
            <| fun _ ->
                let original = HtmlElement(P, [], Empty, [])
                let result = deleteElement [] original
                Expect.equal result (Hole UnNamed) "Should replace root with unnamed hole"

            testCase "Delete element from list"
            <| fun _ ->
                let original = HtmlList(UnorderedList, [HtmlElement(Li, [], Empty, []); HtmlElement(Li, [], Empty, [])], [])
                let result = deleteElement [0] original
                match result with
                | HtmlList(_, items, _) ->
                    Expect.hasLength items 1 "Should have one item left"
                | _ -> failtest "Result should still be an HtmlList"

            testCase "Delete key from object"
            <| fun _ ->
                let original = HtmlObject("div", ["key1"; "key2"], Map.ofList [("key1", HtmlElement(P, [], Empty, [])); ("key2", HtmlElement(Span, [], Empty, []))], [])
                let result = deleteElement [0] original
                match result with
                | HtmlObject(_, keys, items, _) ->
                    Expect.hasLength keys 1 "Should have one key left"
                    Expect.isFalse (items.ContainsKey "key1") "Should not contain deleted key"
                | _ -> failtest "Result should still be an HtmlObject"
        ]

        testList "getElementAtPath Tests" [
            testCase "Get root element"
            <| fun _ ->
                let original = HtmlElement(P, [], Empty, [])
                let result = getElementAtPath [] original
                Expect.equal result original "Should return the root element"

            testCase "Get nested element"
            <| fun _ ->
                let nested = HtmlElement(Span, [], Empty, [])
                let original = HtmlList(UnorderedList, [HtmlElement(Li, [], Empty, []); nested], [])
                let result = getElementAtPath [1] original
                Expect.equal result nested "Should return the nested element"

            testCase "Get element from object"
            <| fun _ ->
                let target = HtmlElement(P, [], Empty, [])
                let original = HtmlObject("div", ["key1"; "key2"], Map.ofList [("key1", HtmlElement(Span, [], Empty, [])); ("key2", target)], [])
                let result = getElementAtPath [1] original
                Expect.equal result target "Should return the element at the specified key"
        ]

        testList "reorderObjectKeys Tests" [
            testCase "Reorder keys in root object"
            <| fun _ ->
                let original = HtmlObject("div", ["key1"; "key2"; "key3"],
                                          Map.ofList [("key1", HtmlElement(P, [], Empty, []));
                                                      ("key2", HtmlElement(Span, [], Empty, []));
                                                      ("key3", HtmlElement(Div, [], Empty, []))], [])
                let result = reorderObjectKeys [] ["key3"; "key1"; "key2"] original
                match result with
                | HtmlObject(_, keys, _, _) ->
                    Expect.equal keys ["key3"; "key1"; "key2"] "Keys should be in the new order"
                | _ -> failtest "Result should be an HtmlObject"

            testCase "Reorder keys in nested object"
            <| fun _ ->
                let nested = HtmlObject("nested", ["a"; "b"],
                                        Map.ofList [("a", HtmlElement(P, [], Empty, []));
                                                    ("b", HtmlElement(Span, [], Empty, []))], [])
                let original = HtmlObject("root", ["outer"; "inner"],
                                          Map.ofList [("outer", HtmlElement(Div, [], Empty, []));
                                                      ("inner", nested)], [])
                let result = reorderObjectKeys [1] ["b"; "a"] original
                match result with
                | HtmlObject(_, _, items, _) ->
                    match items.["inner"] with
                    | HtmlObject(_, keys, _, _) ->
                        Expect.equal keys ["b"; "a"] "Nested keys should be in the new order"
                    | _ -> failtest "Nested item should be an HtmlObject"
                | _ -> failtest "Result should be an HtmlObject"
        ]
    ]

let CodeGenerationTests =
    testList "CodeGeneration Tests" [
        testCase "Generate HTML for simple HtmlElement"
        <| fun _ ->
            let element = HtmlElement(P, [("class", Constant "text")], Constant "Hello", [])
            let html, _ = generateCode element JNull
            Expect.equal html "<p class=\"text\">Hello</p>" "Should generate correct HTML for simple element"

        testCase "Generate HTML for HtmlElement with Data"
        <| fun _ ->
            let json = JNumber 5
            let element = HtmlElement(Div, [], Data, [])
            let html, _ = generateCode element json
            Expect.equal html "<div >5</div>" "Should generate correct HTML with data from JSON"

        testCase "Generate HTML for HtmlList"
        <| fun _ ->
            let list = HtmlList(UnorderedList, [
                HtmlElement(Li, [], Data, [])
                HtmlElement(Li, [], Data, [])
            ], [])
            let json = JArray [JString "Item 1"; JString "Item 2"]
            let html, _ = generateCode list json
            Expect.equal html "<ul >\n  <li>Item 1</li>\n  <li>Item 2</li>\n</ul>" "Should generate correct HTML for list with data from JSON"

        testCase "Generate HTML for HtmlObject"
        <| fun _ ->
            let obj = HtmlObject("section", ["header"; "content"], Map.ofList [
                "header", HtmlElement(H1, [], Data, [])
                "content", HtmlElement(P, [], Data, [])
            ], [])
            let json = JObject(Map [
                "header", JString "Title"
                "content", JString "Content"
            ])
            let html, _ = generateCode obj json
            Expect.equal html "<div>\n  <div data-key=\"header\">\n    <h1 >Title</h1>\n  </div>\n  <div data-key=\"content\">\n    <p >Content</p>\n  </div>\n</div>" "Should generate correct HTML for object with data from JSON"

        testCase "Generate HTML for CustomWrapper"
        <| fun _ ->
            let wrapper = CustomWrapper {
                Tag = Div
                Attributes = [("class", Constant "wrapper")]
                WrappedCode = HtmlElement(P, [], Data, [])
                Children = [HtmlElement(Span, [], Data, [])]
                EventHandlers = []
            }
            let json = JObject(Map [
                "wrapped", JString "Wrapped"
                "child", JString "Child"
            ])
            let html, _ = generateCode wrapper json
            Expect.equal html "<div class=\"wrapper\">\n<p >Wrapped</p>\n<span >Child</span>\n</div>" "Should generate correct HTML for custom wrapper with data from JSON"

        testCase "Generate HTML for CustomElement"
        <| fun _ ->
            let element = CustomElement {
                Tag = Div
                Attributes = [("id", Constant "custom")]
                CustomInnerValue = "Custom content"
                EventHandlers = []
            }
            let html, _ = generateCode element JNull
            Expect.equal html "<div id=\"custom\">Custom content</div>" "Should generate correct HTML for custom element"

        testCase "Generate HTML for Hole"
        <| fun _ ->
            let hole = Hole(Named "placeholder")
            let html, _ = generateCode hole JNull
            Expect.equal html "{{ placeholder }}" "Should generate correct HTML for named hole"

        testCase "Generate JavaScript for event handlers"
        <| fun _ ->
            let element = HtmlElement(Button, [], Constant "Click me", [
                ("click", JSFunction("handleClick", "console.log('Clicked!');"))
            ])
            let _, js = generateCode element JNull
            Expect.equal js "function handleClick(element) {\nconsole.log('Clicked!');\n}\n" "Should generate correct JavaScript for event handler"

        testCase "Generate HTML with event handlers"
        <| fun _ ->
            let element = HtmlElement(Button, [], Constant "Click me", [
                ("click", JSFunction("handleClick", "console.log('Clicked!');"))
            ])
            let html, _ = generateCode element JNull
            Expect.equal html "<button click=\"handleClick(this)\">Click me</button>" "Should generate correct HTML with event handler"

        testCase "Generate HTML for nested structures"
        <| fun _ ->
            let nested = HtmlList(UnorderedList, [
                HtmlElement(Li, [], Data, [])
                HtmlElement(Li, [], Data, [])
            ], [])
            let json = JArray [JString "Item 1"; JString "Item 2"]
            let html, _ = generateCode nested json
            Expect.equal html "<ul >\n  <li>Item 1</li>\n  <li>Item 2</li>\n</ul>" "Should generate correct HTML for nested structures with data from JSON"

        testCase "Generate HTML and JavaScript for complex structure"
        <| fun _ ->
            let complex = HtmlObject("form", ["input"; "button"], Map.ofList [
                "input", HtmlElement(Input, [("type", Constant "text")], Empty, [])
                "button", HtmlElement(Button, [], Data, [
                    ("click", JSFunction("handleSubmit", "console.log('Submitted!');"))
                ])
            ], [])
            let json = JObject(Map [
                "input", JNull
                "button", JString "Submit"
            ])
            let html, js = generateCode complex json
            Expect.equal html "<div>\n  <div data-key=\"input\">\n    <input type=\"text\" ></input>\n  </div>\n  <div data-key=\"button\">\n    <button click=\"handleSubmit(this)\">Submit</button>\n  </div>\n</div>" "Should generate correct HTML for complex structure with data from JSON"
            Expect.equal js "function handleSubmit(element) {\nconsole.log('Submitted!');\n}\n" "Should generate correct JavaScript for complex structure"
    ]

let all = testList "All" [ RenderingCodeReplacementTests ; RenderingCodeOperationTests; CodeGenerationTests]

[<EntryPoint>]
let main _ = Mocha.runTests all