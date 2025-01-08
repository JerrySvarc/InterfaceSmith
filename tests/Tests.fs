module Tests

open Fable.Mocha
open CoreLogic.Types.RenderingTypes
open CoreLogic.Operations.CodeGeneration
open CoreLogic.Operations.RenderingCode
open Editor.Types.PageEditorDomain

let RenderingCodeReplacementTests =
    testList "RenderingCode AST Tests" [
        testList "Delete Element Tests" [
            testCase "Delete root element"
            <| fun _ ->
                let code = RenderingCode.HtmlElement(Tags.div, [], Empty, [])
                let result = deleteElement [] code
                Expect.equal result (Ok(RenderingCode.Hole UnNamed)) "Should replace with unnamed hole"

            testCase "Delete list item"
            <| fun _ ->
                let list =
                    RenderingCode.HtmlList(
                        UnorderedList,
                        [],
                        [
                            RenderingCode.HtmlElement(Tags.li, [], Constant "1", [])
                            RenderingCode.HtmlElement(Tags.li, [], Constant "2", [])
                        ],
                        []
                    )

                let result = deleteElement [ 0 ] list

                match result with
                | Ok(RenderingCode.HtmlList(_, _, items, _)) ->
                    Expect.equal (List.length items) 1 "Should remove one item"

                    match List.head items with
                    | RenderingCode.HtmlElement(_, _, Constant value, _) ->
                        Expect.equal value "2" "Should keep second item"
                    | _ -> failtest "Unexpected item type"
                | Error e -> failtest e
                | _ -> failtest "Should remain a list"

            testCase "Delete nested object field"
            <| fun _ ->
                let obj =
                    RenderingCode.HtmlObject(
                        Div,
                        [],
                        [ "header"; "content" ],
                        Map.ofList [
                            "header", RenderingCode.HtmlElement(Tags.h1, [], Empty, [])
                            "content", RenderingCode.HtmlElement(Tags.div, [], Empty, [])
                        ],
                        []
                    )

                let result = deleteElement [ 0 ] obj

                match result with
                | Ok(RenderingCode.HtmlObject(_, _, keys, items, _)) ->
                    Expect.equal (List.length keys) 1 "Should have one key"
                    Expect.equal (Map.count items) 1 "Should have one item"
                    Expect.isTrue (List.contains "content" keys) "Should keep content key"
                | Error e -> failtest e
                | _ -> failtest "Should remain an object"

            testCase "Delete with invalid path returns error"
            <| fun _ ->
                let list = RenderingCode.HtmlList(UnorderedList, [], [], [])
                let result = deleteElement [ 0 ] list
                Expect.isError result "Should return error for empty list"

            testCase "Delete from deeply nested structure"
            <| fun _ ->
                let nested =
                    RenderingCode.HtmlObject(
                        Div,
                        [],
                        [ "outer" ],
                        Map.ofList [
                            "outer",
                            RenderingCode.HtmlObject(
                                Div,
                                [],
                                [ "inner" ],
                                Map.ofList [ "inner", RenderingCode.HtmlElement(Tags.div, [], Empty, []) ],
                                []
                            )
                        ],
                        []
                    )

                let result = deleteElement [ 0; 0 ] nested
                Expect.isOk result "Should successfully delete nested element"
        ]

        testList "GetElementAtPath Tests" [
            testCase "Get root element"
            <| fun _ ->
                let element = RenderingCode.HtmlElement(Tags.div, [], Empty, [])
                let result = getElementAtPath [] element
                Expect.equal result (Ok element) "Should return root element"

            testCase "Get list item"
            <| fun _ ->
                let list =
                    RenderingCode.HtmlList(
                        UnorderedList,
                        [],
                        [
                            RenderingCode.HtmlElement(Tags.li, [], Constant "1", [])
                            RenderingCode.HtmlElement(Tags.li, [], Constant "2", [])
                        ],
                        []
                    )

                let result = getElementAtPath [ 1 ] list

                match result with
                | Ok(RenderingCode.HtmlElement(_, _, Constant value, _)) ->
                    Expect.equal value "2" "Should get second item"
                | Error e -> failtest e
                | _ -> failtest "Should get list item"

            testCase "Get nested object field"
            <| fun _ ->
                let obj =
                    RenderingCode.HtmlObject(
                        Div,
                        [],
                        [ "header" ],
                        Map.ofList [ "header", RenderingCode.HtmlElement(Tags.h1, [], Constant "Title", []) ],
                        []
                    )

                let result = getElementAtPath [ 0 ] obj

                match result with
                | Ok(RenderingCode.HtmlElement(_, _, Constant value, _)) ->
                    Expect.equal value "Title" "Should get header element"
                | Error e -> failtest e
                | _ -> failtest "Should get object field"

            testCase "Get element with out of bounds index"
            <| fun _ ->
                let list = RenderingCode.HtmlList(UnorderedList, [], [], [])
                let result = getElementAtPath [ 0 ] list
                Expect.isError result "Should return error for invalid index"

            testCase "Get deeply nested element"
            <| fun _ ->
                let nested =
                    RenderingCode.HtmlList(
                        UnorderedList,
                        [],
                        [
                            RenderingCode.HtmlList(
                                UnorderedList,
                                [],
                                [ RenderingCode.HtmlElement(Tags.div, [], Constant "target", []) ],
                                []
                            )
                        ],
                        []
                    )

                let result = getElementAtPath [ 0; 0 ] nested

                match result with
                | Ok(RenderingCode.HtmlElement(_, _, Constant value, _)) ->
                    Expect.equal value "target" "Should get deeply nested element"
                | _ -> failtest "Failed to get nested element"
        ]

        testList "Replace Tests" [
            testCase "Basic replace - HtmlElement to HtmlElement"
            <| fun _ ->
                let original = RenderingCode.HtmlElement(Tags.div, [], Empty, [])
                let replacement = RenderingCode.HtmlElement(Tags.span, [], Constant "new", [])
                let result = replace [] replacement original

                match result with
                | Ok code -> Expect.equal code replacement "Should replace element directly"
                | Error e -> failtest e

            testCase "Basic replace - first item in list"
            <| fun _ ->
                let list =
                    RenderingCode.HtmlList(
                        UnorderedList,
                        [],
                        [
                            RenderingCode.HtmlElement(Tags.li, [], Empty, [])
                            RenderingCode.HtmlElement(Tags.li, [], Empty, [])
                        ],
                        []
                    )

                let replacement = RenderingCode.HtmlElement(Tags.li, [], Constant "new", [])
                let result = replace [ 0 ] replacement list

                match result with
                | Ok(RenderingCode.HtmlList(_, _, items, _)) ->
                    Expect.equal (List.length items) 2 "Should preserve list length"
                    Expect.equal (List.head items) replacement "Should replace first item"
                | Error e -> failtest e
                | _ -> failtest "Wrong type returned"

            testCase "Basic replace - object key"
            <| fun _ ->
                let obj =
                    RenderingCode.HtmlObject(
                        Div,
                        [],
                        [ "key1" ],
                        Map.ofList [ ("key1", RenderingCode.HtmlElement(Tags.div, [], Empty, [])) ],
                        []
                    )

                let replacement = RenderingCode.HtmlElement(Tags.span, [], Constant "new", [])
                let result = replace [ 0 ] replacement obj

                match result with
                | Ok(RenderingCode.HtmlObject(_, _, keys, items, _)) ->
                    Expect.equal keys [ "key1" ] "Should preserve keys"
                    Expect.equal (Map.find "key1" items) replacement "Should replace value"
                | Error e -> failtest e
                | _ -> failtest "Wrong type returned"

            testCase "Replace with invalid index"
            <| fun _ ->
                let list = RenderingCode.HtmlList(UnorderedList, [], [], [])
                let replacement = RenderingCode.HtmlElement(Tags.div, [], Empty, [])
                let result = replace [ 0 ] replacement list
                Expect.isError result "Should fail with invalid index"

            testCase "Replace with invalid path depth"
            <| fun _ ->
                let element = RenderingCode.HtmlElement(Tags.div, [], Empty, [])
                let replacement = RenderingCode.HtmlElement(Tags.span, [], Empty, [])
                let result = replace [ 0; 0 ] replacement element

                Expect.isError result "Should fail with too deep path"

            testCase "Replace in list"
            <| fun _ ->
                let list =
                    RenderingCode.HtmlList(UnorderedList, [], [ RenderingCode.HtmlElement(Tags.li, [], Empty, []) ], [])

                let replacement = RenderingCode.HtmlElement(Tags.li, [], Constant "new", [])
                let result = replace [ 0 ] replacement list

                match result with
                | Ok(RenderingCode.HtmlList(_, _, [ item ], _)) -> Expect.equal item replacement "Should replace item"
                | Error e -> failtest e
                | _ -> failtest "Should remain a list"

            testCase "Replace with invalid path"
            <| fun _ ->
                let element = RenderingCode.HtmlElement(Tags.div, [], Empty, [])
                let replacement = RenderingCode.HtmlElement(Tags.span, [], Empty, [])
                let result = replace [ 0 ] replacement element
                Expect.isError result "Should return error for invalid path"

            testCase "Replace in nested structure"
            <| fun _ ->
                let nested =
                    RenderingCode.HtmlObject(
                        Div,
                        [],
                        [ "outer" ],
                        Map.ofList [
                            "outer",
                            RenderingCode.HtmlList(
                                UnorderedList,
                                [],
                                [ RenderingCode.HtmlElement(Tags.div, [], Empty, []) ],
                                []
                            )
                        ],
                        []
                    )

                let replacement = RenderingCode.HtmlElement(Tags.span, [], Constant "new", [])
                let result = replace [ 0; 0 ] replacement nested
                Expect.isOk result "Should successfully replace nested element"

            testCase "Replace root element maintains attributes and handlers"
            <| fun _ ->

                let handlers = [ "click", JsHandler "handleClick" ]

                let attrs = [
                    {
                        Key = "class"
                        Value = Constant "btn"
                        Namespace = None
                    }
                ]

                let original = RenderingCode.HtmlElement(Tags.div, attrs, Empty, handlers)
                let replacement = RenderingCode.HtmlElement(Tags.span, [], Constant "new", [])


                let result = replace [] replacement original


                match result with
                | Ok code ->
                    match code with
                    | RenderingCode.HtmlElement(tag, _, value, _) ->
                        Expect.equal tag Tags.span "Should update tag"
                        Expect.equal value (Constant "new") "Should update content"
                    | _ -> failtest "Wrong result type"
                | Error e -> failtest $"Failed to replace: {e}"

            testCase "Replace in empty list fails gracefully"
            <| fun _ ->

                let list = RenderingCode.HtmlList(UnorderedList, [], [], [])
                let replacement = RenderingCode.HtmlElement(Tags.li, [], Empty, [])


                let result = replace [ 0 ] replacement list


                match result with
                | Error msg -> Expect.stringContains msg "out of bounds" "Should indicate bounds error"
                | Ok _ -> failtest "Should fail on empty list access"

            testCase "Replace preserves list structure"
            <| fun _ ->

                let attrs = [
                    {
                        Key = "class"
                        Value = Constant "list"
                        Namespace = None
                    }
                ]

                let handlers = [ "hover", MsgHandler "OnHover" ]

                let list =
                    RenderingCode.HtmlList(
                        UnorderedList,
                        attrs,
                        [
                            RenderingCode.HtmlElement(Tags.li, [], Empty, [])
                            RenderingCode.HtmlElement(Tags.li, [], Empty, [])
                        ],
                        handlers
                    )

                let replacement = RenderingCode.HtmlElement(Tags.li, [], Constant "new", [])


                let result = replace [ 0 ] replacement list


                match result with
                | Ok(RenderingCode.HtmlList(listType, resultAttrs, items, resultHandlers)) ->
                    Expect.equal listType UnorderedList "Should preserve list type"
                    Expect.equal resultAttrs attrs "Should preserve attributes"
                    Expect.equal resultHandlers handlers "Should preserve handlers"
                    Expect.equal (List.length items) 2 "Should preserve list length"
                | _ -> failtest "Wrong result type or failed replacement"

            testCase "Replace with deeply nested path"
            <| fun _ ->

                let nested =
                    RenderingCode.HtmlObject(
                        Div,
                        [],
                        [ "outer" ],
                        Map.ofList [
                            "outer",
                            RenderingCode.HtmlList(
                                UnorderedList,
                                [],
                                [
                                    RenderingCode.HtmlObject(
                                        Div,
                                        [],
                                        [ "inner" ],
                                        Map.ofList [ "inner", RenderingCode.HtmlElement(Tags.div, [], Empty, []) ],
                                        []
                                    )
                                ],
                                []
                            )
                        ],
                        []
                    )

                let replacement = RenderingCode.HtmlElement(Tags.span, [], Constant "deep", [])


                let result = replace [ 0; 0; 0 ] replacement nested


                match result with
                | Ok code ->
                    match getElementAtPath [ 0; 0; 0 ] code with
                    | Ok(RenderingCode.HtmlElement(tag, _, value, _)) ->
                        Expect.equal tag Tags.span "Should update deeply nested tag"
                        Expect.equal value (Constant "deep") "Should update deeply nested content"
                    | _ -> failtest "Failed to retrieve replaced element"
                | Error e -> failtest $"Failed to replace: {e}"

            testCase "Replace validates path indices"
            <| fun _ ->

                let obj =
                    RenderingCode.HtmlObject(
                        Div,
                        [],
                        [ "key1" ],
                        Map.ofList [ "key1", RenderingCode.HtmlElement(Tags.div, [], Empty, []) ],
                        []
                    )

                let replacement = RenderingCode.HtmlElement(Tags.span, [], Empty, [])


                let result = replace [ 1 ] replacement obj


                match result with
                | Error msg -> Expect.stringContains msg "Invalid key index" "Should indicate invalid index"
                | Ok _ -> failtest "Should fail with invalid index"
        ]

        testList "ReorderObjectKeys Tests" [
            testCase "Reorder object keys"
            <| fun _ ->
                let obj =
                    RenderingCode.HtmlObject(
                        Div,
                        [],
                        [ "a"; "b" ],
                        Map.ofList [
                            "a", RenderingCode.HtmlElement(Tags.div, [], Empty, [])
                            "b", RenderingCode.HtmlElement(Tags.div, [], Empty, [])
                        ],
                        []
                    )

                let result = reorderObjectKeys [] [ "b"; "a" ] obj

                match result with
                | Ok(RenderingCode.HtmlObject(_, _, keys, _, _)) -> Expect.equal keys [ "b"; "a" ] "Should reorder keys"
                | Error e -> failtest e
                | _ -> failtest "Should remain an object"

            testCase "Reorder with invalid keys"
            <| fun _ ->
                let obj =
                    RenderingCode.HtmlObject(
                        Div,
                        [],
                        [ "a" ],
                        Map.ofList [ "a", RenderingCode.HtmlElement(Tags.div, [], Empty, []) ],
                        []
                    )

                let result = reorderObjectKeys [] [ "b" ] obj
                Expect.isError result "Should return error for invalid keys"

            testCase "Reorder with duplicate keys"
            <| fun _ ->
                let obj =
                    RenderingCode.HtmlObject(
                        Div,
                        [],
                        [ "a"; "b" ],
                        Map.ofList [
                            "a", RenderingCode.HtmlElement(Tags.div, [], Empty, [])
                            "b", RenderingCode.HtmlElement(Tags.div, [], Empty, [])
                        ],
                        []
                    )

                let result = reorderObjectKeys [] [ "a"; "a" ] obj
                Expect.isError result "Should return error for duplicate keys"

            testCase "Reorder nested object"
            <| fun _ ->
                let nested =
                    RenderingCode.HtmlObject(
                        Div,
                        [],
                        [ "outer" ],
                        Map.ofList [
                            "outer",
                            RenderingCode.HtmlObject(
                                Div,
                                [],
                                [ "a"; "b" ],
                                Map.ofList [
                                    "a", RenderingCode.HtmlElement(Tags.div, [], Empty, [])
                                    "b", RenderingCode.HtmlElement(Tags.div, [], Empty, [])
                                ],
                                []
                            )
                        ],
                        []
                    )

                let result = reorderObjectKeys [ 0 ] [ "b"; "a" ] nested
                Expect.isOk result "Should successfully reorder nested object"
        ]
    ]

let all = testList "All" [ RenderingCodeReplacementTests ]

[<EntryPoint>]
let main _ = Mocha.runTests all