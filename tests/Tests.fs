module Tests

open Fable.Mocha
open System.Collections.Generic
open CoreLogic.Types.RenderingTypes
open CoreLogic.Operations.CodeGeneration
open CoreLogic.Operations.RenderingCode
open Fable.SimpleJson
open Editor.Types.PageEditorDomain

let RenderingCodeReplacementTests =
    testList "RenderingCode AST Tests" [
        testList "Delete Element Tests" [
            testCase "Delete root element"
            <| fun _ ->
                let code = RenderingCode.HtmlElement(Tags.div, [], Empty, [])
                let result = deleteElement [] code
                Expect.equal result (RenderingCode.Hole UnNamed) "Should replace with unnamed hole"

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
                | RenderingCode.HtmlList(_, _, items, _) ->
                    Expect.equal (List.length items) 1 "Should remove one item"

                    match List.head items with
                    | RenderingCode.HtmlElement(_, _, Constant value, _) ->
                        Expect.equal value "2" "Should keep second item"
                    | _ -> failtest "Unexpected item type"
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
                | RenderingCode.HtmlObject(_, _, keys, items, _) ->
                    Expect.equal (List.length keys) 1 "Should have one key"
                    Expect.equal (Map.count items) 1 "Should have one item"
                    Expect.isTrue (List.contains "content" keys) "Should keep content key"
                | _ -> failtest "Should remain an object"
        ]

        testList "GetElementAtPath Tests" [
            testCase "Get root element"
            <| fun _ ->
                let element = RenderingCode.HtmlElement(Tags.div, [], Empty, [])
                let result = getElementAtPath [] element
                Expect.equal result element "Should return root element"

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
                | RenderingCode.HtmlElement(_, _, Constant value, _) -> Expect.equal value "2" "Should get second item"
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
                | RenderingCode.HtmlElement(_, _, Constant value, _) ->
                    Expect.equal value "Title" "Should get header element"
                | _ -> failtest "Should get object field"
        ]

        testList "Replace Tests" [
            testCase "Replace in list"
            <| fun _ ->
                let list =
                    RenderingCode.HtmlList(UnorderedList, [], [ RenderingCode.HtmlElement(Tags.li, [], Empty, []) ], [])

                let replacement = RenderingCode.HtmlElement(Tags.li, [], Constant "new", [])
                let result = replace [ 0 ] replacement list

                match result with
                | RenderingCode.HtmlList(_, _, [ item ], _) -> Expect.equal item replacement "Should replace item"
                | _ -> failtest "Should remain a list"
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
                | RenderingCode.HtmlObject(_, _, keys, _, _) -> Expect.equal keys [ "b"; "a" ] "Should reorder keys"
                | _ -> failtest "Should remain an object"
        ]
    ]


let all = testList "All" [ RenderingCodeReplacementTests ]

[<EntryPoint>]
let main _ = Mocha.runTests all