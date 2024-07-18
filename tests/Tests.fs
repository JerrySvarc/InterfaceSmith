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
            let replacement = HtmlElement(Tag.Div, [], Empty, [])
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
                    ObjType.Div,
                    [ "header"; "content"; "footer" ],
                    Map.ofList [
                        "header", HtmlElement(H1, [], Empty, [])
                        "content",
                        HtmlObject(
                            ObjType.Section,
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


        testCase "No replacement for invalid path"
        <| fun _ ->
            let original = HtmlElement(P, [], Empty, [])
            let replacement = HtmlElement(Tag.Div, [], Empty, [])
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
                HtmlObject(ObjType.Div, [ "notHeader" ], Map.ofList [ ("header", HtmlElement(H1, [], Empty, [])) ], [])

            let replacement = HtmlElement(H2, [], Empty, [])
            let result = replace [ 0 ] replacement original
            Expect.equal result original "Should not modify the object for non-existent key"

        testCase "Replace Hole"
        <| fun _ ->
            let original = Hole(Named "placeholder")
            let replacement = HtmlElement(Tag.Div, [], Empty, [])
            let result = replace [] replacement original
            Expect.equal result replacement "Should replace Hole at root level"


        testCase "Replace with complex event handlers"
        <| fun _ ->
            let original =
                HtmlElement(Tag.Input, [], Empty, [ ("click", JSFunction("handleClick", "console.log('clicked')")) ])

            let replacement =
                HtmlElement(A, [], Empty, [ ("hover", JSFunction("handleHover", "console.log('hovered')")) ])

            let result = replace [] replacement original
            Expect.equal result replacement "Should replace element including event handlers"


    ]

let CodeGenerationTests =
    testList "CodeGeneration Tests" [
        testCase "Generate HTML for simple HtmlElement"
        <| fun _ ->
            let element = HtmlElement(P, [ ("class", Constant "text") ], Constant "Hello", [])
            let html, _ = generateCode element "{}" Map.empty

            Expect.equal
                html
                """<!DOCTYPE html>
    <html lang="en">
    <head>
        <meta charset="UTF-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <title>Generated Data-Driven App</title>
    </head>
    <body class="bg-gray-100 p-4">
        <div id="app" class="max-w-md mx-auto bg-white p-6 rounded-lg shadow-md"></div>
        <script id="appData" type="application/json">
        {}
        </script>
        <script>
        function renderApp(data) {
        const app = document.getElementById('app');
        app.innerHTML = `
            <p class="text">Hello</p>
        `;
        setupEventListeners(data);
        }

        function setupEventListeners(data) {
        }

        let appData;

        document.addEventListener('DOMContentLoaded', function() {
        appData = JSON.parse(document.getElementById('appData').textContent);
        renderApp(appData);
        });
        </script>
    </body>
    </html>"""
                "Should generate correct HTML for simple element"

        testCase "Generate HTML for HtmlElement with Data"
        <| fun _ ->
            let json = """{"value": 5}"""
            let element = HtmlElement(Tag.Div, [], Data, [])
            let html, _ = generateCode element json Map.empty
            Expect.stringContains html "<div >${data}</div>" "Should generate correct HTML with data from JSON"

        testCase "Generate HTML for HtmlList"
        <| fun _ ->
            let list =
                HtmlList(UnorderedList, [ HtmlElement(Li, [], Data, []); HtmlElement(Li, [], Data, []) ], [])

            let json = """["Item 1", "Item 2"]"""
            let html, _ = generateCode list json Map.empty

            Expect.stringContains
                html
                "<ul >${data.map((item, index) => `\n  <li>${item}</li>\n`).join('')}</ul>"
                "Should generate correct HTML for list with data from JSON"

        testCase "Generate HTML for HtmlObject"
        <| fun _ ->
            let obj =
                HtmlObject(
                    ObjType.Section,
                    [ "header"; "content" ],
                    Map.ofList [
                        "header", HtmlElement(H1, [], Data, [])
                        "content", HtmlElement(P, [], Data, [])
                    ],
                    []
                )

            let json = """{"header": "Title", "content": "Content"}"""
            let html, _ = generateCode obj json Map.empty

            Expect.stringContains
                html
                """<div >
        <div data-key="header">
          <h1 >${data.header}</h1>
        </div>
        <div data-key="content">
          <p >${data.content}</p>
        </div>
        </div>"""
                "Should generate correct HTML for object with data from JSON"

        testCase "Generate JavaScript for event handlers"
        <| fun _ ->
            let element =
                HtmlElement(
                    Button,
                    [],
                    Constant "Click me",
                    [ ("onClick", JSFunction("handleClick", "console.log('Clicked!');")) ]
                )

            let customHandlers =
                Map.ofList [ "handleClick", JSFunction("handleClick", "console.log('Clicked!');") ]

            let _, js = generateCode element "{}" customHandlers

            Expect.stringContains
                js
                """function handleClick(event, data) {
console.log('Clicked!');
  renderApp(data);
}

function setupEventListeners(data) {
  const handleClickElement = document.getElementById('handleClickElement');
  if (handleClickElement) handleClickElement.addEventListener('click', (event) => handleClick(event, data));
}"""
                "Should generate correct JavaScript for event handler"

        testCase "Generate HTML with event handlers"
        <| fun _ ->
            let element =
                HtmlElement(
                    Button,
                    [],
                    Constant "Click me",
                    [ ("onClick", JSFunction("handleClick", "console.log('Clicked!');")) ]
                )

            let customHandlers =
                Map.ofList [ "handleClick", JSFunction("handleClick", "console.log('Clicked!');") ]

            let html, _ = generateCode element "{}" customHandlers

            Expect.stringContains
                html
                "<button id=\"handleClickElement\">Click me</button>"
                "Should generate correct HTML with event handler"

        testCase "Generate HTML and JavaScript for complex structure"
        <| fun _ ->
            let complex =
                HtmlObject(
                    ObjType.Form,
                    [ "input"; "button" ],
                    Map.ofList [
                        "input", HtmlElement(Input, [ ("type", Constant "text") ], Empty, [])
                        "button",
                        HtmlElement(
                            Button,
                            [],
                            Data,
                            [ ("onClick", JSFunction("handleSubmit", "console.log('Submitted!');")) ]
                        )
                    ],
                    []
                )

            let json = """{"input": null, "button": "Submit"}"""

            let customHandlers =
                Map.ofList [ "handleSubmit", JSFunction("handleSubmit", "console.log('Submitted!');") ]

            let html, js = generateCode complex json customHandlers

            Expect.stringContains
                html
                """<div >
        <div data-key="input">
          <input type="text" ></input>
        </div>
        <div data-key="button">
          <button id="handleSubmitElement">${data.button}</button>
        </div>
        </div>"""
                "Should generate correct HTML for complex structure with data from JSON"

            Expect.stringContains
                js
                """function handleSubmit(event, data) {
console.log('Submitted!');
  renderApp(data);
}

function setupEventListeners(data) {
  const handleSubmitElement = document.getElementById('handleSubmitElement');
  if (handleSubmitElement) handleSubmitElement.addEventListener('click', (event) => handleSubmit(event, data));
}"""
                "Should generate correct JavaScript for complex structure"
    ]

let all = testList "All" [ RenderingCodeReplacementTests; CodeGenerationTests ]

[<EntryPoint>]
let main _ = Mocha.runTests all