module Tests

open Fable.Mocha

let client = testList "Client" [ testCase "Added todo" <| fun _ -> printfn "hello" ]

let all = testList "All" [ client ]

[<EntryPoint>]
let main _ = Mocha.runTests all