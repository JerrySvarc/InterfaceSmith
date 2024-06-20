module Tests

open Fable.Mocha

let client =
    testList "Client" [
        testCase "Added todo"
        <| fun _ ->
          printfn "hello"
    ]

let all =
    testList "All" [
#if FABLE_COMPILER
        Shared.Tests.shared
#endif
        client
    ]

[<EntryPoint>]
let main _ = Mocha.runTests all