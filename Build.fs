open Fake.Core
open Fake.IO
open Farmer
open Farmer.Builders

open Helpers

initializeContext ()

let sourcePath = Path.getFullName "src/"
let deployPath = Path.getFullName "deploy"

let testsPath = Path.getFullName "tests/"
let documentationPath = Path.getFullName "Documentation"

Target.create "Clean" (fun _ ->
    Shell.cleanDir deployPath
    run dotnet [ "fable"; "clean"; "--yes" ] sourcePath)

Target.create "InstallClient" (fun _ -> run npm [ "install" ] ".")

Target.create "Bundle" (fun _ ->
    [
        "client", dotnet [ "fable"; "-o"; "output"; "-s"; "--run"; "npx"; "vite"; "build" ] sourcePath
    ]
    |> runParallel)

Target.create "Run" (fun _ ->
    [
        "client", dotnet [ "fable"; "watch"; "-o"; "output"; "-s"; "--run"; "npx"; "vite" ] sourcePath
        "docs",
        CreateProcess.fromRawCommand "mkdocs" [ "serve"; "-a"; "localhost:8082" ]
        |> CreateProcess.withWorkingDirectory documentationPath
        |> CreateProcess.ensureExitCode
    ]
    |> runParallel)


Target.create "RunTests" (fun _ ->
    [
        "client", dotnet [ "fable"; "watch"; "-o"; "output"; "-s"; "--run"; "npx"; "vite" ] testsPath
    ]
    |> runParallel)

Target.create "BuildDocs" (fun _ ->
    Shell.cleanDir "./Documentation/docs/site"
    Shell.Exec("mkdocs", "build", "./Documentation") |> ignore)

Target.create "Format" (fun _ -> run dotnet [ "fantomas"; "." ] ".")

open Fake.Core.TargetOperators

let dependencies = [
    "Clean" ==> "InstallClient" ==> "Bundle"

    "Clean" ==> "InstallClient" ==> "BuildDocs" ==> "Run"

    "InstallClient" ==> "RunTests"
    "Clean" ==> "BuildDocs"
]

[<EntryPoint>]
let main args = runOrDefault args