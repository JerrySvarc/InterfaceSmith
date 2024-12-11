module CoreLogic.Operations.CodeGeneration

open CoreLogic.Types.RenderingTypes
open Fable.SimpleJson
open CoreLogic.Operations.RenderingCode

let indentCode (code: string) (indentLevel: int) =
    let indent = String.replicate indentLevel "  "

    code.Split('\n')
    |> Array.map (fun line ->
        if System.String.IsNullOrWhiteSpace(line) then
            line
        else
            indent + line)
    |> String.concat "\n"


// Generates the full HTML and JavaScript code for the given RenderingCode and JSON data
let generateCode (code: RenderingCode) (json: string) (customHandlers: Map<string, Javascript>) = ""