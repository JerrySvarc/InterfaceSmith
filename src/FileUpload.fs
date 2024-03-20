// Created based on the guide described in the SAFE stack docs.
// https://safe-stack.github.io/docs/recipes/client-server/upload-file-from-client/

module FileUpload
open Fable.Core.JsInterop

let isJsonFile (file: Browser.Types.File) =
    file.GetType().Name.EndsWith("File")
    && file.name.EndsWith(".json")

let handleFileEvent onLoad (fileEvent: Browser.Types.Event) =
    let files: Browser.Types.FileList = !!fileEvent.target?files

    if files.length > 0 then
        let reader = Browser.Dom.FileReader.Create()
        reader.onload <- (fun _ -> reader.result |> unbox |> onLoad)

        if isJsonFile files.[0] then
            reader.readAsText (files.[0])
        else
            onLoad ""