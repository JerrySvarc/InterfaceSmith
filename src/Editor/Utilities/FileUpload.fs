// Followed the guide described in the SAFE stack docs and added some type safety to the file upload process.
// https://safe-stack.github.io/docs/recipes/client-server/upload-file-from-client/

module Editor.Utilities.FileUpload

open Fable.React
open Fable.Core
open Fable.Core.JsInterop
open Browser.Types
open Editor.Types.PageEditorDomain
open Fable.Core.JS


let isJsonFile (file: Browser.Types.File) =
    let validMimeTypes = [ "application/json"; "text/json" ]

    not (isNull file)
    && file.GetType().Name.EndsWith("File")
    && (validMimeTypes |> List.contains file.``type``
        || file.name.EndsWith(".json", System.StringComparison.OrdinalIgnoreCase))

let handleFileEvent (onLoad: Result<string, FileValidationError> -> unit) (fileEvent: Browser.Types.Event) =
    let files: Browser.Types.FileList = !!fileEvent.target?files

    if files.length = 0 then
        onLoad (Error EmptyFile)
    else
        let file = files.[0]

        match isNull file with
        | true -> onLoad (Error EmptyFile)
        | false ->
            if not (isJsonFile file) then
                onLoad (Error InvalidFileType)
            else
                let reader = Browser.Dom.FileReader.Create()

                reader.onload <-
                    (fun _ ->
                        match reader.result with
                        | null -> onLoad (Error(ReadError "File content is null"))
                        | content -> onLoad (Ok(unbox content)))

                reader.onerror <- (fun _ -> onLoad (Error(ReadError "Failed to read file")))

                reader.readAsText file