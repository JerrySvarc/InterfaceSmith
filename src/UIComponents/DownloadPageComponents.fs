module UIComponents.DownloadPageComponents

open Feliz
open Fable.React

[<ReactComponent>]
let DownloadButton () =
    Html.div [
        prop.children [
            Html.button [
                prop.text "Download"
                prop.className
                    "bg-secondary-300 border-gray-00 m-2 p-2 rounded-md text-xl flex items-center hover:bg-secondary-600 "
            ]
        ]

    ]