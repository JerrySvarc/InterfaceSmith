module UIComponents.MainPageComponents

open Fable.React.ReactiveComponents
open Feliz
open Fable.React
open Types.EditorDomain
open Types.RenderingTypes
open Feliz.UseElmish

[<ReactComponent>]
let Sidebar (model: Model) (dispatch: Msg -> unit) =
    Html.div [
        prop.className (sprintf "bg-gray-800 text-white transition-all duration-300 %s" (if model.IsSidebarOpen then "w-64" else "w-16"))
        prop.children [
            Html.div [
                prop.className "p-4"
                prop.children [
                    Html.h2 [
                        prop.className (sprintf "font-bold %s" (if model.IsSidebarOpen then "" else "hidden"))
                        prop.text "Pages"
                    ]
                    Html.button [
                        prop.className "absolute top-4 right-4 text-white"
                        prop.onClick (fun _ -> dispatch ToggleSidebar)
                        prop.children [
                            if model.IsSidebarOpen then
                                Feliz.Lucide.chevronLeft [ lucide.size 24 ]
                            else
                                Feliz.Lucide.chevronRight [ lucide.size 24 ]
                        ]
                    ]
                ]
            ]
            Html.div [
                prop.className (sprintf "mt-4 %s" (if model.IsSidebarOpen then "" else "hidden"))
                prop.children [
                    for page in model.Pages do
                        Html.div [
                            prop.key page.Id
                            prop.className (sprintf "flex items-center p-2 hover:bg-gray-700 cursor-pointer %s" (if model.ActiveTabId = page.Id then "bg-gray-700" else ""))
                            prop.onClick (fun _ -> dispatch (SetActiveTab page.Id))
                            prop.children [
                                Feliz.Lucide.file [ lucide.size 16; lucide.className "mr-2" ]
                                Html.span page.Title
                            ]
                        ]
                    Html.button [
                        prop.className "w-full mt-4 bg-blue-500 hover:bg-blue-600 text-white font-bold py-2 px-4 rounded"
                        prop.onClick (fun _ -> dispatch CreateNewPage)
                        prop.children [
                            Feliz.Lucide.plus [ lucide.size 16; lucide.className "mr-2 inline" ]
                            Html.span "New Page"
                        ]
                    ]
                ]
            ]
        ]
    ]

[<ReactComponent>]
let Tabs (model: Model) (dispatch: Msg -> unit) =
    Html.div [
        prop.className "bg-white border-b flex"
        prop.children [
            for page in model.Pages do
                Html.div [
                    prop.key page.Id
                    prop.className (sprintf "px-4 py-2 flex items-center %s" (if model.ActiveTabId = page.Id then "bg-gray-200" else ""))
                    prop.children [
                        Html.button [
                            prop.onClick (fun _ -> dispatch (SetActiveTab page.Id))
                            prop.text page.Title
                        ]
                        Html.button [
                            prop.className "ml-2"
                            prop.onClick (fun _ -> dispatch (ClosePage page.Id))
                            prop.children [ Feliz.Lucide.x [ lucide.size 16 ] ]
                        ]
                    ]
                ]
        ]
    ]

[<ReactComponent>]
let PageEditor (model: Model) (dispatch: Msg -> unit) =
    Html.div [
        prop.className "flex-1 flex overflow-hidden"
        prop.children [
            // Left panel
            Html.div [
                prop.className "w-1/2 flex flex-col border-r"
                prop.children [
                    // JSON data window
                    Html.div [
                        prop.className "h-1/2 p-4 overflow-auto border-b"
                        prop.children [
                            Html.h3 [ prop.className "font-bold mb-2"; prop.text "JSON Data" ]
                            Html.p "JSON data will be displayed here"
                        ]
                    ]
                    // Element modification / JavaScript window
                    Html.div [
                        prop.className "h-1/2 flex flex-col"
                        prop.children [
                            Html.div [
                                prop.className "flex justify-between items-center p-2 bg-gray-200"
                                prop.children [
                                    Html.h3 [
                                        prop.className "font-bold"
                                        prop.text (if model.IsJavaScriptMode then "Custom JavaScript" else "Element Modification")
                                    ]
                                    Html.button [
                                        prop.className "p-1 rounded hover:bg-gray-300"
                                        prop.onClick (fun _ -> dispatch ToggleMode)
                                        prop.children [
                                            if model.IsJavaScriptMode then
                                                Feliz.Lucide.settings [ lucide.size 20 ]
                                            else
                                                Feliz.Lucide.code [ lucide.size 20 ]
                                        ]
                                    ]
                                ]
                            ]
                            Html.div [
                                prop.className "flex-1 overflow-auto p-4"
                                prop.children [
                                    if model.IsJavaScriptMode then
                                        Html.textarea [
                                            prop.className "w-full h-full resize-none border rounded p-2"
                                            prop.value model.JsCode
                                            prop.onChange (fun e -> dispatch (UpdateJsCode e))
                                            prop.placeholder "Write your custom JavaScript here"
                                        ]
                                    else
                                        Html.div [
                                            prop.children [
                                                Html.p "Element modification options go here"
                                            ]
                                        ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]

            // Right panel - Preview sandbox
            Html.div [
                prop.className "w-1/2 p-4 overflow-auto"
                prop.children [
                    Html.h3 [ prop.className "font-bold mb-2"; prop.text "Preview" ]
                    Html.p "Preview of the application will be shown here"
                ]
            ]
        ]
    ]

[<ReactComponent>]
let LowCodeEditor() =
    let state, dispatch = React.useElmish(init, update, [||])

    Html.div [
        prop.className "flex h-screen bg-gray-100 text-gray-800"
        prop.children [
            Sidebar state dispatch
            Html.div [
                prop.className "flex-1 flex flex-col overflow-hidden"
                prop.children [
                    Tabs state dispatch
                    PageEditor state dispatch
                ]
            ]
        ]
    ]