module UIComponents.MainPageComponents

open System
open Browser.Types
open Feliz
open Fable.React
open Feliz.UseElmish
open Types.EditorDomain
open Types.RenderingTypes
open Utilities.Icons
open Fable.Core.JsInterop
open Elmish

[<ReactComponent>]
let Sidebar (model: Model) (dispatch: Msg -> unit) =
    Html.div [
        prop.className (
            sprintf
                "bg-gray-800 text-white transition-all duration-300 %s"
                (if model.IsSidebarOpen then "w-64" else "w-16")
        )
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
                                  ReactBindings.React.createElement(chevronLeft, createObj [
                                    "size" ==> 16
                                    "color" ==> "#4A5568"
                                ], [])
                            else
                               ReactBindings.React.createElement(chevronRight, createObj [
                                    "size" ==> 16
                                    "color" ==> "#4A5568"
                                ], [])
                        ]
                    ]
                ]
            ]
            Html.div [
                prop.className (sprintf "mt-4 %s" (if model.IsSidebarOpen then "" else "hidden"))
                prop.children [
                    for page in model.Pages do
                        Html.div [
                            prop.key page.Value.Id
                            prop.className (
                                sprintf
                                    "flex items-center p-2 hover:bg-gray-700 cursor-pointer %s"
                                    (if model.ActivePageId.Value = page.Value.Id then "bg-gray-700" else "")
                            )
                            prop.onClick (fun _ -> dispatch (SetActivePage page.Value.Id))
                            prop.children [
                                ReactBindings.React.createElement(fileIcon, createObj [
                                    "size" ==> 16
                                    "color" ==> "#4A5568"
                                ], []);
                                Html.span (page.Value.Name)
                            ]
                        ]
                    Html.button [
                        prop.className
                            "w-full mt-4 bg-blue-500 hover:bg-blue-600 text-white font-bold py-2 px-4 rounded"
                        prop.onClick (fun _ -> dispatch CreateNewPage)
                        prop.children [
                            ReactBindings.React.createElement(plusIcon, createObj [
                                    "size" ==> 16
                                    "color" ==> "#4A5568"
                                ], [])
                            Html.span "New Page"
                        ]
                    ]
                ]
            ]
        ]
    ]



[<ReactComponent>]
let Tabs (model: Model) (dispatch: Msg -> unit) =
    let scrollRef = React.useRef(null)
    let dragState, setDragState = React.useState(None)

    let scrollLeft () =
        if not (isNull scrollRef.current) then
            scrollRef.current?scrollBy(-100, 0)

    let scrollRight () =
        if not (isNull scrollRef.current) then
            scrollRef.current?scrollBy(100, 0)

    let onDragStart (e: DragEvent) (id: Guid) =
        e.dataTransfer.effectAllowed <- "move"
        setDragState(Some id)

    let onDragOver (e: DragEvent) =
        e.preventDefault()
        e.dataTransfer.dropEffect <- "move"

    let onDrop (e: DragEvent) (targetId: Guid) =
        e.preventDefault()
        match dragState with
        | Some draggedId when draggedId <> targetId ->
            let newOrder =
                model.Pages
                |> Map.toList
                |> List.map fst
                |> List.filter (fun id -> id <> draggedId)
                |> List.map (fun id -> if id = targetId then [draggedId; id] else [id])
                |> List.concat
            dispatch (ReorderTabs newOrder)
        | _ -> ()
        setDragState(None)

    Html.div [
        prop.className "flex items-center bg-white border-b"
        prop.children [
            Html.button [
                prop.className "px-2 py-1 hover:bg-gray-200"
                prop.onClick (fun _ -> scrollLeft())
                prop.children [  ReactBindings.React.createElement(chevronLeft, createObj [
                                    "size" ==> 16
                                    "color" ==> "#4A5568"
                                ], []) ]
            ]
            Html.div [
                //prop.ref  scrollRef
                prop.className "flex-1 overflow-x-auto flex"
                prop.children [
                    for KeyValue(id, page) in model.Pages do
                        Html.div [
                            prop.key (string id)
                            prop.className (sprintf "px-4 py-2 flex items-center cursor-move %s" (if model.ActivePageId = Some id then "bg-gray-200" else ""))
                            prop.draggable true
                            prop.onDragStart (fun e -> onDragStart e id)
                            prop.onDragOver onDragOver
                            prop.onDrop (fun e -> onDrop e id)
                            prop.children [
                                Html.button [
                                    prop.onClick (fun _ -> dispatch (SetActivePage id))
                                    prop.text page.Name
                                ]
                                Html.button [
                                    prop.className "ml-2"
                                    prop.onClick (fun _ -> dispatch (ClosePage id))
                                    prop.children [ ReactBindings.React.createElement(xIcon, createObj [
                                    "size" ==> 16
                                    "color" ==> "#4A5568"
                                ], []) ]
                                ]
                            ]
                        ]
                ]
            ]
            Html.button [
                prop.className "px-2 py-1 hover:bg-gray-200"
                //prop.onClick scrollRight
                prop.children [  ReactBindings.React.createElement(chevronRight, createObj [
                                    "size" ==> 16
                                    "color" ==> "#4A5568"
                                ], []) ]
            ]
        ]
    ]

// PageEditor component
type PageEditorModel =
    { IsJavaScriptMode: bool
      JsCode: string }

type PageEditorMsg =
    | ToggleMode
    | UpdateJsCode of string

let pageEditorInit(): PageEditorModel * Cmd<PageEditorMsg> =
    { IsJavaScriptMode = false
      JsCode = "// Write your custom JavaScript here" }, Cmd.none

let pageEditorUpdate (msg: PageEditorMsg) (model: PageEditorModel): PageEditorModel * Cmd<PageEditorMsg> =
    match msg with
    | ToggleMode -> { model with IsJavaScriptMode = not model.IsJavaScriptMode }, Cmd.none
    | UpdateJsCode code -> { model with JsCode = code }, Cmd.none

[<ReactComponent>]
let PageEditor (page : Page) =
    let model, dispatch = React.useElmish(pageEditorInit, pageEditorUpdate, [| box page|])

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
                                        prop.text (
                                            if model.IsJavaScriptMode then
                                                "Custom JavaScript"
                                            else
                                                "Element Modification"
                                        )
                                    ]
                                    Html.button [
                                        prop.className "p-1 rounded hover:bg-gray-300"
                                        prop.onClick (fun _ -> dispatch ToggleMode)
                                        prop.children [
                                            if model.IsJavaScriptMode then
                                                ReactBindings.React.createElement(settingsIcon, createObj [
                                                "size" ==> 20
                                                "color" ==> "#4A5568"
                                            ], [])
                                            else
                                                ReactBindings.React.createElement(codeIcon, createObj [
                                                "size" ==> 20
                                                "color" ==> "#4A5568"
                                            ], [])
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
                                        Html.div [ prop.children [ Html.p "Element modification options go here" ] ]
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

