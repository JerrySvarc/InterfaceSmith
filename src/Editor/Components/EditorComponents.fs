module Editor.UIComponents.EditorComponents

open System
open Browser.Types
open Feliz
open Fable.React
open Editor.Types.EditorModel
open Editor.Utilities.Icons
open Fable.Core.JsInterop

[<ReactComponent>]
let Sidebar (model: Model) (dispatch: Msg -> unit) =
    Html.div [
        prop.className (
            sprintf
                "relative bg-gray-800 text-white transition-all duration-300 %s"
                (if model.IsSidebarOpen then "w-64" else "w-16")
        )
        prop.children [
            // Toggle button
            Html.button [
                prop.className (
                    sprintf
                        "absolute top-4 %s p-2 rounded hover:bg-gray-700 transition-colors duration-200"
                        (if model.IsSidebarOpen then
                             "right-4"
                         else
                             "left-1/2 -translate-x-1/2")
                )
                prop.onClick (fun _ -> dispatch ToggleSidebar)
                prop.children [
                    if model.IsSidebarOpen then
                        ReactBindings.React.createElement (
                            arrowLeftLine,
                            createObj [ "size" ==> 20; "color" ==> "#FFFFFF" ],
                            []
                        )
                    else
                        ReactBindings.React.createElement (
                            arrowRightLine,
                            createObj [ "size" ==> 20; "color" ==> "#FFFFFF" ],
                            []
                        )
                ]
            ]

            // Sidebar content
            Html.div [
                prop.className "p-4 mt-12"
                prop.children [
                    if model.IsSidebarOpen then
                        Html.h2 [ prop.className "font-bold mb-4"; prop.text "Pages" ]

                    if model.IsSidebarOpen then
                        Html.div [
                            prop.className "space-y-2"
                            prop.children (
                                (model.Pages
                                 |> Map.toList
                                 |> List.mapi (fun index (pageId, page) -> // Use mapi to get the index
                                     Html.div [
                                         prop.key (sprintf "%s-%d" (string pageId) index) // Combine pageId and index for a unique key
                                         prop.className (
                                             sprintf
                                                 "flex items-center p-2 hover:bg-gray-700 cursor-pointer rounded %s"
                                                 (if model.ActiveTabId = Some pageId then
                                                      "bg-gray-700"
                                                  else
                                                      "")
                                         )
                                         prop.onClick (fun _ -> dispatch (OpenOrSelectTab pageId))
                                         prop.children [
                                             ReactBindings.React.createElement (
                                                 fileIcon,
                                                 createObj [ "size" ==> 16; "color" ==> "#FFFFFF" ],
                                                 []
                                             )
                                             Html.span [ prop.className "ml-2"; prop.text page.Name ]
                                         ]
                                     ]))
                                @ [ // Append the "New Page" button to the list
                                    Html.button [
                                        prop.key "create-new-page-button"
                                        prop.className
                                            "w-full mt-4 bg-blue-500 hover:bg-blue-600 text-white font-bold py-2 px-4 rounded flex items-center justify-center"
                                        prop.onClick (fun _ -> dispatch CreatePage)
                                        prop.children [
                                            ReactBindings.React.createElement (
                                                plusIcon,
                                                createObj [ "size" ==> 16; "color" ==> "#FFFFFF" ],
                                                []
                                            )
                                            Html.span [ prop.className "ml-2"; prop.text "New Page" ]
                                        ]
                                    ]
                                ]
                            )
                        ]
                ]
            ]
        ]
    ]

[<ReactComponent>]
let Tabs (model: Model) (dispatch: Msg -> unit) =
    let scrollRef = React.useRef (null)
    let (dragState, setDragState) = React.useState (None)

    let onDragStart (e: DragEvent) (tabId: Guid) =
        e.dataTransfer.effectAllowed <- "move"
        setDragState (Some tabId)

    let onDragOver (e: DragEvent) =
        e.preventDefault ()
        e.dataTransfer.dropEffect <- "move"

    let onDrop (e: DragEvent) (targetTabId: Guid) =
        e.preventDefault ()

        match dragState with
        | Some draggedTabId when draggedTabId <> targetTabId ->
            let newOrder =
                model.OpenTabs
                |> List.filter (fun tab -> tab.Id <> draggedTabId)
                |> List.fold
                    (fun acc tab ->
                        if tab.Id = targetTabId then
                            acc @ [ model.OpenTabs |> List.find (fun t -> t.Id = draggedTabId) ] @ [ tab ]
                        else
                            acc @ [ tab ])
                    []

            dispatch (ReorderTabs newOrder)
        | _ -> ()

        setDragState (None)

    Html.div [
        prop.className "flex items-center bg-white border-b"
        prop.children [
            Html.div [
                prop.ref (fun element -> scrollRef.current <- element)
                prop.className "flex-1 overflow-x-auto flex"
                prop.children [
                    for tab in model.OpenTabs do
                        match Map.tryFind tab.PageId model.Pages with
                        | Some page ->
                            Html.div [
                                prop.key (string tab.Id)
                                prop.className (
                                    sprintf
                                        "px-4 py-2 flex items-center cursor-move %s"
                                        (if model.ActiveTabId = Some tab.Id then
                                             "bg-gray-200"
                                         else
                                             "")
                                )
                                prop.draggable true
                                prop.onDragStart (fun e -> onDragStart e tab.Id)
                                prop.onDragOver onDragOver
                                prop.onDrop (fun e -> onDrop e tab.Id)
                                prop.children [
                                    Html.button [
                                        prop.onClick (fun _ -> dispatch (SetActiveTab tab.Id))
                                        prop.text page.Name
                                    ]
                                    Html.button [
                                        prop.className "ml-2"
                                        prop.onClick (fun _ -> dispatch (CloseTab tab.Id))
                                        prop.children [
                                            ReactBindings.React.createElement (
                                                xIcon,
                                                createObj [ "size" ==> 16; "color" ==> "#000000" ],
                                                []
                                            )
                                        ]
                                    ]
                                ]
                            ]
                        | None -> () // Handle case where page doesn't exist for tab
                ]
            ]
        //maybe run sandbox button here
        ]
    ]