module Editor.Components.EditorComponents

open Feliz
open Fable.React
open Editor.Types.EditorDomain
open Editor.Types.PageEditorDomain
open Editor.Utilities.Icons
open Fable.Core.JsInterop



/// <summary>The tab of the pages side menu.</summary>
/// <param name="model">Current model of the main Elmish app.</param>
/// <param name="page">Page corresponding to the tab.</param>
/// <param name="dispatch">Main Elmish application's dispatch function.</param>
/// <returns></returns>
[<ReactComponent>]
let PageTab (model: Model) (page: PageEditorModel) (dispatch: Msg -> unit) : ReactElement =
    let isNameEditing, setIsNameEditing = React.useState false
    let nameInput, setNameInput = React.useState page.PageData.Name

    Html.div [
        prop.className (
            sprintf
                "flex items-center p-2 hover:bg-gray-700 cursor-pointer rounded group/tab %s"
                (if model.ActivePageId = Some page.PageData.Id then
                     "bg-gray-700"
                 else
                     "")
        )
        prop.onClick (fun _ -> dispatch (OpenPage page.PageData.Id))
        prop.onDoubleClick (fun _ -> setIsNameEditing true)
        prop.children [
            ReactBindings.React.createElement (fileIcon, createObj [ "size" ==> 16; "color" ==> "#FFFFFF" ], [])
            match isNameEditing with
            | true ->
                Html.div [
                    prop.className "flex flex-col space-y-1 w-full"
                    prop.children [
                        Html.input [
                            prop.className
                                "ml-2 bg-gray-800 text-white px-2 py-1 rounded w-full text-sm outline-none border border-gray-600 focus:border-blue-500"
                            prop.value nameInput
                            prop.onChange setNameInput
                        ]
                        Html.div [
                            prop.className "flex space-x-1 ml-2"
                            prop.children [
                                Html.button [
                                    prop.className
                                        "flex-1 bg-gray-700 hover:bg-gray-600 text-white py-1 px-2 rounded text-sm flex items-center justify-center"
                                    prop.onClick (fun _ ->
                                        if nameInput.Length > 0 then
                                            dispatch (
                                                UpdatePage(
                                                    {
                                                        page with
                                                            PageData = { page.PageData with Name = nameInput }
                                                    }
                                                )
                                            )

                                            setIsNameEditing false)
                                    prop.children [
                                        ReactBindings.React.createElement (
                                            checkIcon,
                                            createObj [ "size" ==> 12; "color" ==> "#FFFFFF" ],
                                            []
                                        )
                                        Html.span [ prop.className "ml-1"; prop.text "Save" ]
                                    ]
                                ]
                                Html.button [
                                    prop.className
                                        "flex-1 bg-gray-700 hover:bg-gray-600 text-white py-1 px-2 rounded text-sm flex items-center justify-center"
                                    prop.onClick (fun _ -> setIsNameEditing false)
                                    prop.children [
                                        ReactBindings.React.createElement (
                                            xIcon,
                                            createObj [ "size" ==> 12; "color" ==> "#FFFFFF" ],
                                            []
                                        )
                                        Html.span [ prop.className "ml-1"; prop.text "Cancel" ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]

            | false ->
                Html.div [
                    prop.className "flex items-center flex-1 min-w-0"
                    prop.children [
                        Html.span [
                            prop.className "ml-2 truncate text-sm"
                            prop.title page.PageData.Name
                            prop.text page.PageData.Name
                        ]
                    ]
                ]

                Html.div [
                    prop.className "ml-2 flex-shrink-0 invisible group-hover/tab:visible"
                    prop.children [
                        Html.button [
                            prop.className "p-1 hover:bg-red-900 rounded flex items-center justify-center"
                            prop.onClick (fun e ->
                                e.stopPropagation ()
                                dispatch (DeletePage page.PageData.Id))
                            prop.children [
                                ReactBindings.React.createElement (
                                    trashIcon,
                                    createObj [ "size" ==> 14; "color" ==> "#FFFFFF" ],
                                    []
                                )
                            ]
                        ]
                    ]
                ]
        ]
    ]

/// <summary>Displays the tabs for the different pages and a button to create a new page.</summary>
/// <param name="model"></param>
/// <param name="dispatch"></param>
/// <returns></returns>
[<ReactComponent>]
let SideBarContent (model: Model) (dispatch: Msg -> unit) : ReactElement =
    Html.div [
        prop.className "p-4 mt-12 "
        prop.children [
            if model.IsSidebarOpen then
                Html.h2 [ prop.className "font-bold mb-4"; prop.text "Pages" ]

                Html.div [
                    prop.className "space-y-2"
                    prop.children (
                        (model.PageOrder
                         |> List.map (fun pageId ->
                             let pageOption = model.Pages.TryFind pageId

                             match pageOption with
                             | Some(page) -> PageTab model page dispatch
                             | None -> failwith "Page not found."))
                        @ [
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
                                    Html.span [ prop.className "ml-2"; prop.text "Create a new page" ]
                                ]
                            ]
                        ]
                    )
                ]
        ]
    ]

/// <summary>The collapsible sidebar menu. Contains the SideBarContent.</summary>
/// <param name="model"></param>
/// <param name="dispatch"></param>
/// <returns></returns>
[<ReactComponent>]
let Sidebar (model: Model) (dispatch: Msg -> unit) : ReactElement =
    Html.div [
        prop.className (
            sprintf
                "relative bg-gray-800 text-white transition-all duration-300 %s overflow-auto"
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
            SideBarContent model dispatch
        ]
    ]