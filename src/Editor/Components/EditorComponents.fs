module Editor.UIComponents.EditorComponents

open Feliz
open Fable.React
open Editor.Types.EditorDomain
open Editor.Types.PageEditorDomain
open Editor.Utilities.Icons
open Fable.Core.JsInterop


///<summary>
let PageTab (model: Model) (page: PageEditorModel) (dispatch: Msg -> unit) : ReactElement =
    Html.div [
        prop.className (
            sprintf
                "flex items-center p-2 hover:bg-gray-700 cursor-pointer rounded group %s"
                (if model.ActivePageId = Some page.PageData.Id then
                     "bg-gray-700"
                 else
                     "")
        )
        prop.onClick (fun _ -> dispatch (OpenPage page.PageData.Id))
        prop.children [
            ReactBindings.React.createElement (fileIcon, createObj [ "size" ==> 16; "color" ==> "#FFFFFF" ], [])
            Html.span [ prop.className "ml-2"; prop.text page.PageData.Name ]
            Html.div [
                prop.className "ml-auto relative invisible group-hover:visible hover:bg-red-900 rounded"
                prop.children [
                    Html.button [
                        prop.className "flex items-center justify-center w-8 h-8 rounded-full"
                        prop.onClick (fun _ -> dispatch (DeletePage page.PageData.Id))
                        prop.children [
                            ReactBindings.React.createElement (
                                trashIcon,
                                createObj [ "size" ==> 16; "color" ==> "#FFFFFF" ],
                                []
                            )
                        ]
                    ]
                ]
            ]
        ]
    ]




/// <summary></summary>
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

/// <summary></summary>
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