module UIComponents.MainPageComponents

open Feliz
open Fable.React
open Types

[<ReactComponent>]
let NavItem (text: string, isActive: bool, dispatch, msg) =
    let activeClass = if isActive then "bg-gray-700 " else ""

    Html.button [
        prop.className $"text-white hover:bg-gray-600 {activeClass}px-3 py-2 rounded-md text-xl font-medium mx-2"
        prop.children [ Html.text text ]
        prop.onMouseDown (fun _ -> dispatch msg)
    ]

[<ReactComponent>]
let Header (activePage: TabType, dispatch) =
    Html.header [
        prop.className "fixed top-0 left-0 w-full z-10 flex justify-between items-center bg-primary-800 text-white p-4"
        prop.children [
            Html.nav [
                prop.className "flex items-center"
                prop.children [
                    NavItem("Home", activePage = Main, dispatch, ChangeTab Main)
                    NavItem("Editor", activePage = Editor, dispatch, ChangeTab Editor)
                ]
            ]
        ]
    ]


[<ReactComponent>]
let Application (content, model, dispatch) =
    Html.div [
        prop.className "flex flex-col h-screen"
        prop.children [
            Header(model.CurrentTab, dispatch)
            Html.div [
                prop.className "flex flex-1 overflow-auto pt-16"
                prop.children [
                    Html.div [ prop.className "flex-1 bg-gray-200 overflow-auto"; prop.children [ content ] ]
                ]
            ]
        ]
    ]