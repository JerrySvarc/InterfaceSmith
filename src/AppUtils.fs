module AppUtilities

open Feliz

let block (content: ReactElement list) =
    Html.div [
        prop.className "flex bg-white border border-gray-300 p-6 rounded-lg shadow-md justify-center items-center"
        prop.children content
    ]

let box (content: ReactElement list) =
    Html.div [
        prop.className "flex bg-white border border-gray-300 p-6 rounded-lg shadow-md"
        prop.children content
    ]

let dropdownItem (text: string) (setSelectedItem: string -> unit) =
    Html.a [
        prop.className "block px-4 py-2 text-sm text-gray-700 hover:bg-gray-100"
        prop.href "#"
        prop.text text
        prop.onClick (fun _ -> setSelectedItem text)
    ]

let dropdownMenu items (defaultText: string) =
    let (dropdownVisible, setDropdownVisible) = React.useState false
    let (selectedItem, setSelectedItem) = React.useState defaultText

    Html.div [
        prop.className "relative inline-block text-left"
        prop.children [
            Html.div [
                prop.children [
                    Html.button [
                        prop.className
                            "inline-flex justify-center w-full rounded-md bg-white px-3 py-2 text-sm font-semibold text-gray-900 shadow-sm ring-1 ring-inset ring-gray-300 hover:bg-gray-50"
                        prop.text selectedItem
                        prop.onClick (fun _ -> setDropdownVisible (not dropdownVisible))
                    ]
                ]
            ]
            Html.div [
                prop.className (
                    sprintf
                        "origin-top-right absolute right-0 mt-2 w-56 rounded-md shadow-lg bg-white ring-1 ring-black ring-opacity-5 %s"
                        (if dropdownVisible then "block" else "hidden")
                )
                prop.custom ("role", "menu")
                prop.onMouseLeave (fun _ -> setDropdownVisible false)
                prop.children [
                    Html.div [
                        prop.className "py-1"
                        prop.children (items |> List.map (fun item -> dropdownItem item setSelectedItem))
                    ]
                ]
            ]
        ]
    ]