module Editor.Types.PageModel

type PageEditorModel = {
    IsJavaScriptMode: bool
    JsCode: string
}

type PageEditorMsg =
    | ToggleMode
    | UpdateJsCode of string