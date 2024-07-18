module Editor.Utilities.JavaScriptEditor

open Fable.Core.JsInterop


let CodeMirror :obj = importDefault "@uiw/react-codemirror"
let javascript : obj= importAll "@codemirror/lang-javascript"
let html:obj = importAll "@codemirror/lang-html"
let css :obj= importAll "@codemirror/lang-css"