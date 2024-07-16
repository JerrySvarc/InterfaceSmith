module Editor.Utilities.JavaScriptEditor

open Fable.Core

[<Import("default", from="@uiw/react-codemirror")>]
let CodeMirror: obj = jsNative

[<Import("javascript", from="@codemirror/lang-javascript")>]
let javascript: obj = jsNative

[<Import("javascript", from="@codemirror/theme/material.css")>]
let theme: obj = jsNative
