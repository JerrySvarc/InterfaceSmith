module App

open Fable.Core.JsInterop

importSideEffects "./index.css"

open Elmish
open Elmish.React
open Main
#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram Main.init Main.update Main.view
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactSynchronous "elmish-app"
|> Program.run