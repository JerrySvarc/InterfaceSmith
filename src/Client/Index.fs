module Index

open Elmish
open Fable.Remoting.Client
open Shared

type Model = {  }

type Msg =


let init () : Model * Cmd<Msg> =


let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =


open Feliz
open Feliz.Bulma



let view (model: Model) (dispatch: Msg -> unit) =
