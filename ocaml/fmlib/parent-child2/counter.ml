module H = Fmlib_browser.Html
module A = Fmlib_browser.Attribute
open Printf

type t = { value : int }

let init : t list = [ { value = 0 }; { value = 0 }; { value = 0 } ]

type internal_msg = Inc
type external_msg = Delete

type meta_msg =
  | Internal of internal_msg
  | External of external_msg

let update (msg : internal_msg) (state : t) : t =
  match msg with
  | Inc -> { value = state.value + 1 }
;;

let view (pos : int) (state : t) : meta_msg H.t =
  H.span
    []
    [ H.span
        [ A.on_click (Internal Inc); A.style "user-select" "none" ]
        [ H.text @@ sprintf "Counter[%d]=%d" pos state.value ]
    ; H.button
        [ A.on_click (External Delete); A.style "margin-left" "10px" ]
        [ H.text "Delete" ]
    ]
;;
