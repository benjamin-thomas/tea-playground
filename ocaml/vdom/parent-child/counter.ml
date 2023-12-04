module V = Vdom
open Printf

type t = { value : int }

let init : t list = [ { value = 0 }; { value = 0 }; { value = 0 } ]

type internal_msg = Inc
type external_msg = Delete

type meta_msg =
  | Internal of internal_msg
  | External of external_msg

let update model = function
  | Inc -> { value = model.value + 1 }
;;

let span = V.elt "span"

let btn txt msg =
  V.elt "button" [ V.text txt ] ~a:[ V.onclick (fun _ -> msg); V.type_button ]
;;

let delete_btn = btn "Delete" (External Delete)

let view (pos : int) (model : t) : meta_msg V.vdom =
  let prevent_dbl_click_selecting_txt = V.style "user-select" "none" in
  let view_counter =
    span
      ~a:[ V.onclick (fun _ -> Internal Inc) ]
      [ V.text @@ sprintf "Counter[%d]=%d" pos model.value ]
  in
  span ~a:[ prevent_dbl_click_selecting_txt ] [ view_counter; delete_btn ]
;;
