(* [@@@warning "-32"] *)

module V = Vdom

(*
   terminal 1: dune build -w
   terminal 2: live-server --no-browser --ignore=*.ml,dist/*.bc-for-jsoo .
   terminal 2 (alt): simply load the HTML from the filesystem and reload manually.
*)

type msg =
  | Inc
  | Dec
  | Reset

let init = 0

let update model : msg -> int = function
  | Inc -> model + 1
  | Dec -> max 0 (model - 1)
  | Reset -> 0
;;

let h2 = V.elt "h2"
let h3 = V.elt "h3"
let p = V.elt "p"
let btn1 txt msg = V.input [] ~a:[ V.onclick (fun _ -> msg); V.type_button; V.value txt ]

let btn2 txt msg =
  V.elt "button" [ V.text txt ] ~a:[ V.onclick (fun _ -> msg); V.type_button ]
;;

let view_counter model ~btn =
  [ btn "-" Dec; V.text (string_of_int model); btn "+" Inc; btn "RESET" Reset ]
;;

let view model : msg V.vdom =
  V.div
    [ V.div
        [ h2 [ V.text "An h2 title" ]
        ; h3 [ V.text "An h3 title" ]
        ; p [ V.text "This first counter uses an input type button." ]
        ; p [ V.text "This second counter uses a button element." ]
        ]
    ; V.div (view_counter model ~btn:btn1)
    ; V.div (view_counter model ~btn:btn2)
    ]
;;

let make_app = V.simple_app ~init ~view ~update

let run () =
  Vdom_blit.run (make_app ())
  |> Vdom_blit.dom
  |> Js_browser.(Element.append_child (Document.body document))
;;

let () = Js_browser.(Window.set_onload window run)
