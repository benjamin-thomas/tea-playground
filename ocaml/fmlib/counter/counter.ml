(* [@@@warning "-32"] *)

module H = Fmlib_browser.Html
module A = Fmlib_browser.Attribute

(*
   terminal 1: dune build -w
   terminal 2: ./manage/dev/live-server
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

let view_counter model =
  let btn txt msg = H.button [ A.on_click msg ] [ H.text txt ] in
  H.span
    []
    [ btn "-" Dec
    ; btn "+" Inc
    ; btn "reset" Reset
    ; H.span [] [ H.text (string_of_int model) ]
    ]
;;

let view model =
  H.div
    []
    [ H.h2 [] [ H.text "An h2 title" ]
    ; H.h3 [] [ H.text "An h3 title" ]
    ; H.p [] [ H.text "A paragraph..." ]
    ; H.div [] [ view_counter model ]
    ]
;;

let () =
  (* sandbox takes over the whole body *)
  Fmlib_browser.sandbox init view update
;;
