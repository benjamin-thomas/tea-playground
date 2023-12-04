(* [@@@warning "-32"] *)

module H = Fmlib_browser.Html
module A = Fmlib_browser.Attribute

(*
   terminal 1: dune build -w
   terminal 2: ./manage/dev/live-server (or simply load the HTML from the filesystem and reload manually)

   Dev
   dune build
   dune build ./main.js

   Prod
   dune build --profile release
   dune build --profile release ./main.js
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

let value_of_model model =
  Fmlib_browser.Value.(record [| ("curr", int model); ("other", string "value") |])
;;

let view (model : int) =
  let () =
    ()
    ; Fmlib_browser.debug "view was rendered!"
    ; Fmlib_browser.(debug_value @@ value_of_model model)
  in
  H.div
    []
    [ H.h1 [] [ H.text "An Fmlib example" ]
    ; H.h2 [] [ H.text "An h2 title" ]
    ; H.h3 [] [ H.text "An h3 title" ]
    ; H.p [] [ H.text "A paragraph..." ]
    ; H.div [] [ view_counter model ]
    ]
;;

let () =
  (* sandbox takes over the whole body *)
  Fmlib_browser.sandbox init view update
;;
