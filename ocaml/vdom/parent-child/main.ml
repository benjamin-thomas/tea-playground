[@@@warning "-69-37-34-32"]

module V = Vdom
open Printf

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

type model =
  { first_name : string
  ; last_name : string
  ; counters : Counter.t list
  }

type position = Position of int

type msg =
  | First_name_changed of string
  | Last_name_changed of string
  | Got_counter_msg of (position * Counter.meta_msg)

let init : model = { first_name = "John"; last_name = "Doe"; counters = Counter.init }

let update_internal (Position pos) model msg =
  let update_counter pos' counter =
    if pos = pos' then
      Counter.update counter msg
    else
      counter
  in
  let new_counters = List.mapi update_counter model.counters in
  { model with counters = new_counters }
;;

let update_external (Position pos) model = function
  | Counter.Delete ->
    let new_counters =
      model.counters
      |> List.mapi (fun i counter -> (i, counter))
      |> List.filter (fun (pos', _) -> pos <> pos')
      |> List.map (fun (_, counter) -> counter)
    in
    { model with counters = new_counters }
;;

let update_counters pos model = function
  | Counter.Internal msg -> update_internal pos model msg
  | Counter.External msg -> update_external pos model msg
;;

let update model = function
  | First_name_changed str -> { model with first_name = str }
  | Last_name_changed str -> { model with last_name = str }
  | Got_counter_msg (pos, msg') -> update_counters pos model msg'
;;

let ul = V.elt "ul"
let li = V.elt "li"
let h1 = V.elt "h1"

let view_counter (pos : int) (counter : Counter.t) : msg V.vdom =
  V.map (fun msg -> Got_counter_msg (Position pos, msg)) (li [ Counter.view pos counter ])
;;

let input value onchange = V.input ~a:[ V.value value; V.onchange onchange ] []

let string_of_model model =
  sprintf
    {|first_name=%s, last_name=%s, counters_length=%d|}
    model.first_name
    model.last_name
    (List.length model.counters)
;;

let view model =
  let () =
    ()
    ; printf "model: %s%!" (string_of_model model)
  in
  V.div
    [ h1 [ V.text "Parent/child communication example" ]
    ; input model.first_name (fun str -> First_name_changed str)
    ; input model.last_name (fun str -> Last_name_changed str)
    ; ul @@ List.mapi view_counter model.counters
    ]
;;

let make_app = V.simple_app ~init ~view ~update

let run () =
  Vdom_blit.run (make_app ())
  |> Vdom_blit.dom
  |> Js_browser.(Element.append_child (Document.body document))
;;

let () = Js_browser.(Window.set_onload window run)
