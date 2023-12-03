(* [@@@warning "-69-37-34-32"] *)

module H = Fmlib_browser.Html
module A = Fmlib_browser.Attribute

(*
   terminal 1: dune build -w
   terminal 2: ./manage/dev/live-server (or simply load the HTML from the filesystem and reload manually)

   Dev
   dune build
   dune build ./counter.js

   Prod
   dune build --profile release
   dune build --profile release ./counter.js
*)

type model =
  { first_name : string
  ; last_name : string
  ; counters : Counter.t list
  }

let init : model = { first_name = "John"; last_name = "Doe"; counters = Counter.init }

type position = Position of int

type msg =
  | First_name_changed of string
  | Last_name_changed of string
  | Got_counter_msg of (position * Counter.meta_msg)

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

let update_internal (Position pos) model msg =
  let update_counter pos' counter =
    if pos = pos' then
      Counter.update msg counter
    else
      counter
  in
  let new_counters = List.mapi update_counter model.counters in
  { model with counters = new_counters }
;;

let update_counters pos model = function
  | Counter.Internal msg -> update_internal pos model msg
  | Counter.External msg -> update_external pos model msg
;;

let update model : msg -> model = function
  | First_name_changed str -> { model with first_name = str }
  | Last_name_changed str -> { model with last_name = str }
  | Got_counter_msg (pos, msg') -> update_counters pos model msg'
;;

let value_of_model (model : model) =
  Fmlib_browser.Value.(
    record
      [| ("first_name", string model.first_name)
       ; ("last_name", string model.last_name)
       ; ("counters_length", int (List.length model.counters))
      |])
;;

let view_counter (pos : int) (counter : Counter.t) : msg H.t =
  H.map
    (fun msg -> Got_counter_msg (Position pos, msg))
    (H.li [] [ Counter.view pos counter ])
;;

let view (model : model) : msg H.t =
  let () =
    ()
    ; Fmlib_browser.debug "view was rendered!"
    ; Fmlib_browser.(debug_value @@ value_of_model model)
  in
  let input value msg = H.input [ A.value value; A.on_input msg ] [] in
  H.div
    []
    [ H.h1 [] [ H.text "OCaml/Fmlib: parent/child communication example" ]
    ; input model.first_name (fun str -> First_name_changed str)
    ; input model.last_name (fun str -> Last_name_changed str)
    ; H.ul [] (List.mapi view_counter model.counters)
    ]
;;

let () =
  (* sandbox takes over the whole body *)
  Fmlib_browser.sandbox init view update
;;
