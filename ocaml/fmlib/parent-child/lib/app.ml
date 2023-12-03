module H = Fmlib_browser.Html
module A = Fmlib_browser.Attribute

type state =
  { first_name : string; last_name : string; counters : Counter.t list }

let init : state =
  { first_name = "John"; last_name = "Doe"; counters = Counter.init }
;;

type msg =
  | FirstNameChanged of string
  | LastNameChanged of string
  | GotCounterMsg of (int * Counter.meta_msg)

let update (state : state) (msg : msg) : state =
  match msg with
  | FirstNameChanged str -> { state with first_name = str }
  | LastNameChanged str -> { state with last_name = str }
  | GotCounterMsg (pos1, Counter.Internal sub_msg) ->
      let update_counter pos2 counter =
        if pos1 = pos2 then
          Counter.update sub_msg counter
        else
          counter
      in
      let new_counters = List.mapi update_counter state.counters in
      { state with counters = new_counters }
  | GotCounterMsg (pos1, Counter.External sub_msg) -> (
      match sub_msg with
      | Counter.Delete ->
          let new_counters =
            state.counters
            |> List.mapi (fun i counter -> (i, counter))
            |> List.filter (fun (pos2, _) -> pos1 <> pos2)
            |> List.map (fun (_, counter) -> counter)
          in

          { state with counters = new_counters })
;;

let view (state : state) : msg H.t =
  let view_counter (pos : int) (counter : Counter.t) : msg H.t =
    H.map
      (fun msg -> GotCounterMsg (pos, msg))
      (H.li [] [ Counter.view pos counter ])
  in
  H.div []
    [ H.h1 [] [ H.text "Parent/child communication example" ]
    ; H.ul [] (List.mapi view_counter state.counters)
    ]
;;

let start () = Fmlib_browser.sandbox init view update
