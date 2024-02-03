[@@@warning "-69"]

module H = Fmlib_browser.Html
module A = Fmlib_browser.Attribute
module Cmd = Fmlib_browser.Command
module Sub = Fmlib_browser.Subscription
module D = Fmlib_browser.Decoder
module Task = Fmlib_browser.Task
open Printf

(* opam pin -v fmlib_browser https://github.com/hbr/fmlib.git#d7cfa51c9dab6f8269dffec5a49bdd228e22c6f2 *)

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

type user =
  { id : int
  ; name : string
  ; email : string
  }

let user_decoder : user D.t =
  let ( let* ) = D.( >>= ) in
  let* id = D.field "id" D.int in
  let* name = D.field "name" D.string in
  let* email = D.field "email" D.string in
  D.return { id; name; email }
;;

let root_url = "https://jsonplaceholder.typicode.com"

type http_params =
  { url : string
  ; headers : (string * string) list
  ; body : string
  }

let get { url; headers; body } = Task.http_json "GET" url headers body
let simulate_failure = true

let fetch_user (user_id : int) : (user, Task.http_error) Task.t =
  let url =
    if simulate_failure && user_id = 3 then
      sprintf "%s/users_MAKE_ME_FAIL/%d" root_url user_id
    else
      sprintf "%s/users/%d" root_url user_id
  in
  get { url; headers = []; body = "" } user_decoder
;;

let fetch_in_parallel =
  let tasks : (user, Task.http_error) Task.t list =
    [ fetch_user 1; fetch_user 2; fetch_user 3; fetch_user 4; fetch_user 5 ]
  in
  (* We move the possible error state on the left *)
  let tasks : ((user, Task.http_error) result, Task.empty) Task.t list =
    List.map (Task.make_succeed Fun.id) tasks
  in
  let accumulate (result : (user, Task.http_error) result) acc =
    match (result, acc) with
    | (Ok user, Ok acc) -> Ok (user :: acc)
    | _ -> Error "Failed loading all users"
  in
  Task.parallel (Ok []) accumulate tasks
;;

(** sequence [ Task.return 1; Task.return 2 ] = Task.return [1; 2] *)
let sequence lst =
  let ( let* ) = Task.( >>= ) in
  List.fold_left
    (fun acc item ->
      let* acc = acc in
      let* item = item in
      Task.return (item :: acc))
    (Task.return [])
    lst
;;

let fetch_in_sequence =
  sequence [ fetch_user 1; fetch_user 2; fetch_user 3; fetch_user 4; fetch_user 5 ]
;;

type msg =
  | Clicked_load_in_sequence
  | Got_sequential_data of (user list, Task.http_error) result
  | Clicked_load_in_parallel
  | Got_parallel_data of (user list, string) result
  | Clicked_start_again

let fetch_data =
  fetch_in_sequence
  |> Cmd.attempt (function
    | Error err -> Got_sequential_data (Error err)
    | Ok data -> Got_sequential_data (Ok data))
;;

let fetch_data2 =
  fetch_in_parallel
  |> Task.map (function
    | Error err -> Got_parallel_data (Error err)
    | Ok x -> Got_parallel_data (Ok x))
  |> Cmd.perform
;;

type model =
  | Start
  | Loading
  | Failed
  | Loaded_in_sequence of user list
  | Loaded_in_parallel of user list

let init = Start

let update _model = function
  | Clicked_load_in_sequence -> (Loading, fetch_data)
  | Got_sequential_data res ->
    (match res with
     | Error _ -> (Failed, Cmd.none)
     | Ok lst -> (Loaded_in_sequence lst, Cmd.none))
  | Clicked_load_in_parallel -> (Loading, fetch_data2)
  | Got_parallel_data result ->
    (match result with
     | Error _ -> (Failed, Cmd.none)
     | Ok users -> (Loaded_in_parallel users, Cmd.none))
  | Clicked_start_again -> (Start, Cmd.none)
;;

let view_users title users =
  let to_li (user : user) =
    H.li [] [ H.text @@ sprintf "%-20s (id=%d)" user.name user.id ]
  in
  H.div
    []
    [ H.h2 [] [ H.text title ]
    ; H.ol [ A.style "white-space" "pre"; A.style "font-family" "monospace" ]
      @@ List.map to_li users
    ; H.div [] [ H.button [ A.on_click Clicked_start_again ] [ H.text "Start again?" ] ]
    ]
;;

let view = function
  | Start ->
    H.div
      []
      [ H.p [] [ H.text "Make a request!" ]
      ; H.div
          [ A.style "display" "flex"; A.style "gap" "5px" ]
          [ H.button
              [ A.on_click Clicked_load_in_sequence ]
              [ H.text "Load data (in sequence)" ]
          ; H.button
              [ A.on_click Clicked_load_in_parallel ]
              [ H.text "Load data (in parallel)" ]
          ]
      ]
  | Loading -> H.div [] [ H.text "Loading..." ]
  | Failed -> H.div [] [ H.text "Sorry, we couldn't load the users :(" ]
  | Loaded_in_sequence users -> view_users "Got users (in sequence)" users
  | Loaded_in_parallel users -> view_users "Got users (in parallel)" users
;;

let () =
  Fmlib_browser.basic_application
    init
    Cmd.none
    (fun m -> (view m, "not sure what this is for"))
    (fun _model -> Sub.none)
    update
;;
