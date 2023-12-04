(* [@@@warning "-69-37-34-32-27"] *)
[@@@warning "-69"]

module H = Fmlib_browser.Html
module A = Fmlib_browser.Attribute
module Cmd = Fmlib_browser.Command
module Sub = Fmlib_browser.Subscription
module D = Fmlib_browser.Decoder
module Task = Fmlib_browser.Task
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

let users_decoder = D.array user_decoder
let root_url = "https://jsonplaceholder.typicode.com"

type http_params =
  { url : string
  ; headers : (string * string) list
  ; body : string
  }

let get { url; headers; body } = Task.http_json "GET" url headers body

let fetch_users : (user array, Task.http_error) Task.t =
  get { url = root_url ^ "/users"; headers = []; body = "" } users_decoder
;;

type post =
  { id : int
  ; user_id : int
  ; title : string
  ; body : string
  }

let posts_decoder =
  D.array
    D.(
      let* id = D.field "id" D.int in
      let* user_id = D.field "userId" D.int in
      let* title = D.field "title" D.string in
      let* body = D.field "body" D.string in
      D.return { id; user_id; title; body })
;;

let fetch_posts (user : user) =
  get
    { url = sprintf "%s/posts?userId=%d" root_url user.id; headers = []; body = "" }
    posts_decoder
;;

type user_with_posts =
  { user : user
  ; posts : post array
  }

let fetch_user_with_posts (user : user) =
  fetch_posts user |> Task.map (fun posts -> { user; posts })
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

let fetch_users_with_posts users = sequence (List.map fetch_user_with_posts users)

let fetch_seq =
  let ( let* ) = Task.( >>= ) in
  let* users = fetch_users in
  fetch_users_with_posts (Array.to_list users)
;;

type msg =
  | Clicked_load_data
  | Got_data of (user_with_posts list, Task.http_error) result

let fetch_data =
  fetch_seq
  |> Cmd.attempt (function
    | Error err -> Got_data (Error err)
    | Ok data -> Got_data (Ok data))
;;

type model =
  | Not_asked
  | Loading
  | Failed
  | Loaded of user_with_posts list

let init = Not_asked

let update _model = function
  | Clicked_load_data -> (Loading, fetch_data)
  | Got_data res ->
    (match res with
     | Error _ -> (Failed, Cmd.none)
     | Ok lst -> (Loaded lst, Cmd.none))
;;

let view = function
  | Not_asked ->
    H.div
      []
      [ H.text "Make a request!"
      ; H.button [ A.on_click Clicked_load_data ] [ H.text "Load data" ]
      ]
  | Loading -> H.div [] [ H.text "loading..." ]
  | Failed -> H.div [] [ H.text "Failed!" ]
  | Loaded users_with_posts ->
    let to_li data =
      H.li [] [ H.text @@ sprintf "%s (%d)" data.user.name (Array.length data.posts) ]
    in
    H.div
      []
      [ H.h2 [] [ H.text "Got users with posts!" ]
      ; H.ul [] @@ List.map to_li users_with_posts
      ]
;;

let () =
  Fmlib_browser.basic_application
    init
    Cmd.none
    (fun m -> (view m, "not sure what this is for"))
    (fun _model -> Sub.none)
    update
;;
