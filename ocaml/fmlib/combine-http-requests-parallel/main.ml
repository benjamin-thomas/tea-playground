(* [@@@warning "-69-37-34-32-27"] *)

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
[@@warning "-69"]

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

type post =
  { id : int
  ; user_id : int
  ; title : string
  ; body : string
  }
[@@warning "-69"]

let posts_decoder =
  D.array
    D.(
      let* id = D.field "id" D.int in
      let* user_id = D.field "userId" D.int in
      let* title = D.field "title" D.string in
      let* body = D.field "body" D.string in
      D.return { id; user_id; title; body })
;;

type user_with_posts =
  { user : user
  ; posts : post list
  }

type msg =
  | Clicked_load_data
  | Got_users of (user list, Task.http_error) result
  | Got_posts of (user_with_posts, Task.http_error) result

type model =
  | Not_asked
  | Failed
  | Loading_users
  | Loading_user_posts of
      { expected_count : int
      ; acc : user_with_posts list
      }
  | Loaded of user_with_posts list

let init = Not_asked

let fetch_users_task : (user array, Task.http_error) Task.t =
  get { url = root_url ^ "/users"; headers = []; body = "" } users_decoder
;;

let fetch_users_cmd =
  fetch_users_task
  |> Cmd.attempt (function
    | Error err -> Got_users (Error err)
    | Ok users -> Got_users (Ok (Array.to_list users)))
;;

let simulate_failure = true

let fetch_posts_task (user : user) =
  let p =
    if user.id = 7 && simulate_failure then
      { url = sprintf "%s/posts_FAIL_ME=%d" root_url user.id; headers = []; body = "" }
    else
      { url = sprintf "%s/posts?userId=%d" root_url user.id; headers = []; body = "" }
  in
  get p posts_decoder
;;

let fetch_posts_cmd (users : user list) =
  let all_cmds =
    List.map
      (fun (user : user) ->
        fetch_posts_task user
        |> Cmd.attempt (function
          | Error err -> Got_posts (Error err)
          | Ok data -> Got_posts (Ok { user; posts = Array.to_list data })))
      users
  in
  Cmd.batch all_cmds
;;

let update (model : model) = function
  | Clicked_load_data -> (Loading_users, fetch_users_cmd)
  | Got_users res ->
    let next = function
      | Error _ -> (Failed, Cmd.none)
      | Ok users ->
        ( Loading_user_posts { expected_count = List.length users; acc = [] }
        , fetch_posts_cmd users )
    in
    next res
  | Got_posts res ->
    let next_model = function
      | Error _ -> Failed
      | Ok data ->
        let next_model = function
          | Loading_user_posts { expected_count; acc } ->
            let acc = data :: acc in
            if List.length acc = expected_count then
              Loaded acc
            else
              Loading_user_posts { expected_count; acc }
          | _ -> model
        in
        next_model model
    in
    (next_model res, Cmd.none)
;;

let view_loaded data =
  H.div
    []
    [ H.h2 [] [ H.text "Loaded data!" ]
    ; H.ul
        []
        (data
         |> List.map (fun { user; posts } ->
           H.li
             []
             [ H.text user.name
             ; H.ul
                 []
                 (posts
                  |> List.mapi (fun i post ->
                    H.li [] [ H.text @@ sprintf "Post %02d => %s" (i + 1) post.title ]))
             ]))
    ]
;;

let view = function
  | Not_asked ->
    H.div
      []
      [ H.text "Make a request!"
      ; H.button [ A.on_click Clicked_load_data ] [ H.text "Load data" ]
      ]
  | Failed -> H.div [] [ H.text "Failed!" ]
  | Loading_users -> H.div [] [ H.text "Loading..." ]
  | Loading_user_posts _ -> H.div [] [ H.text "Loading still..." ]
  | Loaded data -> view_loaded data
;;

let () =
  Fmlib_browser.basic_application
    init
    Cmd.none
    (fun m -> (view m, "not sure what this is for"))
    (fun _model -> Sub.none)
    update
;;
