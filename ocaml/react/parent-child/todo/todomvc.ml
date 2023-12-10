(* [@@@warning "-37-32-27"] *)

module Js = Js_of_ocaml.Js
module Url = Js_of_ocaml.Url
module Dom = Js_of_ocaml.Dom
module Dom_html = Js_of_ocaml.Dom_html
module Html5 = Js_of_ocaml_tyxml.Tyxml_js.Html5
module To_dom = Js_of_ocaml_tyxml.Tyxml_js.To_dom
module R = Js_of_ocaml_tyxml.Tyxml_js.R
module Lwt_js_events = Js_of_ocaml_lwt.Lwt_js_events

(* Application data *)
module Model = struct
  type t = { field : string } [@@deriving json]
  (* to save/restore the state in JSON *)

  let empty = { field = "" }
end
[@@warning "-39"]

let task_input =
  Html5.(
    input
      ~a:
        [ a_input_type `Text
        ; a_class [ "new-todo" ]
        ; a_placeholder "What needs to be done?"
        ; a_autofocus ()
        ]
      ())
;;

let main _ =
  let (model, set_model) = React.S.create @@ Model.empty in
  let () =
    Lwt_js_events.(
      async
      @@ fun () ->
      Lwt_js_events.inputs (To_dom.of_input task_input) (fun _ _ ->
        Lwt.return
        @@ set_model Model.{ field = Js.to_string (To_dom.of_input task_input)##.value }))
  in
  let doc = Dom_html.document in
  let parent =
    Js.Opt.get (doc##getElementById (Js.string "todomvc")) @@ fun () -> assert false
  in
  ()
  ; Dom.appendChild
      parent
      (Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_div
         Html5.(
           div
             ~a:[ a_class [ "todomvc-wrapper" ] ]
             [ section
                 ~a:[ a_class [ "todoapp" ] ]
                 [ Html5.(
                     header ~a:[ a_class [ "header" ] ] [ h1 [ txt "todos" ]; task_input ])
                 ]
             ; p [ R.Html.txt @@ React.S.map (fun x -> Model.(x.field)) model ]
             ]))
  ; Lwt.return ()
;;

let _ = main @@ Js_of_ocaml_lwt.Lwt_js_events.onload ()
