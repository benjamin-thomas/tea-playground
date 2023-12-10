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

(*
   module Dom = Js_of_ocaml.Dom
module Dom_html = Js_of_ocaml.Dom_html
module R = Js_of_ocaml_tyxml.Tyxml_js.R
module Html = Js_of_ocaml_tyxml.Tyxml_js.Html
module To_dom = Js_of_ocaml_tyxml.Tyxml_js.To_dom
open Printf

let (counter, set_counter) = React.S.create 0

let r_input =
  let defaults = [ "width:30px"; "text-align:center" ] in
  let bg_towards_red c =
    let bg n =
      let n = max 0 n in
      let bg_style = sprintf "background-color: rgb(255, %d, %d)" n n in
      String.concat ";" (bg_style :: defaults)
    in
    bg (255 - (c * 10))
  in
  R.Html.(
    input
      ~a:
        [ a_style @@ React.S.map bg_towards_red counter
        ; a_disabled ()
        ; a_value @@ React.S.map string_of_int counter
        ])
    ()
;;

let (dec, inc) =
  let add n _evt =
    let curr = React.S.value counter in
    ()
    ; set_counter (max 0 @@ (curr + n))
    ; false
  in
  (add (-1), add 1)
;;

let btn txt' f = Html.(button ~a:[ a_onclick f ] [ txt txt' ])

let r_view =
  Html.(div [ h1 [ txt "Counter example" ]; div [ btn "-" dec; r_input; btn "+" inc ] ])
;;

let append_to_body elt = Dom.appendChild Dom_html.document##.body (To_dom.of_element elt)
let () = append_to_body r_view
*)

module El = Brr.El
module Document = Brr.Document
module G = Brr.G
module Console = Brr.Console
module Ev = Brr.Ev

(* ignore (Ev.listen Ev.click onclick (El.as_target but)); but *)

let make_btn txt f =
  let x = El.button [ El.txt @@ Jstr.v txt ] in
  let _listener = Ev.listen Ev.click f (El.as_target x) in
  x
;;

let btn =
  let x = El.button [ El.txt @@ Jstr.v "+" ] in
  let f _evt = Console.log [ "clicked" ] in
  let _listener = Ev.listen Ev.click f (El.as_target x) in
  x
;;

(* let (counter, set_counter) = Note.S.create *)
let btn2 = make_btn "++" (fun _evt -> Console.log [ "wish" ])

let () =
  let init_tag = Jstr.v "INIT" in
  ()
  ; Console.time init_tag
  ; Console.log [ "Stuff is happening..." ]
  ; Console.warn [ "A warning..." ]
  ; Console.info [ "An info message..." ]
  ; Console.time_log init_tag (Console.msg "Doing stuff")
  ; Console.time_log init_tag [ "Doing more stuff" ]
  ; Console.error [ "An error..." ]
  ; Console.debug [ "Debugging..." ]
    (* ; Console.trace [ "yo" ] *)
    (* ; Console.table (Jv.of_jstr_array [| Jstr.v "ABC"; Jstr.v "BCD"; Jstr.v "CDE" |]) *)
    (* ; Console.table (Jv.of_jv_list [ Jv.of_int 1; Jv.of_float 1.5; Jv.of_bool true ]) *)
  ; Console.time_end init_tag
  ; El.set_children
      (Document.body G.document)
      El.
        [ txt' "Hello, World!"
        ; br ()
        ; br ()
        ; br ()
        ; br ()
        ; br ()
        ; br ()
        ; br ()
        ; btn
        ; br ()
        ; btn2
        ]
;;
