[@@@warning "-32"]

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

module Dom = Js_of_ocaml.Dom
module Dom_html = Js_of_ocaml.Dom_html
module R = Js_of_ocaml_tyxml.Tyxml_js.R
module Html = Js_of_ocaml_tyxml.Tyxml_js.Html
module To_dom = Js_of_ocaml_tyxml.Tyxml_js.To_dom
open Printf

(*
 * SETUP
 *)

let (counter, set_counter) = React.S.create 0
let (first_name, set_first_name) = React.S.create "abc"

(*
 * REACTIVE VIEW
 *)

let r_input =
  let defaults = [ "width:30px"; "text-align:center" ] in
  (* The input's background turns increasingly red, as the button is clicked. *)
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

let r_first_name =
  let on_input () _evt =
    let v = React.S.value first_name in
    ()
    ; printf "Setting value:%s%!" v
    ; set_first_name v
    ; true
  in
  R.Html.(
    R.Html.input ~a:[ a_value @@ React.S.map Fun.id first_name; a_oninput @@ on_input () ])
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

let debug_counter : [> Html_types.txt ] Html.elt =
  R.Html.txt @@ React.S.map string_of_int counter
;;

let debug_counter2 = Html.h4 [ debug_counter ]

let r_view =
  Html.(
    div
      [ h1 [ txt "Parent/child communication" ]
      ; div [ btn "-" dec; r_input; btn "+" inc ]
      ; r_first_name
      ; Html.br ()
      ; R.Html.txt @@ React.S.map string_of_int counter
      ; Html.br ()
      ; R.Html.txt @@ React.S.map string_of_int counter
      ; Html.br ()
      ; R.Html.txt @@ React.S.map (fun (x : string) -> x) first_name
        (* ; h3 [ R.Html.txt (React.S.map string_of_int counter) ]
           ; h4 [ debug_counter ]
           ; h2 [ txt "Form example..." ]
           ; debug_counter2
           ;
           ; h3 [ txt "Extracted..." ] *)
      ])
;;

(*
   * BOOTSTRAP
*)

(* let () = set_first_name "wat" *)
let append_to_body elt = Dom.appendChild Dom_html.document##.body (To_dom.of_element elt)
let () = append_to_body r_view
