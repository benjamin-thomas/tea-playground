(*
   Cmd represents impure operations that will be executed by a runtime.

   You "command" the runtime to execute those impure actions.
*)
type 'action cmd = Cmd of (unit -> 'action) list

let cmd_none : 'action cmd = Cmd []

let cmd_batch (cmds : 'action cmd list) : 'action cmd =
  Cmd (List.concat_map (fun (Cmd actions) -> actions) cmds)
;;

let run_cmd update_f model (Cmd actions) =
  List.fold_left
    (fun m a -> fst (update_f m a))
    model
    (List.map (fun action -> action ()) actions)
;;

(* USER AVAILABLE COMMANDS *)

type tea_time = Tea_time of float

let to_tea_time time = Tea_time time

let show_tea_time (Tea_time time) =
  let tm = Unix.localtime time in
  let msec = int_of_float (1000.0 *. (time -. floor time)) in
  Printf.sprintf
    "%04d-%02d-%02d %02d:%02d:%02d.%03d"
    (tm.tm_year + 1900)
    (tm.tm_mon + 1)
    tm.tm_mday
    tm.tm_hour
    tm.tm_min
    tm.tm_sec
    msec
;;

let request_time to_action =
  Cmd [ (fun () -> to_action (to_tea_time (Unix.gettimeofday ()))) ]
;;

let request_random_number to_action = Cmd [ (fun () -> to_action (Random.int 100 + 1)) ]

(* RENDERING *)

type console_ui =
  | Line of string
  | BlankLine
  | Section of console_ui list
  | Divider of char * int

let clear_screen () = print_string "\027[2J\027[H"

let rec render_console_ui ui =
  ()
  ; clear_screen ()
  ; _render ui

and _render = function
  | Line str -> print_endline str
  | BlankLine -> print_endline ""
  | Section uis -> List.iter _render uis
  | Divider (char, n) -> print_endline (String.make n char)
;;

(* RUNTIME *)

type ('model, 'action) program =
  { init : 'model * 'action cmd
  ; update : 'model -> 'action -> 'model * 'action cmd
  ; view : 'model -> console_ui
  ; input_to_action : char -> 'action option
  }

let setup_console () = ignore @@ Sys.command "stty cbreak -echo"
let restore_console () = ignore @@ Sys.command "stty -cbreak echo"

let quit () =
  ()
  ; restore_console ()
  ; print_endline "\nGoodbye!"
;;

let rec run_loop program model =
  let view_f = program.view in
  let input_to_action_f = program.input_to_action in
  let console_ui = view_f model in
  render_console_ui console_ui
  ; ()
  ; let input = input_char stdin in
    if input = 'q' then
      quit ()
    else (
      match input_to_action_f input with
      | None -> run_loop program model
      | Some action ->
        let (new_model, new_cmd) = program.update model action in
        let final_model = run_cmd program.update new_model new_cmd in
        run_loop program final_model
    )
;;

let start program =
  let (init_model, init_cmd) = program.init in
  let new_model = run_cmd program.update init_model init_cmd in
  ()
  ; setup_console ()
  ; try run_loop program new_model with
    | e ->
      ()
      ; restore_console ()
      ; raise e
;;
