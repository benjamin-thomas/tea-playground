(*
   To enable/enforce functional purity, you mustn't expose ANY impure functions here.

   In other words, make sure nothing having units leaks out.

   The only exception to this rule is the start function, which *is* impure.

   So it *can* and *should* expose/leak out unit.
*)

type 'action cmd

val cmd_none : 'action cmd
val cmd_batch : 'action cmd list -> 'action cmd
val run_cmd : ('a -> 'b -> 'a * 'c) -> 'a -> 'b cmd -> 'a

type tea_time = Tea_time of float

val to_tea_time : float -> tea_time
val show_tea_time : tea_time -> string

(* USER AVAILABLE COMMANDS *)
val request_time : (tea_time -> 'action) -> 'action cmd
val request_random_number : (int -> 'action) -> 'action cmd

type console_ui =
  | Line of string
  | BlankLine
  | Section of console_ui list
  | Divider of char * int

type ('model, 'action) program =
  { init : 'model * 'action cmd
  ; update : 'model -> 'action -> 'model * 'action cmd
  ; view : 'model -> console_ui
  ; input_to_action : char -> 'action option
  }

val start : ('a, 'b) program -> unit
