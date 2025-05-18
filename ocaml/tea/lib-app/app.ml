open Tea

(* MODEL *)

type model =
  { count : int
  ; last_random_value : int option
  ; message : string option
  }

let init = ({ count = 0; last_random_value = None; message = None }, cmd_none)

(* MESSAGE *)

type user_input =
  | Inc_pressed
  | Dec_pressed
  | Reset_pressed
  | Request_random_pressed
  | Request_time_pressed
  | Request_random_and_time_pressed
  | Clear_screen_pressed

type action =
  | User_input_action of user_input
  | Got_random_number of int
  | Got_time of tea_time

(* UPDATE *)

let get_random_number = request_random_number (fun n -> Got_random_number n)
let get_time = request_time (fun t -> Got_time t)

let handle_user_input model = function
  | Inc_pressed -> ({ model with count = model.count + 1 }, cmd_none)
  | Dec_pressed -> ({ model with count = model.count - 1 }, cmd_none)
  | Reset_pressed ->
    ({ model with count = 0; message = Some "Counter reset to zero" }, cmd_none)
  | Request_random_pressed ->
    ({ model with message = Some "Generating random number..." }, get_random_number)
  | Request_time_pressed ->
    ({ model with message = Some "Getting current time..." }, get_time)
  | Request_random_and_time_pressed ->
    ( { model with message = Some "Generating random number and getting current time..." }
    , cmd_batch [ get_random_number; get_time ] )
  | Clear_screen_pressed -> ({ model with message = None }, cmd_none)
;;

let update model = function
  | User_input_action user_input -> handle_user_input model user_input
  | Got_random_number n ->
    ( { model with
        last_random_value = Some n
      ; message = Some ("Random number generated: " ^ string_of_int n)
      }
    , cmd_none )
  | Got_time time ->
    ({ model with message = Some ("Current time: " ^ show_tea_time time) }, cmd_none)
;;

(* VIEW *)

let view model =
  Section
    [ Divider ('-', 50)
    ; Line ("Current count: " ^ string_of_int model.count)
    ; (match model.last_random_value with
       | Some n -> Line ("Last random value: " ^ string_of_int n)
       | None -> Line "No random value generated yet")
    ; (match model.message with
       | Some msg -> Line ("Message: " ^ msg)
       | None -> BlankLine)
    ; BlankLine
    ; Line "Commands:"
    ; Line "  i - Increment"
    ; Line "  d - Decrement"
    ; Line "  r - Reset"
    ; Line "  g - Generate random number"
    ; Line "  t - Get current time"
    ; Line "  b - Get a random number AND get the current time (both)"
    ; Line "  c - Clear message"
    ; Line "  q - Quit"
    ; Divider ('-', 50)
    ]
;;

(* SUBSCRIPTIONS (interpret user input) *)

let input_to_action = function
  | 'i' -> Some (User_input_action Inc_pressed)
  | 'd' -> Some (User_input_action Dec_pressed)
  | 'r' -> Some (User_input_action Reset_pressed)
  | 'g' -> Some (User_input_action Request_random_pressed)
  | 't' -> Some (User_input_action Request_time_pressed)
  | 'b' -> Some (User_input_action Request_random_and_time_pressed)
  | 'c' -> Some (User_input_action Clear_screen_pressed)
  | _ -> None
;;

let program : (model, action) program = { init; update; view; input_to_action }
