module Main

open Feliz

type Model = {
    first_name: string
    last_name: string
    counters: Counter.Model list
}


type Msg =
    | FirstNameChanged of string
    | LastNameChanged of string
    | CounterMsg of (int * Counter.Msg)
    | Delete of int

let init _ : Model = {
    first_name = "John"
    last_name = "Doe"
    counters = [ Counter.init; Counter.init; Counter.init ]
}


let update (msg: Msg) (model: Model) : Model =
    match msg with
    | FirstNameChanged str -> { model with first_name = str }
    | LastNameChanged str -> { model with last_name = str }
    | CounterMsg(pos, msg') ->
        let updateCounter pos2 counter =
            if pos = pos2 then Counter.update msg' counter else counter

        let newCounters = List.mapi updateCounter model.counters in
        { model with counters = newCounters }
    | Delete pos ->
        let newCounters = List.removeAt pos model.counters
        {model with counters = newCounters }


let view (model: Model) (dispatch: Msg -> unit) : ReactElement =
    let viewCounter (pos: int) (counter: Counter.Model) : Fable.React.ReactElement =
        Html.li [
            Counter.view counter (fun msg -> dispatch (CounterMsg(pos, msg)))
            Html.button [ prop.onClick (fun _ -> dispatch (Delete pos)); prop.text "DELETE" ]
        ]

    Html.div [
        Html.h1 "Elmish parent-children communication (without storing position)"
        Html.div [
            Html.pre [ Html.text $"%A{model}" ]
            Html.input [ prop.onChange (dispatch << FirstNameChanged); prop.value model.first_name ]
            Html.input [ prop.onChange (dispatch << LastNameChanged); prop.value model.last_name ]
            Html.ul (List.mapi viewCounter model.counters)
        ]
    ]
