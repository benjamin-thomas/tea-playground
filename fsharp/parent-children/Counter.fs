module Counter

open Feliz

type Model = { count: int }

let init = { count = 0 }

type Msg = Inc

let update msg model : Model =
    match msg with
    | Inc -> { count = model.count + 1 }

let view model dispatch =
    Html.span [
        Html.text model.count
        Html.button [ prop.onClick (fun _ -> dispatch Inc); prop.text "+1" ]
    ]
