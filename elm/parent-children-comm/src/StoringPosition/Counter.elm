module StoringPosition.Counter exposing (Model, Msg, update, view)

import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE


type alias Model =
    { position : Int, value : Int }


type Msg
    = Inc


update : Msg -> Model -> Model
update msg model =
    case msg of
        Inc ->
            { model | value = model.value + 1 }


view : Model -> Html Msg
view model =
    H.li
        [ HE.onClick Inc
        , HA.style "user-select" "none"
        , HA.style "cursor" "pointer"
        ]
        [ H.text <| "Counter[" ++ String.fromInt model.position ++ "] " ++ String.fromInt model.value
        ]
