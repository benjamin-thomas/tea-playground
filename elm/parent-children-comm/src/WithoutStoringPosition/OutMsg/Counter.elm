module WithoutStoringPosition.OutMsg.Counter exposing (Model, Msg(..), OutMsg(..), update, view)

import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE


type alias Model =
    { value : Int }


type Msg
    = Inc
    | ClickedDeleteBtn


type OutMsg
    = None
    | Delete


update : Msg -> Model -> ( Model, OutMsg )
update msg model =
    case msg of
        Inc ->
            ( { model | value = model.value + 1 }, None )

        ClickedDeleteBtn ->
            ( model, Delete )


view : Int -> Model -> Html Msg
view position model =
    H.li
        []
        [ H.span []
            [ H.span
                [ HA.style "user-select" "none"
                , HA.style "cursor" "pointer"
                , HE.onClick Inc
                ]
                [ H.text <| "Counter[" ++ String.fromInt position ++ "] " ++ String.fromInt model.value ]
            , H.button
                [ HA.style "margin-left" "10px"
                , HE.onClick ClickedDeleteBtn
                ]
                [ H.text "Delete" ]
            ]
        ]
