module WithoutStoringPosition.MetaMessage.Counter exposing (ExternalMsg(..), InternalMsg, MetaMsg(..), Model, update, view)

import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE


type alias Model =
    { value : Int }


type InternalMsg
    = Inc


type ExternalMsg
    = DeleteBtnClicked


type MetaMsg
    = Internal InternalMsg
    | External ExternalMsg


update : InternalMsg -> Model -> Model
update msg model =
    case msg of
        Inc ->
            { model | value = model.value + 1 }


view : Int -> Model -> Html MetaMsg
view position model =
    H.li
        []
        [ H.span []
            [ H.span
                [ HA.style "user-select" "none"
                , HA.style "cursor" "pointer"
                , HE.onClick (Internal Inc)
                ]
                [ H.text <| "Counter[" ++ String.fromInt position ++ "] " ++ String.fromInt model.value ]
            , H.button
                [ HA.style "margin-left" "10px"
                , HE.onClick (External DeleteBtnClicked)
                ]
                [ H.text "Delete" ]
            ]
        ]
