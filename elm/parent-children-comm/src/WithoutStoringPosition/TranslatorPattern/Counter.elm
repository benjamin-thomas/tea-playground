module WithoutStoringPosition.TranslatorPattern.Counter exposing
    ( Model
    , Msg
    , update
    , view
    )

import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE


type alias Model =
    { value : Int }


type Msg
    = Inc


update : Msg -> Model -> Model
update msg model =
    case msg of
        Inc ->
            { model | value = model.value + 1 }


type alias MetaMsg msg =
    { internal : Msg -> msg
    , onDeleteBtnClicked : msg
    }


view : Int -> Model -> MetaMsg msg -> Html msg
view position model meta =
    H.li
        []
        [ H.span []
            [ H.span
                [ HA.style "user-select" "none"
                , HA.style "cursor" "pointer"
                , HE.onClick (meta.internal Inc)
                ]
                [ H.text <| "Counter[" ++ String.fromInt position ++ "] " ++ String.fromInt model.value ]
            , H.button
                [ HA.style "margin-left" "10px"
                , HE.onClick meta.onDeleteBtnClicked
                ]
                [ H.text "Delete" ]
            ]
        ]
