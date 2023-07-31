module WithoutStoringPosition.Counter exposing (Model, Msg, RequestToParent(..), WrappedMsg(..), update, view)

import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE


type alias Model =
    { value : Int }


type Msg
    = Inc


type RequestToParent
    = Delete


type WrappedMsg
    = InternalMsg Msg
    | ExternalMsg RequestToParent


update : Msg -> Model -> Model
update msg model =
    case msg of
        Inc ->
            { model | value = model.value + 1 }


view : Int -> Model -> Html WrappedMsg
view position model =
    H.li
        []
        [ H.span []
            [ H.span
                [ HE.onClick (InternalMsg Inc)
                , HA.style "user-select" "none"
                , HA.style "cursor" "pointer"
                ]
                [ H.text <| "Counter[" ++ String.fromInt position ++ "] " ++ String.fromInt model.value ]
            , H.button
                [ HA.style "margin-left" "10px"
                , HE.onClick (ExternalMsg Delete)
                ]
                [ H.text "Delete" ]
            ]
        ]
