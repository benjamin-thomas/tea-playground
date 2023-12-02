port module Main exposing (main)

import Browser
import Html as H exposing (Html)
import Html.Events as HE


type Model
    = Waiting
    | Loaded { counter : Int }


type Msg
    = GotWorkerState { curr : Int }
    | ClickedResetBtn


init : () -> ( Model, Cmd Msg )
init () =
    ( Waiting, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg _ =
    case msg of
        GotWorkerState st ->
            ( Loaded { counter = st.curr }, Cmd.none )

        ClickedResetBtn ->
            ( Waiting, askWorkerStateReset () )


view : Model -> Html Msg
view model =
    case model of
        Waiting ->
            H.h2 [] [ H.text "Waiting..." ]

        Loaded st ->
            H.div []
                [ H.h2 []
                    [ H.span []
                        [ H.text "Counter:\u{00A0}"
                        , H.text (String.fromInt st.counter)
                        ]
                    ]
                , H.button [ HE.onClick ClickedResetBtn ] [ H.text "Reset" ]
                ]



-- INCOMING PORTS


port receivedWorkerState : ({ curr : Int } -> msg) -> Sub msg



-- OUTGOING PORTS


port askWorkerStateReset : () -> Cmd msg



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ receivedWorkerState GotWorkerState
        ]



-- START


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
