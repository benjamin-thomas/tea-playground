module StoringPosition.Main exposing (main)

import Browser
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import StoringPosition.Counter as Counter


main =
    Browser.sandbox { init = init, view = view, update = update }


type alias Model =
    { firstName : String
    , lastName : String
    , counters : List Counter.Model
    }


type Msg
    = FirstNameChanged String
    | LastNameChanged String
    | GotCounterMsg Counter.Model Counter.Msg


init : Model
init =
    { firstName = "John"
    , lastName = "Doe"
    , counters =
        [ { position = 0, value = 0 }
        , { position = 1, value = 0 }
        , { position = 2, value = 0 }
        ]
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        FirstNameChanged str ->
            { model | firstName = str }

        LastNameChanged str ->
            { model | lastName = str }

        GotCounterMsg subModel subMsg ->
            let
                newCounter : Counter.Model
                newCounter =
                    Counter.update subMsg subModel

                updateCounter : Counter.Model -> Counter.Model
                updateCounter c =
                    if c.position == subModel.position then
                        newCounter

                    else
                        c

                newCounters : List Counter.Model
                newCounters =
                    List.map updateCounter model.counters
            in
            { model | counters = newCounters }


view : Model -> Html Msg
view model =
    let
        viewCounter : Counter.Model -> Html Msg
        viewCounter counter =
            H.map (GotCounterMsg counter) (Counter.view counter)
    in
    H.div []
        [ H.h1 [] [ H.text "I am the parent" ]
        , H.div []
            [ H.input [ HE.onInput FirstNameChanged, HA.value model.firstName ] []
            , H.input [ HE.onInput LastNameChanged, HA.value model.lastName ] []
            ]
        , H.pre [] [ H.text (Debug.toString model) ]
        , H.div []
            [ H.ul [] (List.map viewCounter model.counters)
            ]
        ]
