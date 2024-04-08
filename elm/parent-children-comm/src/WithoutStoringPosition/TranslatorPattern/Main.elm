module WithoutStoringPosition.TranslatorPattern.Main exposing (main)

import Browser
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import WithoutStoringPosition.TranslatorPattern.Counter as Counter


main : Program () Model Msg
main =
    Browser.sandbox { init = init, view = view, update = update }


type alias Model =
    { firstName : String
    , lastName : String
    , counters : List Counter.Model
    }


type Position
    = Position Int


type Msg
    = FirstNameChanged String
    | LastNameChanged String
    | GotCounterMsg Position Counter.Msg
    | GotDeleteRequest Position -- from child


init : Model
init =
    { firstName = "John"
    , lastName = "Doe"
    , counters =
        [ { value = 0 }
        , { value = 0 }
        , { value = 0 }
        ]
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        FirstNameChanged str ->
            { model | firstName = str }

        LastNameChanged str ->
            { model | lastName = str }

        GotCounterMsg (Position pos) subMsg ->
            let
                updateCounter : Int -> Counter.Model -> Counter.Model
                updateCounter pos2 subModel =
                    if pos == pos2 then
                        Counter.update subMsg subModel

                    else
                        subModel

                newCounters : List Counter.Model
                newCounters =
                    List.indexedMap updateCounter model.counters
            in
            { model | counters = newCounters }

        GotDeleteRequest (Position pos) ->
            let
                newCounters =
                    model.counters
                        |> List.indexedMap Tuple.pair
                        |> List.filter (Tuple.first >> (/=) pos)
                        |> List.map Tuple.second
            in
            { model | counters = newCounters }


view : Model -> Html Msg
view model =
    let
        -- With this technique, `Html.map` is not necessary anymore!
        viewCounter : Int -> Counter.Model -> Html Msg
        viewCounter position child =
            Counter.view position
                child
                { internal = GotCounterMsg (Position position)
                , onDeleteBtnClicked = GotDeleteRequest (Position position)
                }
    in
    H.div []
        [ H.h1 [] [ H.text "I am the parent" ]
        , H.div []
            [ H.input [ HE.onInput FirstNameChanged, HA.value model.firstName ] []
            , H.input [ HE.onInput LastNameChanged, HA.value model.lastName ] []
            ]
        , H.pre [] [ H.text (Debug.toString model) ]
        , H.div []
            [ H.ul [] (List.indexedMap viewCounter model.counters)
            ]
        ]
