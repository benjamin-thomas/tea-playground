module WithoutStoringPosition.Main exposing (main)

import Browser
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import WithoutStoringPosition.Counter as Counter exposing (RequestToParent(..), WrappedMsg)


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
    | GotCounterMsg Position WrappedMsg


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

        GotCounterMsg (Position pos) subWrappedMsg ->
            let
                newCounters =
                    case subWrappedMsg of
                        Counter.InternalMsg subMsg ->
                            let
                                updateCounter : Int -> Counter.Model -> Counter.Model
                                updateCounter pos2 subModel =
                                    if pos == pos2 then
                                        Counter.update subMsg subModel

                                    else
                                        subModel
                            in
                            model.counters
                                |> List.indexedMap updateCounter

                        Counter.ExternalMsg subMsg ->
                            case subMsg of
                                Counter.Delete ->
                                    model.counters
                                        |> List.indexedMap (\p subModel -> ( p, subModel ))
                                        |> List.filter (\( pos2, _ ) -> pos /= pos2)
                                        |> List.map Tuple.second
            in
            { model | counters = newCounters }


view : Model -> Html Msg
view model =
    let
        viewCounter : Int -> Counter.Model -> Html Msg
        viewCounter position child =
            H.map
                (GotCounterMsg (Position position))
                (Counter.view position child)
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
