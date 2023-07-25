module Main exposing (main)

import Browser
import Child
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE


main =
    Browser.sandbox { init = init, view = view, update = update }


type alias Model =
    { field1 : String
    , field2 : String
    , children : List Child.Model
    }


type Msg
    = Field1Changed String
    | Field2Changed String
    | GotChildMsg Child.Model Child.Msg


init : Model
init =
    { field1 = "John"
    , field2 = "Doe"
    , children =
        [ { position = 0, value = 0 }
        , { position = 1, value = 0 }
        , { position = 2, value = 0 }
        ]
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Field1Changed str ->
            { model | field1 = str }

        Field2Changed str ->
            { model | field2 = str }

        GotChildMsg subModel subMsg ->
            let
                newChild : Child.Model
                newChild =
                    Child.update subMsg subModel

                updateChild : Child.Model -> Child.Model
                updateChild c =
                    if c == subModel then
                        newChild

                    else
                        c

                newChildren : List Child.Model
                newChildren =
                    List.map updateChild model.children
            in
            { model | children = newChildren }


view : Model -> Html Msg
view model =
    let
        viewChild : Child.Model -> Html Msg
        viewChild child =
            H.map (GotChildMsg child) (Child.view child)
    in
    H.div []
        [ H.h1 [] [ H.text "I am the parent" ]
        , H.div []
            [ H.input [ HE.onInput Field1Changed, HA.value model.field1 ] []
            , H.input [ HE.onInput Field2Changed, HA.value model.field2 ] []
            ]
        , H.pre [] [ H.text (Debug.toString model) ]
        , H.div []
            [ H.ul [] (List.map viewChild model.children)
            ]
        ]
