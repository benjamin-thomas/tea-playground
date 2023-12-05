module Main exposing (dialog, main)

import Browser
import Html as H exposing (Attribute, Html)
import Html.Attributes as HA
import Html.Events as HE



{-
   elm-live ./src/Main.elm -- --debug
-}


type Model
    = NoDiag
    | DiagOne
    | DiagTwo


init : Model
init =
    NoDiag


type Msg
    = ClickedEnterDiagOne
    | ClickedEnterDiagTwo
    | RequestDiagExit


update : Msg -> Model -> Model
update msg _ =
    case msg of
        ClickedEnterDiagOne ->
            DiagOne

        ClickedEnterDiagTwo ->
            DiagTwo

        RequestDiagExit ->
            NoDiag


dialog : List (Attribute msg) -> List (Html msg) -> Html msg
dialog attrs =
    let
        open : Attribute msg
        open =
            HA.attribute "open" ""
    in
    H.node "dialog" (open :: attrs)


viewDialog : Model -> Html Msg
viewDialog model =
    case model of
        NoDiag ->
            H.text ""

        DiagOne ->
            dialog
                []
                [ H.div []
                    [ H.h2 [] [ H.text "Diag level 1" ]
                    , H.p [] [ H.text "Some text" ]
                    , H.button [ HE.onClick ClickedEnterDiagTwo ] [ H.text "Confirm?" ]
                    ]
                ]

        DiagTwo ->
            let
                spacing =
                    [ HA.style "margin-right" "10px"
                    , HA.style "margin-top" "10px"
                    ]
            in
            dialog []
                [ H.h2 [] [ H.text "Diag level 2" ]
                , H.p []
                    [ H.text "Some other text"
                    , H.div []
                        [ H.button (HE.onClick ClickedEnterDiagOne :: spacing) [ H.text "Cancel" ]
                        , H.button [ HE.onClick RequestDiagExit ] [ H.text "OK" ]
                        ]
                    ]
                ]


view : Model -> Html Msg
view model =
    H.div []
        [ H.h1 [] [ H.text "Dialog example" ]
        , H.div [] []
        , H.button [ HE.onClick ClickedEnterDiagOne ]
            [ H.text "Open dialog" ]
        , viewDialog model
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }
