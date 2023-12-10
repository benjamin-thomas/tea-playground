module Main exposing (main)

import Browser
import Html as H exposing (Attribute, Html)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as D exposing (Decoder)



{-

   Step 1:
     Open the index.html from the filesystem.

   Step 2:
     Run in a terminal one of these:
       ELM_WATCH_OPEN_EDITOR='code --goto "$file:$line:$column"' elm-watch hot
       ELM_WATCH_OPEN_EDITOR='idea --line "$line" "$file"' elm-watch hot

   NOTE:
     The debugger can be turn on manually.

-}


type alias Model =
    { editing : String
    , todos : List String
    }


type Msg
    = Edited String
    | PressedEnter
    | NoOp


init : flags -> ( Model, Cmd Msg )
init =
    always ( { editing = "", todos = [] }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            let
                _ =
                    Debug.log "NoOp" True
            in
            ( model, Cmd.none )

        Edited v ->
            ( { model | editing = v }, Cmd.none )

        PressedEnter ->
            ( { model
                | editing = ""
                , todos = model.editing :: model.todos
              }
            , Cmd.none
            )


onEnter : msg -> Attribute msg
onEnter msg =
    let
        decodeEnter : Int -> Decoder msg
        decodeEnter n =
            case n of
                13 ->
                    D.succeed msg

                _ ->
                    D.fail "not ENTER"
    in
    HE.on "keydown"
        (HE.keyCode
            |> D.andThen decodeEnter
        )


view : Model -> Html Msg
view model =
    let
        li txt =
            H.li []
                [ H.div [ HA.class "view" ]
                    [ H.input [ HA.class "toggle", HA.type_ "checkbox" ] []
                    , H.label [] [ H.text txt ]
                    , H.button [ HA.class "destroy" ] []
                    ]
                ]
    in
    H.div []
        [ H.pre [] [ H.text <| Debug.toString model ]
        , H.section [ HA.class "todoapp" ]
            [ H.header [ HA.class "header" ]
                [ H.h1 [] [ H.text "todos" ]
                , H.input
                    [ HA.class "new-todo"
                    , HA.autofocus True
                    , HA.placeholder "What needs to be done?"
                    , HA.value model.editing
                    , HE.onInput Edited
                    , onEnter PressedEnter
                    ]
                    []
                ]
            , H.section [ HA.class "main" ]
                [ H.ul [ HA.class "todo-list" ]
                    (List.map li model.todos)
                ]
            ]
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = always <| Sub.none
        , update = update
        , view = view
        }
