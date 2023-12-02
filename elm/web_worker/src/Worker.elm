port module Worker exposing (main)

{-

   let state = null;
   const worker = require('./js/worker.js').Elm.Worker.init();

   worker.ports.stateChanged.subscribe((newState) => state = newState);

   worker.ports.inc.send(null);
   worker.ports.dec.send(null);

   ---

   To debug the shared worker in Chrome, go to:
     chrome://inspect/#workers
-}

import Platform
import Time



-- INCOMING PORTS


port inc : (() -> msg) -> Sub msg


port dec : (() -> msg) -> Sub msg



-- OUTGOING PORTS


port stateChanged : { curr : Int } -> Cmd msg



-- APP


type Model
    = Model Int


type Msg
    = Inc
    | Dec


init : () -> ( Model, Cmd Msg )
init () =
    ( Model 0, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model n) =
    case msg of
        Inc ->
            let
                n2 =
                    min 999 (n + 1)

                -- _ =
                --     Debug.log "[LOG] inc" n2
            in
            ( Model n2, stateChanged { curr = n2 } )

        Dec ->
            let
                n2 =
                    max 0 (n - 1)

                -- _ =
                --     Debug.log "[LOG] dec" n2
            in
            ( Model n2, stateChanged { curr = n2 } )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ inc (always Inc)
        , dec (always Dec)
        , Time.every 1000 (always Inc)
        ]



-- START


main : Program () Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }
