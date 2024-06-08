module Main
  ( main
  ) where

import Prelude

import Affjax.Web as AX
import Affjax.ResponseFormat as AXRF
import Data.Either (hush)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Random (randomInt)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (AutocompleteType(..))
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Web.Event.Event (Event)
import Web.Event.Event as Event

{-

rg --files | entr -c spago build

 -}

type State =
  { n :: Maybe Int
  , loading :: Boolean
  , username :: String
  , result :: Maybe String
  }

initialState :: Unit -> State
initialState _ =
  { n: Nothing
  , loading: false
  , username: ""
  , result: Nothing
  }

data Action
  = Regenerate
  | UsernameChanged String
  | FetchInfo Event {- We want Event to preventDefault on form submit -}

{-

NOTE: for synchronous effect, we could use MonadEffect.
But since we need to do async stuff (fetch), any synchronous effect can be handled by MonadAff.

So MonadAff implies MonadEffect

 -}
handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction =
  case _ of
    Regenerate -> do
      newNumber <- H.liftEffect $ randomInt 1 99
      H.modify_ \state ->
        state { n = Just newNumber }

    UsernameChanged username -> H.modify_ \state ->
      state { username = username }

    FetchInfo evt -> do
      H.liftEffect $ Event.preventDefault evt -- Effect
      username <- H.gets _.username -- HalogenM
      H.modify_ _ { loading = true } -- HalogenM | re-render!
      response <- H.liftAff $ AX.get AXRF.string ("https://api.github.com/users/" <> username) -- Aff | suspends!
      H.modify_ _ { loading = false, result = map _.body (hush response) } -- HalogenM | re-render!

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div_
    [ HH.h1_ [ HH.text "Random number" ]
    , HH.button [ HE.onClick \_ -> Regenerate ] [ HH.text "Generate" ]
    , HH.div [ HP.style "margin-top: 12px" ]
        [ HH.text $ case state.n of
            Nothing -> "No number yet"
            Just number -> "Got number: " <> show number
        ]
    , HH.hr [ HP.style "margin: 20px 0" ]
    , HH.div []
        [ HH.h2_ [ HH.text "Lookup Github user info" ]
        , HH.form [ HE.onSubmit FetchInfo ]
            [ HH.div_
                [ HH.label [ HP.for "username" ] [ HH.text "Username" ]
                , HH.br_
                , HH.input
                    [ HP.id "username"
                    , HP.autocomplete AutocompleteOff
                    , HE.onValueInput UsernameChanged
                    ]
                , HH.button [ HP.type_ HP.ButtonSubmit ] [ HH.text "Fetch info" ]
                ]
            ]

        ]

    , if state.loading then HH.div_ [ HH.text "Loading..." ] else HH.text ""
    , case state.result of
        Nothing -> HH.div_ [ HH.text "Nothing fetched yet!" ]
        Just response ->
          HH.div_
            [ HH.h2_ [ HH.text "Got user data!" ]
            , HH.code_ [ HH.text $ show response ]

            ]

    ]

component :: forall query output m. MonadAff m => H.Component query Unit output m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  }

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body