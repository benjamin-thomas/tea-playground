module Main
  ( main
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Random (randomInt)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)

{-

rg --files | entr -c spago build

 -}

type State = Maybe Int

initialState :: Unit -> State
initialState _ =
  Nothing

data Action = Regenerate

handleAction :: forall output m. MonadEffect m => Action -> H.HalogenM State Action () output m Unit
handleAction =
  case _ of
    Regenerate -> do
      newNumber <- H.liftEffect $ randomInt 1 99
      H.modify_ \_ ->
        Just newNumber

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div_
    [ HH.h1_ [ HH.text "Random number" ]
    , HH.button [ HE.onClick \_ -> Regenerate ] [ HH.text "Generate" ]
    , HH.div [ HP.style "margin-top: 12px" ]
        [ HH.text $ case state of
            Nothing -> "No number yet"
            Just number -> "Got number: " <> show number
        ]
    ]

component :: forall query output m. MonadEffect m => H.Component query Unit output m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  }

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body