module Root (component) where

import Prelude

import Counter as Counter
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (tuple3)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Prelude (Proxy(..))

type Input = Unit
data Action = Clicked

type State = { allDisabled :: Boolean }

type Slots =
  ( counter :: Counter.Slot Int
  )

_counter = Proxy :: Proxy "counter"

component :: forall query output m. H.Component query Input output m
component = H.mkComponent
  { initialState: \_ -> { allDisabled: false }
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      }
  }
  where

  handleAction :: Action -> H.HalogenM State Action Slots output m Unit
  handleAction = case _ of
    {-
     This example is a bit contrived:

     The parent can "command" the child to do things, and also request data from it.
    -}
    Clicked -> do
      H.tell _counter 0 Counter.Inc
      H.tell _counter 1 Counter.Inc
      H.tell _counter 2 Counter.Inc
      a <- H.request _counter 0 Counter.GetDisabled
      b <- H.request _counter 1 Counter.GetDisabled
      c <- H.request _counter 2 Counter.GetDisabled
      let allDisabled = tuple3 a b c == tuple3 (Just true) (Just true) (Just true)
      H.modify_ \state -> state { allDisabled = allDisabled }

  render :: State -> H.ComponentHTML Action Slots m
  render state = HH.div_
    [ HH.h1_ [ HH.text "I am the parent" ]
    , HH.div_
        [ HH.code_ [ HH.text $ show state ]
        ]
    , HH.button
        [ HP.style "margin-top: 10px"
        , HE.onClick \_ -> Clicked
        ]
        [ HH.text "Increase children" ]
    , HH.slot_ _counter 0 Counter.component { disableAt: 1 }
    , HH.slot_ _counter 1 Counter.component { disableAt: 2 }
    , HH.slot_ _counter 2 Counter.component { disableAt: 3 }
    ]