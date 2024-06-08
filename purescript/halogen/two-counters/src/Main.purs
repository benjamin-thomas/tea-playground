module Main
  ( main
  ) where

import Prelude

import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

{-

rg --files | entr -c spago build

 -}

type Input = Unit

type BtnState = Int
type State = { a :: BtnState, b :: BtnState }

initialState :: Input -> State
initialState _ =
  { a: 0
  , b: 0
  }

data Button = ButtonA | ButtonB
data Action = Inc Button | Dec Button

handleAction :: forall output m. Action -> H.HalogenM State Action () output m Unit
handleAction =
  let
    inc v = min 9 $ v + 1
    dec v = max 0 $ v - 1
  in
    case _ of
      Inc btn -> H.modify_ \state ->
        case btn of
          ButtonA -> state { a = inc state.a }
          ButtonB -> state { b = inc state.b }

      Dec btn -> H.modify_ \state ->
        case btn of
          ButtonA -> state { a = dec state.a }
          ButtonB -> state { b = dec state.b }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div_
    [ HH.h1_ [ HH.text "Two counters" ]
    , counter ButtonA state.a
    , counter ButtonB state.b
    ]

counter :: forall m. Button -> BtnState -> H.ComponentHTML Action () m
counter btn state =
  HH.div_
    [ HH.div_
        [ HH.button [ HE.onClick \_ -> Dec btn ] [ HH.text "-" ]
        , HH.text $ show state
        , HH.button [ HE.onClick \_ -> Inc btn ] [ HH.text "+" ]
        ]
    ]

component :: forall query output m. H.Component query Input output m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  }

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body