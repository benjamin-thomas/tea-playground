module Main where

import Prelude

import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

type Input = Unit

type State = Int

initialState :: Input -> State
initialState _ = 0

data Action = Inc | Dec

{-

()     : means our component has no child components
output : would be used if we wanted to comunnicate with a parent
m      : is for effect

 -}
handleAction :: forall output m. Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Inc -> H.modify_ \state -> min 3 $ state + 1
  Dec -> H.modify_ \state -> max 0 $ state - 1

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div_
    [ HH.button [ HE.onClick \_ -> Dec ] [ HH.text "-" ]
    , HH.text $ show state
    , HH.button [ HE.onClick \_ -> Inc ] [ HH.text "+" ]
    ]

{-

query  : describes how the parent can communicate with this component
input  : which data the component accepts
output : describes how the parent can communicate with this component
m      : is for effect
 -}
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