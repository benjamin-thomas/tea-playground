module Main
  ( main
  ) where

import Prelude

import Data.Array (alterAt, intercalate, length, mapWithIndex, modifyAt, snoc)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)

{-

rg --files | entr -c spago build

 -}

type Input = Unit

type BtnState = Int
type State = { counters :: Array BtnState }

initialState :: Input -> State
initialState _ =
  { counters: [ 1, 2, 3 ] }

data Action = Add | Inc Int | Delete Int

handleAction :: forall output m. Action -> H.HalogenM State Action () output m Unit
handleAction =
  case _ of
    Add ->
      H.modify_ \state ->
        state { counters = snoc state.counters 0 }

    Inc idx ->
      H.modify_ \state ->
        state
          { counters =
              ( fromMaybe
                  state.counters
                  (modifyAt idx inc state.counters)
              )
          }
      where
      inc v = min 9 $ v + 1

    Delete idx ->
      H.modify_ \state ->
        state
          { counters =
              fromMaybe
                state.counters
                (alterAt idx (const Nothing) state.counters)
          }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div [ HP.style "zoom: 1.5" ]
    [ HH.code_ $ [ HH.text $ "State: " <> show state ]
    , HH.h1_ [ HH.text $ "Many counters (" <> show (length state.counters) <> ")" ]
    , HH.button [ HE.onClick $ const Add ] [ HH.text "Add" ]
    , HH.ul_ $ mapWithIndex renderCounter state.counters
    ]

renderCounter :: forall m. Int -> BtnState -> H.ComponentHTML Action () m
renderCounter idx state =
  HH.li [ HP.style "margin: 14px 0" ]
    [ HH.div [ HP.style "user-select:none" ]
        [ HH.span
            [ HE.onClick \_ -> Inc idx
            , HP.style $ intercalate ";" $
                [ "border       : 1px solid red"
                , "font-weight  : bold"
                , "cursor       : pointer"
                , "padding      : 4px 10px"
                , "margin-right : 10px"
                ]
            ]
            [ HH.text $ show state ]
        , HH.button [ HE.onClick \_ -> Delete idx ] [ HH.text "Delete" ]
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