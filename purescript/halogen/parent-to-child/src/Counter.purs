module Counter
  ( Query(..)
  , Slot
  , component
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

data Query a
  = Inc a
  | GetDisabled (Boolean -> a)

type Input = { disableAt :: Int }
type State = { disableAt :: Int, count :: Int }
type Slot id = H.Slot Query Void id

isDisabled :: State -> Boolean
isDisabled { count, disableAt } =
  count >= disableAt

component :: forall output m. H.Component Query Input output m
component = H.mkComponent
  { initialState: \{ disableAt } -> { disableAt, count: 0 }
  , render
  , eval: H.mkEval
      $ H.defaultEval
          { handleQuery = handleQuery
          }
  }
  where

  handleQuery :: forall action a. Query a -> H.HalogenM State action () output m (Maybe a)
  handleQuery = case _ of
    Inc a -> do
      H.modify_ \state -> state { count = state.count + 1 }
      pure $ Just a

    GetDisabled reply -> do
      state <- H.get
      pure $ Just $ reply $ isDisabled state

  render state =
    HH.div [ HP.style "margin-top: 5px" ]
      [ HH.button [ HP.disabled $ isDisabled state ] [ HH.text $ show state.count ]
      ]
