module Button
  ( Output(..)
  , component
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

type Input = { label :: String }
type State = { label :: String }

data Output = DidClick
data Action
  = GotInput Input
  | Clicked

component :: forall query m. H.Component query Input Output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { receive = receive
        , handleAction = handleAction
        }
    }
  where

  {- The child receives `Input` every time the parent renders -}
  initialState :: Input -> State
  initialState = identity

  receive :: Input -> Maybe Action
  receive =
    Just <<< GotInput

  handleAction :: Action -> H.HalogenM State Action () Output m Unit
  handleAction = case _ of
    GotInput input -> H.modify_ \st -> st { label = input.label }
    Clicked -> H.raise DidClick

  render :: State -> H.ComponentHTML Action () m
  render { label } =
    HH.button [ HE.onClick \_ -> Clicked ] [ HH.text label ]