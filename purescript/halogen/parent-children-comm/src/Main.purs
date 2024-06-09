module Main
  ( main
  ) where

import Prelude

import Button as Button
import Control.Monad.Rec.Class (forever)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Subscription as HS
import Halogen.VDom.Driver (runUI)
import Type.Proxy (Proxy(..))

type Slots = (button :: forall query. H.Slot query Button.Output Int)

_button = Proxy :: Proxy "button"

-- PARENT

type ParentState = { count :: Int }
data ParentAction
  = Initialize
  | Inc
  | Reset
  | HandleButton Button.Output

-- every1s :: forall m a. MonadAff m => a -> m (HS.Emitter a)
-- every1s action = do
--   { emitter, listener } <- H.liftEffect HS.create
--   _ <- H.liftAff $ Aff.forkAff $ forever do
--     Aff.delay $ Milliseconds 1000.0
--     H.liftEffect $ HS.notify listener action
--   pure emitter

parent :: forall query input output m. MonadAff m => H.Component query input output m
parent =
  H.mkComponent
    { initialState: \_ -> { count: 0 }
    , render
    , eval: H.mkEval $ H.defaultEval
        { initialize = Just Initialize
        , handleAction = handleAction
        }
    }
  where

  handleAction :: ParentAction -> H.HalogenM ParentState ParentAction Slots output m Unit
  handleAction = case _ of
    Initialize -> do
      -- _ <- H.subscribe =<< every1s Inc
      -- pure unit
      { emitter, listener } <- H.liftEffect HS.create
      void $ H.subscribe emitter
      void
        $ H.liftAff
        $ Aff.forkAff
        $ forever do
            Aff.delay $ Milliseconds 1000.0
            log $ "tick"
            H.liftEffect $ HS.notify listener Inc
    Inc ->
      H.modify_ \st -> st { count = st.count + 1 }

    Reset ->
      H.modify_ \st -> st { count = 0 }

    HandleButton output ->
      case output of
        Button.DidClick ->
          H.modify_ \st -> st { count = 0 }

  render :: ParentState -> H.ComponentHTML ParentAction Slots m
  render state = HH.div_
    [ HH.h1_ [ HH.text "I am the parent" ]
    , HH.div_ [ HH.button [ HE.onClick \_ -> Reset ] [ HH.text "Reset" ] ]
    , HH.br_
    , HH.code_ [ HH.text $ show state ]
    , HH.div_
        [ HH.h2_ [ HH.text "Button child below" ]
        , HH.p_ [ HH.text "The child inherits the counter state from it's parent." ]
        , HH.p_ [ HH.text "When clicked, the parent is notified and resets the counter." ]
        , HH.slot _button 0 Button.component { label: show state.count } HandleButton
        ]
    ]

-- MAIN

main :: Effect Unit
main = runHalogenAff do
  body <- awaitBody
  runUI parent unit body