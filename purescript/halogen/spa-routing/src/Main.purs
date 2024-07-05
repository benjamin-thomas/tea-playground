module Main
  ( main
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Console (log)
import Foreign (unsafeToForeign)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import RootComponent as Root
import Routes (Route(..))
import Routes as Routes
import Routing.Hash as RH
import Routing.PushState (makeInterface)

main :: Effect Unit
main = HA.runHalogenAff do
  H.liftEffect $ log "Booting up..."
  body <- HA.awaitBody
  halogenIO <- runUI Root.component unit body
  handleRouteChange halogenIO
  where
  handleRouteChange halogenIO = do -- TODO: put a proper type signature and extract
    nav <- H.liftEffect makeInterface
    void $ H.liftEffect $ RH.matches Routes.routes \oldRoute newRoute -> do
      log $ show oldRoute <> " -> " <> show newRoute
      case oldRoute /\ newRoute of
        Nothing /\ NotFound -> do
          {- Redirect to proper root on app start:
              /         ==>  #/
              /whatever ==>  #/
              #/bogus   ==>  #/
          -}
          H.liftEffect $ nav.replaceState (unsafeToForeign {}) "/#/"
          launchAff_ $ void $ halogenIO.query $ H.mkTell $ Root.Nav RootR
        _ ->
          launchAff_ $ void $ halogenIO.query $ H.mkTell $ Root.Nav newRoute