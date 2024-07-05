module RootComponent
  ( Query(..)
  , component
  ) where

import Prelude

import Button as Button
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Routes (Route(..))
import Type.Proxy (Proxy(..))

type State = Maybe Route
type Slots =
  ( home :: forall query. H.Slot query Void Unit
  , postIndex :: forall query. H.Slot query Void Unit
  , postShow :: forall query. H.Slot query Void Unit
  , postEdit :: forall query. H.Slot query Void Unit
  , button :: forall query. H.Slot query Void Unit
  , notFound :: forall query. H.Slot query Void Unit
  )

{-

Source: https://purescript-halogen.github.io/purescript-halogen/guide/05-Parent-Child-Components.html
> When you implement a query type, remember that the a type parameter should be present in every constructor.

That's because we always return the "context" of the state monad (to allow further computation I think)

https://github.com/benjamin-thomas/multi-playground/blob/ec198c6be388ef0466ab5b18b031ad0973dc5683/monads/state_monad/haskell/state.hs#L33

-}
data Query a = Nav Route a
data Action = Initialize

component :: forall i o m. MonadEffect m => H.Component Query i o m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleQuery = handleQuery
      }
  }
  where

  initialState :: i -> State
  initialState _ = Nothing

  render :: forall a. State -> H.ComponentHTML a Slots m
  render state =
    HH.div_
      [ HH.pre_ [ HH.text $ show state ]
      , HH.ul_
          [ HH.li_ [ HH.a [ HP.href "/" ] [ HH.text "Real root" ] ]
          , HH.li_ [ HH.a [ HP.href "#/" ] [ HH.text "Root" ] ]
          , HH.li_ [ HH.a [ HP.href "#/posts/" ] [ HH.text "Post index" ] ]
          , HH.li_ [ HH.a [ HP.href "#/posts/123" ] [ HH.text "View post 123" ] ]
          , HH.li_ [ HH.a [ HP.href "#/posts/123/edit" ] [ HH.text "Edit post 123" ] ]
          , HH.li_ [ HH.a [ HP.href "#/button" ] [ HH.text "View button" ] ]
          , HH.li_ [ HH.a [ HP.href "#/bogus" ] [ HH.text "Bogus" ] ]
          ]
      , HH.div_
          [ case state of
              Nothing -> HH.text "Nothing"
              Just route -> case route of
                RootR -> HH.text "Root page"
                PostIndex -> HH.text "Post index"
                PostShow n -> HH.text $ "Viewing post #" <> show n
                PostEdit n -> HH.text $ "Editing post #" <> show n
                ButtonR -> HH.slot_ (Proxy :: _ "button") unit Button.component unit
                NotFound -> HH.text "Not found :("
          ]
      ]

  handleQuery :: forall action a. Query a -> H.HalogenM State action Slots o m (Maybe a)
  handleQuery (Nav route a) = do
    log $ "on handleQuery: " <> show route
    H.put $ Just route
    pure $ Just a