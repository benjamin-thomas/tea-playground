module Test.Main where

import Prelude

import Data.Either (Either(..))
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (delay, launchAff_)
import Partial.Unsafe (unsafeCrashWith)
import Routes (Route(..), routes)
import Routing (match)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  describe "Suite" do
    describe "Routes parsing" do routeParsing
    it "supports async specs" do
      res <- delay (Milliseconds 100.0) *> pure "Alligator"
      res `shouldEqual` "Alligator"

unsafeFromRight :: forall a b. Either a b -> b
unsafeFromRight (Right b) = b
unsafeFromRight (Left _) = unsafeCrashWith "unsafeFromRight: Left"

routeParsing ∷ Spec Unit
routeParsing =
  let
    parse = unsafeFromRight <$> match routes
  in
    do
      it "parses post index" do
        parse "/posts" `shouldEqual` PostIndex
      it "parses post show" do
        parse "/posts/123" `shouldEqual` PostShow 123
      it "parses post edit" do
        parse "/posts/123/edit" `shouldEqual` PostEdit 123
      it "returns NotFound if none match" do
        parse "/foo/bar" `shouldEqual` NotFound