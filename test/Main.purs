module Test.Main where

import Prelude

import Effect.Console (log)
import Effect.Class
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (launchAff_, delay)
import Test.Spec (pending, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)

import Main

main :: Effect Unit
main = run [consoleReporter] do
  describe "purescript-spec" do
    describe "consider" do
      it "miss" do
        liftEffect $ log $ show (consider empty 1)
      it "miss" do
        liftEffect $ log $ show (consider (consider empty 2) 1)
      it "hit" do
        liftEffect $ log $ show (consider (consider (consider empty 1) 2) 1)
