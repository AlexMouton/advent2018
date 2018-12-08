module Main where

import Prelude
import Data.Foldable (sum)
import Effect.Console (log)
import Effect.Class (liftEffect)
import Effect (Effect)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)
import Data.Tuple (Tuple(..), fst, snd)

import Calibration
import Inventory

main :: Effect Unit
main = run [consoleReporter] do
  describe "purescript-spec" do
    describe "calibration" do
      it "works" do
        liftEffect $ log $ show $ calibration
        liftEffect $ log $ show $ firstCross
    describe "consider" do
      it "miss" do
        liftEffect $ log $ show (consider cempty 1)
      it "miss" do
        liftEffect $ log $ show (consider (consider cempty 2) 1)
      it "hit" do
        liftEffect $ log $ show (consider (consider (consider cempty 1) 2) 1)
    describe "idScore" do
      it "abcdef" do
        shouldEqual (Tuple 0 0) $ idScore "abcdef"
      it "bababc" do
        shouldEqual (Tuple 1 1) $ idScore "bababc"
      it "abbcde" do
        shouldEqual (Tuple 1 0) $ idScore "abbcde"
      it "abcccd" do
        shouldEqual (Tuple 0 1) $ idScore "abcccd"
      it "aabcdd" do
        shouldEqual (Tuple 1 0) $ idScore "aabcdd"
      it "abcdee" do
        shouldEqual (Tuple 1 0) $ idScore "abcdee"
      it "ababab" do
        shouldEqual (Tuple 0 1) $ idScore "ababab"
      it "checksum" do
        liftEffect $ log $ show $ checksum
      it "common" do
        liftEffect $ log $ show $ common
