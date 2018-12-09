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
import Claims

lls = liftEffect <<< log <<< show

main :: Effect Unit
main = run [consoleReporter] do
  describe "purescript-spec" do
    describe "calibration" do
      it "works" do
        lls calibration
        lls firstCross
    describe "consider" do
      it "miss" do
        lls (consider cempty 1)
      it "miss" do
        lls (consider (consider cempty 2) 1)
      it "hit" do
        lls (consider (consider (consider cempty 1) 2) 1)
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
        lls checksum
      it "common" do
        lls common
    describe "claims" do
      it "parses examples" do
        lls $ parseClaim "#1 @ 1,3: 4x4"
        lls $ parseClaim "#2 @ 3,1: 4x4"
        lls $ parseClaim "#3 @ 5,5: 2x2"
      it "toTiles" do
        lls $ toTiles {id:0, x: 1, y: 1, w: 1, h: 1}
      it "overlapClaims" do
        lls $ overlapClaims [{id:0, x: 1, y: 1, w: 1, h: 1},  {id:0, x: 1, y: 1, w: 1, h: 1}]
      it "generates overlap" do
        lls $ (overlapCount <<< overlapClaims) [{id:0, x: 1, y: 1, w: 1, h: 1},  {id:0, x: 1, y: 1, w: 1, h: 1}]
        lls $ (overlapCount <<< overlapClaims) <$> claims
      it "finds separates" do
        lls $ (nonOverlapping <<< overlapClaims) <$> claims
