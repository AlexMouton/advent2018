module Calibration where

import Prelude (flip, ($), (+), (<$>))

import Control.Lazy (defer)

import Data.String (Pattern(..), split)
import Data.Maybe (Maybe)
import Data.Either (Either(..))
import Data.Int.Parse (parseInt, toRadix)
import Data.Foldable (class Foldable, foldl)
import Data.Array (catMaybes)
import Data.Map as M
import Data.List.Lazy (List, cycle, fromFoldable)
import Data.List.Lazy.Types (Step(..), step) as Lt

import Dfdx (dfdx)

prepared :: Array Int
prepared = catMaybes $ toInt <$> toValues dfdx

toValues :: String -> Array String
toValues = split (Pattern "\n")

toInt :: String -> Maybe Int
toInt = (flip parseInt) (toRadix 10)

calibration :: Int
calibration = sum prepared

sum :: ∀ f. Foldable f => f Int -> Int
sum = foldl (+) 0

---
type Cross = Either Int { seen :: M.Map Int Boolean }

firstCross :: Cross
firstCross = repeat $ cycle $ defer (\_ -> fromFoldable prepared)

repeat :: List Int -> Cross
repeat l = _.cross $ foldc { current: 0, cross: cempty, list: l }

cempty :: Cross
cempty = Right { seen: M.insert 0 true M.empty }

consider :: Cross -> Int -> Cross
consider (Right r) i | (M.member i r.seen) = Left i
consider (Right r) i = Right { seen: (M.insert i true r.seen) }
consider a _ = a

type FoldCross = { current :: Int, cross :: Cross, list :: List Int }

foldc :: FoldCross -> FoldCross
foldc fc =
  case fc.cross of
    Left a -> fc
    Right b ->
      case Lt.step fc.list of
        Lt.Nil -> fc
        Lt.Cons a as ->
          let cur = fc.current + a
          in
            foldc $ { current: cur, cross: (consider fc.cross cur), list: as }
