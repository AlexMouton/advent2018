module Inventory where

import Prelude

import Boxes (ids)
import Data.String (Pattern(..), split)
import Data.String.CodeUnits (toCharArray, fromCharArray)
import Data.Foldable (find, any, sum)
import Data.Array (group, sort, filter, zip, length)
import Data.Array.NonEmpty (length) as Nea
import Data.Tuple (Tuple(..), fst, snd)
import Data.Maybe (Maybe)

boxes :: Array String
boxes = split (Pattern "\n") ids

checksum :: Int
checksum = (\a -> fst a * snd a) $ sum $ idScore <$> boxes

-- two = fold (count 2) hasTwo <$> boxes


idScore :: String -> Tuple Int Int
idScore =
  toCharArray
  >>> sort
  >>> group
  >>> map Nea.length
  >>> toScore

toScore :: (Array Int) -> (Tuple Int Int)
toScore is = Tuple (score 2 is) (score 3 is)
  where
    score :: Int -> (Array Int) -> Int
    score n = btoi <<< any ((==) n)

    btoi :: Boolean -> Int
    btoi true = 1
    btoi false = 0

common :: Maybe String
common =
  Tuple <$> boxes <*> boxes
  # find match
  # map shared

shared :: (Tuple String String) -> String
shared (Tuple a b) =
   zip (toCharArray a) (toCharArray b)
   # filter same
   # map fst
   # fromCharArray

match :: Tuple String String -> Boolean
match (Tuple a b) =
  zip (toCharArray a) (toCharArray b)
  # filter (not <<< same)
  # length
  # (==) 1

same :: Tuple Char Char -> Boolean
same (Tuple x y) = x == y
