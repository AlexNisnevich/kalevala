module Helpers where

import List (..)
import Random (..)

(!!) : List a -> Int -> a
(!!) list idx = head (drop idx list)
infixl 4 !!

without : Int -> List a -> List a
without i arr =
  let before = take i arr
      after = drop (i+1) arr
  in
    before ++ after

replaceAtIndex : Int -> a -> List a -> List a
replaceAtIndex i elt arr =
  let before = take i arr
      after = drop (i+1) arr
  in
    before ++ [elt] ++ after

shuffle : List a -> Seed -> List a
shuffle list seed =
  if isEmpty list
  then []
  else
    let generator = int 0 (length list - 1)
        (i, newSeed) = generate generator seed
    in
      [list !! i] ++ shuffle (without i list) newSeed