module Helpers where

import List exposing (..)
import Random exposing (..)
import Signal
import Graphics.Element exposing (..)

getOrFail : Maybe a -> a
getOrFail maybe =
  case maybe of
    Just something -> something

{- Unsafe list methods -}
headU l = getOrFail <| head l
tailU l = getOrFail <| tail l
maximumU l = getOrFail <| maximum l
minimumU l = getOrFail <| minimum l

(!!) : List a -> Int -> a
(!!) list idx = headU (drop idx list)
infixl 4 !!

tuple : a -> b -> (a, b)
tuple a b = (a, b)

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

filterOn : Signal a -> Signal Bool -> a -> Signal a
filterOn inputSignal conditionSignal default =
  let joinedSignal = Signal.map2 tuple inputSignal conditionSignal
      filteredSignal = Signal.filter snd (default, False) joinedSignal
  in
    Signal.map fst filteredSignal
