module Helpers where

import Random

(!!) : [a] -> Int -> a
(!!) list idx = head (drop idx list)
infixl 4 !!

without : Int -> [a] -> [a]
without i arr =
  let before = take i arr
      after = drop (i+1) arr
  in
    before ++ after

replaceAtIndex : Int -> a -> [a] -> [a]
replaceAtIndex i elt arr =
  let before = take i arr
      after = drop (i+1) arr
  in
    before ++ [elt] ++ after

shuffle : [a] -> Signal b -> Signal [a]
shuffle list signal =
  let randomsFromSignal signal = Random.floatList <| lift (\x -> length list) signal
      shuffleWithRandoms list randoms =
        if isEmpty list
        then []
        else
          let i = floor (head randoms * toFloat (length list))
          in
            [list !! i] ++ (shuffleWithRandoms (without i list) (tail randoms))
  in
    lift2 shuffleWithRandoms (constant list) (randomsFromSignal signal)
