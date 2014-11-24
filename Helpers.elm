module Helpers where

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

