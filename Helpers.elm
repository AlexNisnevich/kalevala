module Helpers where

(!!) : [a] -> Int -> a
(!!) list idx = head (drop idx list)
infixl 4 !!

