module Player where

import GameTypes (..)

playerColor : Player -> Color
playerColor player =
  case player of
    Red -> red
    Blue -> blue

nextPlayer : Player -> Player
nextPlayer player =
  case player of
    Red -> Blue
    Blue -> Red
