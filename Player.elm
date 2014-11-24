module Player where

import Dict

import GameTypes (..)

color : Player -> Color
color player =
  case player of
    Red -> red
    Blue -> blue

next : Player -> Player
next player =
  case player of
    Red -> Blue
    Blue -> Red

noTilesInHand : Player -> State -> Bool
noTilesInHand player state =
  let p = playerName player
  in
    isEmpty <| Dict.getOrFail p state.hands
