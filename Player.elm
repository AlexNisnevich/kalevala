module Player where

import Color
import Color (Color)
import Dict
import List
import Maybe (..)

import GameTypes (..)
import Helpers (..)

color : Player -> Color
color player =
  case player of
    Red -> Color.red
    Blue -> Color.blue

fromString : String -> Player
fromString str =
  case str of
    "red" -> Red
    "blue" -> Blue

next : Player -> Player
next player =
  case player of
    Red -> Blue
    Blue -> Red

getHand : Player -> State -> List String
getHand player state =
  let p = playerName player
  in
    withDefault [] (Dict.get p state.hands)

noTilesInHand : Player -> State -> Bool
noTilesInHand player state =
  List.isEmpty (getHand player state)
