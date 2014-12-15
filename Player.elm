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

name : Player -> String
name player =
  case player of
    Red -> "red"
    Blue -> "blue"

next : Player -> Player
next player =
  case player of
    Red -> Blue
    Blue -> Red

getHand : Player -> State -> List String
getHand player state =
  withDefault [] (Dict.get (name player) state.hands)

noTilesInHand : Player -> State -> Bool
noTilesInHand player state =
  List.isEmpty (getHand player state)
