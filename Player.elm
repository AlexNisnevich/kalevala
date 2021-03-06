module Player where

import Color
import Color exposing (Color)
import Dict
import Maybe exposing (withDefault)
import Random exposing (float, generate)

import GameTypes exposing (..)
import Helpers exposing (..)

toColor : Player -> Color
toColor player =
  case player of
    Red -> Color.rgb 217 33 32
    Blue -> Color.rgb 70 131 193
    SwitchingTo pl -> toColor pl

fromString : String -> Player
fromString str =
  case str of
    "Red" -> Red
    "red" -> Red
    "Blue" -> Blue
    "blue" -> Blue

toString : Player -> String
toString player =
  case player of
    Red -> "Red"
    Blue -> "Blue"
    SwitchingTo pl -> toString pl

next : Player -> Player
next player =
  case player of
    Red -> Blue
    Blue -> Red
    SwitchingTo pl -> pl

getType : Player -> State -> PlayerType
getType player state =
  withDefault Human (Dict.get (toString player) state.players)

getHand : Player -> State -> List String
getHand player state =
  withDefault [] (Dict.get (toString player) state.hands)

noTilesInHand : Player -> State -> Bool
noTilesInHand player state =
  List.isEmpty (getHand player state)

random : Random.Seed -> Player
random seed =
  if fst (generate (float 0 1) seed) > 0.5
  then Red
  else Blue
