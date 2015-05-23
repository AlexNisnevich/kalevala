module Player where

import Color
import Color exposing (Color)
import Dict
import Maybe exposing (withDefault)

import GameTypes exposing (Player (..), PlayerType (..), State)
import Helpers exposing (..)
import Random exposing (float, generate)

toColor : Player -> Color
toColor player =
  case player of
    Red -> Color.red
    Blue -> Color.blue

fromString : String -> Player
fromString str =
  case str of
    "red" -> Red
    "blue" -> Blue

toString : Player -> String
toString player =
  case player of
    Red -> "red"
    Blue -> "blue"

next : Player -> Player
next player =
  case player of
    Red -> Blue
    Blue -> Red

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
