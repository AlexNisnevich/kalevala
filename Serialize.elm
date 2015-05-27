module Serialize where

import Dict
import List exposing (..)
import Json.Encode exposing (..)

import Helpers exposing (..)
import GameTypes exposing (..)
import Player

action : Action -> Value
action a =
  case a of
    PickUpPiece pl idx -> 
      object [("action", string "PickUpPiece"), ("player", player pl), ("idx", int idx)]
    PlacePiece mousePos dims -> 
      object [("action", string "PlacePiece"), ("mousePos", intPair mousePos), ("dims", intPair dims)]
    StartGame t d pl pln -> 
      object [("action", string "StartGame"), ("deck", deck d), ("player", player pl), ("playerName", string pln)]
    Pass -> 
      object [("action", string "Pass")]
    NoAction -> 
      object [("action", string "NoAction")]
    MoveToMainMenu -> 
      object [("action", string "NoAction")]

deck : Deck -> Value
deck = list << map string

player : Player -> Value
player = string << Player.toString

intPair : (Int, Int) -> Value
intPair (x, y) = list <| map int [x, y]
