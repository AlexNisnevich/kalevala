module Serialize where

import Dict
import List (..)
import Json.Encode (..)

import Helpers (..)
import GameTypes (..)

serializeAction : Action -> Value
serializeAction action =
  case action of
    PickUpPiece player idx -> 
      object [("action", string "PickUpPiece"), ("player", serializePlayer player), ("idx", int idx)]
    PlacePiece mousePos dims -> 
      object [("action", string "PlacePiece"), ("mousePos", serializeIntPair mousePos), ("dims", serializeIntPair dims)]
    StartGame deck player -> 
      object [("action", string "StartGame"), ("deck", serializeDeck deck), ("player", serializePlayer player)]
    Pass -> 
      object [("action", string "Pass")]
    NoAction -> 
      object [("action", string "NoAction")]

serializeDeck : Deck -> Value
serializeDeck deck = list <| map string deck

serializePlayer : Player -> Value
serializePlayer player = string <| playerName player

serializeMousePos : MousePos -> Value
serializeMousePos = serializeIntPair

serializeWindowDims : WindowDims -> Value
serializeWindowDims = serializeIntPair

serializeIntPair : (Int, Int) -> Value
serializeIntPair (x, y) = list <| map int [x, y]
