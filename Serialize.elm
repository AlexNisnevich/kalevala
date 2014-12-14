module Serialize where

import Dict
import List (..)
import Json.Encode (..)

import Helpers (..)
import GameTypes (..)

serializeAction : Action -> Value
serializeAction action =
  case action of
    PickUpPiece player idx -> object <| Dict.fromList
      [("action", string "PickUpPiece"), ("player", serializePlayer player), ("idx", int idx)]
    PlacePiece mousePos dims -> object <| Dict.fromList
      [("action", string "PlacePiece"), ("mousePos", serializeIntPair mousePos), ("dims", serializeIntPair dims)]
    StartGame deck player -> object <| Dict.fromList
      [("action", string "StartGame"), ("deck", serializeDeck deck), ("player", serializePlayer player)]
    Pass -> object <| Dict.fromList [("action", string "Pass")]
    NoAction -> object <| Dict.fromList [("action", string "NoAction")]

serializeDeck : Deck -> Value
serializeDeck deck = array <| map string deck

serializePlayer : Player -> Value
serializePlayer player = string <| playerName player

serializeMousePos : MousePos -> Value
serializeMousePos = serializeIntPair

serializeWindowDims : WindowDims -> Value
serializeWindowDims = serializeIntPair

serializeIntPair : (Int, Int) -> Value
serializeIntPair (x, y) = array <| map int [x, y]
