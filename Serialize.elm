module Serialize where

import Dict
import Json
import Json (Value (..))

import GameTypes (..)

serializeAction : Action -> Value
serializeAction action =
  case action of
  PickUpPiece player idx -> Object <| Dict.fromList
    [("action", String "PickUpPiece"), ("player", serializePlayer player), ("idx", Number <| toFloat idx)]
  PlacePiece mousePos dims -> Object <| Dict.fromList
    [("action", String "PlacePiece"), ("mousePos", serializeIntPair mousePos), ("dims", serializeIntPair dims)]
  StartGame deck player -> Object <| Dict.fromList
    [("action", String "StartGame"), ("deck", serializeDeck deck), ("player", serializePlayer player)]
  Pass -> Object <| Dict.fromList [("action", String "Pass")]
  NoAction -> Object <| Dict.fromList [("action", String "NoAction")]

serializeDeck : Deck -> Value
serializeDeck deck = Array <| map String deck

serializePlayer : Player -> Value
serializePlayer player = String <| playerName player

serializeMousePos : MousePos -> Value
serializeMousePos = serializeIntPair

serializeWindowDims : WindowDims -> Value
serializeWindowDims = serializeIntPair

serializeInt : Int -> Value
serializeInt i = Number <| toFloat i

serializeIntPair : (Int, Int) -> Value
serializeIntPair (x, y) = Array <| map serializeInt [x, y]

-- TODO deserialize
