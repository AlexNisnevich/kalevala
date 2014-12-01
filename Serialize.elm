module Serialize where

import Dict
import Json
import Json (Value (..))

import Helpers (..)
import GameTypes (..)

serializeAction : Action -> Value
serializeAction action =
  case action of
    PickUpPiece player idx -> Object <| Dict.fromList
      [("action", String "PickUpPiece"), ("player", serializePlayer player), ("idx", serializeInt idx)]
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

-- Warning: the pattern matches in the deserialize functions are not exhaustive!
-- Be careful. Smokey says "Only you can prevent runtime errors."

deserializeAction : Value -> Action
deserializeAction action =
  case action of
    Object dict ->
      let lookup field = Dict.getOrFail field dict
      in case lookup "action" of
        String "PickUpPiece" -> PickUpPiece (deserializePlayer <| lookup "player") (deserializeInt <| lookup "idx")
        String "PlacePiece" -> PlacePiece (deserializeMousePos <| lookup "mousePos") (deserializeWindowDims <| lookup "dims")
        String "StartGame" -> StartGame (deserializeDeck <| lookup "deck") (deserializePlayer <| lookup "player")
        String "Pass" -> Pass
        String "NoAction" -> NoAction

deserializeDeck : Value -> Deck
deserializeDeck deck =
  case deck of
    Array values -> map deserializeString values

deserializePlayer : Value -> Player
deserializePlayer player =
  case player of
    String "red" -> Red
    String "blue" -> Blue

deserializeMousePos : Value -> MousePos
deserializeMousePos = deserializeIntPair

deserializeWindowDims : Value -> WindowDims
deserializeWindowDims = deserializeIntPair

deserializeInt : Value -> Int
deserializeInt val =
  case val of
    Number f -> round f

deserializeIntPair : Value -> (Int, Int)
deserializeIntPair val =
  case val of
    Array pair -> (deserializeInt <| pair !! 0, deserializeInt <| pair !! 1)

deserializeString : Value -> String
deserializeString val =
  case val of
    String s -> s
