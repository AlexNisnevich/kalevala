module Deserialize where

import Dict
import List (..)
import Json.Decode (..)

import Helpers (..)
import GameTypes (..)
import Player

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
    Array values -> map string values

player : Decoder Player
player = map Player.fromString string

mousePos : Decoder MousePos
mousePos = tuple2 (,) int int

windowDims : Decoder WindowDims
windowDims = tuple2 (,) int int