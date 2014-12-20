module GameTypes where

import Dict
import Dict (Dict)
import Random (Seed)

type GameType = HumanVsCpu
              | HumanVsHumanLocal
              | HumanVsHumanRemote

type PlayerType = Human
                | Cpu

type Player = Red
            | Blue

type alias Move = { piece : Piece, idx : Int, location : Location }
type alias Location = (Int, Int)

type alias Board = Dict Location Piece
type alias Score = Dict String Int
type alias Deck = List String
type alias Hands = Dict String (List String)

type alias State = {
  players : Dict String PlayerType,
  turn : Player,
  board : Board,
  score : Score,
  deck : Deck,
  hands : Hands,
  started : Bool,
  heldPiece : Maybe Int,
  lastPlaced : Maybe Location,
  delta : Dict String String,
  gameOver : Bool
}

type Piece = Odin
           | Thor
           | Troll
           | Dragon
           | Fenrir
           | Skadi
           | Valkyrie
           | Loki
           | NoPiece  -- represents error in board lookup

type alias MousePos = (Int, Int)
type alias WindowDims = (Int, Int)

type Action = PickUpPiece Player Int
            | PlacePiece MousePos WindowDims
            | StartGame GameType Deck Player
            | Pass
            | NoAction

type ClickEvent = Start
                | BoardClick
                | PieceInHand Player Int
                | PassButton
                | None
