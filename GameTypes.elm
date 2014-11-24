module GameTypes where

import Dict
import Dict (Dict)

data PlayerType = Human
                | Cpu

data Player = Red
            | Blue

type Move = { piece : Piece, idx : Int, location : Location }
type Location = (Float, Float) --TODO this probably ought to be (int, int)

type Board = Dict Location Piece
type Score = Dict String Int
type Deck = [String]
type Hands = Dict String [String]

data Piece = Odin
           | Thor
           | Troll
           | Dragon
           | Fenrir
           | Skadi
           | Valkyrie
           | Loki

type State = {
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
