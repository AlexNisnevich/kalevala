module GameTypes where

import Dict
import Dict (Dict)

data PlayerType = Human
                | Cpu

data Player = Red
            | Blue

playerName : Player -> String
playerName player =
  case player of
    Red -> "red"
    Blue -> "blue"

type Move = { piece : Piece, idx : Int, location : Location }
type Location = (Int, Int)

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

pieceFromString : String -> Piece
pieceFromString str =
  case str of
    "Odin" -> Odin
    "Thor" -> Thor
    "Troll" -> Troll
    "Dragon" -> Dragon
    "Fenrir" -> Fenrir
    "Skadi" -> Skadi
    "Valkyrie" -> Valkyrie
    "Loki" -> Loki

pieceToString : Piece -> String
pieceToString piece =
  case piece of
    Odin -> "Odin"
    Thor -> "Thor"
    Troll -> "Troll"
    Dragon -> "Dragon"
    Fenrir -> "Fenrir"
    Skadi -> "Skadi"
    Valkyrie -> "Valkyrie"
    Loki -> "Loki"

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
