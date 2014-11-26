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

data Piece = Odin
           | Thor
           | Troll
           | Dragon
           | Fenrir
           | Skadi
           | Valkyrie
           | Loki

type MousePos = (Int, Int)
type WindowDims = (Int, Int)

data Action = PickUpPiece Player Int
            | PlacePiece MousePos WindowDims
            | StartGame Deck Player
            | Pass
            | NoAction

data ClickEvent = Start
                | Board
                | PieceInHand Player Int
                | PassButton
                | None
