module GameTypes where

import Color exposing (Color)
import Dict
import Dict exposing (Dict)

type GameType = HumanVsCpu
              | HumanVsHumanLocal
              | HumanVsHumanRemote

type GameState = NotStarted
               | WaitingForPlayers
               | Ongoing
               | Connected String
               | GameOver
               | Disconnected

type PlayerType = Human
                | Cpu
                | Remote

type Player = Red
            | Blue

type alias Move = { piece : Piece, idx : Int, location : Location }
type alias Location = (Int, Int)

type alias Board = Dict Location Piece
type alias Score = Dict String Int
type alias Deck = List String
type alias Hands = Dict String (List String)

type alias State = {
  gameType : GameType,
  gameState : GameState,
  players : Dict String PlayerType,
  playerNames : Dict String String,
  turn : Player,
  board : Board,
  score : Score,
  deck : Deck,
  hands : Hands,
  heldPiece : Maybe Int,
  lastPlaced : Maybe Location,
  delta : Dict String String,
  log : List (Color, String)
}

type Piece = Vainamoinen
           | Ukko
           | Kullervo
           | Kaarme
           | Joukahainen
           | SeppoIlmarinen
           | Louhi
           | Lemminkainen
           | NoPiece  -- represents error in board lookup

type alias MousePos = (Int, Int)
type alias WindowDims = (Int, Int)

type Action = PickUpPiece Player Int
            | PlacePiece MousePos WindowDims
            | StartGame GameType Deck Player String
            | MoveToRemoteGameMenu
            | GameStarted Deck Player Player String -- represents a StartGame message sent from the server
            | Pass
            | OpponentDisconnected
            | NoAction
            | ParseError String

type ClickEvent = StartSinglePlayer
                | StartRemoteGameButton
                | StartTwoPlayerOnline
                | StartTwoPlayerHotseat
                | BoardClick
                | PieceInHand Player Int
                | PassButton
                | None
