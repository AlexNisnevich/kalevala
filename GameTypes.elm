module GameTypes where

import Dict
import Dict (Dict)
import Random (Seed)

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
  turn : Player,
  board : Board,
  score : Score,
  deck : Deck,
  hands : Hands,
  heldPiece : Maybe Int,
  lastPlaced : Maybe Location,
  delta : Dict String String
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
            | StartGame GameType Deck Player String
            | GameStarted Deck Player Player String -- represents a StartGame message sent from the server
            | Pass
            | OpponentDisconnected
            | NoAction
            | ParseError String

type ClickEvent = Start
                | BoardClick
                | PieceInHand Player Int
                | PassButton
                | None

-- TODO: This doesn't belong here, but it needs to be used by both Voluspa.elm and Display.elm
-- and we need to avoid circular dependencies (it used to be in Voluspa.elm). Maybe we need a State.elm or something?
{- Is the game ongoing in the given state? -}
isOngoing : State -> Bool
isOngoing state =
  case state.gameState of
    Ongoing -> True
    Connected opponentName -> True
    _ -> False