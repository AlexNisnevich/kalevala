module State where

import GameTypes exposing (..)
import Player

{- Is the game ongoing in the given state? -}
isOngoing : State -> Bool
isOngoing state =
  case state.gameState of
    Ongoing -> True
    Connected opponentName -> True
    _ -> False

isNotStarted : State -> Bool
isNotStarted state =
  case state.gameState of
    NotStarted -> True
    WaitingForPlayers -> True
    _ -> False

{- Does neither player have any tiles left in the given state? -}
isGameOver : State -> Bool
isGameOver state =
  (isOngoing state) && (Player.noTilesInHand Red state) && (Player.noTilesInHand Blue state)

{- Must the current player pass in the given state? -}
mustPass : State -> Bool
mustPass state =
  Player.noTilesInHand state.turn state

{- Is it currently a Human's turn (as opposed to a Cpu or Remote)? -}
isPlayerTurn : State -> Bool
isPlayerTurn state =
  (isOngoing state) && ((Player.getType state.turn state) == Human)