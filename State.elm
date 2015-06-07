module State where

import GameTypes exposing (..)
import Helpers exposing (..)
import Piece
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

isAtMainMenu : State -> Bool
isAtMainMenu state =
  state.gameState == NotStarted && state.gameType /= HumanVsHumanRemote

isSettingUpRemoteGame : State -> Bool
isSettingUpRemoteGame state =
  state.gameState == NotStarted && state.gameType == HumanVsHumanRemote

{- Does neither player have any tiles left in the given state? -}
isGameOver : State -> Bool
isGameOver state =
  (isOngoing state) && (Player.noTilesInHand Red state) && (Player.noTilesInHand Blue state)

{- Must the current player pass in the given state? -}
mustPass : State -> Bool
mustPass state =
  (isOngoing state) && (not <| isSwitchingPlayers state) && Player.noTilesInHand state.turn state

{- Is it currently a Human's turn (as opposed to a Cpu or Remote)? -}
isPlayerTurn : State -> Bool
isPlayerTurn state =
  (isOngoing state) && ((Player.getType state.turn state) == Human)

isSwitchingPlayers : State -> Bool
isSwitchingPlayers state =
  case state.turn of
    SwitchingTo _ -> True
    _ -> False

pieceHeld : State -> Maybe Piece
pieceHeld state =
  case state.heldPiece of
    Just idx ->
      let hand = Player.getHand state.turn state
          pieceStr = hand !! idx
      in
        Just (Piece.fromString pieceStr)
    Nothing -> Nothing

nextPlayer : State -> Player
nextPlayer state = 
  let next = Player.next state.turn
  in
    if state.gameType == HumanVsHumanLocal
    then SwitchingTo next
    else next

leadingPlayer : State -> Maybe Player
leadingPlayer state = 
  let redScore = getU "Red" state.score
      blueScore = getU "Blue" state.score
  in
    if | redScore > blueScore -> Just Red
       | blueScore > redScore -> Just Blue
       | otherwise -> Nothing

endStateMsg : State -> String
endStateMsg state =
  let redScore = getU "Red" state.score
      blueScore = getU "Blue" state.score
  in 
    if | redScore == blueScore -> "Tie game!"
       | redScore > blueScore -> getU "Red" state.playerNames ++ " wins!"
       | redScore < blueScore -> getU "Blue" state.playerNames ++ " wins!"