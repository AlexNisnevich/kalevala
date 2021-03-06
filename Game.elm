module Game where

import Color
import Dict exposing (Dict)
import List exposing (..)
import Maybe exposing (withDefault)

import Helpers exposing (..)
import GameTypes exposing (..)
import Log
import State
import Piece
import Board
import Player
import AI

{- Pick up a piece if it's the given player's turn, otherwise pick up nothing.
   If the piece is already picked up, put it down.
   Returns the new state. -}
tryToPickUpPiece : Player -> Int -> State -> State
tryToPickUpPiece player idx state =
  if (state.turn == player) && (State.isOngoing state)
  then
    if state.heldPiece == Just idx
    then { state | heldPiece <- Nothing }
    else {state | heldPiece <- Just idx }
  else state
 
{- Pass the current player's turn. Returns the new state. -}
pass : State -> State
pass state =
  let logMsg = (withDefault "" <| Dict.get (Player.toString state.turn) state.playerNames) ++ " passed."
  in
    { state | turn <- State.nextPlayer state
            , log <- Log.addPlayerMsg logMsg state.turn state.log }

{- Move the currently held piece to the given location if possible.
   Do nothing if there is no held piece or the move is invalid.
   Returns the new state. -}
tryMove : Location -> State -> State
tryMove location state =
  case state.heldPiece of
    Just idx ->
      let hand = Player.getHand state.turn state
          pieceStr = hand !! idx
          piece = Piece.fromString pieceStr
          move = { piece = piece, idx = idx, location = location }
          nextPlayerType = Player.getType (State.nextPlayer state) state
      in
        if Board.isValidMove move state.board
        then makeMove move state
        else { state | heldPiece <- Nothing }
    Nothing -> state

{- Try to make a move for the AI player.
   If no valid move is found, pass.
   If it's not the AI player's turn, do nothing.
   Returns the new state. -}
tryAIMove : State -> State
tryAIMove state =
  if State.isOngoing state && Player.getType state.turn state == Cpu
  then
    case AI.getMove state of
      Just move -> tryMove move.location { state | heldPiece <- Just move.idx }
      Nothing -> pass state
  else state

{- Make the given move. Returns the new state. -}
makeMove : Move -> State -> State
makeMove move state =
  let p = Player.toString state.turn
      newBoard = Dict.insert move.location move.piece state.board
      delta = Board.scoreMove move newBoard
      newScore = (withDefault 0 <| Dict.get p state.score) + delta
      newScores = Dict.insert p newScore state.score
      hand = Player.getHand state.turn state
      existingTile = Dict.get move.location state.board
      handWithDrawnTile = without move.idx hand ++ (if (not <| List.isEmpty state.deck) then (take 1 state.deck) else [])
      newHand = case existingTile of
        Just piece -> if move.piece == SeppoIlmarinen then (replaceAtIndex move.idx (Piece.toString piece) hand) else handWithDrawnTile
        Nothing -> handWithDrawnTile

      logText = (getU (Player.toString state.turn) state.playerNames) ++ " placed " ++
                  Piece.toDisplayString move.piece ++ " for " ++ toString delta ++ " points" ++
                  " (total : " ++ toString newScore ++ ")"
      leadChangeText = (getU (Player.toString state.turn) state.playerNames) ++ " took the lead!"
      newLog = if State.leadingPlayer {state | score <- newScores} == Just state.turn && State.leadingPlayer state /= Just state.turn
               then state.log |> Log.addPlayerMsg logText state.turn |> Log.addPlayerMsg leadChangeText state.turn
               else state.log |> Log.addPlayerMsg logText state.turn
  in
    { state | turn <- State.nextPlayer state
            , board <- newBoard
            , score <- newScores
            , deck <- drop 1 state.deck
            , hands <- Dict.insert p newHand state.hands
            , heldPiece <- Nothing
            , lastPlaced <- Just move.location
            , lastPlacedPlayer <- Just state.turn
            , log <- newLog }

{- Given a deck, returns the starting center tile (which must not be a Kullervo),
   two 5-tile hands, and the remaining deck. -}
getFirstTileHandsAndDeck : Deck -> (Piece, Hands, Deck)
getFirstTileHandsAndDeck deck =
  let deckWithIndices = List.map2 (,) [0..(List.length deck - 1)] deck
      (idxFirstNonKullervo, pieceFirstNonKullervo) = headU <| filter (\(idx, piece) -> not (piece == "Kullervo")) deckWithIndices
      firstTile = Piece.fromString pieceFirstNonKullervo
      deckMinusFirstTile = without idxFirstNonKullervo deck
      redHand = take 5 deckMinusFirstTile
      blueHand = take 5 (drop 5 deckMinusFirstTile)
      hands = Dict.fromList [("Red", redHand), ("Blue", blueHand)]
      remainder = drop 10 deckMinusFirstTile
  in
    (firstTile, hands, remainder)

{- Start a game of the given type, with the given starting deck and starting player.
   If the first player is an AI player, make their move.
   Returns the state corresponding to the start of the game.
   NOTE: For HumanVsHumanRemote games, startGame triggers when matchmaking begins, and
         gameStarted triggers when a match has been found. -}
startGame : GameType -> Deck -> Player -> String -> State
startGame gameType deck player playerName =
  let players = Dict.fromList [ (Player.toString player, Human)
                              , (Player.toString <| Player.next player, case gameType of
                                                                          HumanVsCpu -> Cpu
                                                                          HumanVsHumanLocal -> Human
                                                                          HumanVsHumanRemote -> Remote)
                              ]
      playerNames = Dict.fromList [ (Player.toString player, if gameType == HumanVsCpu then "You" else Player.toString player)
                                  , (Player.toString <| Player.next player, if gameType == HumanVsCpu then "CPU" else Player.toString <| Player.next player)
                                  ]
      (firstTile, hands, remainder) = getFirstTileHandsAndDeck deck
      state = if gameType == HumanVsHumanRemote
              then { startState | gameType <- gameType
                                , gameState <- WaitingForPlayers
                                , players <- players
                                , playerNames <- playerNames
                                , turn <- player
                                , log <- Log.empty |> Log.addSystemMsg (playerName ++ " joined the game.")
                                                   |> Log.addSystemMsg "Waiting for opponent . . ."
                                }
              else { startState | gameType <- gameType
                                , gameState <- Ongoing
                                , players <- players
                                , playerNames <- playerNames
                                , hands <- hands
                                , deck <- remainder
                                , board <- Dict.singleton (0, 0) firstTile
                                , turn <- player
                                , log <- Log.singleton "Game started." Color.darkGrey }
  in
    -- if first player is Cpu, make their move
    if Player.getType state.turn state == Cpu
    then tryAIMove state
    else state

{- Start a HumanVsHumanRemote game after an opponent has been found.
   Returns the state corresponding to the start of the game. -}
gameStarted : Deck -> Player -> Player -> String -> State -> State
gameStarted deck startPlayer localPlayer opponentName state =
  let players = Dict.fromList [ ("Red", if localPlayer == Red then Human else Remote)
                              , ("Blue", if localPlayer == Blue then Human else Remote)
                              ]
      playerNames = Dict.fromList [ (Player.toString localPlayer, "You")
                                  , (Player.toString <| Player.next localPlayer, opponentName)
                                  ]
      (firstTile, hands, remainder) = getFirstTileHandsAndDeck deck
  in
    { startState | gameType <- HumanVsHumanRemote
                 , gameState <- Connected opponentName
                 , players <- players
                 , playerNames <- playerNames
                 , hands <- hands
                 , deck <- remainder
                 , board <- Dict.singleton (0, 0) firstTile
                 , turn <- startPlayer
                 , log <- state.log |> Log.addSystemMsg (opponentName ++ " joined the game.")
                                    |> Log.addSystemMsg "Game started."
                 }

{- The cards in a Voluspa deck -}
deckContents : List String
deckContents =
    let r = List.repeat
    in
      r 6 "Vain" ++
      r 8 "Ukko" ++
      r 6 "Kullervo" ++
      r 8 "Kaarme" ++
      r 8 "Jouk" ++
      r 9 "Ilmar" ++
      r 9 "Louhi" ++
      r 6 "Lemmi"

{- The initial state on page load (before a game is started) -}
startState : State
startState =
  { gameType = HumanVsCpu
  , gameState = NotStarted
  , players = Dict.fromList [("Red", Human), ("Blue", Cpu)]
  , playerNames = Dict.fromList [("Red", "Player"), ("Blue", "CPU")]
  , turn = Red
  , board = Dict.empty
  , score = Dict.fromList [("Red", 0), ("Blue", 0)]
  , deck = []
  , hands = Dict.fromList [("Red", []), ("Blue", [])]
  , heldPiece = Nothing
  , lastPlaced = Nothing
  , lastPlacedPlayer = Nothing
  , log = Log.empty
  }
