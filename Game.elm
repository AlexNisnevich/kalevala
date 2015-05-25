module Game where

import Color
import Dict exposing (Dict)
import List exposing (..)
import Maybe exposing ( withDefault)

import Helpers exposing (..)
import GameTypes exposing (..)
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
  let p = Player.toString state.turn
  in
    { state | turn <- Player.next state.turn
            , delta <- Dict.insert p "(+0)" state.delta }

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
          nextPlayerType = Player.getType (Player.next state.turn) state
          nextAction = case nextPlayerType of
                         Human -> identity
                         Remote -> identity
                         Cpu -> tryAIMove
      in
        if (Board.isValidMove move state.board) then (makeMove move state |> nextAction) else { state | heldPiece <- Nothing }
    Nothing -> state

{- Try to make a move for the AI player. 
   If no valid move is found, pass.
   Returns the new state. -}
tryAIMove : State -> State
tryAIMove state =
  case AI.getMove state of
    Just move -> tryMove move.location { state | heldPiece <- Just move.idx }
    Nothing -> pass state

{- Make the given move. Returns the new state. -}
makeMove : Move -> State -> State
makeMove move state =
  let p = Player.toString state.turn
      newBoard = Dict.insert move.location move.piece state.board
      delta = Board.scoreMove move newBoard
      newScore = (withDefault 0 <| Dict.get p state.score) + delta
      hand = Player.getHand state.turn state
      existingTile = Dict.get move.location state.board
      handWithDrawnTile = without move.idx hand ++ (if (not <| List.isEmpty state.deck) then (take 1 state.deck) else [])
      newHand = case existingTile of
        Just piece -> if move.piece == SeppoIlmarinen then (replaceAtIndex move.idx (Piece.toString piece) hand) else handWithDrawnTile
        Nothing -> handWithDrawnTile
      logText = (withDefault "" <| Dict.get (Player.toString state.turn) state.playerNames) ++ " placed " ++ 
                  Piece.toDisplayString move.piece ++ " for " ++ toString delta ++ " points" ++ 
                  " (total : " ++ toString newScore ++ ")"
  in
    { state | turn <- Player.next state.turn
            , board <- newBoard
            , score <- Dict.insert p newScore state.score
            , deck <- drop 1 state.deck
            , hands <- Dict.insert p newHand state.hands
            , heldPiece <- Nothing
            , lastPlaced <- Just move.location
            , delta <- Dict.insert p ("(+" ++ (toString delta) ++ ")") state.delta
            , log <- ((Player.toColor state.turn), logText) :: state.log }

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
                              , (Player.toString <| Player.next player, if gameType == HumanVsCpu then Cpu else Human)
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
                                , turn <- player}
              else { startState | gameType <- gameType
                                , gameState <- Ongoing
                                , players <- players
                                , playerNames <- playerNames
                                , hands <- hands
                                , deck <- remainder
                                , board <- Dict.singleton (0, 0) firstTile
                                , turn <- player
                                , log <- [(Color.darkGrey, "Game started!")] }
  in
    -- if first player is Cpu, make their move
    if Player.getType state.turn state == Cpu
    then tryAIMove state
    else state

{- Start a HumanVsHumanRemote game after an opponent has been found.
   Returns the state corresponding to the start of the game. -}
gameStarted : Deck -> Player -> Player -> String -> State
gameStarted deck startPlayer localPlayer opponentName =
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
                 , log <- [(Color.darkGrey, "Connected to " ++ opponentName)] }

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
  , delta = Dict.fromList [("Red", ""), ("Blue", "")]
  , log = []
  }
