module Game where

import Dict
import Dict (Dict)
import List
import List (..)
import Maybe (Maybe (..), withDefault)

import Helpers (..)
import GameTypes (..)
import Piece
import Board
import Player
import AI

{- Pick up a piece if it's the given player's turn, otherwise pick up nothing. 
   Returns the new state. -}
tryToPickUpPiece : Player -> Int -> State -> State
tryToPickUpPiece player idx state =
  if (state.turn == player) && (isOngoing state)
  then { state | heldPiece <- Just idx }
  else state

{- Pass the current player's turn. Returns the new state. -}
pass : State -> State
pass state =
  let p = Player.color state.turn
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
          pieceStr = head <| drop idx hand
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
  let p = Player.color state.turn
      newBoard = Dict.insert move.location move.piece state.board
      delta = Board.scoreMove move newBoard
      newScore = (withDefault 0 <| Dict.get p state.score) + delta
      hand = Player.getHand state.turn state
      existingTile = Dict.get move.location state.board
      handWithDrawnTile = without move.idx hand ++ (if (not <| List.isEmpty state.deck) then (take 1 state.deck) else [])
      newHand = case existingTile of
        Just piece -> if move.piece == Skadi then (replaceAtIndex move.idx (Piece.toString piece) hand) else handWithDrawnTile
        Nothing -> handWithDrawnTile
      logText = (withDefault "" <| Dict.get (Player.color state.turn) state.playerNames) ++ " placed a " ++ 
                  Piece.toString move.piece ++ " for " ++ toString delta ++ " points" ++ 
                  " (total: " ++ toString newScore ++ ")"
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

{- Does neither player have any tiles left in the given state? -}
isGameOver : State -> Bool
isGameOver state =
  (isOngoing state) && (Player.noTilesInHand Red state) && (Player.noTilesInHand Blue state)

{- Must the current player pass in the given state? -}
mustPass : State -> Bool
mustPass state =
  Player.noTilesInHand state.turn state

{- Given a deck, returns the starting center tile (which must not be a Troll),
   two 5-tile hands, and the remaining deck. -}
getFirstTileHandsAndDeck : Deck -> (Piece, Hands, Deck)
getFirstTileHandsAndDeck deck =
  let deckWithIndices = List.map2 (,) [0..(List.length deck - 1)] deck
      idxFirstNonTroll = fst <| head <| filter (\(idx, piece) -> not (piece == "Troll")) deckWithIndices
      firstTile = Piece.fromString (deck !! idxFirstNonTroll)
      deckMinusFirstTile = without idxFirstNonTroll deck
      redHand = take 5 deckMinusFirstTile
      blueHand = take 5 (drop 5 deckMinusFirstTile)
      hands = Dict.fromList [("red", redHand), ("blue", blueHand)]
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
  let players = Dict.fromList [ (Player.color player, Human)
                              , (Player.color <| Player.next player, if gameType == HumanVsCpu then Cpu else Human)
                              ]
      playerNames = Dict.fromList [ (Player.color player, if gameType == HumanVsCpu then "You" else Player.color player)
                                  , (Player.color <| Player.next player, if gameType == HumanVsCpu then "CPU" else Player.color <| Player.next player)
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
                                , turn <- player}
  in
    -- if first player is Cpu, make their move
    if Player.getType state.turn state == Cpu
    then tryAIMove state
    else state

{- Start a HumanVsHumanRemote game after an opponent has been found.
   Returns the state corresponding to the start of the game. -}
gameStarted : Deck -> Player -> Player -> String -> State
gameStarted deck startPlayer localPlayer opponentName =
  let players = Dict.fromList [ ("red", if localPlayer == Red then Human else Remote)
                              , ("blue", if localPlayer == Blue then Human else Remote)
                              ]
      playerNames = Dict.fromList [ (Player.color localPlayer, "You")
                                  , (Player.color <| Player.next localPlayer, opponentName)
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
                 , turn <- startPlayer}

{- The cards in a Voluspa deck -}
deckContents : List String
deckContents =
    let r = List.repeat
    in
      r 6 "Odin" ++
      r 8 "Thor" ++
      r 6 "Troll" ++
      r 8 "Dragon" ++
      r 8 "Fenrir" ++
      r 9 "Skadi" ++
      r 9 "Valkyrie" ++
      r 6 "Loki"

{- The initial state on page load (before a game is started) -}
startState : State
startState =
  { gameType = HumanVsCpu
  , gameState = NotStarted
  , players = Dict.fromList [("red", Human), ("blue", Cpu)]
  , playerNames = Dict.fromList [("red", "Player"), ("blue", "CPU")]
  , turn = Red
  , board = Dict.empty
  , score = Dict.fromList [("red", 0), ("blue", 0)]
  , deck = []
  , hands = Dict.fromList [("red", []), ("blue", [])]
  , heldPiece = Nothing
  , lastPlaced = Nothing
  , delta = Dict.fromList [("red", ""), ("blue", "")]
  , log = []
  }
