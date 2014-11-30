module Voluspa where

import Array
import List
import Dict
import Dict (Dict)
import Mouse
import Graphics.Input (Input, input)
import Window
import WebSocket
import Json

import Helpers (..)
import GameTypes (..)
import Piece
import Board
import Player
import Display
import AI
import Serialize (..)

import Debug

tryToPickUpPiece : Player -> Int -> State -> State
tryToPickUpPiece player idx state =
  if state.turn == player
  then pickUpPiece idx state
  else { state | heldPiece <- Nothing }

pickUpPiece : Int -> State -> State
pickUpPiece idx state =
  { state | heldPiece <- Just idx }

pass : State -> State
pass state =
  let p = playerName state.turn
  in
    { state | turn <- Player.next state.turn
            , delta <- Dict.insert p "(+0)" state.delta }

tryMove : Location -> State -> State
tryMove location state =
  case state.heldPiece of
    Just idx ->
      let p = playerName state.turn
          hand = Dict.getOrFail p state.hands
          pieceStr = head <| drop idx hand
          piece = Piece.fromString pieceStr
          move = { piece = piece, idx = idx, location = location }
          nextPlayerType = Dict.getOrFail (playerName <| Player.next state.turn) state.players
          nextAction = case nextPlayerType of
                         Human -> identity
                         Cpu -> tryAIMove
      in
        if (Board.isValidMove move state.board) then (makeMove move state |> nextAction) else { state | heldPiece <- Nothing }
    Nothing -> state

tryAIMove : State -> State
tryAIMove state =
  case AI.getMove state of
    Just move -> tryMove move.location { state | heldPiece <- Just move.idx }
    Nothing -> pass state

makeMove : Move -> State -> State
makeMove move state =
  let p = playerName state.turn
      newBoard = Dict.insert move.location move.piece state.board
      delta = Board.scoreMove move newBoard
      newScore = (Dict.getOrFail p state.score) + delta
      hand = Dict.getOrFail p state.hands
      existingTile = Dict.get move.location state.board
      handWithDrawnTile = without move.idx hand ++ (if (not <| List.isEmpty state.deck) then (take 1 state.deck) else [])
      newHand = case existingTile of
        Just piece -> if move.piece == Skadi then (replaceAtIndex move.idx (Piece.toString piece) hand) else handWithDrawnTile
        Nothing -> handWithDrawnTile
  in
    { state | turn <- Player.next state.turn
            , board <- newBoard
            , score <- Dict.insert p newScore state.score
            , deck <- drop 1 state.deck
            , hands <- Dict.insert p newHand state.hands
            , started <- True
            , heldPiece <- Nothing
            , lastPlaced <- Just move.location
            , delta <- Dict.insert p ("(+" ++ (show delta) ++ ")") state.delta }

performAction : Action -> State -> State
performAction action state =
  let p = playerName state.turn
      newState =
        case action of
          PickUpPiece player idx -> tryToPickUpPiece player idx state
          PlacePiece mousePos dims -> tryMove (Display.mouseToBoardPosition mousePos state dims) state
          StartGame deck player -> startGame deck player
          Pass -> { state | turn <- Player.next state.turn }
          NoAction -> state
  in
    if | isGameOver newState -> { newState | gameOver <- True }
       | mustPass newState -> pass newState
       | otherwise -> newState

isGameOver : State -> Bool
isGameOver state =
  (Player.noTilesInHand Red state) && (Player.noTilesInHand Blue state)

mustPass : State -> Bool
mustPass state =
  Player.noTilesInHand state.turn state

startGame : Deck -> Player -> State
startGame deck player =
  let deckWithIndices = zip [0..(List.length deck - 1)] deck
      idxFirstNonTroll = fst <| head <| filter (\(idx, piece) -> not (piece == "Troll")) deckWithIndices
      firstTile = Piece.fromString (deck !! idxFirstNonTroll)
      deckMinusFirstTile = without idxFirstNonTroll deck
      redHand = take 5 deckMinusFirstTile
      blueHand = take 5 (drop 5 deckMinusFirstTile)
      hands = Dict.fromList [("red", redHand), ("blue", blueHand)]
      remainder = drop 10 deckMinusFirstTile
      state = { startState | hands <- hands
                           , deck <- remainder
                           , started <- True
                           , board <- Dict.singleton (0, 0) firstTile
                           , turn <- player}
  in
    -- if first player is Cpu, make their move
    if Dict.getOrFail (playerName state.turn) state.players == Cpu
    then tryAIMove state
    else state

clickInput : Input ClickEvent
clickInput = input None

deckContents : [String]
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

startState : State
startState =
  { players = Dict.fromList [("red", Human), ("blue", Cpu)]
  , turn = Red
  , board = Dict.empty
  , score = Dict.fromList [("red", 0), ("blue", 0)]
  , deck = []
  , hands = Dict.fromList [("red", []), ("blue", [])]
  , started = False
  , heldPiece = Nothing
  , lastPlaced = Nothing
  , delta = Dict.fromList [("red", ""), ("blue", "")]
  , gameOver = False
  }

constructAction : ClickEvent -> (Deck, Player) -> MousePos -> WindowDims -> Action
constructAction clickType (startDeck, startPlayer) mousePos dims =
  let
    pos = (Debug.watch "Mouse.position" mousePos)
    click = (Debug.watch "clickInput.signal" clickType)
  in
    case clickType of
      Start -> StartGame startDeck startPlayer
      Board -> PlacePiece mousePos dims
      PieceInHand player idx -> PickUpPiece player idx
      PassButton -> Pass
      None -> NoAction

processClick : Signal ClickEvent -> Signal Action
processClick signal =
  let startState = (,) <~ (shuffle deckContents signal) ~ (head <~ shuffle [Red, Blue] signal)
      sampledMouse = sampleOn signal Mouse.position
  in
    constructAction <~ signal ~ startState ~ sampledMouse ~ Window.dimensions

main : Signal Element
main =
  let
    action = processClick clickInput.signal
    socket = Debug.watch "socket" <~ WebSocket.connect "ws://echo.websocket.org" ((Json.toString "" << serializeAction) <~ action)
    state = foldp performAction startState action
  in
    Display.render clickInput <~ state ~ Window.dimensions
