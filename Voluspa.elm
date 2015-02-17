module Voluspa where

import Array
import Dict
import Dict (Dict)
import Graphics.Element (Element)
import List
import List (..)
import Maybe (Maybe (..), withDefault)
import Mouse
import Random (Seed, initialSeed)
import Signal (..)
import Time
import Window
import WebSocket
import Json.Decode
import Json.Encode

import Helpers (..)
import GameTypes (..)
import Piece
import Board
import Player
import Display
import AI
import Serialize
import Deserialize

import Debug

{- Pick up a piece if it's the given player's turn, do nothing otherwise. 
   Returns the new state. -}
tryToPickUpPiece : Player -> Int -> State -> State
tryToPickUpPiece player idx state =
  if (state.turn == player) && (state.gameState == Ongoing)
  then pickUpPiece idx state
  else { state | heldPiece <- Nothing }

{- Pick up a piece at the given index. Returns the new state. -}
pickUpPiece : Int -> State -> State
pickUpPiece idx state =
  { state | heldPiece <- Just idx }

{- Pass the current player's turn. Returns the new state. -}
pass : State -> State
pass state =
  let p = Player.name state.turn
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
  let p = Player.name state.turn
      newBoard = Dict.insert move.location move.piece state.board
      delta = Board.scoreMove move newBoard
      newScore = (withDefault 0 <| Dict.get p state.score) + delta
      hand = Player.getHand state.turn state
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
            , heldPiece <- Nothing
            , lastPlaced <- Just move.location
            , delta <- Dict.insert p ("(+" ++ (toString delta) ++ ")") state.delta }

{- Perform an action on the current state and return the resulting state. -}
performAction : Action -> State -> State
performAction action state =
  let p = Player.name state.turn
      newState =
        case action of
          PickUpPiece player idx -> tryToPickUpPiece player idx state
          PlacePiece mousePos dims -> tryMove (Display.mouseToBoardPosition mousePos state dims) state
          StartGame gameType deck player -> startGame gameType deck player
          GameStarted deck startPlayer localPlayer -> gameStarted deck startPlayer localPlayer
          Pass -> { state | turn <- Player.next state.turn }
          OpponentDisconnected -> { state | gameState <- Disconnected }
          NoAction -> state
          ParseError e -> state
  in
    if | isGameOver newState -> { newState | gameState <- GameOver }
       | mustPass newState -> pass newState
       | otherwise -> newState

{- Does neither player have any tiles left in the given state? -}
isGameOver : State -> Bool
isGameOver state =
  (state.gameState == Ongoing) && (Player.noTilesInHand Red state) && (Player.noTilesInHand Blue state)

{- Must the current player pass in the given state? -}
mustPass : State -> Bool
mustPass state =
  Player.noTilesInHand state.turn state

{- Start a game of the given type, with the given starting deck and starting player.
   If the first player is an AI player, make their move.
   Returns the state corresponding to the start of the game. -}
startGame : GameType -> Deck -> Player -> State
startGame gameType deck player =
  let players = Dict.fromList [("red", Human), ("blue", if gameType == HumanVsCpu then Cpu else Human)]
      deckWithIndices = List.map2 (,) [0..(List.length deck - 1)] deck
      idxFirstNonTroll = fst <| head <| filter (\(idx, piece) -> not (piece == "Troll")) deckWithIndices
      firstTile = Piece.fromString (deck !! idxFirstNonTroll)
      deckMinusFirstTile = without idxFirstNonTroll deck
      redHand = take 5 deckMinusFirstTile
      blueHand = take 5 (drop 5 deckMinusFirstTile)
      hands = Dict.fromList [("red", redHand), ("blue", blueHand)]
      remainder = drop 10 deckMinusFirstTile
      state = if gameType == HumanVsHumanRemote
              then { startState | gameType <- gameType
                                , gameState <- WaitingForPlayers
                                , players <- players
                                , turn <- player}
              else { startState | gameType <- gameType
                                , gameState <- Ongoing
                                , players <- players
                                , hands <- hands
                                , deck <- remainder
                                , board <- Dict.singleton (0, 0) firstTile
                                , turn <- player}
  in
    -- if first player is Cpu, make their move
    if Player.getType state.turn state == Cpu
    then tryAIMove state
    else state

{- (only for HumanVsHumanRemote games) 
   This functions triggers when players are matched together into a game
   TODO: separate out common code between this and startGame! -}
gameStarted : Deck -> Player -> Player -> State
gameStarted deck startPlayer localPlayer =
  let players = Dict.fromList [ ("red", if localPlayer == Red then Human else Remote)
                              , ("blue", if localPlayer == Blue then Human else Remote)]
      deckWithIndices = List.map2 (,) [0..(List.length deck - 1)] deck
      idxFirstNonTroll = fst <| head <| filter (\(idx, piece) -> not (piece == "Troll")) deckWithIndices
      firstTile = Piece.fromString (deck !! idxFirstNonTroll)
      deckMinusFirstTile = without idxFirstNonTroll deck
      redHand = take 5 deckMinusFirstTile
      blueHand = take 5 (drop 5 deckMinusFirstTile)
      hands = Dict.fromList [("red", redHand), ("blue", blueHand)]
      remainder = drop 10 deckMinusFirstTile
  in
    { startState | gameType <- HumanVsHumanRemote
                 , gameState <- Ongoing
                 , players <- players
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
  , turn = Red
  , board = Dict.empty
  , score = Dict.fromList [("red", 0), ("blue", 0)]
  , deck = []
  , hands = Dict.fromList [("red", []), ("blue", [])]
  , heldPiece = Nothing
  , lastPlaced = Nothing
  , delta = Dict.fromList [("red", ""), ("blue", "")]
  }

{- Turn a ClickEvent into an Action -}
constructAction : ClickEvent -> Seed -> MousePos -> WindowDims -> GameType -> Action
constructAction clickType seed mousePos dims gameType =
  let
    pos = Debug.watch "Mouse.position" mousePos
    click = Debug.watch "clickInput.signal" clickType
  in
    case clickType of
      Start -> StartGame gameType (shuffle deckContents seed) (sample [Red, Blue] seed)
      BoardClick -> PlacePiece mousePos dims
      PieceInHand player idx -> PickUpPiece player idx
      PassButton -> Pass
      None -> NoAction

{- Turn a signal of ClickEvents into a signal of Actions -}
processClick : Signal ClickEvent -> Signal Action
processClick signal =
  let seedSignal = (initialSeed << round << fst) <~ Time.timestamp signal
      sampledMouse = sampleOn signal Mouse.position
      sampledGameType = sampleOn signal <| subscribe Display.gameTypeChannel
  in
    constructAction <~ signal ~ seedSignal ~ sampledMouse ~ Window.dimensions ~ sampledGameType

{- Path to a Voluspa game server 
   (see https://github.com/neunenak/voluspa-server) -}
server : String
server = "ws://ec2-52-10-22-64.us-west-2.compute.amazonaws.com:22000"

{- The main game loop. 
   The state is folded over time over a stream of Actions, coming from the client and from the server (for an online game.)
   Actions from the client are constructed by processing a signal of ClickEvents from Display.clickChannel.
   In the case the the game type is HumanVsHumanRemote, Actions are also sent to the server over a websocket,
   and the Actions that are received from the server are merged into the signal of client Actions.
   For each Action in this resulting signal, (performAction action state) returns the new state. -}
main : Signal Element
main =
  let
    encode action = Json.Encode.encode 0 (Serialize.action action)
    decode actionJson = case Json.Decode.decodeString Deserialize.action actionJson of Ok action -> action
                                                                                       Err e -> ParseError e

    action = processClick (subscribe Display.clickChannel)

    actionWithGameType = (\a t -> (a, t)) <~ action ~ (subscribe Display.gameTypeChannel)
    actionForRemote = (\(a, t) -> a) <~ keepIf (\(a, t) -> t == HumanVsHumanRemote) (NoAction, HumanVsCpu) actionWithGameType

    request = Debug.watch "request" <~ (encode <~ actionForRemote)
    response = Debug.watch "response" <~ WebSocket.connect server request
    responseAction = Debug.watch "deserialized" <~ (decode <~ response)

    state = foldp performAction startState (merge action responseAction)
  in
    Display.render <~ state ~ Window.dimensions
