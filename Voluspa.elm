module Voluspa where

import Array
import Dict
import Dict (Dict)
import Graphics.Element (Element)
import Graphics.Input.Field (Content)
import List
import List (..)
import Maybe (Maybe (..), withDefault)
import Mouse
import Random (Seed, initialSeed)
import Signal
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

{- Pick up a piece if it's the given player's turn, otherwise pick up nothing. 
   Returns the new state. -}
tryToPickUpPiece : Player -> Int -> State -> State
tryToPickUpPiece player idx state =
  if (state.turn == player) && (isOngoing state)
  then { state | heldPiece <- Just idx }
  else { state | heldPiece <- Nothing }

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
          StartGame gameType deck player playerName -> startGame gameType deck player
          GameStarted deck startPlayer localPlayer opponentName -> gameStarted deck startPlayer localPlayer opponentName
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
startGame : GameType -> Deck -> Player -> State
startGame gameType deck player =
  let players = Dict.fromList [("red", Human), ("blue", if gameType == HumanVsCpu then Cpu else Human)]
      (firstTile, hands, remainder) = getFirstTileHandsAndDeck deck
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

{- Start a HumanVsHumanRemote game after an opponent has been found.
   Returns the state corresponding to the start of the game. -}
gameStarted : Deck -> Player -> Player -> String -> State
gameStarted deck startPlayer localPlayer opponentName =
  let players = Dict.fromList [ ("red", if localPlayer == Red then Human else Remote)
                              , ("blue", if localPlayer == Blue then Human else Remote)]
      (firstTile, hands, remainder) = getFirstTileHandsAndDeck deck
  in
    { startState | gameType <- HumanVsHumanRemote
                 , gameState <- Connected opponentName
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
constructAction : ClickEvent -> Seed -> MousePos -> WindowDims -> GameType -> Content -> Action
constructAction clickType seed mousePos dims gameType playerName =
  let
    pos = Debug.watch "Mouse.position" mousePos
    click = Debug.watch "clickInput.signal" clickType
  in
    case clickType of
      Start -> StartGame gameType (shuffle deckContents seed) (sample [Red, Blue] seed) playerName.string
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
      sampledPlayerName = sampleOn signal <| subscribe Display.playerNameChannel
  in
    constructAction <~ signal ~ seedSignal ~ sampledMouse ~ Window.dimensions ~ sampledGameType ~ sampledPlayerName

{- Path to a Voluspa game server 
   (see https://github.com/neunenak/voluspa-server) -}
server : String
server = "ws://ec2-52-10-22-64.us-west-2.compute.amazonaws.com:22000"

{- stuff for tooltip -}

type Hovering = YesHovering (Time.Time, MousePos) | NoHovering Time.Time
tooltipState : Signal (Maybe MousePos) -- Just (x, y) when the mouse has been static for a while, Nothing otherwise
tooltipState =
  let
     mousePosWithTime : Signal (Time.Time, MousePos)
     mousePosWithTime = Debug.watch "MP: " Signal.sampleOn (Time.every (Time.millisecond*10)) (Time.timestamp Mouse.position)

     interval : Time.Time
     interval = 500 * Time.millisecond -- hover for 500 seconds before it registers

     foldFn : (Time.Time, MousePos) -> Hovering -> Hovering
     foldFn (tNew, (xNew, yNew)) oldState =
         case oldState of 
             NoHovering tOld -> if (tNew - tOld) > interval then YesHovering (tNew, (xNew, yNew)) else NoHovering tNew
             YesHovering (tOld, (xOld, yOld)) -> if xOld /= xNew || yOld /= yNew then NoHovering tNew else YesHovering (tNew, (xNew, yNew))

     intermediarySignal : Signal Hovering
     intermediarySignal = Signal.foldp foldFn (NoHovering 0) mousePosWithTime

  in 
     --Signal.foldp foldFn Nothing mousePosWithTime
    Signal.map (\h -> case h of
       YesHovering (t, mp) -> Just mp
       NoHovering _ -> Nothing)
       (Debug.watch "im" intermediarySignal)

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
    Display.render <~ state ~ Window.dimensions ~ (subscribe Display.gameTypeChannel) ~ (subscribe Display.playerNameChannel) ~ (Debug.watch "q" tooltipState)
