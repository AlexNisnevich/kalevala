module Kalevala where

import Dict
import Graphics.Element exposing (Element)
import Graphics.Input.Field as Field
import Json.Decode
import Json.Encode
import Mouse
import Random
import Signal exposing (..)
import Time
import Window

import Deprecated.WebSocket as WebSocket

import Helpers exposing (..)
import GameTypes exposing (..)
import State exposing (isGameOver, mustPass)
import Display exposing (..)
import Display.Board exposing (mouseToBoardPosition)
import Game
import Player
import Serialize
import Deserialize

import Debug

isRemoteSignal : Signal Bool
isRemoteSignal = 
  let getIsRemoteFromClickSignal event = 
    case event of
    StartSinglePlayer -> Just False
    StartTwoPlayerHotseat -> Just False
    StartTwoPlayerOnline -> Just True
    otherwise -> Nothing
  in
    filterMap getIsRemoteFromClickSignal False clickMailbox.signal

{- Perform an action on the current state and return the resulting state. -}
performAction : Action -> State -> State
performAction action state =
  let newState =
        case action of
          PickUpPiece player idx -> Game.tryToPickUpPiece player idx state
          PlacePiece mousePos dims -> Game.tryMove (mouseToBoardPosition mousePos state dims) state
          StartGame gameType deck player playerName -> Game.startGame gameType deck player playerName
          GameStarted deck startPlayer localPlayer opponentName -> Game.gameStarted deck startPlayer localPlayer opponentName
          Pass -> { state | turn <- Player.next state.turn }
          MoveToRemoteGameMenu -> { state | gameType <- HumanVsHumanRemote, gameState <- NotStarted }
          OpponentDisconnected -> { state | gameState <- Disconnected }
          NoAction -> state
          ParseError e -> state
  in
    if | isGameOver newState -> { newState | gameState <- GameOver }
       | mustPass newState -> Game.pass newState
       | otherwise -> newState

{- Turn a ClickEvent into an Action -}
constructAction : ClickEvent -> Random.Seed -> MousePos -> WindowDims -> Field.Content -> Action
constructAction clickType seed mousePos dims playerName =
  let
    pos = Debug.watch "Mouse.position" mousePos
    click = Debug.watch "clickInput.signal" clickType
    deck = shuffle Game.deckContents seed
  in
    case clickType of
      StartSinglePlayer -> StartGame HumanVsCpu deck (Player.random seed) playerName.string
      StartRemoteGameButton -> MoveToRemoteGameMenu
      StartTwoPlayerOnline -> StartGame HumanVsHumanRemote deck (Player.random seed) playerName.string
      StartTwoPlayerHotseat -> StartGame HumanVsHumanLocal deck (Player.random seed) playerName.string
      BoardClick -> PlacePiece mousePos dims
      PieceInHand player idx -> PickUpPiece player idx
      PassButton -> Pass
      None -> NoAction

{- Turn a signal of ClickEvents into a signal of Actions -}
processClick : Signal ClickEvent -> Signal Action
processClick signal =
  let seedSignal = (Random.initialSeed << round << fst) <~ Time.timestamp signal
      sampledMouse = sampleOn signal Mouse.position
      sampledPlayerName = sampleOn signal <| playerNameMailbox.signal
  in
    constructAction <~ signal ~ seedSignal ~ sampledMouse ~ Window.dimensions ~ sampledPlayerName

{- Path to a Voluspa game server 
   (see https://github.com/neunenak/voluspa-server) -}
server : String
server = "ws://ec2-52-10-22-64.us-west-2.compute.amazonaws.com:22000"

{- Encodes an Action as a JSON string -}
encode : Action -> String
encode action = Json.Encode.encode 0 (Serialize.action action)

{- Decodes an Action from a JSON string -}
decode : String -> Action
decode actionJson =
  case Json.Decode.decodeString Deserialize.action actionJson of
    Ok action -> action
    Err e -> ParseError e

{- The main game loop. 
   The state is folded over time over a stream of Actions, coming from the client and from the server (for an online game.)
   Actions from the client are constructed by processing a signal of ClickEvents from clickMailbox.
   In the case the the game type is HumanVsHumanRemote, Actions are also sent to the server over a websocket,
   and the Actions that are received from the server are merged into the signal of client Actions.
   For each Action in this resulting signal, (performAction action state) returns the new state. -}
main : Signal Element
main =
  let
    action = processClick clickMailbox.signal
    actionForRemote = filterOn action isRemoteSignal NoAction

    request = Debug.watch "request" <~ (encode <~ actionForRemote)
    response = Debug.watch "response" <~ WebSocket.connect server request
    responseAction = Debug.watch "deserialized" <~ (decode <~ response)

    state = foldp performAction Game.startState (merge action responseAction)
  in
    render <~ state ~ Window.dimensions ~ playerNameMailbox.signal