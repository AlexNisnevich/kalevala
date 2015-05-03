module Kalevala where

import Dict
import Graphics.Element (Element)
import Graphics.Input.Field (Content)
import Json.Decode
import Json.Encode
import Mouse
import Random (Seed, initialSeed)
import Signal (..)
import Time
import WebSocket
import Window

import Helpers (..)
import GameTypes (..)
import Display
import Game
import Player
import Serialize
import Deserialize

import Debug

{- Perform an action on the current state and return the resulting state. -}
performAction : Action -> State -> State
performAction action state =
  let newState =
        case action of
          PickUpPiece player idx -> Game.tryToPickUpPiece player idx state
          PlacePiece mousePos dims -> Game.tryMove (Display.mouseToBoardPosition mousePos state dims) state
          StartGame gameType deck player playerName -> Game.startGame gameType deck player playerName
          GameStarted deck startPlayer localPlayer opponentName -> Game.gameStarted deck startPlayer localPlayer opponentName
          Pass -> { state | turn <- Player.next state.turn }
          OpponentDisconnected -> { state | gameState <- Disconnected }
          NoAction -> state
          ParseError e -> state
  in
    if | Game.isGameOver newState -> { newState | gameState <- GameOver }
       | Game.mustPass newState -> Game.pass newState
       | otherwise -> newState

{- Turn a ClickEvent into an Action -}
constructAction : ClickEvent -> Seed -> MousePos -> WindowDims -> GameType -> Content -> Action
constructAction clickType seed mousePos dims gameType playerName =
  let
    pos = Debug.watch "Mouse.position" mousePos
    click = Debug.watch "clickInput.signal" clickType
  in
    case clickType of
      Start -> StartGame gameType (shuffle Game.deckContents seed) (Player.random seed) playerName.string
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

    state = foldp performAction Game.startState (merge action responseAction)

  in
    Display.render <~ state ~ Window.dimensions ~ (subscribe Display.gameTypeChannel) ~ (subscribe Display.playerNameChannel)
