module Voluspa where

import Array
import List
import Random
import Dict
import Dict (Dict)
import Mouse
import Graphics.Collage (Form)
import Graphics.Input (..)
import String
import Text
import Window

import Debug

import Helpers (..)
import GameTypes (..)
import Board (..)
import AI

-- TYPES

type MousePos = (Int, Int)
type WindowDims = (Int, Int)

data Action = PickUpPiece Player Int
            | PlacePiece MousePos WindowDims
            | StartGame Deck
            | Pass
            | NoAction

data ClickEvent = Start
                | Board
                | PieceInHand Player Int
                | PassButton
                | None

-- GLOBAL CONSTANTS

gameHeaderSize : Int
gameHeaderSize = 100

handPadding : Int
handPadding = 10

handTileSize : Float
handTileSize = 100

-- MAGIC STRINGS

playerColor : Player -> Color
playerColor player =
  case player of
    Red -> red
    Blue -> blue

-- HELPERS

shuffle : [a] -> Signal b -> Signal [a]
shuffle list signal =
  let randomsFromSignal signal = Random.floatList <| lift (\x -> List.length list) signal
      shuffleWithRandoms list randoms =
        if List.isEmpty list
        then []
        else
          let i = floor (head randoms * toFloat (List.length list))
          in
            [list !! i] ++ (shuffleWithRandoms (without i list) (tail randoms))
  in
    lift2 shuffleWithRandoms (constant list) (randomsFromSignal signal)

-- BOARD

getTotalBoardSize : WindowDims -> Int
getTotalBoardSize (width, height) = height - gameHeaderSize

getTileSizeFromBoardSize : Int -> WindowDims -> Float
getTileSizeFromBoardSize boardSize dims = toFloat (getTotalBoardSize dims // boardSize)

mouseToBoardPosition: MousePos -> State -> WindowDims -> Location
mouseToBoardPosition (x', y') state dims =
  let x = x'
      y = (y' - gameHeaderSize)
      boardSize = getBoardSize state
      tileSize = round <| getTileSizeFromBoardSize boardSize dims
      offset = boardSize // 2
      boardX = (x // tileSize) - offset
      boardY = 0 - ((y // tileSize) - offset)
  in (boardX, boardY)

-- MOVES

tryToPickUpPiece : Player -> Int -> State -> State
tryToPickUpPiece player idx state =
  if state.turn == player
  then pickUpPiece idx state
  else { state | heldPiece <- Nothing }

pickUpPiece : Int -> State -> State
pickUpPiece idx state =
  { state | heldPiece <- Just idx }

pass : State -> State
pass state = { state | turn <- nextPlayer state.turn }

tryMove : Location -> State -> State
tryMove location state =
  case state.heldPiece of
    Just idx ->
      let p = playerName state.turn
          hand = Dict.getOrFail p state.hands
          pieceStr = head <| drop idx hand
          piece = pieceFromString pieceStr
          move = { piece = piece, idx = idx, location = location }
          nextPlayerType = Dict.getOrFail (playerName <| nextPlayer state.turn) state.players
          nextAction = case nextPlayerType of
                         Human -> identity
                         Cpu -> \state ->
                                    case AI.getMove state of
                                        Just move -> tryMove move.location { state | heldPiece <- Just move.idx }
                                        Nothing -> pass state
      in
        if (isValidMove move state) then (makeMove move state |> nextAction) else { state | heldPiece <- Nothing }
    Nothing -> state

makeMove : Move -> State -> State
makeMove move state =
  let p = playerName state.turn
      newBoard = Dict.insert move.location move.piece state.board
      delta = scoreMove move { state | board <- newBoard }
      newScore = (Dict.getOrFail p state.score) + delta
      hand = Dict.getOrFail p state.hands
      existingTile = Dict.get move.location state.board
      handWithDrawnTile = without move.idx hand ++ (if (not <| List.isEmpty state.deck) then (take 1 state.deck) else [])
      newHand = case existingTile of
        Just piece -> if move.piece == Skadi then (replaceAtIndex move.idx (pieceToString piece) hand) else handWithDrawnTile
        Nothing -> handWithDrawnTile
  in
    { state | turn <- nextPlayer state.turn
            , board <- newBoard
            , score <- Dict.insert p newScore state.score
            , deck <- drop 1 state.deck
            , hands <- Dict.insert p newHand state.hands
            , started <- True
            , heldPiece <- Nothing
            , lastPlaced <- Just move.location
            , delta <- Dict.insert p (String.concat ["(+", show delta, ")"]) state.delta }


nextPlayer : Player -> Player
nextPlayer player =
  case player of
    Red -> Blue
    Blue -> Red

-- GAME

performAction : Action -> State -> State
performAction action state =
  let p = playerName state.turn
      newState =
        case action of
          PickUpPiece player idx -> tryToPickUpPiece player idx state
          PlacePiece mousePos dims -> tryMove (mouseToBoardPosition mousePos state dims) state
          StartGame deck -> startGame deck
          Pass -> { state | turn <- nextPlayer state.turn }
          NoAction -> state
  in
    if | isGameOver newState -> { newState | gameOver <- True }
       | mustPass newState -> { newState | turn <- nextPlayer newState.turn
                                         , delta <- Dict.insert p "(+0)" newState.delta }
       | otherwise -> newState

noTilesInHand : Player -> State -> Bool
noTilesInHand player state =
  let p = playerName player
  in
    isEmpty <| Dict.getOrFail p state.hands

isGameOver : State -> Bool
isGameOver state =
  (noTilesInHand Red state) && (noTilesInHand Blue state)

mustPass : State -> Bool
mustPass state =
  noTilesInHand state.turn state

startGame : Deck -> State
startGame deck =
  let deckWithIndices = zip [0..(List.length deck - 1)] deck
      idxFirstNonTroll = fst <| head <| filter (\(idx, piece) -> not (piece == "troll")) deckWithIndices
      firstTile = pieceFromString (deck !! idxFirstNonTroll)
      deckMinusFirstTile = without idxFirstNonTroll deck
      redHand = take 5 deckMinusFirstTile
      blueHand = take 5 (drop 5 deckMinusFirstTile)
      hands = Dict.fromList [("red", redHand), ("blue", blueHand)]
      remainder = drop 10 deckMinusFirstTile
      newState = { startState | hands <- hands
                              , deck <- remainder
                              , started <- True
                              , board <- Dict.singleton (0, 0) firstTile }
  in
    -- if first player is Cpu, make their move
    if Dict.getOrFail (playerName newState.turn) newState.players == Cpu
    then newState --TODO this doesn't make sense in the cae where the first player is the AI, but we'll fix it later
    else newState

-- DISPLAY

clickInput : Input ClickEvent
clickInput = input None

pieceToImage: Piece -> Float -> Element
pieceToImage piece tileSize =
  let imgPath =
        case piece of
          Odin -> "images/tile_7.jpg"
          Thor -> "images/tile_6.jpg"
          Troll -> "images/tile_5.jpg"
          Dragon -> "images/tile_4.jpg"
          Fenrir -> "images/tile_3.jpg"
          Skadi -> "images/tile_2.jpg"
          Valkyrie -> "images/tile_1.jpg"
          Loki -> "images/tile_0.jpg"
  in
    image (round tileSize) (round tileSize) imgPath

drawGrid : Int -> WindowDims -> [Form]
drawGrid boardSize dims =
  let num = toFloat boardSize
      tileSize = getTileSizeFromBoardSize boardSize dims
      size = num * tileSize
      offset = tileSize / 2 - size / 2
      shape x y = move (tileSize * x + offset, tileSize * y + offset) (outlined (solid black) (square tileSize))
  in
    (concatMap (\x -> (map (\y -> shape x y) [0..(num - 1)])) [0..(num - 1)])

drawPiece : (Location, Piece) -> Float -> Form
drawPiece ((x', y'), piece) tileSize =
  let x = toFloat x' * tileSize
      y = toFloat y' * tileSize
  in
    move (x, y) (toForm (pieceToImage piece tileSize))

drawLastPlacedOutline : State -> Float -> [Form]
drawLastPlacedOutline state tileSize =
  case state.lastPlaced of
    Just (x, y) ->
      let thick c = { defaultLine | color <- c, width <- 4}
          lastPlacedColor = playerColor <| nextPlayer state.turn
          lastPlacedOutline = move (tileSize * (toFloat x), tileSize * (toFloat y)) (outlined (thick lastPlacedColor) (square (tileSize + 4)))
      in [lastPlacedOutline]
    Nothing -> []

renderBoard : State -> Int -> WindowDims -> Element
renderBoard state boardSize dims =
  let tileSize = getTileSizeFromBoardSize boardSize dims
      size = boardSize * (round tileSize) + 1

      grid = drawGrid boardSize dims
      pieces = map (\p -> drawPiece p tileSize) (Dict.toList state.board)
      outline = drawLastPlacedOutline state tileSize
  in
    collage size size (grid ++ pieces ++ outline)

renderHand : Player -> State -> Element
renderHand player state =
  let p = playerName player
      playerType = Dict.getOrFail p state.players
      hand = Dict.getOrFail p state.hands
      isPieceHeld idx = state.turn == player && state.heldPiece == Just idx
      pieceImage pieceStr = pieceToImage (pieceFromString pieceStr)
      pieceSize = (round handTileSize) + handPadding
      makePiece idx pieceStr = pieceImage pieceStr handTileSize |> container pieceSize pieceSize middle
                                                                |> color (if isPieceHeld idx then (playerColor state.turn) else white)
                                                                |> clickable clickInput.handle (PieceInHand player idx)
      playerHand = if isEmpty hand && (not state.gameOver)
                   then [button clickInput.handle PassButton "Pass" |> container 100 100 middle]
                   else indexedMap makePiece hand
      hiddenPiece = image (round handTileSize) (round handTileSize) "images/tile_back.jpg"
      cpuHand = map (\x -> hiddenPiece |> container pieceSize pieceSize middle) hand
      handContents = if Dict.getOrFail p state.players == Human
                     then playerHand
                     else cpuHand
      handText = playerType |> show
                            |> String.toUpper
                            |> toText
                            |> (if state.turn == player && (not state.gameOver) then bold else identity)
                            |> Text.color (playerColor player)
                            |> leftAligned
                            |> container 80 pieceSize middle
      score = Dict.getOrFail p state.score |> asText
                                           |> container 25 pieceSize midLeft
      delta = Dict.getOrFail p state.delta |> toText
                                           |> Text.height 9
                                           |> leftAligned
                                           |> container 20 pieceSize midLeft
  in
    flow right ([handText] ++ [score] ++ [delta] ++ handContents)

rulesRow : Piece -> Int -> String -> Element
rulesRow piece value description =
  let image = pieceToImage piece 50 `beside` spacer 10 10
      text = flow right [ leftAligned <| bold <| toText <| concat [pieceToString piece, " (", show value, "): "]
                        , plainText description
                        ]
  in
    image `beside` container 600 50 midLeft text

display : State -> WindowDims -> Element
display state dims =
  let boardSize = getBoardSize state
      totalBoardSize = getTotalBoardSize dims
      tileSize = getTileSizeFromBoardSize boardSize dims
      handGap = totalBoardSize - 2 * (round handTileSize) - (handPadding * 2)
      withSpacing padding elt = spacer padding padding `beside` elt
      rulesAreaWidth = 650
      minRulesHeight = 570
      pieceRules = flow down [ rulesRow Odin 8 "No special power"
                             , rulesRow Thor 7 "No special power"
                             , rulesRow Troll 6 "No other tiles (except Trolls) may be placed adjacent to a Troll."
                             , rulesRow Dragon 5 "May be placed on top of other tiles (except other Dragons)."
                             , rulesRow Fenrir 4 "Value is the sum of all Fenrir tiles in the same row or column."
                             , rulesRow Skadi 3 "You may exchange it with any tile on the table (except other Skadi)."
                             , rulesRow Valkyrie 2 "Automatically scores when there are Valkyries on both ends of a line."
                             , rulesRow Loki 1 "All tiles adjacent to Loki have value 0."
                             ]
      startButton = container rulesAreaWidth 50 middle <| button clickInput.handle Start (if not state.started then "Begin game!" else "Restart game")
      rulesArea = flow down [ size rulesAreaWidth 50 <| centered (Text.height 25 (typeface ["Rock Salt", "cursive"] (toText "Rules")))
                            , spacer 5 5
                            , width rulesAreaWidth <| leftAligned <| toText "&bull; Players take turns placing tiles from their hand. You must place a tile next to an existing tile. Rows and columns cannot exceed seven tiles."
                            , width rulesAreaWidth <| leftAligned <| toText "&bull; If the tile you placed has the highest value in a row and/or column (ties don't count), you score one point for each tile in that row and/or column."
                            , spacer 5 5
                            , pieceRules
                            , startButton
                            ]
      rightArea = if | handGap >= 570 -> rulesArea
                     | handGap >= 440 -> pieceRules `above` startButton
                     | otherwise -> startButton
  in
    flow down
      [ size totalBoardSize gameHeaderSize (centered (Text.height 50 (typeface ["Rock Salt", "cursive"] (toText "V&ouml;lusp&aacute;"))))
      , flow right [ renderBoard state boardSize dims |> clickable clickInput.handle Board
                   , flow down [ renderHand Red state
                               , spacer 1 5
                               , withSpacing 10 (withSpacing 10 (container rulesAreaWidth (handGap - 10) midLeft rightArea) |> color gray)
                               , spacer 1 5
                               , renderHand Blue state
                               ]
                   ]
      , asText state
      ]

-- MAIN

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

processClick : Signal ClickEvent -> Signal Action
processClick signal =
  let shuffled = shuffle deckContents signal
      sampledMouse = sampleOn signal Mouse.position
  in
    lift4 (\clickType shuffledDeck mousePos dims ->
            let
              pos = (Debug.watch "Mouse.position" mousePos)
              click = (Debug.watch "clickInput.signal" clickType)
            in
              case clickType of
                Start -> StartGame shuffledDeck
                Board -> PlacePiece mousePos dims
                PieceInHand player idx -> PickUpPiece player idx
                PassButton -> Pass
                None -> NoAction)
      signal shuffled sampledMouse Window.dimensions

main : Signal Element
main =
  let
    state = (foldp performAction startState (processClick clickInput.signal))
  in
    display <~ state ~ Window.dimensions
