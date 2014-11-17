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

import Debug

-- TYPES

type State = { turn: Player, board : Board, score : Score, deck: Deck, hands: Hands, started: Bool, heldPiece: Maybe Int}
type Board = Dict Location Piece
type Score = Dict String Int
type Deck = [String]
type Hands = Dict String [String]

type Move = { piece : Piece, location : Location }
type Location = (Float, Float)

data Piece = Odin
           | Thor
           | Troll
           | Dragon
           | Fenrir
           | Skadi
           | Valkyrie
           | Loki


data Player = Red
            | Blue

data Action = PickUpPiece Player Int
            | PlacePiece Location
            | StartGame Deck
            | MakeRandomMove Float
            | NoAction

data ClickEvent = Start
                | Board
                | PieceInHand Player Int
                | None

-- GLOBAL CONSTANTS

gameHeaderSize : Int
gameHeaderSize = 100

gameTileSize : Float
gameTileSize = 50

gameBoardSize : Int
gameBoardSize = 15

handPadding : Int
handPadding = 5

-- MAGIC STRINGS

playerName : Player -> String
playerName player =
  case player of
    Red -> "red"
    Blue -> "blue"

pieceFromString : String -> Piece
pieceFromString str =
  case str of
    "odin" -> Odin
    "thor" -> Thor
    "troll" -> Troll
    "dragon" -> Dragon
    "fenrir" -> Fenrir
    "skadi" -> Skadi
    "valkyrie" -> Valkyrie
    "loki" -> Loki

-- HELPERS

get : [a] -> Int -> a
get list idx = head (drop idx list)
(!!) = get
infixl 4 !!

without : Int -> [a] -> [a]
without i arr =
  let before = take i arr
      after = drop (i+1) arr
  in
    before ++ after

shuffle : [a] -> Signal b -> Signal [a]
shuffle list signal =
  let randomsFromSignal signal = Random.floatList (lift (\x -> List.length list) signal)
      shuffleWithRandoms list randoms =
        if (List.isEmpty list)
        then []
        else
          let i = floor (head randoms * toFloat (List.length list))
          in
            [list !! i] ++ (shuffleWithRandoms (without i list) (tail randoms))
  in
    lift2 shuffleWithRandoms (constant list) (randomsFromSignal signal)

-- MOVES

tryToPickUpPiece : Player -> Int -> State -> State
tryToPickUpPiece player idx state =
  if (state.turn == player) then (pickUpPiece idx state) else { state | heldPiece <- Nothing }

pickUpPiece : Int -> State -> State
pickUpPiece idx state =
  { state | heldPiece <- Just idx }

tryMove : Location -> State -> State
tryMove location state =
  case state.heldPiece of
    Just idx ->
      let p = playerName state.turn
          hand = Dict.getOrFail p state.hands
          pieceStr = head <| drop idx hand
          piece = pieceFromString pieceStr
          move = { piece = piece, location = location }
      in
        if (isValidMove move state) then (makeMove move state) else { state | heldPiece <- Nothing }
    Nothing -> state

isAdjacent : Location -> Location -> Bool
isAdjacent (x1, y1) (x2, y2) =
  (y1 == y2 && abs (x1 - x2) == 1) || (x1 == x2 && abs (y1 - y2) == 1)

isValidMove : Move -> State -> Bool
isValidMove move state =
  let isOccupied = Dict.member move.location state.board
      hasAdjacentTile = any (\loc -> isAdjacent loc move.location) (Dict.keys state.board)
  in
    not isOccupied && (hasAdjacentTile || ((List.isEmpty <| Dict.toList state.board) && (move.location == (0, 0))))
  -- TODO: there are more conditions for a move to not be valid
  --       (e.g. not touching an existing tile)

makeMove : Move -> State -> State
makeMove move state =
  let p = playerName state.turn
      newBoard = Dict.insert move.location move.piece state.board
      newScore = (Dict.getOrFail p state.score) + (scoreMove move state)
      newHand = (drop 1 (Dict.getOrFail p state.hands)) ++ (take 1 state.deck)
                 -- TODO correctly remove placed piece from hand
  in
    { turn = nextPlayer state.turn
    , board = newBoard
    , score = Dict.insert p newScore state.score
    , deck = drop 1 state.deck
    , hands = Dict.insert p newHand state.hands
    , started = True
    , heldPiece = Nothing
    }

scoreMove : Move -> State -> Int
scoreMove move state = 1      -- TODO: actually score moves

makeRandomMove : State -> Float -> State
makeRandomMove state seed =
  if state.started
  then
    let p = playerName state.turn
        piece = pieceFromString <| head <| Dict.getOrFail p state.hands
        halfGameBoardSize = gameBoardSize // 2
        xs = map (\x -> toFloat (x - halfGameBoardSize)) [0..(gameBoardSize - 1)]
        locations = concatMap (\x -> (map (\y -> (x, y)) xs)) xs
        validLocations = List.filter (\loc -> isValidMove { piece = piece, location = loc } state) locations
        idx = floor (seed * toFloat (List.length validLocations))
        location = validLocations !! idx
    in
      tryMove location state
  else state

nextPlayer : Player -> Player
nextPlayer player =
  case player of
    Red -> Blue
    Blue -> Red

-- GAME

tryStartGame : State -> Deck -> State
tryStartGame state deck =
  if (not state.started) then (startGame state deck) else state

startGame : State -> Deck -> State
startGame state deck =
  let deckWithIndices = zip [0..(List.length deck - 1)] deck
      idxFirstNonTroll = fst <| head <| filter (\(idx, piece) -> not (piece == "troll")) deckWithIndices
      firstTile = pieceFromString <| deck !! idxFirstNonTroll
      deckMinusFirstTile = without idxFirstNonTroll deck
      redHand = take 5 deckMinusFirstTile
      blueHand = take 5 (drop 5 deckMinusFirstTile)
      hands = Dict.fromList [("red", redHand), ("blue", blueHand)]
      remainder = drop 10 deckMinusFirstTile
  in
    { state | hands <- hands,
              deck <- remainder,
              started <- True,
              board <- Dict.singleton (0, 0) firstTile }

-- DISPLAY

clickInput : Input ClickEvent
clickInput = input None

pieceToImage: Piece -> Float -> Element
pieceToImage piece tileSize =
  let tileSize = round gameTileSize
      pos =
        case piece of
          Odin -> (3, 1)
          Thor -> (2, 1)
          Troll -> (1, 1)
          Dragon -> (0, 1)
          Fenrir -> (3, 0)
          Skadi -> (2, 0)
          Valkyrie -> (1, 0)
          Loki -> (0, 0)
  in
    case pos of
      (x, y) -> croppedImage (x * tileSize, y * tileSize) tileSize tileSize "http://i.imgur.com/5yLICgb.png?1"

drawGrid : [Form]
drawGrid =
  let num = (toFloat gameBoardSize)
      size = num * gameTileSize
      xShift = gameTileSize / 2 - size / 2
      yShift = gameTileSize / 2 - size / 2
      shape x y = move (gameTileSize * x + xShift, gameTileSize * y + yShift) (outlined (solid black) (square gameTileSize))
  in
    (concatMap (\x -> (map (\y -> shape x y) [0..(num - 1)])) [0..(num - 1)])

drawPiece : (Location, Piece) -> Float -> Form
drawPiece ((x', y'), piece) tileSize =
  let x = x' * tileSize
      y = y' * tileSize
  in
    move (x, y) (toForm (pieceToImage piece tileSize))

renderBoard : Board -> Element
renderBoard board =
  let size = gameBoardSize * (round gameTileSize) + 1
      pieces = map (\p -> drawPiece p gameTileSize) (Dict.toList board)
  in
    collage size size (drawGrid ++ pieces)

renderHand : Player -> State -> Element
renderHand player state =
  let p = playerName player
      tileSize = round gameTileSize
      hand = Dict.getOrFail p state.hands
      isPieceHeld idx = state.turn == player && state.heldPiece == Just idx
      pieceImage pieceStr = pieceToImage (pieceFromString pieceStr) gameTileSize
      pieceSize = tileSize + handPadding
      makePiece idx pieceStr = pieceImage pieceStr |> container pieceSize pieceSize middle
                                                   |> color (if isPieceHeld idx then blue else white)
                                                   |> clickable clickInput.handle (PieceInHand player idx)
      handContents = indexedMap makePiece hand
      handText = String.toUpper p |> toText
                                  |> (if state.turn == player then bold else identity)
                                  |> leftAligned
                                  |> container 70 pieceSize middle
      score = Dict.getOrFail p state.score |> asText
                                           |> container 40 pieceSize middle
  in
    flow right ([handText] ++ handContents ++ [score])

display : State -> Element
display state =
  flow down
    [ size 750 gameHeaderSize (centered (Text.height 50 (typeface ["Rock Salt", "cursive"] (toText "V&ouml;lusp&aacute;"))))
    , flow right [ renderBoard state.board |> clickable clickInput.handle Board
                 , flow down [ renderHand Red state
                             , spacer 1 ((round gameTileSize) * (gameBoardSize - 2) - handPadding * 2)
                             , renderHand Blue state]]
    , if not state.started then (button clickInput.handle Start "Begin game!") else empty
    , asText state
    ]

-- MAIN

performAction : Action -> State -> State
performAction action state =
  case action of
    PickUpPiece player idx -> tryToPickUpPiece player idx state
    PlacePiece location -> tryMove location state
    StartGame deck -> tryStartGame state deck
    MakeRandomMove seed -> makeRandomMove state seed
    NoAction -> state

deckContents : [String]
deckContents = (Array.toList (Array.repeat 6 "odin") ++
                Array.toList (Array.repeat 8 "thor") ++
                Array.toList (Array.repeat 6 "troll") ++
                Array.toList (Array.repeat 8 "dragon") ++
                Array.toList (Array.repeat 8 "fenrir") ++
                Array.toList (Array.repeat 9 "skadi") ++
                Array.toList (Array.repeat 9 "valkyrie") ++
                Array.toList (Array.repeat 6 "loki"))

startState : State
startState =
  { turn = Red
  , board = Dict.empty
  , score = Dict.fromList [("red", 0), ("blue", 0)]
  , deck = []
  , hands = Dict.fromList [("red", []), ("blue", [])]
  , started = False
  , heldPiece = Nothing
  }

mouseToBoardPosition: (Int, Int) -> (Float, Float)
mouseToBoardPosition (x', y') =
  let x = x'
      y = (y' - gameHeaderSize)
      tileSize = (round gameTileSize)
      offset = gameBoardSize // 2
      boardX = (x // tileSize) - offset |> toFloat
      boardY = 0 - ((y // tileSize) - offset) |> toFloat
  in (boardX, boardY)

processClick : Signal ClickEvent -> Signal Action
processClick signal =
  let random = Random.float signal
      shuffled = shuffle deckContents signal
      sampledMouse = sampleOn signal Mouse.position
  in
    lift4 (\clickType randomFloat shuffledDeck mousePos ->
            let
              pos = (Debug.watch "Mouse.position" mousePos)
              click = (Debug.watch "clickInput.signal" clickType)
              boardPos = (Debug.watch "Board position" (mouseToBoardPosition mousePos))
            in
              case clickType of
                Start -> StartGame shuffledDeck
                Board -> PlacePiece boardPos
                PieceInHand player idx -> PickUpPiece player idx
                None -> NoAction)
      signal random shuffled sampledMouse

main : Signal Element
main =
  display <~ (foldp performAction startState (processClick clickInput.signal))
