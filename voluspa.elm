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

-- TYPES

type State = {
  players : Dict String PlayerType,
  turn : Player,
  board : Board,
  score : Score,
  deck : Deck,
  hands : Hands,
  started : Bool,
  heldPiece : Maybe Int,
  lastPlaced : Maybe Location,
  delta : Dict String String,
  gameOver : Bool
}

type Board = Dict Location Piece
type Score = Dict String Int
type Deck = [String]
type Hands = Dict String [String]

type Move = { piece : Piece, idx : Int, location : Location }
type Location = (Float, Float)
type MousePos = (Int, Int)
type WindowDims = (Int, Int)

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

data PlayerType = Human
                | Cpu

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

data Direction = Vertical
               | Horizontal

-- GLOBAL CONSTANTS

gameHeaderSize : Int
gameHeaderSize = 100

handPadding : Int
handPadding = 10

handTileSize : Float
handTileSize = 100

-- MAGIC STRINGS

playerName : Player -> String
playerName player =
  case player of
    Red -> "red"
    Blue -> "blue"

playerColor : Player -> Color
playerColor player =
  case player of
    Red -> red
    Blue -> blue

pieceFromString : String -> Piece
pieceFromString str =
  case str of
    "Odin" -> Odin
    "Thor" -> Thor
    "Troll" -> Troll
    "Dragon" -> Dragon
    "Fenrir" -> Fenrir
    "Skadi" -> Skadi
    "Valkyrie" -> Valkyrie
    "Loki" -> Loki

pieceToString : Piece -> String
pieceToString piece =
  case piece of
    Odin -> "Odin"
    Thor -> "Thor"
    Troll -> "Troll"
    Dragon -> "Dragon"
    Fenrir -> "Fenrir"
    Skadi -> "Skadi"
    Valkyrie -> "Valkyrie"
    Loki -> "Loki"

-- HELPERS

(!!) : [a] -> Int -> a
(!!) list idx = head (drop idx list)
infixl 4 !!

repeat : Int -> a -> [a]
repeat num elt = Array.toList <| Array.repeat num elt

without : Int -> [a] -> [a]
without i arr =
  let before = take i arr
      after = drop (i+1) arr
  in
    before ++ after

replaceAtIndex : Int -> a -> [a] -> [a]
replaceAtIndex i elt arr =
  let before = take i arr
      after = drop (i+1) arr
  in
    before ++ [elt] ++ after

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

getBoardSize : State -> Int
getBoardSize state =
  if List.isEmpty <| Dict.toList state.board
  then 5
  else
    let locations = Dict.keys state.board
        xs = map fst locations
        ys = map snd locations
        maxX = max (maximum xs) (abs <| minimum xs)
        maxY = max (maximum ys) (abs <| minimum ys)
        distFromCenter = (max maxX maxY) + 2
    in
      (distFromCenter * 2) + 1

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
      boardX = (x // tileSize) - offset |> toFloat
      boardY = 0 - ((y // tileSize) - offset) |> toFloat
  in (boardX, boardY)

isAdjacent : Location -> Location -> Bool
isAdjacent (x1, y1) (x2, y2) =
  (y1 == y2 && abs (x1 - x2) == 1) || (x1 == x2 && abs (y1 - y2) == 1)

adjacentTiles : Location -> Board -> [Location]
adjacentTiles (x, y) board =
  filter (\loc -> isAdjacent loc (x, y)) (Dict.keys board)

getTileScore : Location -> Direction -> Move -> Board -> Int
getTileScore (x,y) dir move board =
  let piece = Dict.getOrFail (x,y) board
      adjacents = adjacentTiles (x,y) board
      adjacentToLoki = any (\loc -> Dict.getOrFail loc board == Loki) adjacents
      isCurrentTile = (move.location == (x,y)) -- is this the tile that was placed this turn?
  in
    if adjacentToLoki && not (piece == Loki)
    then 0 -- Loki makes all tiles around him 0 (except other Lokis)
    else
      case piece of
        Odin -> 8
        Thor -> 7
        Troll -> 6
        Dragon -> 5
        Fenrir -> let line = (case dir of Horizontal -> findRow
                                          Vertical -> findColumn) (x,y) board
                      piecesInLine = map (\loc -> Dict.getOrFail loc board) line
                      numOtherFenrirs = length <| filter (\p -> p == Fenrir) piecesInLine
                      numFenrirsToCount = numOtherFenrirs + if (isCurrentTile || not (move.piece == Fenrir)) then 1 else 0
                      -- count the tile itself, but don't count Fenrir placed this turn for other Fenrirs
                      -- (this is so that Fenrirs can beat other Fenrirs)
                  in
                    4 * numFenrirsToCount
                  -- TODO: Fenrir-Loki interaction is actually quite tricky.
                  -- "Fenrir tiles next to Loki tiles are worth zero and do not contribute to the value of other Fenrir tiles."
        Skadi -> 3
        Valkyrie -> if isCurrentTile && hasSamePieceAtOtherEnd (x,y) board dir
                    then 100 -- i.e. instantly score line
                    else 2
                  -- TODO: Loki shouldn't prevent Valkyrie from auto-scoring a line
        Loki -> 1

findColumn : Location -> Board -> [Location]
findColumn (x,y) board = (findAbove (x,y-1) board) ++ (findBelow (x,y+1) board)

findRow : Location -> Board -> [Location]
findRow (x,y) board = (findLeftward (x-1,y) board) ++ (findRightward (x+1,y) board)

findAbove : Location -> Board -> [Location]
findAbove (x,y) board =
  if Dict.member (x,y) board
  then [(x,y)] ++ findAbove (x,y-1) board
  else []

findBelow : Location -> Board -> [Location]
findBelow (x,y) board =
  if Dict.member (x,y) board
  then [(x,y)] ++ findBelow (x,y+1) board
  else []

findLeftward : Location -> Board -> [Location]
findLeftward (x,y) board =
  if Dict.member (x,y) board
  then [(x,y)] ++ findLeftward (x-1,y) board
  else []

findRightward : Location -> Board -> [Location]
findRightward (x,y) board =
  if Dict.member (x,y) board
  then [(x,y)] ++ findRightward (x+1,y) board
  else []

-- is this piece at one end of a line with the same kind of piece
-- at the other end? (used by Valkyrie)
hasSamePieceAtOtherEnd : Location -> Board -> Direction -> Bool
hasSamePieceAtOtherEnd (x,y) board dir =
  let pieceAt pos = Dict.getOrFail pos board
      samePieces pos1 pos2 = pieceAt pos1 == pieceAt pos2
      above = findAbove (x,y-1) board
      below = findBelow (x,y+1) board
      left = findLeftward (x-1,y) board
      right = findRightward (x+1,y) board
      samePieceBelow = isEmpty above && not (isEmpty below) && samePieces (last below) (x,y)
      samePieceAbove = isEmpty below && not (isEmpty above) && samePieces (last above) (x,y)
      samePieceLeft = isEmpty right && not (isEmpty left) && samePieces (last left) (x,y)
      samePieceRight = isEmpty left && not (isEmpty right) && samePieces (last right) (x,y)
  in
    case dir of
      Horizontal -> samePieceLeft || samePieceRight
      Vertical -> samePieceBelow || samePieceAbove

-- MOVES

tryToPickUpPiece : Player -> Int -> State -> State
tryToPickUpPiece player idx state =
  if state.turn == player
  then pickUpPiece idx state
  else { state | heldPiece <- Nothing }

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
          move = { piece = piece, idx = idx, location = location }
          nextPlayerType = Dict.getOrFail (playerName <| nextPlayer state.turn) state.players
          nextAction = case nextPlayerType of
                         Human -> identity
                         Cpu -> makeCpuMove
      in
        if (isValidMove move state) then (makeMove move state |> nextAction) else { state | heldPiece <- Nothing }
    Nothing -> state

isValidMove : Move -> State -> Bool
isValidMove move state =
  let isUnoccupied = not <| Dict.member move.location state.board
      existingTile = Dict.get move.location state.board
      canOverlapExistingTile = (move.piece == Dragon || move.piece == Skadi)
                               && not (existingTile == Just move.piece)
                               -- can't Skadi a Skadi, can't Dragon a Dragon
      columnLength = length (findColumn move.location state.board) + 1
      rowLength = length (findRow move.location state.board) + 1
      longestLine = max columnLength rowLength
      adjacents = adjacentTiles move.location state.board
      hasAdjacentTile = not <| List.isEmpty adjacents
      adjacentToTroll = any (\loc -> Dict.getOrFail loc state.board == Troll) adjacents
  in
    (isUnoccupied || canOverlapExistingTile)
    && hasAdjacentTile
    && ((not adjacentToTroll) || move.piece == Troll) -- only Trolls can be placed next to other Trolls
    && longestLine <= 7

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

scoreMove : Move -> State -> Int
scoreMove move state =
  let column = findColumn move.location state.board
      columnSize = List.length column + 1
      columnScores = map (\loc -> getTileScore loc Vertical move state.board) column
      columnHighScore = if isEmpty column then 0 else maximum columnScores
      tileScoreInColumn = getTileScore move.location Vertical move state.board
      columnPoints = if (tileScoreInColumn > columnHighScore && columnSize >= 2) then columnSize else 0

      row = findRow move.location state.board
      rowSize = List.length row + 1
      rowScores = map (\loc -> getTileScore loc Horizontal move state.board) row
      rowHighScore = if isEmpty row then 0 else maximum rowScores
      tileScoreInRow = getTileScore move.location Horizontal move state.board
      rowPoints = if (tileScoreInRow > rowHighScore && rowSize >= 2) then rowSize else 0
  in
    columnPoints + rowPoints

-- 1-ply AI
makeCpuMove : State -> State
makeCpuMove state =
  if Dict.getOrFail (playerName state.turn) state.hands |> List.isEmpty
  then state -- TODO: handle turn skipping (in case one player runs out of tiles before the other)
  else
    let p = playerName state.turn
        hand = Dict.getOrFail p state.hands
        idxs = [0..(List.length hand)-1]

        boardSize = getBoardSize state
        xs = map (\x -> toFloat (x - (boardSize // 2))) [0..(boardSize - 1)]
        locations = concatMap (\x -> (map (\y -> (x, y)) xs)) xs

        pieceAtIdx i = pieceFromString (hand !! i)
        validLocationsByPiece piece = List.filter (\loc -> isValidMove { piece = piece, idx = 0, location = loc } state) locations
        validMoves = concatMap (\i -> let piece = pieceAtIdx i
                                          move loc = { piece = piece, idx = i, location = loc}
                                          stateAfterMoveTo loc = { state | board <- Dict.insert loc piece state.board }
                                          moveWithScore loc = (move loc, scoreMove (move loc) (stateAfterMoveTo loc))
                                      in
                                        map moveWithScore (validLocationsByPiece piece)) idxs
        (bestMove, bestScore) = sortBy snd validMoves |> reverse |> Debug.watch "best moves" |> head
    in
      tryMove bestMove.location { state | heldPiece <- Just bestMove.idx }

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
    then makeCpuMove newState
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
  let x = x' * tileSize
      y = y' * tileSize
  in
    move (x, y) (toForm (pieceToImage piece tileSize))

drawLastPlacedOutline : State -> Float -> [Form]
drawLastPlacedOutline state tileSize =
  case state.lastPlaced of
    Just (x,y) ->
      let thick c = { defaultLine | color <- c, width <- 4}
          lastPlacedColor = playerColor <| nextPlayer state.turn
          lastPlacedOutline = move (tileSize * x, tileSize * y) (outlined (thick lastPlacedColor) (square (tileSize + 4)))
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
deckContents = repeat 6 "Odin" ++
               repeat 8 "Thor" ++
               repeat 6 "Troll" ++
               repeat 8 "Dragon" ++
               repeat 8 "Fenrir" ++
               repeat 9 "Skadi" ++
               repeat 9 "Valkyrie" ++
               repeat 6 "Loki"

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
