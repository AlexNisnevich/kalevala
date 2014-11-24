module Display where

import Dict
import Graphics.Input (..)
import String
import Text

import GameTypes (..)
import Board (getBoardSize)
import Player (..)

gameHeaderSize : Int
gameHeaderSize = 100

handPadding : Int
handPadding = 10

handTileSize : Float
handTileSize = 100

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

renderHand : Player -> State -> Input ClickEvent -> Element
renderHand player state clickInput =
  let p = playerName player
      playerType = Dict.getOrFail p state.players
      hand = Dict.getOrFail p state.hands
      isPieceHeld idx = state.turn == player && state.heldPiece == Just idx
      pieceImage pieceStr = pieceToImage (pieceFromString pieceStr)
      pieceSize = (round handTileSize) + handPadding
      makePiece idx pieceStr = pieceImage pieceStr handTileSize |> container pieceSize pieceSize middle
                                                                |> color (if isPieceHeld idx then (playerColor state.turn) else white)
                                                                |> clickable clickInput.handle (PieceInHand player idx)
      playerHand = if isEmpty hand && state.started && (not state.gameOver)
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

pieceRules : Element
pieceRules = flow down
    [ rulesRow Odin 8 "No special power"
    , rulesRow Thor 7 "No special power"
    , rulesRow Troll 6 "No other tiles (except Trolls) may be placed adjacent to a Troll."
    , rulesRow Dragon 5 "May be placed on top of other tiles (except other Dragons)."
    , rulesRow Fenrir 4 "Value is the sum of all Fenrir tiles in the same row or column."
    , rulesRow Skadi 3 "You may exchange it with any tile on the table (except other Skadi)."
    , rulesRow Valkyrie 2 "Automatically scores when there are Valkyries on both ends of a line."
    , rulesRow Loki 1 "All tiles adjacent to Loki have value 0."
    ]

render : Input ClickEvent -> State -> WindowDims -> Element
render clickInput state dims =
  let boardSize = getBoardSize state
      totalBoardSize = getTotalBoardSize dims
      tileSize = getTileSizeFromBoardSize boardSize dims
      handGap = totalBoardSize - 2 * (round handTileSize) - (handPadding * 2)
      withSpacing padding elt = spacer padding padding `beside` elt
      rulesAreaWidth = 650
      minRulesHeight = 570
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
                   , flow down [ renderHand Red state clickInput
                               , spacer 1 5
                               , withSpacing 10 (withSpacing 10 (container rulesAreaWidth (handGap - 10) midLeft rightArea) |> color gray)
                               , spacer 1 5
                               , renderHand Blue state clickInput
                               ]
                   ]
      ]
