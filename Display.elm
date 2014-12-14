module Display where

import Color (..)
import Dict
import Graphics.Collage (..)
import Graphics.Element
import Graphics.Element (..)
import Graphics.Input (..)
import List
import List (..)
import Maybe (Maybe (..), withDefault)
import Signal (Channel, send)
import String
import Text
import Text (..)

import GameTypes (..)
import Piece
import Board (getBoardSize)
import Player

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
      boardSize = getBoardSize state.board
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

drawGrid : Int -> WindowDims -> List Form
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

drawLastPlacedOutline : State -> Float -> List Form
drawLastPlacedOutline state tileSize =
  case state.lastPlaced of
    Just (x, y) ->
      let thick c = { defaultLine | color <- c, width <- 4}
          lastPlacedColor = Player.color <| Player.next state.turn
          lastPlacedOutline = move (tileSize * (toFloat x), tileSize * (toFloat y)) (outlined (thick lastPlacedColor) (square (tileSize + 4)))
      in [lastPlacedOutline]
    Nothing -> []

renderBoard : State -> Channel ClickEvent -> Int -> WindowDims -> Element
renderBoard state channel boardSize dims =
  let tileSize = getTileSizeFromBoardSize boardSize dims
      size = boardSize * (round tileSize) + 1

      grid = drawGrid boardSize dims
      pieces = map (\p -> drawPiece p tileSize) (Dict.toList state.board)
      outline = drawLastPlacedOutline state tileSize

      board = collage size size (grid ++ pieces ++ outline)
  in
    clickable (send channel BoardClick) board

renderHand : Player -> State -> Channel ClickEvent -> Element
renderHand player state channel =
  let p = playerName player
      playerType = withDefault Human (Dict.get p state.players)
      hand = Player.getHand player state
      isPieceHeld idx = state.turn == player && state.heldPiece == Just idx
      pieceImage pieceStr = pieceToImage (Piece.fromString pieceStr)
      pieceSize = (round handTileSize) + handPadding
      makePiece idx pieceStr = pieceImage pieceStr handTileSize |> container pieceSize pieceSize middle
                                                                |> Graphics.Element.color (if isPieceHeld idx then (Player.color state.turn) else white)
                                                                |> clickable (send channel (PieceInHand player idx))
      playerHand = if isEmpty hand && state.started && (not state.gameOver)
                   then [button (send channel PassButton) "Pass" |> container 100 100 middle]
                   else indexedMap makePiece hand
      hiddenPiece = image (round handTileSize) (round handTileSize) "images/tile_back.jpg"
      cpuHand = map (\x -> hiddenPiece |> container pieceSize pieceSize middle) hand
      handContents = if playerType == Human
                     then playerHand
                     else cpuHand
      handText = playerType |> toString
                            |> String.toUpper
                            |> fromString
                            |> (if state.turn == player && (not state.gameOver) then bold else identity)
                            |> Text.color (Player.color player)
                            |> leftAligned
                            |> container 80 pieceSize middle
      score = Dict.get p state.score |> withDefault 0
                                     |> asText
                                     |> container 25 pieceSize midLeft
      delta = Dict.get p state.delta |> withDefault ""
                                     |> fromString
                                     |> Text.height 9
                                     |> leftAligned
                                     |> container 20 pieceSize midLeft
  in
    flow right ([handText] ++ [score] ++ [delta] ++ handContents)

rulesRow : Piece -> String -> Element
rulesRow piece description =
  let height = 30
      image = pieceToImage piece height `beside` spacer 10 10
      value = Piece.baseValue piece
      nameAndValue = Text.concat <| map fromString [Piece.toString piece, " (", toString value, "): "]
      text = flow right [ leftAligned <| bold <| nameAndValue
                        , plainText description
                        ]
  in
    image `beside` container 600 height midLeft text

pieceRules : Element
pieceRules = flow down
    [ rulesRow Odin "No special power"
    , rulesRow Thor "No special power"
    , rulesRow Troll "No other tiles (except other Trolls) may be placed adjacent to a Troll."
    , rulesRow Dragon "May be placed on top of other tiles (except other Dragons)."
    , rulesRow Fenrir "Value is the sum of all Fenrir tiles in the same row or column."
    , rulesRow Skadi "You may exchange it with any tile on the table (except other Skadi)."
    , rulesRow Valkyrie "Automatically scores when there are Valkyries on both ends of a line."
    , rulesRow Loki "All tiles adjacent to Loki (except other Lokis) have value 0."
    ]

render : Channel ClickEvent -> State -> WindowDims -> Element
render channel state dims =
  let boardSize = getBoardSize state.board
      totalBoardSize = getTotalBoardSize dims
      tileSize = getTileSizeFromBoardSize boardSize dims
      handGap = totalBoardSize - 2 * (round handTileSize) - (handPadding * 2)
      withSpacing padding elt = spacer padding padding `beside` elt
      rulesAreaWidth = 650
      minRulesHeight = 570
      startButton = container rulesAreaWidth 50 middle <| button (send channel Start) (if not state.started then "Begin game!" else "Restart game")
      rulesArea = flow down [ size rulesAreaWidth 50 <| centered (Text.height 25 (typeface ["Rock Salt", "cursive"] (fromString "Rules")))
                            , spacer 5 5
                            , width rulesAreaWidth <| leftAligned <| fromString "&bull; Players take turns placing tiles from their hand. You must place a tile next to an existing tile. Rows and columns cannot exceed seven tiles."
                            , width rulesAreaWidth <| leftAligned <| fromString "&bull; If the tile you placed has the highest value in a row and/or column (ties don't count), you score one point for each tile in that row and/or column."
                            , spacer 5 5
                            , pieceRules
                            , startButton
                            ]
      rightArea = if | handGap >= 420 -> rulesArea
                     | handGap >= 280 -> pieceRules `above` startButton
                     | otherwise -> startButton
  in
    flow down
      [ size totalBoardSize gameHeaderSize (centered (Text.height 50 (typeface ["Rock Salt", "cursive"] (fromString "V&ouml;lusp&aacute;"))))
      , flow right [ renderBoard state channel boardSize dims
                   , flow down [ renderHand Red state channel
                               , spacer 1 5
                               , withSpacing 10 (withSpacing 10 (container rulesAreaWidth (handGap - 10) midLeft rightArea) |> Graphics.Element.color gray)
                               , spacer 1 5
                               , renderHand Blue state channel
                               ]
                   ]
      ]
