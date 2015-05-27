module Display.Board where

import Color exposing (..)
import Dict
import Graphics.Collage exposing (..)
import Graphics.Element as Element exposing (..)
import Graphics.Input exposing (..)
import Graphics.Input.Field exposing (..)
import List exposing (..)
import Maybe exposing (withDefault)
import Signal exposing (Mailbox, mailbox, message)
import String
import Text exposing (..)

import GameTypes exposing (..)
import State
import Piece
import Board
import Player
import Game

import Display.Constants exposing (..)
import Display.Helpers exposing (..)

getTotalBoardSize : WindowDims -> Int
getTotalBoardSize (width, height) =
  let availableWidth = width - sidebarWidth - 3 * gameMargin
      availableHeight = height - 2 * gameMargin
  in
    (max (min availableHeight availableWidth) minSidebarHeight)

getTileSizeFromBoardSize : Int -> WindowDims -> Float
getTileSizeFromBoardSize boardSize dims = toFloat (getTotalBoardSize dims // boardSize)

getBoardSize : State -> Int
getBoardSize state = Board.getBoardSize state.board

mouseToBoardPosition: MousePos -> State -> WindowDims -> Location
mouseToBoardPosition (x', y') state dims =
  let x = x' - gameMargin
      y = y' - gameMargin
      boardSize = Board.getBoardSize state.board
      tileSize = round <| getTileSizeFromBoardSize boardSize dims
      offset = boardSize // 2
      boardX = (x // tileSize) - offset
      boardY = 0 - ((y // tileSize) - offset)
  in (boardX, boardY)

drawGrid : State -> Int -> WindowDims -> List Form
drawGrid state boardSize dims =
  let tileSize = getTileSizeFromBoardSize boardSize dims
      totalSize = (toFloat boardSize) * tileSize
      offset = tileSize / 2 - totalSize / 2
      shape x y = 
        let pos = (tileSize * (toFloat x) + offset, tileSize * (toFloat y) + offset)
            imgSize = if tileSize > 50 then 100 else 50
            tile = "images/" ++ toString imgSize ++ "/Board-" ++ toString ((x ^ 2 + 7 * y) % 6) ++ ".png"
        in
          move pos (toForm (image (round tileSize) (round tileSize) tile))
  in
    (concatMap (\x -> (map (\y -> shape x y) 
                                 [0..(boardSize - 1)])) 
               [0..(boardSize - 1)])

drawPiece : (Location, Piece) -> Board -> Float -> Form
drawPiece ((x', y'), piece) board tileSize =
  let x = toFloat x' * tileSize
      y = toFloat y' * tileSize
      value = Board.getDisplayedTileValue (x',y') board
  in
    move (x, y) (toForm (pieceToImage piece value tileSize))

drawAvailableOverlay : State -> Int -> WindowDims -> List Form
drawAvailableOverlay state boardSize dims =
  let tileSize = getTileSizeFromBoardSize boardSize dims
      totalSize = (toFloat boardSize) * tileSize
      imgSize = if tileSize > 50 then 100 else 50
      overlayImgPath = "images/" ++ (toString imgSize) ++ "/Green-H.png"
      offset = tileSize / 2 - totalSize / 2
      shape x y = 
        let pos = (tileSize * (toFloat x) + offset, tileSize * (toFloat y) + offset)
            overlay = if Board.isValidSquareToMove state (x, y) boardSize
                      then image (round tileSize) (round tileSize) overlayImgPath
                      else Element.empty
        in
          move pos (toForm overlay)
  in
    (concatMap (\x -> (map (\y -> shape x y) 
                                 [0..(boardSize - 1)])) 
               [0..(boardSize - 1)])

drawLastPlacedOutline : State -> Float -> List Form
drawLastPlacedOutline state tileSize =
  case (state.lastPlaced, state.lastPlacedPlayer) of
    (Just (x, y), Just player) ->
      let imgSize = if tileSize > 50 then 100 else 50
          lastPlacedColorStr = Player.toString <| player
          lastPlacedOutlinePath = "images/" ++ (toString imgSize) ++ "/" ++ lastPlacedColorStr ++ "-H.png"
          lastPlacedOutline = move (tileSize * (toFloat x), tileSize * (toFloat y)) (toForm (image tileSize tileSize lastPlacedOutlinePath))
      in [lastPlacedOutline]
    otherwise -> []