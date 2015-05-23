module Display where

import Color exposing (..)
import Dict
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
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

import Debug

-- Constants

gameHeaderSize : Int
gameHeaderSize = 100

handPadding : Int
handPadding = 10

handTileSize : Float
handTileSize = 100

transparent : Color
transparent = rgba 0 0 0 0.0

transpGreen : Color
transpGreen = rgba 0 255 0 0.5

-- Mailboxes

clickMailbox : Mailbox ClickEvent
clickMailbox = mailbox None

gameTypeMailbox : Mailbox GameType
gameTypeMailbox = mailbox HumanVsCpu

playerNameMailbox : Mailbox Content
playerNameMailbox = mailbox noContent

-- Helpers

getTotalBoardSize : WindowDims -> Int
getTotalBoardSize (width, height) = height - gameHeaderSize

getTileSizeFromBoardSize : Int -> WindowDims -> Float
getTileSizeFromBoardSize boardSize dims = toFloat (getTotalBoardSize dims // boardSize)

mouseToBoardPosition: MousePos -> State -> WindowDims -> Location
mouseToBoardPosition (x', y') state dims =
  let x = x'
      y = (y' - gameHeaderSize)
      boardSize = Board.getBoardSize state.board
      tileSize = round <| getTileSizeFromBoardSize boardSize dims
      offset = boardSize // 2
      boardX = (x // tileSize) - offset
      boardY = 0 - ((y // tileSize) - offset)
  in (boardX, boardY)

pieceToImage: Piece -> String -> Float -> Element
pieceToImage piece value tileSize =
  let imgSize = if tileSize > 75 then 100 else 50
      imgPath = "images/" ++ toString imgSize ++ "/" ++ Piece.toString piece ++ "-" ++ value ++ ".png"
  in
    image (round tileSize) (round tileSize) imgPath

-- Display

drawGrid : State -> Int -> WindowDims -> List Form
drawGrid state boardSize dims =
  let tileSize = getTileSizeFromBoardSize boardSize dims
      totalSize = (toFloat boardSize) * tileSize
      offset = tileSize / 2 - totalSize / 2
      shape x y = 
        let pos = (tileSize * (toFloat x) + offset, tileSize * (toFloat y) + offset)
            imgSize = if tileSize > 75 then 100 else 50
            tile = "images/" ++ toString imgSize ++ "/board" ++ toString ((x ^ 2 + 7 * y) % 6) ++ ".png"
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
      offset = tileSize / 2 - totalSize / 2
      shape x y = 
        let pos = (tileSize * (toFloat x) + offset, tileSize * (toFloat y) + offset)
            color = if (Board.isValidSquareToMove state (x, y) boardSize) then transpGreen else transparent
        in
          move pos (filled color (square tileSize))
  in
    (concatMap (\x -> (map (\y -> shape x y) 
                                 [0..(boardSize - 1)])) 
               [0..(boardSize - 1)])

drawLastPlacedOutline : State -> Float -> List Form
drawLastPlacedOutline state tileSize =
  case state.lastPlaced of
    Just (x, y) ->
      let thick c = { defaultLine | color <- c, width <- 4}
          lastPlacedColor = Player.toColor <| Player.next state.turn
          lastPlacedOutline = move (tileSize * (toFloat x), tileSize * (toFloat y)) (outlined (thick lastPlacedColor) (square (tileSize + 4)))
      in [lastPlacedOutline]
    Nothing -> []

renderBoard : State -> Int -> WindowDims -> Element
renderBoard state boardSize dims =
  let tileSize = getTileSizeFromBoardSize boardSize dims
      size = boardSize * (round tileSize) + 1

      grid = drawGrid state boardSize dims
      overlay = drawAvailableOverlay state boardSize dims
      pieces = map (\p -> drawPiece p state.board tileSize) (Dict.toList state.board)
      outline = drawLastPlacedOutline state tileSize

      board = collage size size (grid ++ pieces ++ overlay ++ outline)
  in
    clickable (message clickMailbox.address BoardClick) board

renderHand : Player -> State -> Element
renderHand player state =
  let p = Player.toString player
      playerType = withDefault Human (Dict.get p state.players)
      hand = Player.getHand player state
      isPieceHeld idx = state.turn == player && state.heldPiece == Just idx
      pieceImage pieceStr = pieceToImage <| Piece.fromString pieceStr
      pieceSize = (round handTileSize) + handPadding
      makePiece idx pieceStr = pieceImage pieceStr (toString <| Piece.baseValue <| Piece.fromString pieceStr) handTileSize
                                  |> container pieceSize pieceSize middle
                                  |> Graphics.Element.color (if isPieceHeld idx then (Player.toColor state.turn) else white)
                                  |> clickable (message clickMailbox.address (PieceInHand player idx))
      playerHand = if isEmpty hand && state.gameState == Ongoing
                   then [button (message clickMailbox.address PassButton) "Pass" |> container 100 100 middle]
                   else indexedMap makePiece hand
      hiddenPiece = image (round handTileSize) (round handTileSize) "images/100/back1.png"
      cpuHand = map (\x -> hiddenPiece |> container pieceSize pieceSize middle) hand
      handContents = if playerType == Human
                     then playerHand
                     else cpuHand
      handText = playerType |> toString
                            |> (\t -> if t == "Human" then "Player" else t)
                            |> String.toUpper
                            |> fromString
                            |> (if state.turn == player && State.isOngoing state then bold else identity)
                            |> Text.color (Player.toColor player)
                            |> leftAligned
                            |> container 80 pieceSize middle
      score = Dict.get p state.score |> withDefault 0
                                     |> show
                                     |> container 25 pieceSize midLeft
      delta = Dict.get p state.delta |> withDefault ""
                                     |> fromString
                                     |> Text.height 9
                                     |> leftAligned
                                     |> container 20 pieceSize midLeft
  in
    flow right ([handText] ++ [score] ++ [delta] ++ handContents)

render : State -> WindowDims -> GameType -> Content -> Element
render state dims gameType playerName =
  let boardSize = Board.getBoardSize state.board
      totalBoardSize = getTotalBoardSize dims
      tileSize = getTileSizeFromBoardSize boardSize dims
      handGap = totalBoardSize - 2 * (round handTileSize) - (handPadding * 2)
      withSpacing padding elt = spacer padding padding `beside` elt
      rulesAreaWidth = 650
      startButton = button (message clickMailbox.address Start) "New game"
      gameTypeDropDown = dropDown (message gameTypeMailbox.address)
                            [ ("Player vs AI", HumanVsCpu)
                            , ("Player vs Player (hotseat)" , HumanVsHumanLocal)
                            , ("Player vs Player (online)" , HumanVsHumanRemote)
                            ]
                         |> size 180 40
      remoteGameStatusText = case state.gameState of
                               WaitingForPlayers -> "Waiting for opponent ... "
                               Connected opponentName -> "Connected to " ++ opponentName ++ " "
                               Disconnected -> "Opponent disconnected "
                               _ -> ""
      remoteGameStatusArea = if state.gameState == NotStarted
                             then field Graphics.Input.Field.defaultStyle (message playerNameMailbox.address) "Your name" playerName
                             else container 150 40 middle <| centered <| Text.height 11 <| fromString <| remoteGameStatusText
      statusArea = if gameType == HumanVsHumanRemote then remoteGameStatusArea else Graphics.Element.empty
      deckSizeArea = if State.isOngoing state
                     then container 70 40 middle <| centered <| Text.height 11 <| fromString <| "Deck: " ++ toString (length state.deck)
                     else Graphics.Element.empty
      controls = container rulesAreaWidth 40 middle <| flow right [ statusArea, gameTypeDropDown, startButton, deckSizeArea ]
      logArea = state.log
                |> take 5
                |> (List.map (\(color, text) -> leftAligned <| Text.color color <| fromString text))
                |> flow down
      rightArea = flow down [ logArea
                            , controls
                            ]
  in
    flow down
      [ size totalBoardSize gameHeaderSize (centered (Text.height 50 (typeface ["Rock Salt", "cursive"] (fromString "Kalevala"))))
      , flow right [ renderBoard state boardSize dims
                   , flow down [ renderHand Red state
                               , spacer 1 5
                               , withSpacing 10 (withSpacing 10 (container rulesAreaWidth (handGap - 10) midLeft rightArea) |> Graphics.Element.color gray)
                               , spacer 1 5
                               , renderHand Blue state
                               ]
                   ]
      ]
