module Display where

import Color (..)
import Dict
import Graphics.Collage (..)
import Graphics.Element
import Graphics.Element (..)
import Graphics.Input (..)
import Graphics.Input.Field
import Graphics.Input.Field (..)
import List
import List (..)
import Maybe (Maybe (..), withDefault)
import Signal (Channel, channel, send)
import String
import Text
import Text (..)

import GameTypes (..)
import Piece
import Board (getBoardSize, isValidMove)
import Player

import Debug

-- Constants

gameHeaderSize : Int
gameHeaderSize = 100

handPadding : Int
handPadding = 10

handTileSize : Float
handTileSize = 100

-- Channels

clickChannel : Channel ClickEvent
clickChannel = channel None

gameTypeChannel : Channel GameType
gameTypeChannel = channel HumanVsCpu

playerNameChannel : Channel Content
playerNameChannel = channel noContent

-- Helpers

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

-- Display

drawGrid : State -> Int -> WindowDims -> List Form
drawGrid state boardSize dims =
  let num = toFloat boardSize
      tileSize = getTileSizeFromBoardSize boardSize dims
      size = num * tileSize
      offset = tileSize / 2 - size / 2
      shape x y color = let pos = (tileSize * x + offset, tileSize * y + offset) 
                        in
                          [ move pos (outlined (solid black) (square tileSize))
                          , move pos (filled color (square tileSize))
                          ]
      isValidSquare x y = if Player.isPlayerTurn state
                          then
                            case state.heldPiece of
                              Just idx ->
                                let hand = Player.getHand state.turn state
                                    piece = Piece.fromString <| head <| drop idx hand
                                    location = ((round x) - floor (num / 2), (round y) - floor (num / 2))
                                in
                                  isValidMove { piece = piece, idx = idx, location = location } state.board
                              Nothing -> False
                          else False
      transparent = rgba 0 0 0 0.0
      transpGreen = rgba 0 255 0 0.5
  in
    (concatMap (\x -> (concatMap (\y -> shape x y (if (isValidSquare x y) then transpGreen else transparent)) [0..(num - 1)])) [0..(num - 1)])

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
          lastPlacedColor = Player.toColor <| Player.next state.turn
          lastPlacedOutline = move (tileSize * (toFloat x), tileSize * (toFloat y)) (outlined (thick lastPlacedColor) (square (tileSize + 4)))
      in [lastPlacedOutline]
    Nothing -> []

renderBoard : State -> Int -> WindowDims -> Element
renderBoard state boardSize dims =
  let tileSize = getTileSizeFromBoardSize boardSize dims
      size = boardSize * (round tileSize) + 1

      grid = drawGrid state boardSize dims
      pieces = map (\p -> drawPiece p tileSize) (Dict.toList state.board)
      outline = drawLastPlacedOutline state tileSize

      board = collage size size (pieces ++ grid ++ outline)
  in
    clickable (send clickChannel BoardClick) board

renderHand : Player -> State -> Element
renderHand player state =
  let p = Player.color player
      playerType = withDefault Human (Dict.get p state.players)
      hand = Player.getHand player state
      isPieceHeld idx = state.turn == player && state.heldPiece == Just idx
      pieceImage pieceStr = pieceToImage (Piece.fromString pieceStr)
      pieceSize = (round handTileSize) + handPadding
      makePiece idx pieceStr = pieceImage pieceStr handTileSize |> container pieceSize pieceSize middle
                                                                |> Graphics.Element.color (if isPieceHeld idx then (Player.toColor state.turn) else white)
                                                                |> clickable (send clickChannel (PieceInHand player idx))
      playerHand = if isEmpty hand && state.gameState == Ongoing
                   then [button (send clickChannel PassButton) "Pass" |> container 100 100 middle]
                   else indexedMap makePiece hand
      hiddenPiece = image (round handTileSize) (round handTileSize) "images/tile_back.jpg"
      cpuHand = map (\x -> hiddenPiece |> container pieceSize pieceSize middle) hand
      handContents = if playerType == Human
                     then playerHand
                     else cpuHand
      handText = playerType |> toString
                            |> (\t -> if t == "Human" then "Player" else t)
                            |> String.toUpper
                            |> fromString
                            |> (if state.turn == player && isOngoing state then bold else identity)
                            |> Text.color (Player.toColor player)
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

render : State -> WindowDims -> GameType -> Content -> Element
render state dims gameType playerName =
  let boardSize = getBoardSize state.board
      totalBoardSize = getTotalBoardSize dims
      tileSize = getTileSizeFromBoardSize boardSize dims
      handGap = totalBoardSize - 2 * (round handTileSize) - (handPadding * 2)
      withSpacing padding elt = spacer padding padding `beside` elt
      rulesAreaWidth = 650
      startButton = button (send clickChannel Start) "New game"
      gameTypeDropDown = dropDown (send gameTypeChannel)
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
                             then field Graphics.Input.Field.defaultStyle (send playerNameChannel) "Your name" playerName
                             else container 150 40 middle <| centered <| Text.height 11 <| fromString <| remoteGameStatusText
      statusArea = if gameType == HumanVsHumanRemote then remoteGameStatusArea else Graphics.Element.empty
      deckSizeArea = if isOngoing state
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
      [ size totalBoardSize gameHeaderSize (centered (Text.height 50 (typeface ["Rock Salt", "cursive"] (fromString "V&ouml;lusp&aacute;"))))
      , flow right [ renderBoard state boardSize dims
                   , flow down [ renderHand Red state
                               , spacer 1 5
                               , withSpacing 10 (withSpacing 10 (container rulesAreaWidth (handGap - 10) midLeft rightArea) |> Graphics.Element.color gray)
                               , spacer 1 5
                               , renderHand Blue state
                               ]
                   ]
      ]
