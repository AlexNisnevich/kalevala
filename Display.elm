module Display where

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
import Log
import State
import Piece
import Player
import Game

import Display.Board as Board
import Display.Constants exposing (..)
import Display.Helpers exposing (..)

{- Mailboxes -}

clickMailbox : Mailbox ClickEvent
clickMailbox = mailbox None

playerNameMailbox : Mailbox Content
playerNameMailbox = mailbox noContent

{- Top-level render methods -}

render : State -> WindowDims -> Content -> Element
render state dims playerName =
  withMargin (gameMargin, gameMargin) <| renderGameArea state dims playerName

renderGameArea : State -> WindowDims -> Content -> Element
renderGameArea state dims playerName = 
  flow right [ renderBoard state dims
             , spacer gameMargin 1
             , renderSidebar state dims playerName
             ]

{- Board -}

renderBoard : State -> WindowDims -> Element
renderBoard state dims =
  let boardSize = Board.getBoardSize state
      tileSize = Board.getTileSizeFromBoardSize boardSize dims
      size = boardSize * (round tileSize) + 1

      grid = Board.drawGrid state boardSize dims
      overlay = Board.drawAvailableOverlay state boardSize dims
      pieces = map (\p -> Board.drawPiece p state.board tileSize) (Dict.toList state.board)
      outline = Board.drawLastPlacedOutline state tileSize

      board = collage size size (grid ++ pieces ++ overlay ++ outline)
  in
    clickable (message clickMailbox.address BoardClick) board

{- Sidebar -}

renderSidebar : State -> WindowDims -> Content -> Element
renderSidebar state (w, h) playerName =
  let sidebarInnerPaddingHeight = (Board.getTotalBoardSize (w, h) - minSidebarHeight) // 2
  in
    flow down [ image sidebarWidth sidebarImageHeight "images/100/kalevala.png"
              , flow down [ renderHand Red state
                          , spacer 1 sidebarInnerPaddingHeight
                          , flow right [ renderScoreArea state |> withMargin (16, 11)
                                       , renderRightArea state playerName |> withMargin (13, 19)
                                       ]
                          , spacer 1 sidebarInnerPaddingHeight
                          , renderHand Blue state
                          ] |> withMargin (12, 11)
              ]

{- Sidebar/Hand -}

renderHand : Player -> State -> Element
renderHand player state =
  let p = Player.toString player
      playerType = withDefault Human (Dict.get p state.players)
      hand = Player.getHand player state
      isPieceHeld idx = state.turn == player && state.heldPiece == Just idx
      pieceImage pieceStr = pieceToImage <| Piece.fromString pieceStr
      pieceSize = handTileSize + handPadding
      makePiece idx pieceStr = pieceImage pieceStr (toString <| Piece.baseValue <| Piece.fromString pieceStr) (toFloat handTileSize)
                                  |> container pieceSize pieceSize middle
                                  |> Element.color (if isPieceHeld idx then (Player.toColor state.turn) else white)
                                  |> clickable (message clickMailbox.address (PieceInHand player idx))
      hiddenPiece = image handTileSize handTileSize "images/100/back2.png"

      playerHand = if isEmpty hand && state.gameState == Ongoing
                   then [button (message clickMailbox.address PassButton) "Pass" |> container 100 100 middle]
                   else indexedMap makePiece hand
      cpuHand = map (\x -> hiddenPiece |> container pieceSize pieceSize middle) hand
      dummyHand = repeat 5 (hiddenPiece |> container pieceSize pieceSize middle)

      handContents = if | State.isNotStarted state -> dummyHand
                        | playerType == Human      -> playerHand
                        | otherwise                -> cpuHand
  in
    flow right handContents

{- Sidebar/ScoreArea -}

renderScoreArea : State -> Element
renderScoreArea state = 
  flow down [ playerHandText Red state |> centered |> container 85 30 middle
            , playerScoreText Red state |> centered |> container 85 40 middle |> withMargin (1, 6)
            , renderDeck state |> withMargin (1, 14)
            , playerScoreText Blue state |> centered |> container 85 40 middle |> withMargin (1, 6)
            , playerHandText Blue state |> centered |> container 85 30 middle
            ]

playerHandText : Player -> State -> Text
playerHandText player state =
  let p = Player.toString player
      playerType = withDefault Human (Dict.get p state.players)
  in 
    playerType |> toString
               |> (\t -> if t == "Human" then "Player" else t)
               |> String.toUpper
               |> fromString
               |> (if state.turn == player && State.isOngoing state then bold else identity)
               |> Text.color (Player.toColor player)
               |> Text.height 20

playerScoreText : Player -> State -> Text
playerScoreText player state =
  let p = Player.toString player
  in
    Dict.get p state.score |> withDefault 0
                           |> toString
                           |> fromString
                           |> Text.color (Player.toColor player)
                           |> Text.height 40

renderDeck : State -> Element
renderDeck state =
  let deckSize = if | State.isNotStarted state -> length Game.deckContents
                    | State.isOngoing state    -> length state.deck
                    | otherwise                -> 0
      deckSizeStr = "Deck : " ++ toString deckSize
  in 
    flow down [ image 85 85 "images/100/deck.png"
              , deckSizeStr |> fromString
                            |> Text.height 14
                            |> centered
                            |> container 85 20 midBottom
              ]

{- Sidebar/RightArea -}

renderRightArea : State -> Content -> Element
renderRightArea state playerName = 
  let content = if | State.isAtMainMenu state -> renderMenu
                   | State.isSettingUpRemoteGame state -> renderRemoteSetupMenu playerName
                   | State.isConnectingToRemoteGame state -> renderRemoteConnecting
                   | otherwise -> case State.pieceHeld state of
                                    Just piece -> renderPieceDescription piece
                                    Nothing -> renderLog state
  in 
    content |> container 400 sidebarRightAreaHeight middle |> withBorder (2, 2) darkGrey

renderMenu : Element
renderMenu =
  flow down [ customButton (message clickMailbox.address StartSinglePlayer) 
                (image 208 48 "images/buttonSinglePlayer.png")
                (image 208 48 "images/buttonSinglePlayer.png")
                (image 208 48 "images/buttonSinglePlayer.png") |> withMargin (1, 3)
            , customButton (message clickMailbox.address StartRemoteGameButton) 
                (image 208 48 "images/button2PlayerOnline.png")
                (image 208 48 "images/button2PlayerOnline.png")
                (image 208 48 "images/button2PlayerOnline.png") |> withMargin (1, 3)
            , customButton (message clickMailbox.address StartTwoPlayerHotseat) 
                (image 208 48 "images/button2PlayerHotseat.png")
                (image 208 48 "images/button2PlayerHotseat.png")
                (image 208 48 "images/button2PlayerHotseat.png") |> withMargin (1, 3)
            , image 208 48 "images/buttonViewRules.png" |> withMargin (1, 3)
            ] |> withMargin (95, 35)

renderLog : State -> Element
renderLog state =
  flow down [ Log.display (380, 168) state.log |> container 380 220 midTop
            , if state.gameState == GameOver
              then button (message clickMailbox.address MainMenuButton) "Main Menu" |> container 380 40 middle
              else spacer 380 40
            ]

renderPieceDescription : Piece -> Element
renderPieceDescription piece = 
  flow down [ spacer 1 10
            , flow down [ Piece.toDisplayString piece |> fromString |> Text.height 40 |> leftAligned |> width 370 |> withMargin (5, 1)
                        , Piece.flavorText piece |> fromString |> Text.height 16 |> leftAligned |> width 320 |> withMargin (30, 1)
                        ] |> container 380 158 topLeft
            , collage 380 46 [ traced {defaultLine | width <- 2, color <- darkGrey} <| segment (-170.0, 0.0) (170.0, 0.0) ]
            , Piece.rulesText piece |> fromString |> Text.height 18 |> leftAligned |> width 360 |> container 360 80 topLeft |> withMargin (10, 1)
            ]

renderRemoteSetupMenu : Content -> Element
renderRemoteSetupMenu playerName = 
  flow down [ fromString "Enter your name" |> centered |> container 300 30 middle
            , field Graphics.Input.Field.defaultStyle (message playerNameMailbox.address) "Your name" playerName
            , button (message clickMailbox.address StartTwoPlayerOnline) "Start"
            ]

renderRemoteConnecting : Element
renderRemoteConnecting = fromString "Waiting for opponent ..." |> centered |> container 300 30 middle