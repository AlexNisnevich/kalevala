module Display.Helpers where

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
import Player
import Game
import Display.Constants exposing (..)

withMargin : (Int, Int) -> Element -> Element
withMargin (x, y) elt = withBorder (x, y) transparent elt

withBorder : (Int, Int) -> Color -> Element -> Element
withBorder (x, y) color elt =
  flow down [ spacer (widthOf elt + x * 2) y |> Element.color color
            , flow right [ spacer x (heightOf elt) |> Element.color color
                         , elt
                         , spacer x (heightOf elt) |> Element.color color
                         ]
            , spacer (widthOf elt + x * 2) y |> Element.color color
            ]

pieceToImage: Piece -> String -> Float -> Element
pieceToImage piece value tileSize =
  let imgSize = if tileSize > 75 then 100 else 50
      imgPath = "images/" ++ toString imgSize ++ "/" ++ Piece.toString piece ++ "-" ++ value ++ ".png"
  in
    image (round tileSize) (round tileSize) imgPath