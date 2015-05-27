module Log where

import Color exposing (Color)
import Graphics.Element as Element exposing (..)
import List exposing (..)
import Text exposing (..)

import GameTypes exposing (..)
import Decorate exposing (decorate)
import Player

empty : Log
empty = []

singleton : String -> Color -> Log
singleton str color = [(color, str)]

addPlayerMsg : String -> Player -> Log -> Log
addPlayerMsg str player log = 
  (Player.toColor player, str) :: log

addSystemMsg : String -> Log -> Log
addSystemMsg str log = 
  (Color.darkGrey, str) :: log

display : (Int, Int) -> Log -> Element
display (width, height) log =
  let widthMinusSidebar = width - 16
      entryToElt (color, text) = fromString text |> Text.color color |> leftAligned |> Element.width widthMinusSidebar
      fullLog = map entryToElt log |> flow down |> Element.width width
  in
    if Element.heightOf fullLog > height
    then fullLog |> Element.height height
                 |> decorate ("style", "overflow-y: scroll; overflow-x: hidden;")
    else fullLog