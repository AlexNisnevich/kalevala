module Log where

import Color exposing (Color)
import Graphics.Element as Element exposing (..)
import List exposing (..)
import Text exposing (..)

import Decorate exposing (decorate)

type alias Log = List (Color, String)

empty : Log
empty = []

singleton : String -> Color -> Log
singleton str color = [(color, str)]

add : String -> Color -> Log -> Log
add str color log = (color, str) :: log

display : (Int, Int) -> Log -> Element
display (width, height) log =
  let widthMinusSidebar = width - 20
      entryToElt (color, text) = fromString text |> Text.color color |> leftAligned |> Element.width widthMinusSidebar
      fullLog = map entryToElt log |> flow down |> Element.width width
  in
    if Element.heightOf fullLog > height
    then fullLog |> Element.height height
                 |> decorate ("style", "overflow-y: scroll; overflow-x: hidden;")
    else fullLog