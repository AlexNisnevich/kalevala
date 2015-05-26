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
display (w, h) log =
  let fullLog = log |> map (\(c, t) -> fromString t |> Text.color c |> leftAligned |> width w )
                    |> flow down
  in
    if Element.heightOf fullLog > h
    then fullLog |> Element.height h
                 |> decorate ("style", "overflow-y: scroll; overflow-x: hidden;")
    else fullLog