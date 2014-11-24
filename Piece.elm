module Piece where

import GameTypes (..)

fromString : String -> Piece
fromString str =
  case str of
    "Odin" -> Odin
    "Thor" -> Thor
    "Troll" -> Troll
    "Dragon" -> Dragon
    "Fenrir" -> Fenrir
    "Skadi" -> Skadi
    "Valkyrie" -> Valkyrie
    "Loki" -> Loki

toString : Piece -> String
toString piece =
  case piece of
    Odin -> "Odin"
    Thor -> "Thor"
    Troll -> "Troll"
    Dragon -> "Dragon"
    Fenrir -> "Fenrir"
    Skadi -> "Skadi"
    Valkyrie -> "Valkyrie"
    Loki -> "Loki"

baseValue : Piece -> Int
baseValue piece =
  case piece of
    Odin -> 8
    Thor -> 7
    Troll -> 6
    Dragon -> 5
    Fenrir -> 4
    Skadi -> 3
    Valkyrie -> 2
    Loki -> 1
