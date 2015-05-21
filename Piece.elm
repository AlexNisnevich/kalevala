module Piece where

import GameTypes (Piece (..))

fromString : String -> Piece
fromString str =
  case str of
    "Vain" -> Vainamoinen
    "Ukko" -> Ukko
    "Kullervo" -> Kullervo
    "Kaarme" -> Kaarme
    "Jouk" -> Joukahainen
    "Ilmar" -> SeppoIlmarinen
    "Louhi" -> Louhi
    "Lemmi" -> Lemminkainen

toString : Piece -> String
toString piece =
  case piece of
    Vainamoinen -> "Vain"
    Ukko -> "Ukko"
    Kullervo -> "Kullervo"
    Kaarme -> "Kaarme"
    Joukahainen -> "Jouk"
    SeppoIlmarinen -> "Ilmar"
    Louhi -> "Louhi"
    Lemminkainen -> "Lemmi"

toDisplayString : Piece -> String
toDisplayString piece =
  case piece of
    Vainamoinen -> "V&auml;in&auml;m&ouml;inen"
    Ukko -> "Ukko"
    Kullervo -> "Kullervo"
    Kaarme -> "K&auml;&auml;rme"
    Joukahainen -> "Joukahainen"
    SeppoIlmarinen -> "Seppo Ilmarinen"
    Louhi -> "Louhi"
    Lemminkainen -> "Lemmink&auml;inen"

baseValue : Piece -> Int
baseValue piece =
  case piece of
    Vainamoinen -> 8
    Ukko -> 7
    Kullervo -> 6
    Kaarme -> 5
    Joukahainen -> 4
    SeppoIlmarinen -> 3
    Louhi -> 2
    Lemminkainen -> 1
