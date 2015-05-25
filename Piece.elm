module Piece where

import GameTypes exposing (Piece (..))

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

flavorText : Piece -> String
flavorText piece =
  case piece of
    Vainamoinen -> "V&auml;in&auml;m&ouml;inen, the central character of the Kalevala, was a shamanistic hero with a magical power of song and music."
    Ukko -> "Ukko, the god of the sky, created lightning with his hammer Ukonvasara and caused thunderstorms by driving his chariot through the skies."
    Kullervo -> "Kullervo grew up thinking his family was dead, amongst his people's murderers, and killed himself after unknowingly seducing his sister."
    Kaarme -> "Plowing a field of poisonous snakes (k&auml;&auml;rmeit&auml;) was the first task that Seppo Ilmarinen had to perform to marry Louhi's daughter."
    Joukahainen -> "After losing a contest, the arrogant archer Joukahainen pledged his sister Aino to V&auml;in&auml;m&ouml;inen, but she drowned herself rather than marry him."
    SeppoIlmarinen -> "Seppo Ilmarinen, the Eternal Hammerer, was an immortal blacksmith who was capable of creating practically anything, but unlucky in love."
    Louhi -> "Louhi was a powerful witch with the ability to change shape, and the main opponent of V&auml;in&auml;m&ouml;inen in the battle for the magical artifact Sampo."
    Lemminkainen -> "After Lemmink&auml;inen drowned in the underworld, his mother sewed his body together and restored him to life with ointment from Ukko's halls."

rulesText : Piece -> String
rulesText piece =
  case piece of
    Vainamoinen -> "8 - No special rules."
    Ukko -> "7 - No special rules."
    Kullervo -> "6 - Only other Kullervos may be placed next to this tile."
    Kaarme -> "5 - May be placed on top of other tiles (except other K&auml;&auml;rmes)."
    Joukahainen -> "4 - Value is the sum of all Joukahainen tiles in the same row or column."
    SeppoIlmarinen -> "3 - You may exchange it with any tile on the table (except other Seppo Ilmarinens)."
    Louhi -> "2 - Automatically scores when there are Louhis on both ends of a line."
    Lemminkainen -> "1 - All adjacent tiles (except other Lemmink&auml;inens) have value 0."
