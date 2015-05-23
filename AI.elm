module AI where

import Dict
import List exposing (..)

import Helpers exposing (..)
import GameTypes exposing (..)
import Piece
import Player
import Board

getMove : State -> Maybe Move
getMove state =
  if List.isEmpty (Player.getHand state.turn state)
  then Nothing
  else
    let hand = Player.getHand state.turn state
        idxs = [0..(List.length hand)-1]

        boardSize = Board.getBoardSize state.board
        xs = map (\x -> (x - (boardSize // 2))) [0..(boardSize - 1)]
        locations = concatMap (\x -> (map (\y -> (x, y)) xs)) xs

        pieceAtIdx i = Piece.fromString (hand !! i)
        validLocsByPiece piece = List.filter (\loc -> Board.isValidMove { piece = piece, idx = 0, location = loc } state.board) locations
        validMoves = concatMap (\i -> let piece = pieceAtIdx i
                                          move loc = { piece = piece, idx = i, location = loc}
                                          boardAfterMoveTo loc = Dict.insert loc piece state.board
                                          moveWithScore loc = (move loc, Board.scoreMove (move loc) (boardAfterMoveTo loc))
                                      in
                                        map moveWithScore (validLocsByPiece piece)) idxs
        (bestMove, _) = sortBy snd validMoves |> reverse
                                            --|> Debug.watch "best moves"
                                              |> headU
    in
      Just bestMove
