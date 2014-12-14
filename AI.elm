module AI where

import List
import List (..)
import Dict

import Helpers (..)
import GameTypes (..)
import Piece
import Player
import Board (getBoardSize, isValidMove, scoreMove)

import Debug

getMove : State -> Maybe Move
getMove state =
  if List.isEmpty (Player.getHand state.turn state)
  then Nothing
  else
    let p = playerName state.turn
        hand = Player.getHand state.turn state
        idxs = [0..(List.length hand)-1]

        boardSize = getBoardSize state.board
        xs = map (\x -> (x - (boardSize // 2))) [0..(boardSize - 1)]
        locations = concatMap (\x -> (map (\y -> (x, y)) xs)) xs

        pieceAtIdx i = Piece.fromString (hand !! i)
        validLocationsByPiece piece = List.filter (\loc -> isValidMove { piece = piece, idx = 0, location = loc } state.board) locations
        validMoves = concatMap (\i -> let piece = pieceAtIdx i
                                          move loc = { piece = piece, idx = i, location = loc}
                                          boardAfterMoveTo loc = Dict.insert loc piece state.board
                                          moveWithScore loc = (move loc, scoreMove (move loc) (boardAfterMoveTo loc))
                                      in
                                        map moveWithScore (validLocationsByPiece piece)) idxs
        (bestMove, _) = sortBy snd validMoves |> reverse
                                            --|> Debug.watch "best moves"
                                              |> head
    in
      Just bestMove
